module ParserTokenAtomSpec (parserTokenAtomTests) where

import CGrep.Common (
    RawPrefilter (..),
    getTargetContents,
    getTargetContentsRef,
    ignoreCase,
    ignoreCaseRef,
    readTargetFiltered,
    readTargetFilteredWith,
    sliceToMaxIndex,
 )
import CGrep.Distance (approxEqRef, matchEditPattern, mkEditPattern)
import CGrep.FileType (FileType (..))
import CGrep.FileTypeMapTH (fileTypeInfoLookup, mkContextFilterFn)
import CGrep.Search (buildRawPrefilterLookup, getSearcher, normalizePatterns)
import CGrep.Line (
    buildIndex,
    getLineByNumber,
    getLineByOffset',
    getLineOffsets,
    getLineOffsetsRef,
    lookupLineAndPosition,
    totalLines,
 )
import CGrep.Match (Match (..), putMatches)
import CGrep.Parser.Atom (
    extractAtomMatchers,
    findAllMatches,
    findAllMatchesRef,
    mkAtomsFromTokens,
 )
import CGrep.Parser.Chunk (
    Chunk (..),
    ChunkType,
    cOffset,
    parseChunks,
    parseChunksRef,
    pattern ChunkBracket,
    pattern ChunkDigit,
    pattern ChunkIdentifier,
    pattern ChunkOperator,
    pattern ChunkString,
 )
import CGrep.Parser.Token (
    TokenFilter (..),
    parseTokens,
    parseTokensRef,
    tOffset,
    tToken,
    tTyp,
 )
import CGrep.Semantic.ContextFilter (
    contextBitComment,
    contextFilterAll,
    endLiteralMarker,
    startLiteralMarker,
    (~!),
 )
import qualified CGrep.Strategy.Regex as Regex
import CGrep.Text (firstIndex, textIndices, textIndicesRef)
import Config (defaultConfig)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import Options (Options (..), defaultOptions)
import Reader (Env (..))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, getTemporaryDirectory, listDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestHelpers (
    allTokensFilter,
    lexSource,
    lookupFTInfo,
    matchChunkTexts,
    matchLineNums,
    matchLines,
    runSearchFile,
    runSearchMem,
 )

parserTokenAtomTests :: TestTree
parserTokenAtomTests =
    testGroup
        "Parser.Token, Parser.Atom, Line & Text corner cases"
        [ tokenLexerEdgeCases
        , atomMatcherEdgeCases
        , lineAndSliceUtf8EdgeCases
        , nonUtf8SourceFileTests
        , corpusLexerEquivalenceTests
        ]

tokenLexerEdgeCases :: TestTree
tokenLexerEdgeCases =
    testGroup
        "Token lexer (CGrep.Parser.Token) edge cases"
        [ testCase "Empty and whitespace-only inputs yield [] (no phantom -1 token)" $ do
            let info = snd <$> lookupFTInfo C
            parseTokens allTokensFilter info False "" @?= []
            parseTokens allTokensFilter info True "" @?= []
            parseTokens allTokensFilter info False "   \n\t  \r\n " @?= []
        , testCase "Empty string literals \"\" and '' produce valid non-negative byte offsets on exact line" $ do
            let src = "int a = 1;\nint b = 2;\nconst char *s = \"\";\n"
                toks = lexSource C defaultOptions False src
                lindex = buildIndex src
                strToks = [t | t <- toks, tTyp t == ChunkString]
            length strToks @?= 1
            case strToks of
                (st : _) -> do
                    assertBool "tOffset must be >= 0" (tOffset st >= 0)
                    let (# lineNo, colNo #) = lookupLineAndPosition lindex (tOffset st)
                    lineNo @?= 3
                    -- Content slice starts immediately after opening '"' at column 17 -> col 18
                    colNo @?= 18
                    tToken st @?= ""
                [] -> assertBool "expected string token" False
        , testCase "Empty single-quoted literal '' in Python has valid offset and matches line 2" $ do
            let src = "x = 1\ny = ''\n"
            ms <- runSearchMem defaultOptions{semantic = True} Python "a.py" src ["y = STR"]
            matchLineNums ms @?= [2]
        , testCase "Operator immediately preceding string literal (label=\"default\") emits ChunkOperator, not ChunkBracket" $ do
            let src = "label=\"default\""
                toks = lexSource Python defaultOptions False src
                typed :: [(ChunkType, T.Text)]
                typed = map (\t -> (tTyp t, tToken t)) toks
            typed
                @?= [ (ChunkIdentifier, "label")
                    , (ChunkOperator, "=")
                    , (ChunkString, "default")
                    ]
        , testCase "Compound operators adjacent to string literals (!=\"foo\", :='bar') emit ChunkOperator (non-strict & strict)" $ do
            let toksNonStrict = lexSource C defaultOptions False "x!=\"foo\""
                toksStrict = lexSource C defaultOptions True "x!=\"foo\""
                toksGoStrict = lexSource Go defaultOptions True "s:=\"bar\""
            map tTyp toksNonStrict @?= [ChunkIdentifier, ChunkOperator, ChunkOperator, ChunkString]
            map tTyp toksStrict @?= [ChunkIdentifier, ChunkOperator, ChunkString]
            map tTyp toksGoStrict @?= [ChunkIdentifier, ChunkOperator, ChunkString]
        , testCase "Bracket immediately preceding string literal (f(\"abc\")) still emits ChunkBracket" $ do
            let toks = lexSource C defaultOptions False "f(\"abc\")"
            map tTyp toks @?= [ChunkIdentifier, ChunkBracket, ChunkString, ChunkBracket]
        , testCase "Semantic pattern '_ = \"default\"' matches spaceless assignment label=\"default\"" $ do
            let src = "x = 1\nlabel=\"default\"\n"
            ms <- runSearchMem defaultOptions{semantic = True} Python "a.py" src ["_ = \"default\""]
            matchLineNums ms @?= [2]
        , testCase "Unterminated string literal at EOF does not crash or produce out-of-bounds slice" $ do
            let src = "const char *s = \"unterminated"
                toks = lexSource C defaultOptions False src
            assertBool "all token offsets within input byte length" $
                all (\t -> tOffset t >= 0 && tOffset t <= TU.lengthWord8 src) toks
        ]

atomMatcherEdgeCases :: TestTree
atomMatcherEdgeCases =
    testGroup
        "Semantic Atom & Text needle edge cases"
        [ testCase "textIndices and firstIndex safely handle empty needles without throwing" $ do
            textIndices [""] "hello world" @?= [[]]
            textIndices ["", "world"] "hello world" @?= [[], [6]]
            firstIndex "" "hello world" @?= Nothing
        , testCase "Empty string semantic pattern (-S '\"\"') matches only empty string literals" $ do
            let patToks = parseTokens allTokensFilter (snd <$> lookupFTInfo C) False (T.pack [startLiteralMarker, endLiteralMarker])
                atoms = mkAtomsFromTokens patToks
            extractAtomMatchers atoms @?= []
            let src = "a = \"hello\"\nb = \"\"\nc = \"world\"\n"
            ms <- runSearchMem defaultOptions{semantic = True} Python "a.py" src ["\"\""]
            matchLineNums ms @?= [2]
        , testCase "HEX wildcard matches both 0x... and uppercase 0X... literals" $ do
            let src = "int a = 0x1A;\nint b = 0XFF;\nint c = 42;\n"
            ms <- runSearchMem defaultOptions{semantic = True} C "a.c" src ["int _ = HEX"]
            matchLineNums ms @?= [1, 2]
        , testCase "Numbered placeholder unification (_1 == _1) respects both case-sensitive and -i modes" $ do
            let src = "bool a = (foo == foo);\nbool b = (foo == FOO);\nbool c = (foo == bar);\n"
            msSens <- runSearchMem defaultOptions{semantic = True} C "a.c" src ["_1 == _1"]
            msInsens <- runSearchMem defaultOptions{semantic = True, ignore_case = True} C "a.c" src ["_1 == _1"]
            matchLineNums msSens @?= [1]
            matchLineNums msInsens @?= [1, 2]
        , testCase "Combinators NOT, OR, and bracket-balanced Ellipsis (...) work together" $ do
            let src =
                    T.unlines
                        [ "call(a, \"skip\");"
                        , "call(a, 123);"
                        , "alt(x);"
                        , "wrap(f(1, 2), end_marker);"
                        , "wrap(a; end_marker);"
                        ]
            msNot <- runSearchMem defaultOptions{semantic = True} C "a.c" src ["call(_, NOT STR)"]
            msOr <- runSearchMem defaultOptions{semantic = True} C "a.c" src ["call OR alt(_)"]
            msEllipsis <- runSearchMem defaultOptions{semantic = True} C "a.c" src ["wrap(..., end_marker)"]
            matchLineNums msNot @?= [2]
            matchLineNums msOr @?= [3]
            -- Line 4 matches nested f(1, 2); line 5 has ';' at ellipsis depth 0 and is rejected
            matchLineNums msEllipsis @?= [4]
            let topSrc = lexSource C defaultOptions False "start_tok = 1; mid = 2; end_tok = 3;"
                topPat = mkAtomsFromTokens (lexSource C defaultOptions False "start_tok ... end_tok")
            -- Top-level ';' at depth 0 blocks '...' from crossing statements:
            findAllMatches defaultOptions [topPat] topSrc @?= []
        , testCase "Escaped wildcard identifiers (_ANY, _STR, _NOT, _OR, __1) match literal names instead of acting as wildcards" $ do
            let src = "int ANY = 1;\nint other = 1;\nint NOT = 2;\nint _1 = 3;\n"
            msAny <- runSearchMem defaultOptions{semantic = True, word_match = True} C "a.c" src ["int _ANY = 1"]
            msNot <- runSearchMem defaultOptions{semantic = True, word_match = True} C "a.c" src ["int _NOT = 2"]
            msPh <- runSearchMem defaultOptions{semantic = True, word_match = True} C "a.c" src ["int __1 = 3"]
            matchLineNums msAny @?= [1]
            matchLineNums msNot @?= [3]
            matchLineNums msPh @?= [4]
        , testCase "Multi-line empty-string semantic pattern ('x = \"\"') does not prematurely slice at line 1" $ do
            let src = "x =\n  \"\"\ny = \"nonempty\"\n"
            ms <- runSearchMem defaultOptions{semantic = True} Python "a.py" src ["x = \"\""]
            matchLineNums ms @?= [1, 2]
        , testCase "Leading-dot floating literal (.5) vs member access (.field) in both parseTokens and parseChunks" $ do
            let toks = lexSource C defaultOptions False "x = .5 + s.field;"
            [(tTyp t, tToken t) | t <- toks]
                @?= [ (ChunkIdentifier, "x")
                    , (ChunkOperator, "=")
                    , (ChunkDigit, ".5")
                    , (ChunkOperator, "+")
                    , (ChunkIdentifier, "s")
                    , (ChunkOperator, ".")
                    , (ChunkIdentifier, "field")
                    , (ChunkOperator, ";")
                    ]
            let chunks = parseChunks (snd <$> lookupFTInfo C) "x = .5 + s.field;"
            map cToken chunks @?= ["x", "=", ".5", "+", "s", ".", "field", ";"]
        ]

lineAndSliceUtf8EdgeCases :: TestTree
lineAndSliceUtf8EdgeCases =
    testGroup
        "LineIndex & sliceToMaxIndex UTF-8 byte-offset edge cases"
        [ testCase "getLineByOffset' returns full last line (no trailing newline) after multi-byte UTF-8 lines" $ do
            let src = "# © € 日本語 🦀\nresult = 42"
                lindex = buildIndex src
            case firstIndex "\n" src of
                Just nlOff -> getLineByOffset' lindex (nlOff + 1) @?= "result = 42"
                Nothing -> assertFailure "expected newline in src"
        , testCase "getLineByOffset' preserves multi-byte UTF-8 characters on final line without trailing newline" $ do
            let src = "// header ©\nconst char *euro = \"€ 100 🦀\";"
                lindex = buildIndex src
            case firstIndex "\n" src of
                Just nlOff -> getLineByOffset' lindex (nlOff + 5) @?= "const char *euro = \"€ 100 🦀\";"
                Nothing -> assertFailure "expected newline in src"
        , testCase "End-to-end search on file without trailing newline after UTF-8 comment returns full line" $ do
            let src = "// αβγδε €€€ 🦀🦀\nint final_answer = 42;"
            ms <- runSearchMem defaultOptions{semantic = True} C "utf8.c" src ["final_answer = 42"]
            matchLineNums ms @?= [2]
            matchLines ms @?= ["int final_answer = 42;"]
        , testCase "sliceToMaxIndex slices at exact newline byte offset in presence of 2/3/4-byte UTF-8" $ do
            let line1 = "// © αβ € 語 🦀"
                line2 = "int target = 1; // Ω € 🚀"
                line3 = "int trailing = 2;"
                src = line1 <> "\n" <> line2 <> "\n" <> line3
                indices = textIndices ["target"] src
                sliced = sliceToMaxIndex indices src
            sliced @?= (line1 <> "\n" <> line2)
        , testCase "BoyerMoore suffix_match (-s) computes exact Word8 offsets on multi-byte UTF-8 lines" $ do
            let src = "// © € 🦀\nint my_target = 1; // αβγ\nint target_prefix = 2;\n"
                opt = defaultOptions{suffix_match = True}
            ms <- runSearchMem opt C "utf8.c" src ["target"]
            matchLineNums ms @?= [2]
            matchLines ms @?= ["int my_target = 1; // αβγ"]
        , testCase "BoyerMoore word_match (-w) splits words cleanly in ftIdentCharSet = Nothing formats (Text, Yaml, Conf, Assembly)" $ do
            let optW = defaultOptions{word_match = True}
                txtSrc = "in init/main.c.\ndomain mainly\ncompatible = \"ti,keystone,main-pll-clock\";\n"
                mdSrc = "where the `main()` function runs\nmainly ignored\n"
                yamlSrc = "- ti,am654-navss-main-udmap\n- domain-udmap\n"
                confSrc = "#access_log logs/access.log main;\ndomain;\n"
                asmSrc = ".asciz \"main() returned\"\n.asciz \"domain\"\n"
            msTxt <- runSearchMem optW Text "changes.txt" txtSrc ["main"]
            msMd <- runSearchMem optW Text "README.md" mdSrc ["main"]
            msYaml <- runSearchMem optW Yaml "k3.yaml" yamlSrc ["main"]
            msConf <- runSearchMem optW Conf "nginx.conf" confSrc ["main"]
            msAsm <- runSearchMem optW Assembly "locore.S" asmSrc ["main"]
            matchLineNums msTxt @?= [1, 3]
            matchLineNums msMd @?= [1]
            matchLineNums msYaml @?= [1]
            matchLineNums msConf @?= [1]
            matchLineNums msAsm @?= [1]
        , testCase "totalLines, getLineByNumber, and invert_match (-v) handle empty, trailing-newline, and UTF-8 files accurately" $ do
            totalLines (buildIndex "") @?= 0
            totalLines (buildIndex "only_one") @?= 1
            totalLines (buildIndex "a\nb\n") @?= 2
            totalLines (buildIndex "a\nb") @?= 2
            let idx = buildIndex "first ©\nsecond €\nthird 🦀\n"
            getLineByNumber idx 1 @?= "first ©"
            getLineByNumber idx 2 @?= "second €"
            getLineByNumber idx 3 @?= "third 🦀"
            -- Invert match (-v) must return individual original lines (preserving comments), never the whole file or phantom EOF line
            let cSrc = "int a = 1; // keep comment\nint drop_me = 2;\nint c = 3;\n"
            msInv <- runSearchMem defaultOptions{semantic = True, code = True, invert_match = True} C "a.c" cSrc ["drop_me"]
            matchLineNums msInv @?= [1, 3]
            matchLines msInv @?= ["int a = 1; // keep comment", "int c = 3;"]
            msEmptyInv <- runSearchMem defaultOptions{invert_match = True} C "empty.c" "" ["anything"]
            msEmptyInv @?= []
        ]

nonUtf8SourceFileTests :: TestTree
nonUtf8SourceFileTests =
    testGroup
        "Non-UTF-8 source file decoding resilience (Latin-1 / CP1252 / GBK bytes)"
        [ testCase "getTargetContents & semantic/literal/regex/fuzzy searches handle 0xD5, 0xA9, 0xB5 bytes without failing" $ do
            sysTmp <- getTemporaryDirectory
            let tmpDir = sysTmp </> "cgrep-non-utf8-spec"
            bracket (createDirectoryIfMissing True tmpDir >> pure tmpDir) removePathForcibly $ \dir -> do
                let cppFile = dir </> "micro_thread.cpp"
                    cFile = dir </> "ncsw_mem.c"
                    -- Raw GBK lead byte 0xD5 in comment + 0xB5 (µs) + 0xA9 (©) on same line before `main`
                    cppBytes =
                        BS.concat
                            [ "// GBK comment byte: "
                            , BS.pack [0xD5, 0x50]
                            , " and delay 10 "
                            , BS.pack [0xB5]
                            , "s\n"
                            , "const char *tag = \"raw "
                            , BS.pack [0xD5, 0xA9]
                            , "\";\n"
                            , "/* Copyright "
                            , BS.pack [0xA9]
                            , " 2025 */ int main(int argc, char **argv) { return 0; }\n"
                            ]
                    cBytes =
                        BS.concat
                            [ "/* Copyright "
                            , BS.pack [0xA9]
                            , " Freescale */\n"
                            , "int helper(void) { return 1; }\n"
                            , "int main(void) { return helper(); }\n"
                            ]
                BS.writeFile cppFile cppBytes
                BS.writeFile cFile cBytes

                msCppSem <- runSearchFile defaultOptions{semantic = True} cppFile ["main"]
                matchLineNums msCppSem @?= [3]
                matchChunkTexts msCppSem @?= [["main"]]

                msCSem <- runSearchFile defaultOptions{semantic = True} cFile ["main"]
                matchLineNums msCSem @?= [3]

                msCppBm <- runSearchFile defaultOptions{code = True} cppFile ["main"]
                matchLineNums msCppBm @?= [3]

                msCppRx <- runSearchFile defaultOptions{regex_posix = True, code = True} cppFile ["int[[:space:]]+main"]
                matchLineNums msCppRx @?= [3]

                msCppLev <- runSearchFile defaultOptions{edit_dist = True, code = True} cppFile ["mian"]
                matchLineNums msCppLev @?= [3]

                -- Verify forced mmap (threshold = 1 byte) + RawPrefilter on non-UTF-8 & empty files
                mHitMmap <- readTargetFilteredWith 1 (RawRequireAnyCS ["main"]) (unsafeEncodeUtf cppFile)
                mMissMmap <- readTargetFilteredWith 1 (RawRequireAnyCS ["__absent__"]) (unsafeEncodeUtf cppFile)
                assertBool "forced mmap hit must return Just" (mHitMmap /= Nothing)
                mMissMmap @?= Nothing

                let emptyFile = dir </> "empty.c"
                BS.writeFile emptyFile BS.empty
                emptyTxt <- getTargetContents (unsafeEncodeUtf emptyFile)
                emptyTxt @?= ""
                emptyMiss <- readTargetFilteredWith 1 (RawRequireAnyCS ["main"]) (unsafeEncodeUtf emptyFile)
                emptyMiss @?= Nothing

                -- Verify colored output rendering (column + byte slicing after U+FFFD replacement) succeeds cleanly
                let colorOpt = defaultOptions{semantic = True, color = True}
                rendered <- runReaderT (putMatches msCppSem) (Env defaultConfig colorOpt)
                assertBool "expected rendered output builder" (rendered /= Nothing)
        ]

corpusLexerEquivalenceTests :: TestTree
corpusLexerEquivalenceTests =
    testGroup
        "Real-world 31-file corpus equivalence for parseTokens and parseChunks"
        [ testCase "All 31 open-source corpus files match parseTokensRef & parseChunksRef on type, Word8 offset, and slice" $ do
            let dir = "bench" </> "corpus"
            exists <- doesDirectoryExist dir
            assertBool "bench/corpus directory must exist" exists
            files <- filter (/= "SOURCES.md") <$> listDirectory dir
            assertBool "expected at least 31 corpus files" (length files >= 31)
            let identOnlyFilter = TokenFilter True False False False False False False
                kwStrFilter = TokenFilter False True True True False False False
                tfilters :: [(String, TokenFilter)]
                tfilters =
                    [ ("allTokens", allTokensFilter)
                    , ("identOnly", identOnlyFilter)
                    , ("kwAndString", kwStrFilter)
                    ]
                tokSig t = (tTyp t, tOffset t, tToken t)
                chunkSig c = (cTyp c, cOffset c, cToken c)
            forM_ files $ \fname -> do
                let relPath = dir </> fname
                    osp = unsafeEncodeUtf relPath
                txt <- getTargetContents osp
                refTxt <- getTargetContentsRef osp
                when (txt /= refTxt) $
                    assertFailure ("getTargetContents (adaptive mmap) /= getTargetContentsRef in " <> fname)
                let info = fileTypeInfoLookup defaultOptions osp
                    fti = snd <$> info
                    cf = mkContextFilterFn (fst <$> info) (contextFilterAll ~! contextBitComment) True
                    filtered = cf txt
                    fastChunks = map chunkSig (parseChunks fti filtered)
                    refChunks = map chunkSig (parseChunksRef fti filtered)
                when (fastChunks /= refChunks) $
                    assertFailure ("parseChunks mismatch in " <> fname)
                when (getLineOffsets txt /= getLineOffsetsRef txt) $
                    assertFailure ("getLineOffsets mismatch in " <> fname)
                let optIC = defaultOptions{ignore_case = True}
                when (ignoreCase optIC txt /= ignoreCaseRef optIC txt) $
                    assertFailure ("ignoreCase mismatch in " <> fname)
                let probeNeedles :: [T.Text]
                    probeNeedles = ["\n", "a", "if", "return", "==", "__cgrep_absent__"]
                when (textIndices probeNeedles txt /= textIndicesRef probeNeedles txt) $
                    assertFailure ("textIndices mismatch in " <> fname)
                let sampleChunks = take 400 (parseChunks fti txt)
                    editProbes :: [T.Text]
                    editProbes = ["return", "value", "options"]
                forM_ editProbes $ \epTxt -> do
                    let ep = mkEditPattern epTxt
                        epStr = T.unpack epTxt
                        fastHits = map cOffset (filter (matchEditPattern ep . cToken) sampleChunks)
                        refHits = map cOffset (filter (\c -> approxEqRef epStr (T.unpack (cToken c))) sampleChunks)
                    when (fastHits /= refHits) $
                        assertFailure ("EditPattern mismatch in " <> fname <> " for " <> epStr)
                forM_ ([False, True] :: [Bool]) $ \isStrict ->
                    forM_ tfilters $ \(label, tf) -> do
                        let fastToks = map tokSig (parseTokens tf fti isStrict filtered)
                            refToks = map tokSig (parseTokensRef tf fti isStrict filtered)
                        when (fastToks /= refToks) $
                            assertFailure
                                ("parseTokens mismatch in " <> fname <> " (" <> label <> ", strict=" <> show isStrict <> ")")
                -- Verify compiled Atom matcher & pre-filtered POSIX Regex engine across all 31 corpus files
                let allToks = parseTokens allTokensFilter fti False filtered
                    sampleAtoms =
                        [ mkAtomsFromTokens (parseTokens allTokensFilter fti False "_ = _")
                        , mkAtomsFromTokens (parseTokens allTokensFilter fti False "_1 == _1")
                        , mkAtomsFromTokens (parseTokens allTokensFilter fti False "_ ( ... )")
                        , mkAtomsFromTokens (parseTokens allTokensFilter fti False "ANY OR STR")
                        ]
                forM_ ([defaultOptions{semantic = True}, defaultOptions{semantic = True, ignore_case = True, word_match = True}] :: [Options]) $ \semOpt -> do
                    let fastAtomHits = map tokSig (findAllMatches semOpt sampleAtoms allToks)
                        refAtomHits = map tokSig (findAllMatchesRef semOpt sampleAtoms allToks)
                    when (fastAtomHits /= refAtomHits) $
                        assertFailure ("findAllMatches mismatch in " <> fname)
                lock <- newMVar ()
                let rxOpt = defaultOptions{regex_posix = True, code = True}
                    rxEnv = Env defaultConfig rxOpt
                    rxProbes :: [T.Text]
                    rxProbes = ["return[[:space:]]+[A-Za-z0-9_]+", "(if|while)[[:space:]]*\\("]
                    matchSig m = (mLineNumb m, mLine m, [(cOffset c, cToken c) | c <- mChunks m])
                fastRx <- runReaderT (Regex.search lock info osp txt rxProbes False) rxEnv
                refRx <- runReaderT (Regex.searchRef lock info osp txt rxProbes False) rxEnv
                when (map matchSig fastRx /= map matchSig refRx) $
                    assertFailure ("Regex.search mismatch in " <> fname)
                -- Verify end-to-end buildRawPrefilterLookup + readTargetFiltered matches reference search across modes
                forM_
                    ( [ (defaultOptions, ["return"])
                      , (defaultOptions{ignore_case = True}, ["RETURN"])
                      , (defaultOptions{semantic = True}, ["return ANY"])
                      , (defaultOptions{identifier = True, word_match = True}, ["options"])
                      , (defaultOptions{regex_posix = True, code = True}, ["return[[:space:]]+[A-Za-z0-9_]+"])
                      ] ::
                        [(Options, [T.Text])]
                    )
                    $ \(modeOpt, rawPats) -> do
                        let mEnv = Env defaultConfig modeOpt
                            normPats = normalizePatterns modeOpt rawPats
                            pf = buildRawPrefilterLookup mEnv normPats info
                            srch = getSearcher mEnv
                        mFastTxt <- readTargetFiltered pf osp
                        fastDiskMs <- case mFastTxt of
                            Nothing -> pure []
                            Just t -> runReaderT (srch lock info osp t normPats (strict modeOpt)) mEnv
                        refDiskMs <- runReaderT (srch lock info osp refTxt normPats (strict modeOpt)) mEnv
                        when (map matchSig fastDiskMs /= map matchSig refDiskMs) $
                            assertFailure ("readTargetFiltered disk pipeline mismatch in " <> fname <> " for " <> show rawPats)
        ]
