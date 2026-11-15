module ParserTokenAtomSpec (parserTokenAtomTests) where

import CGrep.Common (getTargetContents, sliceToMaxIndex)
import CGrep.FileType (FileType (..))
import CGrep.FileTypeMapTH (fileTypeInfoLookup, mkContextFilterFn)
import CGrep.Line (
    buildIndex,
    getLineByNumber,
    getLineByOffset',
    lookupLineAndPosition,
    totalLines,
 )
import CGrep.Parser.Atom (
    extractAtomMatchers,
    findAllMatches,
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
import CGrep.Text (firstIndex, textIndices)
import Control.Monad (forM_, when)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import Options (Options (..), defaultOptions)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.OsPath (unsafeEncodeUtf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestHelpers (
    allTokensFilter,
    lexSource,
    lookupFTInfo,
    matchLineNums,
    matchLines,
    runSearchMem,
 )

parserTokenAtomTests :: TestTree
parserTokenAtomTests =
    testGroup
        "Parser.Token, Parser.Atom, Line & Text corner cases"
        [ tokenLexerEdgeCases
        , atomMatcherEdgeCases
        , lineAndSliceUtf8EdgeCases
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
                let info = fileTypeInfoLookup defaultOptions osp
                    fti = snd <$> info
                    cf = mkContextFilterFn (fst <$> info) (contextFilterAll ~! contextBitComment) True
                    filtered = cf txt
                    fastChunks = map chunkSig (parseChunks fti filtered)
                    refChunks = map chunkSig (parseChunksRef fti filtered)
                when (fastChunks /= refChunks) $
                    assertFailure ("parseChunks mismatch in " <> fname)
                forM_ ([False, True] :: [Bool]) $ \isStrict ->
                    forM_ tfilters $ \(label, tf) -> do
                        let fastToks = map tokSig (parseTokens tf fti isStrict filtered)
                            refToks = map tokSig (parseTokensRef tf fti isStrict filtered)
                        when (fastToks /= refToks) $
                            assertFailure
                                ("parseTokens mismatch in " <> fname <> " (" <> label <> ", strict=" <> show isStrict <> ")")
        ]
