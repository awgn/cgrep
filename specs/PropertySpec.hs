module PropertySpec (propertyTests) where

import CGrep.Common (
    ignoreCase,
    ignoreCaseRef,
    sliceToLastMatch,
    sliceToMaxIndex,
    textContainsOneOfOpt,
 )
import CGrep.Distance (
    approxEqRef,
    distance,
    distanceRef,
    matchEditPattern,
    mkEditPattern,
    textApproxEq,
 )
import CGrep.FileType (FileType (..))
import CGrep.FileTypeMap (FileTypeInfo (..))
import CGrep.Line (buildIndex, getLineByNumber, getLineOffsets, getLineOffsetsRef, totalLines)
import CGrep.Match (Match (..))
import CGrep.Parser.Atom (
    Atom (..),
    findAllMatches,
    findAllMatchesRef,
    mkAtomsFromTokens,
 )
import CGrep.Parser.Chunk (
    cOffset,
    cToken,
    cTyp,
    parseChunks,
    parseChunksRef,
    pattern ChunkOperator,
 )
import CGrep.Parser.Token (
    Token,
    TokenFilter (..),
    mkTokenIdentifier,
    mkTokenKeyword,
    mkTokenOperator,
    mkTokenString,
    parseTokens,
    parseTokensRef,
    tOffset,
    tToken,
    tTyp,
 )
import CGrep.Semantic.ContextFilter (
    contextBitComment,
    endLiteralMarker,
    mkContextFilter,
    mkParConfig,
    runContextFilter,
    runContextFilterRef,
    startLiteralMarker,
    (~!),
 )
import CGrep.Semantic.Tests (filterTests)
import qualified CGrep.Strategy.Regex as Regex
import CGrep.Text (
    isAsciiText,
    lastIndex,
    textContainsOneOf,
    textEqCI,
    textIndices,
    textIndicesRef,
    textIsInfixOf,
    textIsInfixOfCI,
    textIsPrefixOfCI,
    textIsSuffixOfCI,
 )
import Config (defaultConfig)
import Control.Concurrent.MVar (newMVar)
import Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import Options (Options (..), defaultOptions)
import Reader (Env (..))
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (unsafeEncodeUtf)
import Test.QuickCheck (
    Gen,
    Property,
    chooseInt,
    elements,
    forAll,
    listOf,
    oneof,
    (===),
 )
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..), testProperty)
import TestHelpers (filterSource, lexSource, lookupFTInfo)

propertyTests :: TestTree
propertyTests =
    localOption (QuickCheckTests 200) $
        testGroup
            "Generative QuickCheck property invariants"
            [ testProperty "ContextFilter preserves exact UTF-8 Word8 length and newline count" prop_contextFilterByteLengthAndNewlines
            , localOption (QuickCheckTests 1000) $
                testProperty "ContextFilter fast ST byte engine matches reference unfoldr byte-for-byte" prop_contextFilterMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "parseTokens fast unboxed lexer matches reference STRef+Seq lexer" prop_parseTokensMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "parseChunks fast unboxed lexer matches reference STRef+Seq lexer" prop_parseChunksMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "getLineOffsets SIMD memchr vector matches reference indices+fromList" prop_lineOffsetsMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "ignoreCase fast ASCII+ST engine matches reference T.map toLowerSameWidth" prop_ignoreCaseMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "textIndices fast memchr+bloom engine matches reference Data.Text.Internal.Search.indices" prop_textIndicesMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "distance unboxed single-row DP matches reference lazy diagonal distanceRef" prop_distanceMatchesReference
            , localOption (QuickCheckTests 2000) $
                testProperty "EditPattern Myers 64-bit & Unicode fallback matches reference approxEqRef" prop_editPatternMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "Text CI/infix/lastIndex/sliceToLastMatch/textContainsOneOfOpt match reference implementations" prop_textHelpersMatchReference
            , localOption (QuickCheckTests 1000) $
                testProperty "Atom.findAllMatches compiled matcher matches reference tails+Map findAllMatchesRef" prop_findAllMatchesMatchesReference
            , localOption (QuickCheckTests 1000) $
                testProperty "Regex.search line-prefiltered POSIX engine matches reference full-text TDFA searchRef" prop_regexMatchesReference
            , testProperty "parseTokens produces in-bounds valid UTF-8 slices and zero blanked-char operators" prop_parseTokensValidSlicesAndNoBlankLeaks
            , testProperty "ignoreCase preserves exact UTF-8 Word8 length across arbitrary Unicode" prop_ignoreCasePreservesByteLength
            , testProperty "LineIndex totalLines and getLineByNumber reconstruct lines accurately" prop_lineIndexRoundTrip
            , testProperty "sliceToMaxIndex never splits multi-byte UTF-8 codepoints" prop_sliceToMaxIndexValidUtf8
            , testProperty "filterTests (-T vs --no-tests) never classifies the same token offset as both test and prod" prop_filterTestsDisjoint
            ]

-- | Generator biased toward lexical/comment/quote/escape/UTF-8 corner-case fragments.
genAdversarialText :: Gen T.Text
genAdversarialText = do
    pieces <- listOf genPiece
    pure (T.concat pieces)
  where
    genPiece :: Gen T.Text
    genPiece =
        elements
            [ ""
            , " "
            , "\t"
            , "\0"
            , "\n"
            , "\r\n"
            , "\""
            , "\"\""
            , "\"\"\""
            , "\"\"\"\""
            , "'"
            , "''"
            , "'''"
            , "`"
            , "\\"
            , "\\\\"
            , "\\\""
            , "\\'"
            , "\\€"
            , "\\x41"
            , "'\\x41'"
            , "'\\u{1F980}'"
            , "'€'"
            , "'🦀'"
            , "'a"
            , "//"
            , "/*"
            , "*/"
            , "/+"
            , "+/"
            , "{|"
            , "|}"
            , "#"
            , "#["
            , "$'"
            , "###"
            , "r#\""
            , "\"#"
            , "R\"("
            , ")\""
            , "[==["
            , "]==]"
            , "$a"
            , "foo"
            , "x_1"
            , "42"
            , ".5"
            , "0x1F"
            , "="
            , "!="
            , "();{}"
            , "\x00A0"
            , "\x3000"
            , "\x100000"
            , "©"
            , "αβ"
            , "ΩΔ"
            , "İ"
            , "K"
            , "ꙀꙂ"
            , "\x10400"
            , "HELLO_World"
            , "€"
            , "日本語"
            , "ﬀ"
            , "🦀"
            , "🚀"
            , T.singleton startLiteralMarker
            , T.singleton endLiteralMarker
            , T.pack [startLiteralMarker, endLiteralMarker]
            , T.pack [startLiteralMarker, 'a', '€', endLiteralMarker]
            ]

genSampleLang :: Gen FileType
genSampleLang =
    elements
        [ Agda
        , C
        , Clojure
        , Cpp
        , Csharp
        , Rust
        , Python
        , Go
        , Haskell
        , Html
        , Julia
        , Lisp
        , Lua
        , Coffee
        , SmallTalk
        , Text
        , Toml
        , Java
        , Javascript
        , Bash
        , PHP
        , Zig
        , OCaml
        , Kotlin
        , Scala
        , D
        , R
        , Elixir
        , Ruby
        ]

genTokenFilter :: Gen TokenFilter
genTokenFilter =
    TokenFilter
        <$> elements [False, True]
        <*> elements [False, True]
        <*> elements [False, True]
        <*> elements [False, True]
        <*> elements [False, True]
        <*> elements [False, True]
        <*> elements [False, True]

genContextOptions :: Gen (Options, Bool)
genContextOptions = do
    c <- elements [False, True]
    m <- elements [False, True]
    l <- elements [False, True]
    markers <- elements [False, True]
    pure (defaultOptions{code = c, comment = m, literal = l}, markers)

prop_contextFilterByteLengthAndNewlines :: Property
prop_contextFilterByteLengthAndNewlines =
    forAll genSampleLang $ \ft ->
        forAll genContextOptions $ \(opt, useMarkers) ->
            forAll genAdversarialText $ \txt ->
                let out = filterSource ft opt useMarkers txt
                    nlIn = T.count "\n" txt
                    nlOut = T.count "\n" out
                    endMarks = T.count (T.singleton endLiteralMarker) out
                    -- Zig line-strings (`\\ ... \n`) use `\n` as their closing delimiter,
                    -- which becomes `endLiteralMarker` (`\x03`, 1 byte) when `useMarkers = True`.
                    nlOk
                        | ft == Zig && useMarkers = nlOut <= nlIn && nlOut + endMarks >= nlIn
                        | otherwise = nlOut == nlIn
                 in (TU.lengthWord8 out, nlOk) === (TU.lengthWord8 txt, True)

prop_contextFilterMatchesReference :: Property
prop_contextFilterMatchesReference =
    forAll genSampleLang $ \ft ->
        forAll genContextOptions $ \(opt, useMarkers) ->
            forAll (elements [False, True]) $ \stripComm ->
                forAll (elements ["", "🦀€//", "/*\"\"\""]) $ \slicePrefix ->
                    forAll (elements ["", "\"\"\"*/\n'a'", "//\"#"]) $ \sliceSuffix ->
                        forAll genAdversarialText $ \txtCore ->
                            let full = slicePrefix <> txtCore <> sliceSuffix
                                txt = T.take (T.length txtCore) (T.drop (T.length slicePrefix) full)
                             in case snd <$> lookupFTInfo ft of
                                    Nothing -> True === True
                                    Just FileTypeInfo{..} ->
                                        let cfg = mkParConfig ftComment ftString ftRawString ftChar useMarkers
                                            baseFilt = mkContextFilter opt
                                            filt = if stripComm then baseFilt ~! contextBitComment else baseFilt
                                         in runContextFilter cfg filt txt === runContextFilterRef cfg filt txt

prop_parseTokensValidSlicesAndNoBlankLeaks :: Property
prop_parseTokensValidSlicesAndNoBlankLeaks =
    forAll genSampleLang $ \ft ->
        forAll genContextOptions $ \(opt, _) ->
            forAll (elements [False, True]) $ \isStrict ->
                forAll genAdversarialText $ \txt ->
                    let toks = lexSource ft opt isStrict txt
                        totalBytes = TU.lengthWord8 txt
                        isBlankPlaceholder ch = ch == '\x00A0' || ch == '\x3000' || ch == '\x100000'
                        validTok t =
                            let off = tOffset t
                                sub = tToken t
                                blen = TU.lengthWord8 sub
                                -- Force full UTF-8 decoding of slice via T.length
                                charCount = T.length sub
                                noPhantomOp =
                                    tTyp t /= ChunkOperator || not (T.any isBlankPlaceholder sub)
                             in off >= 0
                                    && blen >= 0
                                    && off + blen <= totalBytes
                                    && charCount >= 0
                                    && noPhantomOp
                     in all validTok toks === True

prop_ignoreCasePreservesByteLength :: Property
prop_ignoreCasePreservesByteLength =
    forAll genAdversarialText $ \txt ->
        let lowered = ignoreCase defaultOptions{ignore_case = True} txt
         in TU.lengthWord8 lowered === TU.lengthWord8 txt

prop_lineIndexRoundTrip :: Property
prop_lineIndexRoundTrip =
    forAll (listOf genSingleLine) $ \rawLs ->
        forAll (elements [False, True]) $ \trailingNl ->
            let ls = case reverse rawLs of
                    ("" : rest) | not trailingNl -> reverse ("end" : rest)
                    _ -> rawLs
                body = T.intercalate "\n" ls
                txt
                    | null ls = ""
                    | trailingNl = body <> "\n"
                    | otherwise = body
                idx = buildIndex txt
                n = totalLines idx
                reconstructed = [getLineByNumber idx i | i <- [1 .. n]]
             in reconstructed === ls
  where
    genSingleLine :: Gen T.Text
    genSingleLine = do
        k <- chooseInt (0, 6)
        chunks <- sequence [elements ["abc", "©", "€", "🦀", "İ", ""] | _ <- [1 .. k]]
        pure (T.concat chunks)

prop_sliceToMaxIndexValidUtf8 :: Property
prop_sliceToMaxIndexValidUtf8 =
    forAll genAdversarialText $ \txt ->
        forAll (oneof [pure "foo", pure "€", pure "🦀", pure ""]) $ \needle ->
            let idxs = textIndices [needle] txt
                sliced = sliceToMaxIndex idxs txt
             in (T.length sliced >= 0 && sliced `T.isPrefixOf` txt) === True

prop_filterTestsDisjoint :: Property
prop_filterTestsDisjoint =
    forAll (elements [Rust, Python, Java, Go, Javascript, PHP, OCaml, Bash, Cpp, Csharp]) $ \ft ->
        forAll genAdversarialText $ \noise ->
            let src = "fn a() {}\n" <> noise <> "\n#[test]\nfn t() {}\n@Test void j() {}\n"
                toks = lexSource ft defaultOptions False src
                osp = unsafeEncodeUtf "check_file"
                prodOffsets = Set.fromList (map tOffset (filterTests osp ft (Just False) toks))
                testOffsets = Set.fromList (map tOffset (filterTests osp ft (Just True) toks))
             in Set.disjoint prodOffsets testOffsets === True

prop_parseTokensMatchesReference :: Property
prop_parseTokensMatchesReference =
    forAll genSampleLang $ \ft ->
        forAll genTokenFilter $ \tf ->
            forAll (elements [False, True]) $ \isStrict ->
                forAll (elements [False, True]) $ \preFilter ->
                    forAll (elements ["", "🦀€//", "abc = 123\n"]) $ \slicePrefix ->
                        forAll (elements ["", "\n\"\"\"", " + 42"]) $ \sliceSuffix ->
                            forAll genAdversarialText $ \rawCore ->
                                let core =
                                        if preFilter
                                            then filterSource ft defaultOptions True rawCore
                                            else rawCore
                                    full = slicePrefix <> core <> sliceSuffix
                                    txt = T.take (T.length core) (T.drop (T.length slicePrefix) full)
                                    fti = snd <$> lookupFTInfo ft
                                    tokSig t = (tTyp t, tOffset t, tToken t)
                                 in map tokSig (parseTokens tf fti isStrict txt)
                                        === map tokSig (parseTokensRef tf fti isStrict txt)

prop_parseChunksMatchesReference :: Property
prop_parseChunksMatchesReference =
    forAll genSampleLang $ \ft ->
        forAll (elements [False, True]) $ \preFilter ->
            forAll (elements ["", "🦀€//", "abc = 123\n"]) $ \slicePrefix ->
                forAll (elements ["", "\n\"\"\"", " + 42"]) $ \sliceSuffix ->
                    forAll genAdversarialText $ \rawCore ->
                        let core =
                                if preFilter
                                    then filterSource ft defaultOptions False rawCore
                                    else rawCore
                            full = slicePrefix <> core <> sliceSuffix
                            txt = T.take (T.length core) (T.drop (T.length slicePrefix) full)
                            fti = snd <$> lookupFTInfo ft
                            chunkSig c = (cTyp c, cOffset c, cToken c)
                         in map chunkSig (parseChunks fti txt)
                                === map chunkSig (parseChunksRef fti txt)

prop_lineOffsetsMatchesReference :: Property
prop_lineOffsetsMatchesReference =
    forAll (elements ["", "\n", "a\nb\n", "🦀€\n"]) $ \slicePrefix ->
        forAll (elements ["", "\n", "\nx\ny", "🦀"]) $ \sliceSuffix ->
            forAll genAdversarialText $ \core ->
                let full = slicePrefix <> core <> sliceSuffix
                    txt = T.take (T.length core) (T.drop (T.length slicePrefix) full)
                 in getLineOffsets txt === getLineOffsetsRef txt

prop_ignoreCaseMatchesReference :: Property
prop_ignoreCaseMatchesReference =
    forAll (elements ["", "ABC🦀", "xyz\n"]) $ \slicePrefix ->
        forAll (elements ["", "DEFΩ", "\n123"]) $ \sliceSuffix ->
            forAll genAdversarialText $ \core ->
                let full = slicePrefix <> core <> sliceSuffix
                    txt = T.take (T.length core) (T.drop (T.length slicePrefix) full)
                    optIC = defaultOptions{ignore_case = True}
                 in ignoreCase optIC txt === ignoreCaseRef optIC txt

prop_textIndicesMatchesReference :: Property
prop_textIndicesMatchesReference =
    forAll (elements ["", "foo\n", "🦀=="]) $ \slicePrefix ->
        forAll (elements ["", "==foo", "\n🦀"]) $ \sliceSuffix ->
            forAll (listOf genNeedle) $ \needles ->
                forAll genAdversarialText $ \core ->
                    let full = slicePrefix <> core <> sliceSuffix
                        txt = T.take (T.length core) (T.drop (T.length slicePrefix) full)
                     in textIndices needles txt === textIndicesRef needles txt
  where
    genNeedle :: Gen T.Text
    genNeedle =
        elements
            [ ""
            , " "
            , "\n"
            , "="
            , "=="
            , "!="
            , "a"
            , "aa"
            , "aaa"
            , "aba"
            , "foo"
            , "x_1"
            , "€"
            , "🦀"
            , "🦀🚀"
            , T.drop 1 "!foo"
            ]

genEditWord :: Gen T.Text
genEditWord =
    oneof
        [ pure ""
        , pure "a"
        , pure "ab"
        , pure "abcd"
        , pure "abcde"
        , pure "abcdef"
        , pure "abcdefg"
        , pure "abcdefgh"
        , pure "ZSTD_DCtx"
        , pure "abc€"
        , pure "🦀x€"
        , pure (T.replicate 63 "a" <> "b")
        , pure (T.replicate 64 "a")
        , pure (T.replicate 65 "a")
        , do
            len <- elements ([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 31, 32, 63, 64, 65, 72] :: [Int])
            cs <- sequence [elements ['a', 'b', 'c', '_', '1', '€', '🦀'] | _ <- [1 .. len]]
            pure (T.pack cs)
        ]

genPerturbedWord :: T.Text -> Gen T.Text
genPerturbedWord base =
    oneof
        [ pure base
        , genEditWord
        , do
            -- 1 to 4 random single-character edits around `base` to hit exact threshold boundaries
            k <- chooseInt (1, 4)
            let step s = do
                    let len = T.length s
                    op <- chooseInt (0, 2)
                    ch <- elements ['a', 'x', '_', '€', '🦀']
                    if len == 0 || op == 0
                        then do
                            pos <- chooseInt (0, len)
                            pure (T.take pos s <> T.singleton ch <> T.drop pos s)
                        else
                            if op == 1
                                then do
                                    pos <- chooseInt (0, len - 1)
                                    pure (T.take pos s <> T.drop (pos + 1) s)
                                else do
                                    pos <- chooseInt (0, len - 1)
                                    pure (T.take pos s <> T.singleton ch <> T.drop (pos + 1) s)
            foldr (\_ acc -> acc >>= step) (pure base) ([1 .. k] :: [Int])
        ]

prop_distanceMatchesReference :: Property
prop_distanceMatchesReference =
    forAll genEditWord $ \aTxt ->
        forAll (genPerturbedWord aTxt) $ \bTxt ->
            let a = T.unpack aTxt
                b = T.unpack bTxt
             in distance a b === distanceRef a b

prop_editPatternMatchesReference :: Property
prop_editPatternMatchesReference =
    forAll (elements ["", "pre_", "🦀"]) $ \preA ->
        forAll (elements ["", "_suf", "€"]) $ \sufA ->
            forAll (elements ["", "xx", "🦀"]) $ \preB ->
                forAll (elements ["", "yy", "€"]) $ \sufB ->
                    forAll genEditWord $ \coreA ->
                        forAll (genPerturbedWord coreA) $ \coreB ->
                            let fullA = preA <> coreA <> sufA
                                fullB = preB <> coreB <> sufB
                                a = T.take (T.length coreA) (T.drop (T.length preA) fullA)
                                b = T.take (T.length coreB) (T.drop (T.length preB) fullB)
                                ep = mkEditPattern a
                                ref = approxEqRef (T.unpack a) (T.unpack b)
                             in (matchEditPattern ep b, textApproxEq a b) === (ref, ref)

prop_textHelpersMatchReference :: Property
prop_textHelpersMatchReference =
    forAll (elements ["", "ABC🦀", "foo\n"]) $ \slicePrefix ->
        forAll (elements ["", "DEFΩ", "\nbar"]) $ \sliceSuffix ->
            forAll (listOf genHelperNeedle) $ \needles ->
                forAll genHelperNeedle $ \singleNeedle ->
                    forAll (elements [False, True]) $ \ic ->
                        forAll genAdversarialText $ \core ->
                            let full = slicePrefix <> core <> sliceSuffix
                                txt = T.take (T.length core) (T.drop (T.length slicePrefix) full)
                                opt = defaultOptions{ignore_case = ic}
                                patLc = T.toLower singleNeedle
                                asc = isAsciiText patLc
                                txtLc = T.toLower txt
                                refLast = case textIndicesRef [singleNeedle] txt of
                                    [xs@(_ : _)] -> Just (last xs)
                                    _ -> Nothing
                                icTxt = ignoreCase opt txt
                                icNeedles = ignoreCase opt <$> needles
                                refContains =
                                    textContainsOneOf icNeedles icTxt
                                expectedSlice
                                    | null icNeedles = icTxt
                                    | refContains = sliceToMaxIndex (textIndices icNeedles icTxt) icTxt
                                    | otherwise = T.empty
                                fastSlice =
                                    sliceToLastMatch icNeedles icTxt
                             in ( singleNeedle `textIsInfixOf` txt
                                , textEqCI asc patLc txt
                                , textIsPrefixOfCI asc patLc txt
                                , textIsSuffixOfCI asc patLc txt
                                , textIsInfixOfCI asc patLc txt
                                , lastIndex singleNeedle txt
                                , textContainsOneOfOpt opt needles txt
                                , fastSlice
                                )
                                    === ( singleNeedle `T.isInfixOf` txt
                                        , patLc == txtLc
                                        , patLc `T.isPrefixOf` txtLc
                                        , patLc `T.isSuffixOf` txtLc
                                        , patLc `T.isInfixOf` txtLc
                                        , refLast
                                        , refContains
                                        , expectedSlice
                                        )
  where
    genHelperNeedle :: Gen T.Text
    genHelperNeedle =
        elements
            [ ""
            , "a"
            , "A"
            , "foo"
            , "FOO"
            , "x_1"
            , "_1"
            , "a1b1"
            , "=="
            , "K"
            , "k"
            , "İ"
            , "i"
            , "Ω"
            , "ω"
            , "🦀"
            , T.drop 1 "!FOO"
            ]

prop_findAllMatchesMatchesReference :: Property
prop_findAllMatchesMatchesReference =
    forAll genAtomOptions $ \opt ->
        forAll (chooseInt (1, 3)) $ \numPats ->
            forAll (sequence [genAtomPattern | _ <- [1 .. numPats]]) $ \pats ->
                forAll genSemanticTokenStream $ \toks ->
                    let tokSig t = (tTyp t, tOffset t, tToken t)
                     in map tokSig (findAllMatches opt pats toks)
                            === map tokSig (findAllMatchesRef opt pats toks)
  where
    genAtomOptions :: Gen Options
    genAtomOptions = do
        mode <- chooseInt (0, 4)
        ic <- elements [False, True]
        pure
            defaultOptions
                { semantic = True
                , ignore_case = ic
                , word_match = mode == 1
                , prefix_match = mode == 2
                , suffix_match = mode == 3
                , edit_dist = mode == 4
                }

    genBaseAtom :: Gen Atom
    genBaseAtom =
        oneof
            [ pure Any
            , pure Keyword
            , pure Operator
            , pure Number
            , pure Oct
            , pure Hex
            , pure String
            , pure Literal
            , Placeholder <$> elements ["_", "_1", "_2"]
            , Exact . mkTokenIdentifier <$> elements ["foo", "FOO", "bar", "x_1", "if", "int", "Ω", "K"]
            , Exact . mkTokenString <$> elements ["", "\"\"", "\"abc\"", "\"FOO\"", "\"€\""]
            , Exact . mkTokenKeyword <$> elements ["if", "return"]
            , Exact . mkTokenOperator <$> elements ["=", "==", "+", ";"]
            ]

    genAtomElem :: Gen Atom
    genAtomElem =
        oneof
            [ genBaseAtom
            , pure Ellipsis
            , Not <$> genBaseAtom
            , Or <$> genBaseAtom <*> genBaseAtom
            ]

    genAtomPattern :: Gen [Atom]
    genAtomPattern =
        oneof
            [ do
                len <- chooseInt (1, 5)
                sequence [genAtomElem | _ <- [1 .. len]]
            , do
                src <- elements ["_1 == _1", "_ = HEX", "if ( ... )", "foo OR bar ( _ )", "NOT STR"]
                pure (mkAtomsFromTokens (lexSource C defaultOptions False src))
            ]

    genSemanticTokenStream :: Gen [Token]
    genSemanticTokenStream = do
        pieces <-
            listOf $
                elements
                    [ "int foo = 0x1A;"
                    , "if (foo == FOO) return 077;"
                    , "bar(\"abc\", \"\");"
                    , "x_1 = bar + foo;"
                    , "const char *s = \"€\";"
                    , "foo(a, b(1, 2));"
                    , "Ω = K;"
                    ]
        isStrict <- elements [False, True]
        pure (lexSource C defaultOptions isStrict (T.unwords pieces))

prop_regexMatchesReference :: Property
prop_regexMatchesReference =
    forAll (elements [False, True]) $ \ic ->
        forAll (elements [True, False]) $ \codeOnly ->
            forAll (chooseInt (1, 2)) $ \nPats ->
                forAll (sequence [genPosixRegex | _ <- [1 .. nPats]]) $ \pats ->
                    forAll genRegexHaystack $ \txt ->
                        let opt = defaultOptions{regex_posix = True, ignore_case = ic, code = codeOnly}
                            normPats = map (ignoreCase opt) pats
                            fast = runRegexFn Regex.search opt txt normPats
                            ref = runRegexFn Regex.searchRef opt txt normPats
                         in fast === ref
  where
    runRegexFn fn opt txt pats =
        unsafePerformIO $ do
            lock <- newMVar ()
            let env = Env defaultConfig opt
                osp = unsafeEncodeUtf "prop_regex.c"
                info = lookupFTInfo C
            ms <- runReaderT (fn lock info osp txt pats False) env
            pure [(mLineNumb m, mLine m, [(cOffset c, cToken c) | c <- mChunks m]) | m <- ms]

    genPosixRegex :: Gen T.Text
    genPosixRegex =
        elements
            [ "foo"
            , "foo.*bar"
            , "a.b"
            , "^int[[:space:]]+[a-z_]+"
            , "return[[:space:]]+[0-9]+;$"
            , "(foo|bar)_[0-9]+"
            , "(foo|bar)?_test"
            , "[a-z]+_test"
            , "[^x]+foo"
            , "[]a]foo"
            , "\\<foo\\>"
            , "\\bfoo\\b"
            , "\\Bfoo"
            , "foo\\{1,2\\}|bar{1,2}"
            , "(ab){0,2}cd"
            , "(ab){2,3}cd"
            , "€+[0-9]+"
            , "^$"
            ]

    genRegexHaystack :: Gen T.Text
    genRegexHaystack = do
        ls <-
            listOf $
                elements
                    [ "int foo_12 = 1; // foo_99"
                    , "return 42;"
                    , "a"
                    , "b"
                    , "axb"
                    , "+foo bar_7 x_test ]foo afoo"
                    , "abcd ababcd €123"
                    , ""
                    ]
        pure (T.intercalate "\n" ls)
