module PropertySpec (propertyTests) where

import CGrep.Common (ignoreCase, sliceToMaxIndex)
import CGrep.FileType (FileType (..))
import CGrep.FileTypeMap (FileTypeInfo (..))
import CGrep.Line (buildIndex, getLineByNumber, totalLines)
import CGrep.Parser.Chunk (
    cOffset,
    cToken,
    cTyp,
    parseChunks,
    parseChunksRef,
    pattern ChunkOperator,
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
    endLiteralMarker,
    mkContextFilter,
    mkParConfig,
    runContextFilter,
    runContextFilterRef,
    startLiteralMarker,
    (~!),
 )
import CGrep.Semantic.Tests (filterTests)
import CGrep.Text (textIndices)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import Options (Options (..), defaultOptions)
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
            , "İ"
            , "K"
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
