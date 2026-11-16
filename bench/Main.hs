module Main (main) where

import BenchCorpus (
    Corpus (..),
    CorpusEntry (..),
    allTokensFilter,
    corpusLangSpecs,
    forceChunksSeq,
    forceTokens,
    forceTokensSeq,
    loadCorpus,
    runCorpusDiskSearchFast,
    runCorpusDiskSearchRef,
    runCorpusReadDecodeFast,
    runCorpusReadDecodeRef,
    runCorpusSearch,
    runEntrySearch,
 )
import CGrep.FileTypeMapTH (mkContextFilterFn)
import CGrep.Line (buildIndex, totalLines)
import CGrep.Parser.Chunk (parseChunks)
import CGrep.Parser.Token (parseTokens)
import CGrep.Semantic.ContextFilter (
    ContextFilter,
    contextBitComment,
    contextFilterAll,
    mkContextFilter,
    (~!),
 )
import CGrep.Semantic.Tests (filterTests)
import Criterion.Main (Benchmark, bench, bgroup, defaultMain, env, nf, nfIO)
import qualified Data.Text as T
import qualified Data.Vector as V
import Options (Options (..), defaultOptions)

main :: IO ()
main =
    defaultMain
        [ env loadCorpus $ \corpus ->
            bgroup
                "cgrep"
                [ stageBenchmarks corpus
                , corpusBenchmarks corpus
                , ioBenchmarks corpus
                , perLanguageBenchmarks corpus
                ]
        ]

stageBenchmarks :: Corpus -> Benchmark
stageBenchmarks corpus =
    bgroup
        "Stage"
        [ bench "LineIndex/buildIndex" $
            nf (sumEntries (\e -> totalLines (buildIndex (ceText e)))) corpus
        , bench "ContextFilter/semanticMarkers" $
            nf (runCF (contextFilterAll ~! contextBitComment) True) corpus
        , bench "ContextFilter/codeOnly" $
            nf (runCF (mkContextFilter optCodeOnly) False) corpus
        , bench "ContextFilter/commentOnly" $
            nf (runCF (mkContextFilter optCommentOnly) False) corpus
        , bench "ContextFilter/defaultNoMarkers" $
            nf (runCF (mkContextFilter defaultOptions) False) corpus
        , bench "Chunk/parseChunks" $
            nf (sumEntries (\e -> forceChunksSeq (parseChunks (snd <$> ceInfo e) (ceFilteredText e)))) corpus
        , bench "Token/parseTokens-nonStrict" $
            nf (sumEntries (\e -> forceTokensSeq (parseTokens allTokensFilter (snd <$> ceInfo e) False (ceFilteredText e)))) corpus
        , bench "Token/parseTokens-strict" $
            nf (sumEntries (\e -> forceTokensSeq (parseTokens allTokensFilter (snd <$> ceInfo e) True (ceFilteredText e)))) corpus
        , bench "Tests/filterTests-noTests" $
            nf (sumEntries (\e -> forceTokens (filterTests (ceOsPath e) (ceFileType e) (Just False) (ceTokens e)))) corpus
        , bench "Tests/filterTests-onlyTests" $
            nf (sumEntries (\e -> forceTokens (filterTests (ceOsPath e) (ceFileType e) (Just True) (ceTokens e)))) corpus
        ]
  where
    sumEntries :: (CorpusEntry -> Int) -> Corpus -> Int
    sumEntries f Corpus{..} = V.foldl' (\ !acc e -> acc + f e) 0 cEntries

    runCF :: ContextFilter -> Bool -> Corpus -> Int
    runCF filt useMarkers =
        sumEntries $ \e ->
            let fn = mkContextFilterFn (Just (ceFileType e)) filt useMarkers
                !out = fn (ceText e)
             in if T.null out then 0 else 1

corpusBenchmarks :: Corpus -> Benchmark
corpusBenchmarks corpus =
    bgroup
        "Corpus"
        [ bgroup
            "BoyerMoore"
            [ bench "exact" $
                nfIO $ runCorpusSearch corpus defaultOptions (\e -> [cePrimaryIdent e])
            , bench "miss" $
                nfIO $ runCorpusSearch corpus defaultOptions (const ["__cgrep_absent_symbol_991__"])
            , bench "multiPattern" $
                nfIO $ runCorpusSearch corpus defaultOptions (\e -> [cePrimaryIdent e, ceSecondaryIdent e, "__absent_pat__"])
            , bench "wordMatch" $
                nfIO $ runCorpusSearch corpus optWordMatch (\e -> [cePrimaryIdent e])
            , bench "ignoreCase" $
                nfIO $ runCorpusSearch corpus optIgnoreCase (\e -> [cePrimaryIdent e])
            , bench "codeOnly" $
                nfIO $ runCorpusSearch corpus optCodeOnly (\e -> [cePrimaryIdent e])
            ]
        , bgroup
            "Tokenizer"
            [ bench "identifier" $
                nfIO $ runCorpusSearch corpus optTokIdent (\e -> [cePrimaryIdent e])
            , bench "keywordAndString" $
                nfIO $ runCorpusSearch corpus optTokKwStr (\e -> [cePrimaryIdent e, ceSecondaryIdent e])
            ]
        , bgroup
            "Semantic"
            [ bench "idiomPattern" $
                nfIO $ runCorpusSearch corpus optSemantic (\e -> [ceSemanticPat e])
            , bench "exactPair" $
                nfIO $ runCorpusSearch corpus optSemantic (\e -> [cePrimaryIdent e, ceSecondaryIdent e])
            , bench "combinators" $
                nfIO $
                    runCorpusSearch
                        corpus
                        optSemantic
                        ( \e ->
                            [ "OR " <> cePrimaryIdent e <> " " <> ceSecondaryIdent e
                            , "NOT " <> ceSecondaryIdent e <> " " <> cePrimaryIdent e
                            ]
                        )
            , bench "strict" $
                nfIO $ runCorpusSearch corpus optSemanticStrict (\e -> [ceSemanticPat e])
            , bench "ignoreCase" $
                nfIO $ runCorpusSearch corpus optSemanticIgnoreCase (\e -> [ceSemanticPat e])
            , bench "noTests" $
                nfIO $ runCorpusSearch corpus optSemanticNoTests (\e -> [ceSemanticPat e])
            , bench "onlyTests" $
                nfIO $ runCorpusSearch corpus optSemanticOnlyTests (\e -> [ceSemanticPat e])
            ]
        , bgroup
            "Regex"
            [ bench "posix" $
                nfIO $ runCorpusSearch corpus optRegex (\e -> [ceRegexPat e])
            ]
        , bgroup
            "Levenshtein"
            [ bench "editDist" $
                nfIO $ runCorpusSearch corpus optEditDist (\e -> [cePrimaryIdent e])
            ]
        ]

-- | Disk-to-Match I/O benchmarks comparing reference @BS.readFile@ + @decodeUtf8Lenient@
-- against adaptive @mmap@ (>= 64 KiB) + raw-@ByteString@ pre-filtering across all 31 corpus files.
ioBenchmarks :: Corpus -> Benchmark
ioBenchmarks corpus =
    bgroup
        "IO"
        [ bgroup
            "ReadDecode"
            [ bench "ref-readFile" $ nfIO (runCorpusReadDecodeRef corpus)
            , bench "fast-mmapAdaptive" $ nfIO (runCorpusReadDecodeFast corpus)
            ]
        , bgroup
            "BoyerMoore-miss"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus defaultOptions ["__cgrep_absent_symbol_991__"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus defaultOptions ["__cgrep_absent_symbol_991__"])
            ]
        , bgroup
            "BoyerMoore-selective"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus defaultOptions ["ZSTD_DCtx"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus defaultOptions ["ZSTD_DCtx"])
            ]
        , bgroup
            "BoyerMoore-ignoreCase"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus optIgnoreCase ["zstd_dctx"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus optIgnoreCase ["zstd_dctx"])
            ]
        , bgroup
            "Semantic-miss"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus optSemantic ["__cgrep_absent_symbol_991__ ( ... )"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus optSemantic ["__cgrep_absent_symbol_991__ ( ... )"])
            ]
        , bgroup
            "Semantic-selective"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus optSemantic ["ZSTD_DCtx * ANY"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus optSemantic ["ZSTD_DCtx * ANY"])
            ]
        , bgroup
            "Semantic-ignoreCase"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus optSemanticIgnoreCase ["zstd_dctx * ANY"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus optSemanticIgnoreCase ["zstd_dctx * ANY"])
            ]
        , bgroup
            "Tokenizer-selective"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus optTokIdent ["ZSTD_DCtx"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus optTokIdent ["ZSTD_DCtx"])
            ]
        , bgroup
            "Regex-selective"
            [ bench "ref" $ nfIO (runCorpusDiskSearchRef corpus optRegex ["ZSTD_decompress[A-Za-z0-9_]*"])
            , bench "fast" $ nfIO (runCorpusDiskSearchFast corpus optRegex ["ZSTD_decompress[A-Za-z0-9_]*"])
            ]
        ]

perLanguageBenchmarks :: Corpus -> Benchmark
perLanguageBenchmarks corpus =
    bgroup "Lang" $
        map mkLangGroup corpusLangSpecs
  where
    mkLangGroup :: (Int, String) -> Benchmark
    mkLangGroup (!idx, langId) =
        bgroup
            langId
            [ bench "BoyerMoore" $
                nfIO $ do
                    let e = V.unsafeIndex (cEntries corpus) idx
                    runEntrySearch (cLock corpus) defaultOptions e [cePrimaryIdent e]
            , bench "Tokenizer" $
                nfIO $ do
                    let e = V.unsafeIndex (cEntries corpus) idx
                    runEntrySearch (cLock corpus) optTokIdent e [cePrimaryIdent e]
            , bench "Semantic" $
                nfIO $ do
                    let e = V.unsafeIndex (cEntries corpus) idx
                    runEntrySearch (cLock corpus) optSemantic e [ceSemanticPat e]
            , bench "Semantic-noTests" $
                nfIO $ do
                    let e = V.unsafeIndex (cEntries corpus) idx
                    runEntrySearch (cLock corpus) optSemanticNoTests e [ceSemanticPat e]
            , bench "Regex" $
                nfIO $ do
                    let e = V.unsafeIndex (cEntries corpus) idx
                    runEntrySearch (cLock corpus) optRegex e [ceRegexPat e]
            , bench "Levenshtein" $
                nfIO $ do
                    let e = V.unsafeIndex (cEntries corpus) idx
                    runEntrySearch (cLock corpus) optEditDist e [cePrimaryIdent e]
            , bench "filterTests" $
                nf
                    ( \c ->
                        let e = V.unsafeIndex (cEntries c) idx
                         in forceTokens (filterTests (ceOsPath e) (ceFileType e) (Just False) (ceTokens e))
                    )
                    corpus
            ]

optCodeOnly :: Options
optCodeOnly = defaultOptions{code = True}

optCommentOnly :: Options
optCommentOnly = defaultOptions{comment = True}

optWordMatch :: Options
optWordMatch = defaultOptions{word_match = True}

optIgnoreCase :: Options
optIgnoreCase = defaultOptions{ignore_case = True}

optTokIdent :: Options
optTokIdent = defaultOptions{identifier = True, word_match = True}

optTokKwStr :: Options
optTokKwStr = defaultOptions{keyword = True, string = True}

optSemantic :: Options
optSemantic = defaultOptions{semantic = True}

optSemanticStrict :: Options
optSemanticStrict = defaultOptions{semantic = True, strict = True}

optSemanticIgnoreCase :: Options
optSemanticIgnoreCase = defaultOptions{semantic = True, ignore_case = True}

optSemanticNoTests :: Options
optSemanticNoTests = defaultOptions{semantic = True, tests = Just False}

optSemanticOnlyTests :: Options
optSemanticOnlyTests = defaultOptions{semantic = True, tests = Just True}

optRegex :: Options
optRegex = defaultOptions{regex_posix = True}

optEditDist :: Options
optEditDist = defaultOptions{edit_dist = True}
