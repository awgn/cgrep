module BenchCorpus (
    Corpus (..),
    CorpusEntry (..),
    corpusLangSpecs,
    allTokensFilter,
    loadCorpus,
    forceMatches,
    forceTokens,
    forceChunksSeq,
    forceTokensSeq,
    runEntrySearch,
    runCorpusSearch,
) where

import CGrep.Common (getTargetContents)
import CGrep.FileType (FileType (..))
import CGrep.FileTypeMap (FileTypeInfo)
import CGrep.FileTypeMapTH (fileTypeInfoLookup, mkContextFilterFn)
import CGrep.Match (Match (..))
import CGrep.Parser.Chunk (Chunk (..))
import CGrep.Parser.Token (Token, TokenFilter (..), parseTokens)
import CGrep.Search (getSearcher, normalizePatterns)
import CGrep.Semantic.ContextFilter (contextBitComment, contextFilterAll, (~!))
import Config (defaultConfig)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.DeepSeq (NFData (..))
import Control.Monad (forM, unless)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Text as T
import qualified Data.Vector as V
import Options (Options (..), defaultOptions)
import Reader (Env (..))
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.OsPath (OsPath, unsafeEncodeUtf)

data CorpusEntry = CorpusEntry
    { ceId :: !String
    , ceFileName :: !FilePath
    , ceOsPath :: !OsPath
    , ceFileType :: !FileType
    , ceInfo :: !(Maybe (FileType, FileTypeInfo))
    , ceText :: !T.Text
    , ceFilteredText :: !T.Text
    , ceTokens :: ![Token]
    , cePrimaryIdent :: !T.Text
    , ceSecondaryIdent :: !T.Text
    , ceSemanticPat :: !T.Text
    , ceRegexPat :: !T.Text
    }

instance NFData CorpusEntry where
    rnf CorpusEntry{..} =
        rnf ceId
            `seq` rnf ceFileName
            `seq` ceOsPath
            `seq` ceFileType
            `seq` rnfInfo ceInfo
            `seq` rnf ceText
            `seq` rnf ceFilteredText
            `seq` forceTokens ceTokens
            `seq` rnf cePrimaryIdent
            `seq` rnf ceSecondaryIdent
            `seq` rnf ceSemanticPat
            `seq` rnf ceRegexPat
      where
        rnfInfo Nothing = ()
        rnfInfo (Just (!ft, !fti)) = ft `seq` fti `seq` ()

data Corpus = Corpus
    { cLock :: !(MVar ())
    , cEntries :: !(V.Vector CorpusEntry)
    }

instance NFData Corpus where
    rnf Corpus{..} = cLock `seq` rnf cEntries

allTokensFilter :: TokenFilter
allTokensFilter =
    TokenFilter
        { tfIdentifier = True
        , tfKeyword = True
        , tfNativeType = True
        , tfString = True
        , tfNumber = True
        , tfOperator = True
        , tfBracket = True
        }

-- | Strictly forces every Match, line number, line Text slice, and Chunk in [Match].
forceMatches :: [Match] -> Int
forceMatches =
    foldl'
        ( \ !acc m ->
            mFilePath m
                `seq` foldl' (\ !a !c -> c `seq` (a + 1)) (acc + mLineNumb m) (mChunks m)
        )
        0
{-# INLINE forceMatches #-}

-- | Strictly forces every Token in a list and returns token count.
forceTokens :: [Token] -> Int
forceTokens = foldl' (\ !acc !t -> t `seq` (acc + 1)) 0
{-# INLINE forceTokens #-}

-- | Strictly forces every Chunk in a Seq Chunk and returns count.
forceChunksSeq :: Seq Chunk -> Int
forceChunksSeq = foldl' (\ !acc !c -> c `seq` (acc + 1)) 0
{-# INLINE forceChunksSeq #-}

-- | Strictly forces every Token in a Seq Token and returns count.
forceTokensSeq :: Seq Token -> Int
forceTokensSeq = foldl' (\ !acc !t -> t `seq` (acc + 1)) 0
{-# INLINE forceTokensSeq #-}

runEntrySearch :: MVar () -> Options -> CorpusEntry -> [T.Text] -> IO Int
runEntrySearch lock opt CorpusEntry{..} rawPatterns = do
    let env = Env defaultConfig opt
        pats = normalizePatterns opt rawPatterns
        searcher = getSearcher env
    matches <- runReaderT (searcher lock ceInfo ceOsPath ceText pats (strict opt)) env
    let !n = forceMatches matches
    pure n
{-# INLINE runEntrySearch #-}

runCorpusSearch :: Corpus -> Options -> (CorpusEntry -> [T.Text]) -> IO Int
runCorpusSearch Corpus{..} opt mkPats = do
    let env = Env defaultConfig opt
        searcher = getSearcher env
        step !acc e@CorpusEntry{..} = do
            let pats = normalizePatterns opt (mkPats e)
            matches <- runReaderT (searcher cLock ceInfo ceOsPath ceText pats (strict opt)) env
            pure $! acc + forceMatches matches
    V.foldM' step 0 cEntries

data RawSpec = RawSpec
    { rsId :: !String
    , rsFile :: !FilePath
    , rsExpectedFT :: !FileType
    , rsPrimary :: !T.Text
    , rsSecondary :: !T.Text
    , rsSemantic :: !T.Text
    , rsRegex :: !T.Text
    }

corpusSpecs :: [RawSpec]
corpusSpecs =
    [ RawSpec "bash" "bash_git_completion.sh" Bash "__git_complete_refs" "subcommands" "__git_complete_refs ANY" "__git_complete_[a-z_]+"
    , RawSpec "c" "c_zstd.c" C "ZSTD_DCtx" "ddict" "ZSTD_DCtx * ANY" "ZSTD_decompress[A-Za-z0-9_]*"
    , RawSpec "clojure" "clojure_cl_format.clj" Clojure "clauses" "offsets" "( defn ANY [ ... ] ... )" "cl-format|compile-format"
    , RawSpec "cpp" "cpp_llvm_cmdline.cpp" Cpp "Value" "Option" "Value = ANY" "Option[A-Za-z0-9_]+"
    , RawSpec "csharp" "csharp_newtonsoft_reader.cs" Csharp "SetToken" "JsonToken" "SetToken ( ... )" "JsonToken\\.[A-Za-z]+"
    , RawSpec "d" "d_phobos_array.d" D "ElementType" "subject" "if ( ... )" "ElementType|Appender"
    , RawSpec "dart" "dart_collection.dart" Dart "moveNext" "Iterable" "moveNext ( )" "Iterable<[A-Za-z]+>"
    , RawSpec "elixir" "elixir_enum.ex" Elixir "empty_fallback" "enumerable" "def ANY ( ... )" "reduce_[a-z_]+"
    , RawSpec "elm" "elm_ui_element.elm" Elm "element" "Attribute" "element ANY" "Attribute[A-Za-z]*"
    , RawSpec "erlang" "erlang_lists.erl" Erlang "elements" "Tuple" "[ _1 | ANY ]" "keyfind|keymember"
    , RawSpec "fsharp" "fsharp_seq.fs" Fsharp "source1" "IEnumerator" "let ANY = ANY" "checkNonNull|IEnumerator"
    , RawSpec "go" "go_http_transport.go" Go "pconn" "roundTrip" "if _1 != nil" "persistConn|roundTrip"
    , RawSpec "haskell" "haskell_containers_map.hs" Haskell "WhenMissing" "compare" "WhenMissing ANY ANY" "WhenMatched|WhenMissing"
    , RawSpec "java" "java_guava_map.java" Java "entryOf" "ImmutableMap" "entryOf ( _1 , _2 )" "ImmutableMap<[A-Za-z, ]+>"
    , RawSpec "javascript" "javascript_sortable.js" Javascript "putSortable" "ghostEl" "if ( _1 )" "multiDrag[A-Za-z]*"
    , RawSpec "typescript" "typescript_zod_types.ts" Javascript "ZodObject" "ZodType" "ZodObject . ANY ( ... )" "Zod[A-Z][A-Za-z]+"
    , RawSpec "julia" "julia_abstractarray.jl" Julia "eachindex" "AbstractVector" "eachindex ( ... )" "Abstract[A-Za-z]+Array"
    , RawSpec "kotlin" "kotlin_buffered_channel.kt" Kotlin "operation" "ChannelSegment" "override fun ANY ( ... )" "Channel[A-Za-z]+"
    , RawSpec "lua" "lua_neovim_lsp.lua" Lua "textDocument" "params" "if _1 then" "textDocument|make_position"
    , RawSpec "nim" "nim_strutils.nim" Nim "rsplit" "maxsplit" "proc ANY ( ... )" "split[A-Za-z]*"
    , RawSpec "objc" "objc_afnetworking.m" ObjectiveC "downloadTask" "delegate" "if ( _1 )" "URLSession[A-Za-z]+"
    , RawSpec "ocaml" "ocaml_stdlib_scanf.ml" OCaml "take_format_readers" "readers" "match _1 with" "Scanning\\.[a-z_]+"
    , RawSpec "perl" "perl_mojo_useragent.pm" Perl "blocking" "transactor" "unless ( ... )" "transactor|inactivity_timeout"
    , RawSpec "php" "php_symfony_request.php" PHP "requestUri" "headers" "public function ANY ( ... )" "get[A-Z][A-Za-z]+Uri"
    , RawSpec "python" "python_requests_models.py" Python "headers" "encoding" "if _1 is None" "PreparedRequest|Response"
    , RawSpec "r" "r_ggplot2_theme.R" R "inherits" "spacing" "inherits ( _1 , STR )" "element_[a-z_]+"
    , RawSpec "ruby" "ruby_sinatra_base.rb" Ruby "settings" "response" "unless _1" "content_type|halt"
    , RawSpec "rust" "rust_bytes.rs" Rust "bytes" "Shared" "unsafe { ... }" "BytesMut|Promotable"
    , RawSpec "scala" "scala_cats_chain.scala" Scala "Append" "Singleton" "case Append ( _1 , _2 )" "NonEmpty[A-Za-z]+"
    , RawSpec "swift" "swift_nio_bytebuffer.swift" Swift "writerIndex" "_slice" "public mutating func ANY ( ... )" "writerIndex|readerIndex"
    , RawSpec "zig" "zig_array_list.zig" Zig "ArrayList" "ensureTotalCapacity" "pub fn ANY ( ... )" "ensure[A-Za-z]+Capacity"
    ]

corpusLangSpecs :: [(Int, String)]
corpusLangSpecs = zip [0 ..] (map rsId corpusSpecs)

loadCorpus :: IO Corpus
loadCorpus = do
    let dir = "bench" </> "corpus"
    exists <- doesDirectoryExist dir
    unless exists $
        ioError $
            userError ("cgrep-bench: corpus directory not found at " <> dir <> " (run from repository root)")
    lock <- newMVar ()
    let filt = contextFilterAll ~! contextBitComment
    entries <- forM corpusSpecs $ \RawSpec{..} -> do
        let relPath = dir </> rsFile
            osp = unsafeEncodeUtf relPath
        txt <- getTargetContents osp
        let info = fileTypeInfoLookup defaultOptions osp
            ft = maybe rsExpectedFT fst info
            cf = mkContextFilterFn (Just ft) filt True
            filtered = cf txt
            toks = toList $ parseTokens allTokensFilter (snd <$> info) False filtered
        pure
            CorpusEntry
                { ceId = rsId
                , ceFileName = relPath
                , ceOsPath = osp
                , ceFileType = ft
                , ceInfo = info
                , ceText = txt
                , ceFilteredText = filtered
                , ceTokens = toks
                , cePrimaryIdent = rsPrimary
                , ceSecondaryIdent = rsSecondary
                , ceSemanticPat = rsSemantic
                , ceRegexPat = rsRegex
                }
    pure Corpus{cLock = lock, cEntries = V.fromList entries}
