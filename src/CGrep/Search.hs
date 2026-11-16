{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module CGrep.Search (
    startSearch,
    getSearcher,
    buildRawPrefilterLookup,
    normalizePatterns,
    isRegexp,
)
where

import CGrep.Common (
    CPrefilter,
    RawPrefilter (..),
    WorkerBuffer,
    compileCPrefilter,
    dirInodeBuf,
    getTargetName,
    ignoreCase,
    joinPosixPath,
    mkAsciiRawPrefilter,
    readTargetFilteredBuf,
    scandirBuf,
    takeN,
    withWorkerBuffer,
 )
import CGrep.FileKind (FileKind)
import CGrep.FileType (FileType (..))
import CGrep.FileTypeMap (
    FileTypeInfo,
    FileTypeInfoMap (..),
 )
import CGrep.FileTypeMapTH (
    fileTypeInfoLookup,
    fileTypeInfoMap,
    fileTypeLookupBase,
    mkContextFilterFn,
 )
import CGrep.Match (
    Match (..),
    prettyFileName,
    putMatchesBSB,
 )
import CGrep.Parser.Atom (extractAtomAnchors, mkAtomsFromTokens)
import CGrep.Parser.Token (TokenFilter (..), parseTokens)
import CGrep.Semantic.ContextFilter (
    contextBitComment,
    contextFilterAll,
    isContextFilterAll,
    mkContextFilter,
    (~!),
 )
import qualified CGrep.Strategy.BoyerMoore as BoyerMoore
import qualified CGrep.Strategy.Levenshtein as Levenshtein
import CGrep.Strategy.Regex (extractPosixLinePrefilter, mkRegexSearcher)
import qualified CGrep.Strategy.Regex as Regex
import qualified CGrep.Strategy.Semantic as Semantic
import qualified CGrep.Strategy.Tokenizer as Tokenizer
import Config (
    Config (
        Config,
        configColorFile,
        configColorMatch,
        configColors,
        configFileLine,
        configFileTypes,
        configPruneDirs
    ),
 )

import Control.Applicative ((<|>))
import Control.Arrow (Arrow ((&&&)))
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.Async (asyncOn, wait)
import Control.Concurrent.Extra (newMVar, withMVar)
import Control.Exception as E (SomeException, catch, onException)
import Control.Monad (forM, unless, void, when)
import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (
    ReaderT (runReaderT),
    ask,
 )
import Data.Atomics.Counter (incrCounter, newCounter, readCounter)
import qualified Data.Bifunctor
import Data.Bits (unsafeShiftL, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashSet as S
import Data.IORef (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
 )
import Data.List (maximumBy)
import Data.List.Extra (notNull)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word64, Word8)
import GHC.Conc (getNumCapabilities)
import GHC.Exts (Int (I#), dataToTag#)
import Options (Options (..))
import qualified OsPath as OS
import PutMessage (putMessageLn)
import Reader (
    Env (..),
    ReaderIO,
 )
import System.Clock (Clock (Monotonic), diffTimeSpec, getTime, toNanoSecs)
import System.Directory.OsPath (doesDirectoryExist, getDirectoryContents)
import System.Environment (lookupEnv)
import System.IO (
    stderr,
    stdin,
    stdout,
 )
import System.OsPath (OsPath, osp, unsafeEncodeUtf, (</>))
import System.OsString as OS (isSuffixOf)
import System.Process (runProcess, waitForProcess)

data SearcherKind
    = Levenshtein
    | BoyerMoore
    | Semantic
    | Tokenizer
    | Regex

class Searcher (s :: SearcherKind) where
    run :: (MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match])

instance Searcher 'Levenshtein where
    run = Levenshtein.search

instance Searcher 'BoyerMoore where
    run = BoyerMoore.search

instance Searcher 'Semantic where
    run = Semantic.search

instance Searcher 'Tokenizer where
    run = Tokenizer.search

instance Searcher 'Regex where
    run = Regex.search

getSearcher :: Env -> (MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match])
getSearcher Env{..} =
    if
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> run @Levenshtein
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) -> run @BoyerMoore
        | (not . isRegexp) opt && semantic opt -> run @Semantic
        | (not . isRegexp) opt -> run @Tokenizer
        | isRegexp opt -> run @Regex
        | otherwise -> undefined

getSearcherForPatterns :: Env -> [T.Text] -> (MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> Bool -> ReaderIO [Match])
getSearcherForPatterns env@Env{..} patterns
    | isRegexp opt = mkRegexSearcher env patterns
    | otherwise =
        let base = getSearcher env
         in \lock info fname txt str -> base lock info fname txt patterns str

-- ---------------------------------------------------------------------------
-- O(1) bitmasks for FileType (65 constructors) and FileKind (6 constructors)
-- ---------------------------------------------------------------------------

fileTypeTag :: FileType -> Int
fileTypeTag !ft = I# (dataToTag# ft)
{-# INLINE fileTypeTag #-}

data FileTypeMask = FileTypeMask !Bool !Word64 !Word64

mkFileTypeMask :: [FileType] -> FileTypeMask
mkFileTypeMask [] = FileTypeMask True 0 0
mkFileTypeMask ts =
    let step (!lo, !hi) ft =
            let !ix = fileTypeTag ft
             in if ix < 64
                    then (lo .|. (1 `unsafeShiftL` ix), hi)
                    else (lo, hi .|. (1 `unsafeShiftL` (ix - 64)))
        (!l, !h) = foldl' step (0, 0) ts
     in FileTypeMask False l h

testFileTypeMask :: FileTypeMask -> FileType -> Bool
testFileTypeMask (FileTypeMask True _ _) _ = True
testFileTypeMask (FileTypeMask False lo hi) ft =
    let !ix = fileTypeTag ft
     in if ix < 64
            then (lo .&. (1 `unsafeShiftL` ix)) /= 0
            else (hi .&. (1 `unsafeShiftL` (ix - 64))) /= 0
{-# INLINE testFileTypeMask #-}

data FileKindMask = FileKindMask !Bool !Word8

mkFileKindMask :: [FileKind] -> FileKindMask
mkFileKindMask [] = FileKindMask True 0
mkFileKindMask ks =
    FileKindMask False (foldl' (\acc k -> acc .|. (1 `unsafeShiftL` fromEnum k)) 0 ks)

testFileKindMask :: FileKindMask -> FileKind -> Bool
testFileKindMask (FileKindMask True _) _ = True
testFileKindMask (FileKindMask False m) fk =
    (m .&. (1 `unsafeShiftL` fromEnum fk)) /= 0
{-# INLINE testFileKindMask #-}

matchFileEntry :: Options -> FileTypeMask -> FileKindMask -> Bool -> OsPath -> Maybe (FileType, FileTypeInfo)
matchFileEntry opt ftMask fkMask !isBare name =
    case fileTypeLookupBase opt isBare name of
        Nothing -> Nothing
        Just (ft, fk, fti)
            | testFileTypeMask ftMask ft && testFileKindMask fkMask fk -> Just (ft, fti)
            | otherwise -> Nothing
{-# INLINE matchFileEntry #-}

-- ---------------------------------------------------------------------------
-- Parallel work-stealing directory + file batch queue
-- ---------------------------------------------------------------------------

type FileTask = (OsPath, Maybe (FileType, FileTypeInfo))

data WorkItem
    = WorkDir !OsPath !Bool
    | WorkFiles ![FileTask]
    | WorkWait
    | WorkDone

data WorkState = WorkState
    { wsDirs :: ![(OsPath, Bool)]
    , wsFiles :: ![[FileTask]]
    , wsFileBatches :: !Int
    , wsActiveDirs :: !Int
    , wsWaiters :: ![MVar ()]
    }

batchChunkSize :: Int
batchChunkSize = 32

claimWork :: IORef WorkState -> MVar () -> IO WorkItem
claimWork wsRef myWake = do
    item <- atomicModifyIORef' wsRef $ \ws@WorkState{..} ->
        if
            | wsFileBatches >= 8, (b : bs) <- wsFiles ->
                (ws{wsFiles = bs, wsFileBatches = wsFileBatches - 1}, WorkFiles b)
            | (d, isSym) : ds <- wsDirs ->
                (ws{wsDirs = ds}, WorkDir d isSym)
            | (b : bs) <- wsFiles ->
                (ws{wsFiles = bs, wsFileBatches = wsFileBatches - 1}, WorkFiles b)
            | wsActiveDirs == 0 ->
                (ws, WorkDone)
            | otherwise ->
                (ws{wsWaiters = myWake : wsWaiters}, WorkWait)
    case item of
        WorkWait -> do
            takeMVar myWake
            claimWork wsRef myWake
        other -> pure other

completeDirWork :: IORef WorkState -> [(OsPath, Bool)] -> [[FileTask]] -> IO ()
completeDirWork wsRef newDirs newBatches = do
    let !nBatches = length newBatches
        !nDirs = length newDirs
        !addedUnits = nDirs + nBatches
    toWake <- atomicModifyIORef' wsRef $ \WorkState{..} ->
        let !active' = wsActiveDirs - 1 + nDirs
            !dirs' = newDirs <> wsDirs
            !files' = newBatches <> wsFiles
            !fCnt' = wsFileBatches + nBatches
            (!wakeNow, !stayAsleep)
                | active' == 0 && fCnt' == 0 = (wsWaiters, [])
                | addedUnits > 0 = splitAt addedUnits wsWaiters
                | otherwise = ([], wsWaiters)
         in ( WorkState
                { wsDirs = dirs'
                , wsFiles = files'
                , wsFileBatches = fCnt'
                , wsActiveDirs = active'
                , wsWaiters = stayAsleep
                }
            , wakeNow
            )
    mapM_ (\w -> putMVar w ()) toWake

scanOneDirectory ::
    WorkerBuffer ->
    Options ->
    FileTypeMask ->
    FileKindMask ->
    [OsPath] ->
    IORef (S.HashSet (Word64, Word64)) ->
    OsPath ->
    Bool ->
    IO ([(OsPath, Bool)], [[FileTask]], Int)
scanOneDirectory wb opt ftMask fkMask prunableDirs visitedInodes dirPath isSymDir = do
    proceed <-
        if follow opt && isSymDir
            then do
                mIno <- dirInodeBuf wb dirPath
                case mIno of
                    Nothing -> pure True
                    Just ino ->
                        atomicModifyIORef' visitedInodes $ \s ->
                            if ino `S.member` s
                                then (s, False)
                                else (S.insert ino s, True)
            else pure True
    if not proceed
        then pure ([], [], 0)
        else do
            mFast <- scandirBuf wb (follow opt) dirPath
            case mFast of
                Just (rawDirs, rawFiles) -> do
                    let !matchedFiles =
                            mapMaybe
                                ( \nm ->
                                    case matchFileEntry opt ftMask fkMask True nm of
                                        Nothing -> Nothing
                                        Just info ->
                                            let !full = joinPosixPath dirPath nm
                                             in Just (full, Just info)
                                )
                                rawFiles
                        !goodDirs =
                            if null prunableDirs
                                then map (\(nm, sym) -> (joinPosixPath dirPath nm, sym)) rawDirs
                                else
                                    filter
                                        (\(fullD, _) -> not (isPrunableDir fullD prunableDirs))
                                        (map (\(nm, sym) -> (joinPosixPath dirPath nm, sym)) rawDirs)
                        !batches = chunksOf batchChunkSize matchedFiles
                    pure (goodDirs, batches, length matchedFiles)
                Nothing -> do
                    -- Portable fallback (Windows or ultra-long path >= 4096 bytes)
                    xs <- E.catch (getDirectoryContents dirPath) (\(_ :: SomeException) -> pure [])
                    let nonDots = filter (not . isDot) xs
                    (dirs, files) <- partitionM (\p -> doesDirectoryExist (dirPath </> p)) nonDots
                    let !matchedFiles =
                            mapMaybe
                                ( \nm ->
                                    case matchFileEntry opt ftMask fkMask True nm of
                                        Nothing -> Nothing
                                        Just info -> Just (dirPath </> nm, Just info)
                                )
                                files
                        !goodDirs =
                            [ (dFull, True)
                            | d <- dirs
                            , let dFull = dirPath </> d
                            , null prunableDirs || not (isPrunableDir dFull prunableDirs)
                            ]
                    pure (goodDirs, chunksOf batchChunkSize matchedFiles, length matchedFiles)

-- ---------------------------------------------------------------------------
-- Main parallel search orchestrator
-- ---------------------------------------------------------------------------

startSearch :: [OsPath] -> [T.Text] -> [FileType] -> [FileKind] -> Bool -> ReaderIO ()
startSearch paths patterns fTypes fKinds isTermIn = do
    Env{..} <- ask

    startTime <- liftIO $ getTime Monotonic
    totalFiles <- liftIO $ newCounter 0
    totalMatchingFiles <- liftIO $ newCounter 0
    totalMatches <- liftIO $ newCounter 0

    lock <- liftIO $ newMVar ()
    outLock <- liftIO $ newMVar ()

    let Config{..} = conf
        Options{..} = opt
        !ftMask = mkFileTypeMask fTypes
        !fkMask = mkFileKindMask fKinds
        !prunableDirs = mkPrunableDirName <$> (unsafeEncodeUtf <$> configPruneDirs) <> (unsafeEncodeUtf <$> prune_dir)

    numCaps <- liftIO getNumCapabilities
    let !totalWorkers = max 1 numCaps

    let env = Env conf opt
        !runSearch = getSearcherForPatterns env patterns
    !lookupPrefilter <- liftIO $ buildCPrefilterLookup env patterns

    visitedInodes <- liftIO $ newIORef S.empty

    initState <- liftIO $ do
        if recursive || follow
            then do
                let roots = if null paths then [unsafeEncodeUtf "."] else paths
                (dirs, files) <- partitionM doesDirectoryExist roots
                let explicitTasks = map (\p -> (p, fileTypeInfoLookup opt p)) files
                when (notNull explicitTasks) $
                    void $ incrCounter (length explicitTasks) totalFiles
                let !batches = chunksOf batchChunkSize explicitTasks
                    !rootDirPairs = map (,True) dirs
                pure
                    WorkState
                        { wsDirs = rootDirPairs
                        , wsFiles = batches
                        , wsFileBatches = length batches
                        , wsActiveDirs = length rootDirPairs
                        , wsWaiters = []
                        }
            else do
                let rawTargets =
                        if null paths && not isTermIn
                            then [unsafeEncodeUtf ""]
                            else paths
                    explicitTasks = map (\p -> (p, fileTypeInfoLookup opt p)) rawTargets
                when (notNull explicitTasks) $
                    void $ incrCounter (length explicitTasks) totalFiles
                let !batches = chunksOf 16 explicitTasks
                pure
                    WorkState
                        { wsDirs = []
                        , wsFiles = batches
                        , wsFileBatches = length batches
                        , wsActiveDirs = 0
                        , wsWaiters = []
                        }

    wsRef <- liftIO $ newIORef initState
    matchingFiles :: IORef (S.HashSet (OsPath, Int)) <- liftIO $ newIORef S.empty

    workers <- forM ([0 .. totalWorkers - 1] :: [Int]) $ \idx -> do
        liftIO . asyncOn idx $
            withWorkerBuffer $ \wb -> do
                myWake <- newEmptyMVar
                let workerLoop = do
                        item <- claimWork wsRef myWake
                        case item of
                            WorkWait -> workerLoop
                            WorkDone ->
                                when verbose $
                                    putMessageLn lock stderr $
                                        "worker_" <> T.pack (show idx) <> " terminated!"
                            WorkDir dirPath isSymDir -> do
                                (newDirs, newBatches, nFiles) <-
                                    scanOneDirectory wb opt ftMask fkMask prunableDirs visitedInodes dirPath isSymDir
                                        `E.onException` completeDirWork wsRef [] []
                                when (nFiles > 0) $
                                    void $ incrCounter nFiles totalFiles
                                completeDirWork wsRef newDirs newBatches
                                workerLoop
                            WorkFiles fs' -> do
                                outBuilders <-
                                    runReaderT
                                        ( catMaybes
                                            <$> forM
                                                fs'
                                                ( \(f, mInfo) ->
                                                    liftIO $
                                                        E.catch
                                                            ( runReaderT
                                                                ( do
                                                                    let (!rawPf, !cPf) = lookupPrefilter mInfo
                                                                    mText <- liftIO $ readTargetFilteredBuf wb cPf rawPf f
                                                                    case mText of
                                                                        Nothing -> pure Nothing
                                                                        Just text -> do
                                                                            !matches <- take max_count <$> runSearch lock mInfo (getTargetName f) text strict
                                                                            when (vim || editor) $
                                                                                liftIO $
                                                                                    mapM_
                                                                                        ( \m ->
                                                                                            atomicModifyIORef'
                                                                                                matchingFiles
                                                                                                (\s -> (S.insert ((mFilePath &&& mLineNumb) m) s, ()))
                                                                                        )
                                                                                        matches
                                                                            when (notNull matches) $ do
                                                                                _ <- liftIO $ incrCounter (length matches) totalMatches
                                                                                void $ liftIO $ incrCounter 1 totalMatchingFiles
                                                                            putMatchesBSB matches
                                                                )
                                                                env
                                                            )
                                                            ( \e -> do
                                                                let msg = show (e :: SomeException)
                                                                putMessageLn lock stderr (prettyFileName conf opt (getTargetName f) <> ": error: " <> TL.pack (takeN 120 msg))
                                                                return Nothing
                                                            )
                                                )
                                        )
                                        env
                                unless (null outBuilders) $ do
                                    let !combined = mconcat (map (<> BSB.char7 '\n') outBuilders)
                                        !bs = LBS.toStrict (BSB.toLazyByteString combined)
                                    withMVar outLock (\_ -> BS.hPut stdout bs)
                                workerLoop
                workerLoop

    liftIO $ do
        mapM_ wait workers
        when verbose $ putMessageLn @T.Text lock stderr $ "All workers terminated!"

    endTime <- liftIO $ getTime Monotonic
    when stats $ liftIO $ do
        let diffTime = fromIntegral (toNanoSecs (diffTimeSpec endTime startTime)) / 1_000_000_000 :: Double
        total <- readCounter totalFiles
        matches <- readCounter totalMatches
        mfiles <- readCounter totalMatchingFiles
        putMessageLn @T.Text lock stderr $ "\nTotal matches " <> T.pack (show matches)
        putMessageLn @T.Text lock stderr $ "Matching files " <> T.pack (show mfiles)
        putMessageLn @T.Text lock stderr $ "Total files searched " <> T.pack (show total)
        putMessageLn @T.Text lock stderr $ "Elapsed time " <> T.pack (show diffTime) <> " seconds"

    -- run editor...
    when (vim || editor) $ liftIO $ do
        editor' <-
            if vim
                then return (Just "vim")
                else lookupEnv "EDITOR"

        files <- S.toList <$> readIORef matchingFiles
        let filesUnpacked = Data.Bifunctor.first (T.unpack . OS.toText) <$> files

        let editFiles =
                if fileline || configFileLine
                    then fmap (\(a, b) -> a <> ":" <> show b) filesUnpacked
                    else fmap fst filesUnpacked

        putStrLn $ "cgrep: open files " <> unwords editFiles <> "..."

        void $
            runProcess
                (fromJust $ editor' <|> Just "vi")
                editFiles
                Nothing
                Nothing
                (Just stdin)
                (Just stdout)
                (Just stderr)
                >>= waitForProcess

isDot :: OsPath -> Bool
isDot p = p == [osp|.|] || p == [osp|..|]
{-# INLINE isDot #-}

isPrunableDir :: OsPath -> [OsPath] -> Bool
isPrunableDir dir = any (`OS.isSuffixOf` pdir)
  where
    pdir = mkPrunableDirName dir
{-# INLINE isPrunableDir #-}

mkPrunableDirName :: OsPath -> OsPath
mkPrunableDirName xs
    | unsafeEncodeUtf "/" `OS.isSuffixOf` xs = xs
    | otherwise = xs <> unsafeEncodeUtf "/"
{-# INLINE mkPrunableDirName #-}

hasTokenizerOpt :: Options -> Bool
hasTokenizerOpt Options{..} =
    identifier
        || nativeType
        || keyword
        || number
        || string
        || operator

#ifdef ENABLE_PCRE
isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt || regex_pcre opt
#else
isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt
#endif
{-# INLINE isRegexp #-}

normalizePatterns :: Options -> [T.Text] -> [T.Text]
normalizePatterns opt@Options{..}
    | (not . isRegexp) opt && semantic = id
    | otherwise = map (ignoreCase opt)

buildCPrefilterLookup :: Env -> [T.Text] -> IO (Maybe (FileType, FileTypeInfo) -> (RawPrefilter, CPrefilter))
buildCPrefilterLookup env@(Env _ opt) patterns
    | (not . isRegexp) opt && semantic opt && not (invert_match opt || debug opt > 0 || edit_dist opt) && notNull patterns = do
        let rawLookup = buildRawPrefilterLookup env patterns
            FileTypeInfoMap ftMap = fileTypeInfoMap
            !rawNothing = rawLookup Nothing
        !cNothing <- compileCPrefilter rawNothing
        byFt <-
            Map.traverseWithKey
                ( \ft fti -> do
                    let !rpf = rawLookup (Just (ft, fti))
                    !cpf <- compileCPrefilter rpf
                    pure (rpf, cpf)
                )
                ftMap
        pure $ \case
            Nothing -> (rawNothing, cNothing)
            Just (ft, _) -> Map.findWithDefault (rawNothing, cNothing) ft byFt
    | otherwise = do
        let !rpf = buildRawPrefilterLookup env patterns Nothing
        !cpf <- compileCPrefilter rpf
        let !pair = (rpf, cpf)
        pure (const pair)

-- | Precompute a sound raw-@ByteString@ pre-filter lookup function once per search
-- (before spawning worker threads) for the already-normalized @patterns@ list.
buildRawPrefilterLookup :: Env -> [T.Text] -> (Maybe (FileType, FileTypeInfo) -> RawPrefilter)
buildRawPrefilterLookup (Env _ opt) patterns
    | invert_match opt || debug opt > 0 || edit_dist opt =
        const RawAcceptAll
    | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) =
        let nonEmpty = filter (not . T.null) patterns
            !hasCF = not (isContextFilterAll (mkContextFilter opt))
            !pf
                | null nonEmpty = RawRejectAll
                | otherwise = mkAsciiRawPrefilter hasCF (ignore_case opt) nonEmpty
         in const pf
    | (not . isRegexp) opt && semantic opt =
        if null patterns
            then const RawRejectAll
            else
                let FileTypeInfoMap ftMap = fileTypeInfoMap
                    pfilter = TokenFilter True True True True True True True
                    patFilt = contextFilterAll ~! contextBitComment
                    computeSem :: Maybe (FileType, FileTypeInfo) -> RawPrefilter
                    computeSem mInfo =
                        let patCF = mkContextFilterFn (fst <$> mInfo) patFilt True
                            toksPerPat = map (parseTokens pfilter (snd <$> mInfo) (strict opt) . patCF) patterns
                            atomsPerPat = map mkAtomsFromTokens toksPerPat
                            anchorsPerPat = map extractAtomAnchors atomsPerPat
                         in if all (not . null) anchorsPerPat
                                then
                                    let normAnchors = map (ignoreCase opt) (concat anchorsPerPat)
                                     in mkAsciiRawPrefilter False (ignore_case opt) normAnchors
                                else RawAcceptAll
                    nothingPrefilt = computeSem Nothing
                    byFt = Map.mapWithKey (\ft fti -> computeSem (Just (ft, fti))) ftMap
                 in \case
                        Nothing -> nothingPrefilt
                        Just (ft, fti) -> Map.findWithDefault (computeSem (Just (ft, fti))) ft byFt
    | (not . isRegexp) opt =
        if null patterns
            then const RawAcceptAll
            else
                let nonEmpty = filter (not . T.null) patterns
                    !pf
                        | null nonEmpty = RawRejectAll
                        | otherwise = mkAsciiRawPrefilter False (ignore_case opt) nonEmpty
                 in const pf
    | otherwise =
#ifdef ENABLE_PCRE
        if regex_pcre opt
            then const RawAcceptAll
            else posixRegexPrefilter
#else
        posixRegexPrefilter
#endif
  where
    posixRegexPrefilter :: Maybe (FileType, FileTypeInfo) -> RawPrefilter
    posixRegexPrefilter
        | null patterns = const RawRejectAll
        | otherwise =
            let !hasCF = not (isContextFilterAll (mkContextFilter opt))
                !pf = case traverse extractPosixLinePrefilter patterns of
                    Just allDisjs ->
                        let anchors =
                                [ maximumBy (comparing T.length) conj
                                | disjs <- allDisjs
                                , conj <- disjs
                                , not (null conj)
                                ]
                         in if null anchors
                                then RawAcceptAll
                                else mkAsciiRawPrefilter hasCF (ignore_case opt) anchors
                    Nothing -> RawAcceptAll
             in const pf
