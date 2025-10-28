{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
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
-- nc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE QuasiQuotes #-}

module CGrep.Search (
    startSearch,
    isRegexp,
)
where

import CGrep.Common (getTargetContents, getTargetName, takeN)
import CGrep.FileKind (FileKind)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo,
 )
import CGrep.FileTypeMapTH (
    fileTypeInfoLookup,
    fileTypeLookup,
 )
import CGrep.Match (
    Match (..),
    prettyFileName,
    putMatches,
 )
import qualified CGrep.Strategy.BoyerMoore as BoyerMoore
import qualified CGrep.Strategy.Levenshtein as Levenshtein
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

import Control.Arrow (Arrow ((&&&)))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (Async, async, asyncOn, forConcurrently_, wait)
import Control.Exception as E (SomeException, catch)
import Control.Monad (forM, forM_, forever, replicateM_, unless, void, when)
import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Reader (
    ReaderT (runReaderT),
    ask,
 )
import qualified Data.Bifunctor
import Data.Functor as F (unzip)
import qualified Data.HashSet as S
import Data.IORef (
    IORef,
    modifyIORef,
    modifyIORef',
    newIORef,
    readIORef,
 )
import Data.List.Split (chunksOf)
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Tuple.Extra ()
import GHC.Conc (getNumCapabilities)
import Options (Options (..))
import PutMessage (putMessageLn)
import Reader (
    Env (..),
    ReaderIO,
 )
import System.Directory.OsPath (doesDirectoryExist, getDirectoryContents, makeAbsolute)
import System.Environment (lookupEnv)
import System.IO (
    stderr,
    stdin,
    stdout,
 )

import Control.Applicative ((<|>))
import Control.Concurrent (MVar)
import Control.Concurrent.Classy (newBoundedChan, readBoundedChan)
import Control.Concurrent.Classy.BoundedChan (writeBoundedChan)
import Control.Concurrent.Extra (newMVar)
import Data.Atomics.Counter (incrCounter, newCounter, readCounter)
import Data.List.Extra (notNull)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.IO as LTIO
import qualified OsPath as OS
import System.Clock
import System.OsPath (OsPath, OsString, osp, takeBaseName, unsafeEncodeUtf, (</>))
import System.OsString as OS (isPrefixOf, isSuffixOf)
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
getSearcher Env{..} = do
    if
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> run @Levenshtein
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) -> run @BoyerMoore
        | (not . isRegexp) opt && semantic opt -> run @Semantic
        | (not . isRegexp) opt -> run @Tokenizer
        | isRegexp opt -> run @Regex
        | otherwise -> undefined

data RecursiveContext = RecursiveContext
    { rcFileTypes :: [FileType]
    , rcFileKinds :: [FileKind]
    , rcPrunableDirs :: [OsPath]
    , rcParallel :: Bool
    }

withRecursiveContents ::
    RecursiveContext ->
    Options ->
    OsPath ->
    S.HashSet OsPath ->
    ([OsPath] -> IO ()) ->
    IO ()
withRecursiveContents ctx@RecursiveContext{..} opt@Options{..} dir visited action = do
    xs <- getDirectoryContents dir
    (dirs, files) <- partitionM (\p -> doesDirectoryExist (dir </> p)) xs

    -- filter the list of files
    let files' :: [OsPath] = (dir </>) <$> filter (\f -> fileFilter opt rcFileTypes rcFileKinds f) files
    let dirs' :: [OsPath] = (dir </>) <$> filter (\d -> not $ isDot d) dirs

    -- run IO action
    mapM_ action (chunksOf 16 files')

    foreach <-
        if rcParallel
            then pure (forConcurrently_ @[])
            else pure forM_

    foreach dirs' $ \dirPath -> do
        unless (isPrunableDir dirPath rcPrunableDirs) $
            -- this is a good directory, unless already visited...
            makeAbsolute dirPath >>= \cpath -> do
                unless (cpath `S.member` visited) $ withRecursiveContents ctx opt dirPath (S.insert cpath visited) action

startSearch :: [OsPath] -> [T.Text] -> [FileType] -> [FileKind] -> Bool -> ReaderIO ()
startSearch paths patterns fTypes fKinds isTermIn = do
    Env{..} <- ask

    startTime <- liftIO $ getTime Monotonic
    totalFiles <- liftIO $ newCounter 0
    totalMatchingFiles <- liftIO $ newCounter 0
    totalMatches <- liftIO $ newCounter 0

    lock <- liftIO $ newMVar ()

    let Config{..} = conf
        Options{..} = opt

    numCaps <- liftIO getNumCapabilities

    let multiplier = 4
    let totalJobs = multiplier * fromMaybe (max (numCaps - 1) 1) jobs

    -- create channels ...
    fileCh <- liftIO $ newBoundedChan 65536

    -- recursively traverse the filesystem ...
    _ <- liftIO . forkIO $ do
        if recursive || follow
            then forM_ (if null paths then [unsafeEncodeUtf "."] else paths) $ \path ->
                doesDirectoryExist path >>= \case
                    True ->
                        withRecursiveContents
                            RecursiveContext
                                { rcFileTypes = fTypes
                                , rcFileKinds = fKinds
                                , rcPrunableDirs = (mkPrunableDirName <$> (unsafeEncodeUtf <$> configPruneDirs) <> (unsafeEncodeUtf <$> prune_dir))
                                , rcParallel = True
                                }
                            opt
                            path
                            (S.singleton path)
                            ( \paths' -> do
                                when (notNull paths') $
                                    -- putMessageLn @T.Text lock stderr $ "Discovered empty directory!"
                                    incrCounter (length paths') totalFiles >> writeBoundedChan fileCh paths'
                            )
                    _ -> incrCounter 1 totalFiles >> writeBoundedChan fileCh [path]
            else
                forM_
                    ( if null paths && not isTermIn
                        then [(unsafeEncodeUtf "", 0 :: Int)]
                        else paths `zip` [0 ..]
                    )
                    (\(p, _idx) -> incrCounter 1 totalFiles >> writeBoundedChan fileCh [p])

        when verbose $
            putMessageLn @T.Text lock stderr $
                "File discovery completed..."

        replicateM_ totalJobs $ writeBoundedChan fileCh []

    -- launch the worker threads...
    matchingFiles :: IORef (S.HashSet (OsPath, Int)) <- liftIO $ newIORef S.empty

    let env = Env conf opt
        runSearch = getSearcher env

    workers <- forM ([0 .. totalJobs - 1] :: [Int]) $ \idx -> do
        let processor = 1 + idx `div` multiplier
        liftIO . asyncOn processor $ void . runExceptT $ do
            asRef <- liftIO $ newIORef ([] :: [Async ()])
            forever $ do
                paths' <- liftIO $ readBoundedChan fileCh
                case paths' of
                    [] -> liftIO $ do
                        -- when verbose $
                        --    putMessageLn @T.Text lock stderr $ "worker_" <> T.pack (show idx) <> "@" <> T.pack (show processor) <> " received termination signal!"
                        readIORef asRef >>= mapM_ wait
                    fs' -> do
                        out <-
                            liftIO $
                                runReaderT
                                    ( catMaybes
                                        <$> forM
                                            fs'
                                            ( \f ->
                                                liftIO $
                                                    E.catch
                                                        ( runReaderT
                                                            ( do
                                                                text <- liftIO $ getTargetContents f
                                                                !matches <- take max_count <$> runSearch lock (fileTypeInfoLookup opt f) (getTargetName f) text patterns strict

                                                                when (vim || editor) $
                                                                    liftIO $
                                                                        mapM_ (modifyIORef matchingFiles . S.insert . (mFilePath &&& mLineNumb)) matches
                                                                -- putMessage @T.Text lock stderr $ "."
                                                                _ <- liftIO $ incrCounter (length matches) totalMatches
                                                                when (notNull matches) $ void $ liftIO $ incrCounter 1 totalMatchingFiles
                                                                putMatches matches
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
                        unless (null out) $
                            liftIO $
                                async
                                    ( do
                                        let !dump = TLB.toLazyText (mconcat ((<> TLB.singleton '\n') <$> out))
                                        LTIO.hPutStr stdout dump
                                    )
                                    >>= \a -> modifyIORef' asRef (a :)
                when (null paths') $ do
                    when verbose $
                        putMessageLn lock stderr $
                            "worker_" <> T.pack (show idx) <> "@" <> T.pack (show processor) <> " terminated!"
                    throwE ()

    -- wait workers to complete the job
    liftIO $ do
        mapM_ wait workers
        when verbose $ putMessageLn @T.Text lock stderr $ "All workers terminated!"

    endTime <- liftIO $ getTime Monotonic
    when stats $ liftIO $ do
        let diffTime = fromIntegral (toNanoSecs (diffTimeSpec endTime startTime)) / 1_000_000_000 :: Double
        total <- (readCounter totalFiles)
        matches <- (readCounter totalMatches)
        mfiles <- (readCounter totalMatchingFiles)
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

fileFilter :: Options -> [FileType] -> [FileKind] -> OsPath -> Bool
fileFilter opt fTypes fKinds filename = (fileFilterTypes typ) && (fileFilterKinds kin)
  where
    (typ, kin) = F.unzip $ fileTypeLookup opt filename
    fileFilterTypes = maybe False (liftA2 (||) (const $ null fTypes) (`elem` fTypes))
    fileFilterKinds = maybe False (liftA2 (||) (const $ null fKinds) (`elem` fKinds))

isNotTestFile :: OsPath -> Bool
isNotTestFile f =
    let fs =
            [ ([osp|_test|] `OS.isSuffixOf`)
            , ([osp|-test|] `OS.isSuffixOf`)
            , ([osp|test-|] `OS.isPrefixOf`)
            , ([osp|test_|] `OS.isPrefixOf`)
            , ([osp|test|] ==)
            ] ::
                [OsString -> Bool]
        basename = takeBaseName f
     in not $ any ($ basename) fs
{-# INLINE isNotTestFile #-}

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

-- (.!.) :: V.Vector a -> Int -> a
-- v .!. i = v ! (i `mod` V.length v)
-- {-# INLINE (.!.) #-}

-- hasFileType :: OsPath -> Options -> [FileType] -> Bool
-- hasFileType path opt xs = isJust $ fileTypeLookup opt path >>= (\(typ, _) -> typ `elemIndex` xs)
-- {-# INLINE hasFileType #-}

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
