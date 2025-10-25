--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

{-# LANGUAGE QuasiQuotes #-}

module CGrep.Search (
    startSearch,
    isRegexp,
)
where

import CGrep.Common (getTargetName, takeN)
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
    putMatches,
    prettyFileName,
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
import Control.Concurrent.Chan.Unagi.Bounded (
    newChan,
    readChan,
    writeChan,
 )
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

import qualified OsPath as OS
import System.OsPath (OsPath, OsString, osp, takeBaseName, unsafeEncodeUtf, (</>))
import System.OsString as OS (isSuffixOf, isPrefixOf)
import System.Process (runProcess, waitForProcess)
import qualified Data.Text.Lazy.IO as LTIO
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Control.Applicative ((<|>))

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
    let files' :: [OsPath] = (dir </>) <$> filter (\f -> fileFilter opt rcFileTypes rcFileKinds f && (not skip_test || isNotTestFile f)) files
    let dirs' :: [OsPath] = (dir </>) <$> filter (\d -> not $ isDot d) dirs

    -- run IO action
    mapM_ action (chunksOf 4 files')

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

    let Config{..} = conf
        Options{..} = opt

    numCaps <- liftIO getNumCapabilities

    let !parallelSearch = maybe True (> 1) jobs
    let multiplier = if parallelSearch then 4 else 1
    let totalJobs = multiplier * fromMaybe numCaps jobs

    -- create channels ...
    fileCh <- liftIO $ newChan 65536

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
                                , rcParallel = parallelSearch
                                }
                            opt
                            path
                            (S.singleton path)
                            ( \file' -> do
                                writeChan (fst fileCh) file'
                            )
                    _ -> writeChan (fst fileCh) [path]
            else
                forM_
                    ( if null paths && not isTermIn
                        then [(unsafeEncodeUtf "", 0 :: Int)]
                        else paths `zip` [0 ..]
                    )
                    (\(p, _idx) -> writeChan (fst fileCh) [p])


        replicateM_ totalJobs $ writeChan (fst fileCh) []

    -- launch the worker threads...
    matchingFiles :: IORef (S.HashSet (OsPath, Int)) <- liftIO $ newIORef S.empty

    let env = Env conf opt
        runSearch = getSearcher env

    workers <- forM ([0 .. totalJobs - 1] :: [Int]) $ \idx -> do
        let processor = idx `div` multiplier
        liftIO . asyncOn processor $ void . runExceptT $ do
            asRef <- liftIO $ newIORef ([] :: [Async ()])
            forever $ do
                fs <- liftIO $ readChan (snd fileCh)
                case fs of
                    [] -> liftIO $ readIORef asRef >>= mapM_ wait
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
                                                                !matches <- take max_count <$> runSearch (fileTypeInfoLookup opt f) f patterns strict
                                                                -- liftIO $ putStrLn $ show matches
                                                                when (vim || editor) $
                                                                    liftIO $
                                                                        mapM_ (modifyIORef matchingFiles . S.insert . (mFilePath &&& mLineNumb)) matches
                                                                putMatches matches
                                                                -- putOutputMatches []
                                                            )
                                                            env
                                                        )
                                                        ( \e -> do
                                                            let msg = show (e :: SomeException)
                                                            LTIO.hPutStrLn stderr (prettyFileName conf opt (getTargetName f) <> ": error: " <> TL.pack (takeN 120 msg))
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
                when (null fs) $ do
                    when (verbose > 3) $ putMessageLn stderr $ "[" <> T.pack (show idx) <> "]@" <> T.pack (show processor) <> " searcher terminated!"
                    throwE ()

    -- wait workers to complete the job
    liftIO $ mapM_ wait workers

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

getSearcher :: Env -> (Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [Match])
getSearcher Env{..} = do
    if
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> Levenshtein.search
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) -> BoyerMoore.search
        | (not . isRegexp) opt && semantic opt -> Semantic.search
        | (not . isRegexp) opt -> Tokenizer.search
        | isRegexp opt -> Regex.search
        | otherwise -> undefined

fileFilter :: Options -> [FileType] -> [FileKind] -> OsPath -> Bool
fileFilter opt fTypes fKinds filename = (fileFilterTypes typ) && (fileFilterKinds kin)
  where
    (typ, kin) = F.unzip $ fileTypeLookup opt filename
    fileFilterTypes = maybe False (liftA2 (||) (const $ null fTypes) (`elem` fTypes))
    fileFilterKinds = maybe False (liftA2 (||) (const $ null fKinds) (`elem` fKinds))

isNotTestFile :: OsPath -> Bool
isNotTestFile f =
    let fs = [ ([osp|_test|] `OS.isSuffixOf`)
             , ([osp|-test|] `OS.isSuffixOf`)
             , ([osp|test-|] `OS.isPrefixOf`)
             , ([osp|test_|] `OS.isPrefixOf`)
             , ([osp|test|] ==)
         ] :: [OsString -> Bool]
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
