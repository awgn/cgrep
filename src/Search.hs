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

module Search (
    startSearch,
    isRegexp,
)
where

import CGrep.Common (Text8, getTargetName, takeN)
import CGrep.FileKind (FileKind)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo,
 )
import CGrep.FileTypeMapTH (
    fileTypeInfoLookup,
    fileTypeLookup,
 )
import CGrep.Output (
    Output (..),
    putOutputElements,
    showFileName,
 )
import qualified CGrep.Strategy.BoyerMoore as BoyerMoore
import qualified CGrep.Strategy.Levenshtein as Levenshtein
import qualified CGrep.Strategy.Regex as Regex
import qualified CGrep.Strategy.Semantic as Semantic
import qualified CGrep.Strategy.Tokenizer as Tokenizer
import qualified Codec.Binary.UTF8.String as UC
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
    dumpPalette,
 )
import Control.Applicative (
    Alternative ((<|>)),
    Applicative (liftA2),
 )
import Control.Arrow (Arrow ((&&&)))
import Control.Concurrent (MVar, forkIO, forkOn, putMVar, threadDelay)
import Control.Concurrent.Async (Async, async, asyncOn, forConcurrently, forConcurrently_, mapConcurrently_, wait)
import Control.Concurrent.Chan.Unagi.Bounded (
    newChan,
    readChan,
    writeChan,
 )
import Control.Concurrent.MVar (newMVar, takeMVar)
import Control.Exception as E (SomeException, catch)
import Control.Monad (forM, forM_, forever, replicateM_, unless, void, when)
import Control.Monad.Catch (MonadCatch (catch), SomeException)
import Control.Monad.Extra (partitionM)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (whileM_)
import Control.Monad.Trans (MonadIO (liftIO))
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Reader (
    ReaderT (runReaderT),
    ask,
    local,
    reader,
 )
import qualified Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Function (fix)
import Data.Functor (void, ($>), (<&>))
import Data.Functor as F (unzip)
import Data.IORef (
    IORef,
    atomicModifyIORef,
    atomicModifyIORef',
    modifyIORef,
    modifyIORef',
    newIORef,
    readIORef,
 )
import Data.IORef.Extra (atomicWriteIORef')
import Data.Int (Int64)
import Data.List (elemIndex, intersperse, isPrefixOf, isSuffixOf, partition)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust)
import qualified Data.Set as S
import Data.Tuple.Extra ()
import Data.Vector ((!))
import qualified Data.Vector as V hiding ((!))
import Debug.Trace
import GHC.Conc (getNumCapabilities)
import Options (Options (..))
import OsPath (toByteString)
import PutMessage (putMessageLn, putMessageLnVerb)
import Reader (
    Env (..),
    ReaderIO,
 )
import System.Directory.OsPath (canonicalizePath, doesDirectoryExist, getDirectoryContents, makeAbsolute)
import System.Environment (lookupEnv)
import System.IO (
    BufferMode (BlockBuffering),
    hPutStrLn,
    hSetBinaryMode,
    hSetBuffering,
    stderr,
    stdin,
    stdout,
 )
import System.OsPath (OsPath, takeBaseName, takeExtension, unsafeEncodeUtf, (</>))
import System.OsString as OS (isSuffixOf)
import System.PosixCompat.Files as PC (
    FileStatus,
    getFileStatus,
    getSymbolicLinkStatus,
    isDirectory,
 )
import System.Process (runProcess, waitForProcess)

isDot :: OsPath -> Bool
isDot p = let n = toByteString p in n == "." || n == ".."
{-# INLINE isDot #-}

data RecursiveContext = RecursiveContext
    { rcFileTypes :: [FileType]
    , rcFileKinds :: [FileKind]
    , rcPrunableDirs :: [OsPath]
    , rcWalkers :: IORef Int
    , rcParallel :: Bool
    }

withRecursiveContents ::
    RecursiveContext ->
    Options ->
    OsPath ->
    S.Set OsPath ->
    ([OsPath] -> IO ()) ->
    IO ()
withRecursiveContents ctx@RecursiveContext{..} opt@Options{..} dir visited action = do
    xs <- getDirectoryContents dir
    (dirs, files) <- partitionM (\p -> doesDirectoryExist (dir </> p)) xs
    -- putStrLn $ "CURRENT DIR:" <> show dir <> " CONTENT:" <> show xs <>  " SUBDIR:" <> show dirs <> " FILES:" <> show files

    -- filter the list of files
    let files' :: [OsPath] = (dir </>) <$> filter (\f -> fileFilter opt rcFileTypes rcFileKinds f && (not skip_test || isNotTestFile f)) files
    let dirs' :: [OsPath] = (dir </>) <$> filter (\d -> not $ isDot d) dirs

    -- run IO action
    mapM_ action (chunksOf 8 files')

    -- process directories recursively...
    foreach <-
        readIORef rcWalkers >>= \tot -> do
            if tot < 64
                then pure (forConcurrently_ @[])
                else pure forM_

    foreach dirs' $ \dirPath -> do
        unless (isPrunableDir dirPath rcPrunableDirs) $
            -- this is a good directory, unless already visited...
            makeAbsolute dirPath >>= \cpath -> do
                unless (cpath `S.member` visited) $ do
                    incrRef rcWalkers
                        *> withRecursiveContents ctx opt dirPath (S.insert cpath visited) action

    decrRef rcWalkers

startSearch :: [OsPath] -> [C.ByteString] -> [FileType] -> [FileKind] -> Bool -> ReaderIO ()
startSearch paths patterns fTypes fKinds isTermIn = do
    Env{..} <- ask

    let Config{..} = conf
        Options{..} = opt

    numCaps <- liftIO getNumCapabilities

    let !parallelSearch = maybe True (> 1) jobs
    let multiplier = if parallelSearch then 4 else 1
    let totalJobs = multiplier * fromMaybe (numCaps - 1) jobs

    -- create channels ...
    fileCh <- liftIO $ newChan 4096

    -- recursively traverse the filesystem ...
    _ <- liftIO . forkOn 0 $ do
        walkers <- newIORef (0 :: Int)
        if recursive || follow
            then forM_ (if null paths then [unsafeEncodeUtf "."] else paths) $ \path ->
                doesDirectoryExist path >>= \case
                    True ->
                        incrRef walkers
                            *> withRecursiveContents
                                RecursiveContext
                                    { rcFileTypes = fTypes
                                    , rcFileKinds = fKinds
                                    , rcPrunableDirs = (mkPrunableDirName <$> (unsafeEncodeUtf <$> configPruneDirs) <> (unsafeEncodeUtf <$> prune_dir))
                                    , rcWalkers = walkers
                                    , rcParallel = parallelSearch
                                    }
                                opt
                                path
                                (S.singleton path)
                                ( \file -> do
                                    writeChan (fst fileCh) file
                                )
                    _ -> writeChan (fst fileCh) [path]
            else
                forM_
                    ( if null paths && not isTermIn
                        then [(unsafeEncodeUtf "", 0)]
                        else paths `zip` [0 ..]
                    )
                    (\(p, idx) -> writeChan (fst fileCh) [p])

        -- enqueue EOF messages...
        when (verbose > 0) $
            putMessageLn @Text8 stderr "filesystem traversal completed!"

        replicateM_ totalJobs $ writeChan (fst fileCh) []

    -- launch the worker threads...
    matchingFiles :: IORef (S.Set (OsPath, Int64)) <- liftIO $ newIORef S.empty

    let env = Env conf opt
        runSearch = getSearcher env

    let firstProcessor = if numCaps > 1 then 1 else 0

    workers <- forM ([0 .. totalJobs - 1] :: [Int]) $ \idx -> do
        let processor = firstProcessor + idx `div` multiplier
        liftIO . asyncOn processor $ void . runExceptT $ do
            asRef <- liftIO $ newIORef ([] :: [Async ()])
            forever $ do
                fs <- liftIO $ readChan (snd fileCh)
                case fs of
                    [] -> liftIO $ readIORef asRef >>= mapM_ wait
                    fs -> do
                        out <-
                            liftIO $
                                runReaderT
                                    ( catMaybes
                                        <$> forM
                                            fs
                                            ( \f ->
                                                liftIO $
                                                    E.catch
                                                        ( runReaderT
                                                            ( do
                                                                out' <- take max_count <$> runSearch (fileTypeInfoLookup opt f) f patterns strict
                                                                when (vim || editor) $
                                                                    liftIO $
                                                                        mapM_ (modifyIORef matchingFiles . S.insert . (outFilePath &&& outLineNumb)) out'
                                                                putOutputElements out'
                                                            )
                                                            env
                                                        )
                                                        ( \e -> do
                                                            let msg = show (e :: SomeException)
                                                            C.hPutStrLn stderr (showFileName conf opt (getTargetName f) <> ": error: " <> C.pack (takeN 120 msg))
                                                            return Nothing
                                                        )
                                            )
                                    )
                                    env
                        unless (null out) $
                            liftIO $
                                async
                                    ( do
                                        let !dump = LB.toStrict $ B.toLazyByteString (mconcat ((<> B.char8 '\n') <$> out))
                                        B.hPut stdout dump
                                    )
                                    >>= \a -> modifyIORef' asRef (a :)
                when (null fs) $ do
                    when (verbose > 3) $ putMessageLn stderr $ "[" <> C.pack (show idx) <> "]@" <> C.pack (show processor) <> " searcher terminated!"
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
        let filesUnpacked = Data.Bifunctor.first (C.unpack . toByteString) <$> files

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

getSearcher :: Env -> (Maybe (FileType, FileTypeInfo) -> OsPath -> [Text8] -> Bool -> ReaderIO [Output])
getSearcher Env{..} = do
    if
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> Levenshtein.search
        | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) -> BoyerMoore.search
        | (not . isRegexp) opt && semantic opt -> Semantic.search
        | (not . isRegexp) opt -> Tokenizer.search
        | isRegexp opt -> Regex.search
        | otherwise -> undefined

incrRef :: IORef Int -> IO ()
incrRef ref = atomicModifyIORef' ref (\n -> (n + 1, ()))
{-# INLINE incrRef #-}

decrRef :: IORef Int -> IO ()
decrRef ref = atomicModifyIORef' ref (\n -> (n - 1, ()))
{-# INLINE decrRef #-}

fileFilter :: Options -> [FileType] -> [FileKind] -> OsPath -> Bool
fileFilter opt fTypes fKinds filename = (fileFilterTypes typ) && (fileFilterKinds kin)
  where
    (typ, kin) = F.unzip $ fileTypeLookup opt filename
    fileFilterTypes typ = maybe False (liftA2 (||) (const $ null fTypes) (`elem` fTypes)) typ
    fileFilterKinds typ = maybe False (liftA2 (||) (const $ null fKinds) (`elem` fKinds)) typ

isNotTestFile :: OsPath -> Bool
isNotTestFile f =
    let fs = [("_test" `C.isSuffixOf`), ("-test" `C.isSuffixOf`), ("test-" `C.isPrefixOf`), ("test_" `C.isPrefixOf`), ("test" ==)] :: [C.ByteString -> Bool]
        basename = toByteString (takeBaseName f)
     in not $ any ($ basename) fs
{-# INLINE isNotTestFile #-}

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

(.!.) :: V.Vector a -> Int -> a
v .!. i = v ! (i `mod` V.length v)
{-# INLINE (.!.) #-}

hasFileType :: OsPath -> Options -> [FileType] -> Bool
hasFileType path opt xs = isJust $ fileTypeLookup opt path >>= (\(typ, _) -> typ `elemIndex` xs)
{-# INLINE hasFileType #-}

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
