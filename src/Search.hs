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
--nc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--

module Search (
      parallelSearch
    , isRegexp
) where

import Data.List ( isPrefixOf, isSuffixOf, partition, elemIndex, intersperse )
import Data.List.Split ( chunksOf )
import qualified Data.Map as M
import Data.Maybe ( fromJust, isJust, catMaybes, fromMaybe )
import Data.Function ( fix )
import qualified Data.Set as S

import Control.Exception as E ( catch, SomeException )
import Control.Concurrent ( forkIO, MVar, putMVar, forkOn, threadDelay )

import Control.Monad ( when, forM_, forever, unless, void, forM, replicateM_ )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Trans.Except ( runExceptT, throwE )
import Control.Monad.Trans.Reader
    ( ReaderT(runReaderT), ask, reader, ask, local )
import Control.Applicative
    ( Applicative(liftA2), Alternative((<|>)) )

import System.Environment ( lookupEnv )
import System.PosixCompat.Files as PC
    ( getFileStatus, getSymbolicLinkStatus, isDirectory, FileStatus )
import System.IO
    ( BufferMode(BlockBuffering),
      hSetBinaryMode,
      hSetBuffering,
      hPutStrLn,
      stderr,
      stdin,
      stdout,
      stderr,
      hPutStrLn )

import System.Process ( runProcess, waitForProcess )

import CGrep.Output
    ( putOutputElements,
      showFileName,
      Output(..),
      showFileName )
import CGrep.Common ( takeN, getTargetName, Text8, takeN )
import Options ( Options(..) )
import Config
    ( Config(Config, configFileTypes, configFileLine, configColorMatch,
             configColorFile, configColors, configPruneDirs),
      dumpPalette )
import Reader
    ( Env(Env, opt, conf),
      ReaderIO,
      Env(..) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Data.Tuple.Extra ( )
import qualified Data.Bifunctor

import qualified Data.Vector as V hiding ((!))
import Data.Vector ((!))

import Data.IORef
    ( modifyIORef, modifyIORef', newIORef, readIORef, IORef, atomicModifyIORef, atomicModifyIORef' )
import Control.Concurrent.Chan.Unagi.Bounded
    ( newChan, readChan, writeChan )

import System.Posix.Directory.Traversals ( getDirectoryContents )
import System.Posix.FilePath ( RawFilePath, takeBaseName, (</>) )
import System.Posix.Directory.Foreign (dtDir)
import System.Directory (makeAbsolute, canonicalizePath)
import Data.Functor ( void, (<&>), ($>))
import RawFilePath.Directory (doesDirectoryExist)
import Control.Arrow ( Arrow((&&&)) )
import Control.Concurrent.Async (forConcurrently, forConcurrently_, mapConcurrently_, async, Async, wait, asyncOn)

import qualified CGrep.Strategy.BoyerMoore       as BoyerMoore
import qualified CGrep.Strategy.Levenshtein      as Levenshtein
import qualified CGrep.Strategy.Regex            as Regex
import qualified CGrep.Strategy.Tokenizer        as Tokenizer
import qualified CGrep.Strategy.Semantic         as Semantic
import Control.Monad.Catch ( SomeException, MonadCatch(catch) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.FileType ( FileType )
import CGrep.FileTypeMap
    ( fileTypeInfoLookup, fileTypeLookup, FileTypeInfo )

import Control.Monad.Loops ( whileM_ )
import Verbose (putMsgLnVerbose, putMsgLn)
import Control.Concurrent.MVar ( newMVar, takeMVar )
import Data.IORef.Extra (atomicWriteIORef')
import CGrep.FileKind ( FileKind )
import qualified Data.List.NonEmpty as NE (unzip)


withRecursiveContents :: Options
                      -> RawFilePath
                      -> [FileType]
                      -> [FileKind]
                      -> [RawFilePath]
                      -> S.Set RawFilePath
                      -> IORef Int
                      -> ([RawFilePath] -> IO ()) -> IO ()
withRecursiveContents opt@Options{..} dir fTypes fKinds pdirs visited walkers action = do
    xs <- getDirectoryContents dir
    let (dirs, files) = partition ((== dtDir) . fst) xs

    -- filter the list of files
    let files' = (dir </>) . snd <$> filter (\f -> fileFilter opt fTypes  fKinds (snd f) && (not skip_test || isNotTestFile (snd f))) files
    let dirs'  = (dir </>) . snd <$> dirs

    -- run IO action
    mapM_ action (chunksOf 8 files')

    -- process directories recursively...
    foreach <- readIORef walkers >>= \tot -> do
        if tot < 64 then pure (forConcurrently_ @[])
                    else pure forM_

    foreach dirs' $ \dirPath -> do
        unless (isPrunableDir dirPath pdirs) $
             -- this is a good directory, unless already visited...
             -- this is a good directory, unless already visited...
             -- this is a good directory, unless already visited...
             -- this is a good directory, unless already visited...

             -- this is a good directory, unless already visited...

             -- this is a good directory, unless already visited...

             -- this is a good directory, unless already visited...
             -- this is a good directory, unless already visited...

             -- this is a good directory, unless already visited...
            makeRawAbsolute dirPath >>= \cpath ->
                unless (cpath `S.member` visited) $ incrRef walkers *>
                    withRecursiveContents opt dirPath fTypes fKinds pdirs (S.insert cpath visited) walkers action

    decrRef walkers


parallelSearch :: [RawFilePath] -> [C.ByteString] -> [FileType] -> [FileKind] -> Bool -> ReaderIO ()
parallelSearch paths patterns fTypes fKinds isTermIn = do
    Env{..} <- ask

    let Config{..} = conf
        Options{..} = opt

    let multiplier = 4
        jobs' = fromMaybe 1 jobs
        totalJobs = jobs' * multiplier

    -- create channels ...
    fileCh <- liftIO $ newChan 65536

    -- recursively traverse the filesystem ...
    _ <- liftIO . forkOn 0 $ do
        walkers <- newIORef (0 :: Int)
        if recursive || follow
            then forM_ (if null paths then ["."] else paths) $ \p ->
                doesDirectoryExist p >>= \case
                    True -> incrRef walkers *>
                            withRecursiveContents opt p fTypes fKinds
                                (mkPrunableDirName <$> configPruneDirs <> (C.pack <$> prune_dir)) (S.singleton p) walkers (do
                                    writeChan (fst fileCh))
                    _     ->  writeChan (fst fileCh) [p]
            else forM_ (if null paths && not isTermIn
                    then [("", 0)]
                    else paths `zip` [0..]) (\(p, idx) -> writeChan (fst fileCh) [p])

        -- enqueue EOF messages...
        when (verbose > 0) $ putMsgLn @Text8 stderr "filesystem traversal completed!"
        replicateM_ totalJobs $ writeChan (fst fileCh) []

    -- launch the worker threads...
    matchingFiles <- liftIO $ newIORef S.empty

    let env = Env conf opt
        runSearch = getSearcher env

    workers <- forM ([0 .. totalJobs-1] :: [Int]) $ \idx -> do
        let processor = 1 + idx `div` multiplier
        liftIO . asyncOn processor $ void . runExceptT $ do
            asRef <- liftIO $ newIORef ([] :: [Async ()])
            forever $ do
                fs <- liftIO $ readChan (snd fileCh)
                liftIO $ E.catch (
                    case fs of
                        [] -> liftIO $ readIORef asRef >>= mapM_ wait
                        fs -> runReaderT (do
                            out <- catMaybes <$> forM fs (\f -> do
                                    out' <- take max_count <$> runSearch (fileTypeInfoLookup opt f) f patterns
                                    when (vim || editor) $
                                        liftIO $ mapM_ (modifyIORef matchingFiles . S.insert . (outFilePath &&& outLineNumb)) out'
                                    putOutputElements out')
                            unless (null out) $
                                liftIO $ async (do
                                    let !dump = LB.toStrict $ B.toLazyByteString (mconcat ((<> B.char8 '\n') <$> out))
                                    B.hPut stdout dump) >>= \a -> modifyIORef' asRef (a:)
                            ) env
                    ) (\e -> let msg = show (e :: SomeException) in
                            C.hPutStrLn stderr (showFileName conf opt (getTargetName (head fs)) <> ": error: " <> C.pack (takeN 120 msg)))
                when (null fs) $ do
                    when (verbose > 0) $ putMsgLn stderr $ "[" <> C.pack (show idx) <> "]@" <> C.pack (show processor) <>" searcher done!"
                    throwE ()

    -- wait workers to complete the job
    liftIO $ mapM_ wait workers

    -- run editor...
    when (vim || editor ) $ liftIO $ do
        editor' <- if vim
                    then return (Just "vim")
                    else lookupEnv "EDITOR"

        files <- S.toList <$> readIORef matchingFiles
        let filesUnpacked = Data.Bifunctor.first C.unpack <$> files

        let editFiles = (if fileline || configFileLine
                            then fmap (\(a,b) -> a <> ":" <> show b)
                            else fmap fst) filesUnpacked

        putStrLn $ "cgrep: open files " <> unwords editFiles <> "..."

        void $ runProcess (fromJust $ editor' <|> Just "vi")
                          editFiles
                          Nothing
                          Nothing
                          (Just stdin)
                          (Just stdout)
                          (Just stderr) >>= waitForProcess

getSearcher :: Env -> (Maybe (FileType, FileTypeInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output])
getSearcher Env{..} = do
  if | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> Levenshtein.search
     | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt)                  -> BoyerMoore.search
     | (not . isRegexp) opt && semantic opt                                                     -> Semantic.search
     | (not . isRegexp) opt                                                                     -> Tokenizer.search
     | isRegexp opt                                                                             -> Regex.search
     | otherwise                                                                                -> undefined


makeRawAbsolute :: RawFilePath -> IO RawFilePath
makeRawAbsolute p = makeAbsolute (C.unpack p) <&> C.pack
{-# INLINE makeRawAbsolute #-}

incrRef  :: IORef Int -> IO ()
incrRef ref = atomicModifyIORef' ref (\n -> (n+1, ()))
{-# INLINE incrRef #-}

decrRef :: IORef Int -> IO ()
decrRef ref = atomicModifyIORef' ref (\n -> (n-1, ()))
{-# INLINE decrRef #-}


fileFilter :: Options -> [FileType] -> [FileKind] -> RawFilePath -> Bool
fileFilter opt fTypes fKinds filename = fileFilterTypes typ && fileFilterKinds kin
    where (typ, kin) = NE.unzip $ fileTypeLookup opt filename
          fileFilterTypes = maybe False (liftA2 (||) (const $ null fTypes) (`elem` fTypes))
          fileFilterKinds = maybe False (liftA2 (||) (const $ null fKinds) (`elem` fKinds))


isNotTestFile :: RawFilePath -> Bool
isNotTestFile f =
    let fs = [("_test" `C.isSuffixOf`), ("-test" `C.isSuffixOf`), ("test-" `C.isPrefixOf`), ("test_" `C.isPrefixOf`), ("test" == )] :: [C.ByteString -> Bool]
        in not $ any ($ takeBaseName f) fs
{-# INLINE isNotTestFile #-}


isPrunableDir:: RawFilePath -> [RawFilePath] -> Bool
isPrunableDir dir = any (`C.isSuffixOf` pdir)
    where pdir = mkPrunableDirName dir
{-# INLINE isPrunableDir #-}


mkPrunableDirName :: RawFilePath -> RawFilePath
mkPrunableDirName xs | "/" `C.isSuffixOf` xs = xs
                     | otherwise = xs <> "/"
{-# INLINE mkPrunableDirName #-}


(.!.) :: V.Vector a -> Int -> a
v .!. i = v ! (i `mod` V.length v)
{-# INLINE  (.!.) #-}


hasFileType :: RawFilePath -> Options -> [FileType] -> Bool
hasFileType path opt xs = isJust $ fileTypeLookup opt path >>= (\(typ, _) -> typ `elemIndex` xs)
{-# INLINE hasFileType #-}


hasTokenizerOpt :: Options -> Bool
hasTokenizerOpt Options{..} =
  identifier ||
  nativeType ||
  keyword    ||
  number     ||
  string     ||
  operator


isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt || regex_pcre opt
{-# INLINE isRegexp #-}
