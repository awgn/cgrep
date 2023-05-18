-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Search (
    parallelSearch
) where

import Data.List ( isPrefixOf, isSuffixOf, partition )
import Data.List.Split ( chunksOf )
import qualified Data.Map as M
import Data.Maybe ( fromJust )
import Data.Function ( fix )
import qualified Data.Set as Set

import Control.Exception as E ( catch, SomeException )
import Control.Concurrent ( forkIO )

import Control.Monad ( when, forM_, forever, unless, void, forM )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Trans.Except ( runExceptT, throwE )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import Control.Applicative
    ( Applicative(liftA2), Alternative((<|>)) )

import System.Directory
    ( canonicalizePath, doesDirectoryExist, listDirectory )

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
      stdout )

import System.Process ( runProcess, waitForProcess )

import CGrep.Search ( run )
import CGrep.Language ( Language )
import CGrep.LanguagesMap ( languageLookup )

import CGrep.Output
    ( putOutputElements,
      showFileName,
      Output(outLineNumb, outFilePath) )
import CGrep.Common ( takeN, getTargetName )
import Options ( Options(..) )
import Config
    ( Config(Config, configLanguages, configFileLine, configColorMatch,
             configColorFile, configColors, configPruneDirs) )
import Reader ( Env(Env, langInfo, langType, opt, conf), OptionIO )

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Data.Tuple.Extra ( (&&&) )
import System.FilePath.Posix ( (</>), takeBaseName )
import GHC.Conc ( forkIO )
import qualified Data.Bifunctor

import qualified Data.Vector as V hiding ((!))
import Data.Vector ((!))

import Data.IORef
    ( modifyIORef, modifyIORef', newIORef, readIORef )
import Control.Concurrent.Chan.Unagi.Bounded
    ( newChan, readChan, writeChan )

-- push file names in Chan...

data StatusFile = StatusFile{
    fullPath :: FilePath,
    status :: FileStatus
}

getStatusFile :: Bool -> FilePath -> IO StatusFile
getStatusFile True path =
    PC.getSymbolicLinkStatus path >>= \s -> pure $ StatusFile path s
getStatusFile False path =
    PC.getFileStatus path >>= \s -> pure $ StatusFile path s
{-# INLINE getStatusFile #-}


withRecursiveContents :: Options -> FilePath -> [Language] -> [String] -> Set.Set FilePath -> ([FilePath] -> IO ()) -> IO ()
withRecursiveContents opt@Options{..} dir langs pdirs visited action = do
    xs <- mapM (getStatusFile follow . (dir </>)) =<< listDirectory dir

    let (dirs, files) = partition (PC.isDirectory . status) xs

    -- filter the list of files

    let files' = fullPath <$> filter (\f -> fileFilter opt langs (fullPath f) && (not skip_test || isNotTestFile (fullPath f))) files

    -- run action

    unless (null files') $ do
        let bl = length files' `div` jobs
            batches = chunksOf (if bl == 0 then 1 else bl) files'
        mapM_ action batches

    -- process dirs recursively

    forM_ dirs $ \dirPath -> do
        let path = fullPath dirPath
        unless (isPruneableDir path pdirs) $ do -- this is a good directory (unless already visited)!
                canonicalizePath path >>= \cpath -> unless (cpath `Set.member` visited) $
                    withRecursiveContents opt path langs pdirs (Set.insert cpath visited) action

{-# NOINLINE toLazyByteStringShort #-}
toLazyByteStringShort =
  B.toLazyByteStringWith (B.untrimmedStrategy 128 B.defaultChunkSize) LB.empty

(.!.) :: V.Vector a -> Int -> a
v .!. i = v ! (i `mod` V.length v)
{-# INLINE  (.!.) #-}

parallelSearch :: [FilePath] -> [C.ByteString] -> [Language] -> Bool -> OptionIO ()
parallelSearch paths patterns langs isTermIn = do

    Env{..} <- ask

    let Config{..} = conf
        Options{..} = opt

    -- create channels ...

    fileCh <- liftIO $ forM [1 .. jobs] $ const (newChan 131072)
    respCh <- liftIO $ forM [1 .. jobs] $ const (newChan 131072)

    -- recursively traverse the filesystem ...

    _ <- liftIO . forkIO $ do
        cnt <- newIORef (0 :: Int)
        if recursive || follow
            then forM_ (if null paths then ["."] else paths) $ \p -> do
                    isDir <- doesDirectoryExist p
                    if isDir
                        then withRecursiveContents opt p langs
                                (mkPrunableDirName <$> configPruneDirs <> prune_dir) (Set.singleton p)
                                (\a -> readIORef cnt >>= \idx -> writeChan (fst (fileCh .!. idx)) a >> modifyIORef' cnt (+1))
                        else readIORef cnt >>= \idx -> writeChan (fst (fileCh .!. idx)) [p] >> modifyIORef' cnt (+1)

            else forM_ (if null paths && not isTermIn
                        then [("", 0)]
                        else paths `zip` [0..]) (\(p, idx) -> writeChan (fst (fileCh .!. idx)) [p] )

        -- enqueue EOF messages...
        forM_ ([0..jobs-1] :: [Int]) $ \idx -> writeChan (fst (fileCh .!. idx)) []
        putStrLn "Done."

    -- launch the worker threads...

    matchingFiles <- liftIO $ newIORef Set.empty

    forM_ ([0 .. jobs-1] :: [Int]) $ \idx -> liftIO . forkIO $ do
        void $ runExceptT . forever $ do
            fs <- liftIO $ readChan (snd (fileCh ! idx))
            liftIO $ E.catch (
                case fs of
                    [] -> writeChan (fst (respCh ! idx)) []
                    xs -> forM_ xs $ \x -> do
                            out <- fmap (take max_count)
                                (runReaderT (do
                                    out' <- run x patterns
                                    when (vim || editor) $
                                        liftIO $ mapM_ (modifyIORef matchingFiles . Set.insert . (outFilePath &&& outLineNumb)) out'
                                    putOutputElements out'
                                ) (Env conf opt Nothing Nothing))

                            unless (null out) $
                                writeChan (fst (respCh ! idx)) (toLazyByteStringShort . (<> B.char8 '\n') <$> out)
                )  (\e -> let msg = show (e :: SomeException) in
                        hPutStrLn stderr (showFileName conf opt (getTargetName (head fs)) <> ": error: " <> takeN 80 msg))
            when (null fs) $ throwE ()

    -- dump output until workers are done

    liftIO $ do
        hSetBuffering stdout (BlockBuffering $ Just 8192)
        hSetBinaryMode stdout True

        fix (\action !n !i !indexes -> unless (n == jobs) $ do
            let idx :: Int = indexes .!. i
            readChan (snd (respCh ! idx)) >>= \case
                [] -> action (n+1) i (V.filter (/= idx) indexes)
                out -> mapM_ (LB.hPut stdout) out *> action n (i+1) indexes
            ) 0 0 ([0.. jobs-1] :: V.Vector Int)

    -- run editor...

    when (vim || editor ) $ liftIO $ do

        editor' <- if vim
                    then return (Just "vim")
                    else lookupEnv "EDITOR"

        files <- Set.toList <$> readIORef matchingFiles
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


fileFilter :: Options -> [Language] -> FilePath -> Bool
fileFilter opt langs filename = maybe False (liftA2 (||) (const $ null langs) (`elem` langs)) (languageLookup opt filename)
{-# INLINE fileFilter #-}


isNotTestFile :: FilePath -> Bool
isNotTestFile  f =
    let fs = [("_test" `isSuffixOf`), ("-test" `isSuffixOf`), ("test-" `isPrefixOf`), ("test_" `isPrefixOf`), ("test" == )] :: [String -> Bool]
        in not $ any ($ takeBaseName f) fs
{-# INLINE isNotTestFile #-}


isPruneableDir:: FilePath -> [FilePath] -> Bool
isPruneableDir dir = any (`isSuffixOf` pdir)
    where pdir = mkPrunableDirName dir
{-# INLINE isPruneableDir #-}


mkPrunableDirName :: FilePath -> FilePath
mkPrunableDirName xs | "/" `isSuffixOf` xs = xs
                     | otherwise           = xs <> "/"
{-# INLINE mkPrunableDirName #-}
