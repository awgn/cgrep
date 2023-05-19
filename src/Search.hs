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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Search (
      parallelSearch
    , isRegexp
) where

import Data.List ( isPrefixOf, isSuffixOf, partition, elemIndex, intersperse )
import Data.List.Split ( chunksOf )
import qualified Data.Map as M
import Data.Maybe ( fromJust, isJust, catMaybes )
import Data.Function ( fix )
import qualified Data.Set as Set

import Control.Exception as E ( catch, SomeException )
import Control.Concurrent ( forkIO )

import Control.Monad ( when, forM_, forever, unless, void, forM )
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
    ( Config(Config, configLanguages, configFileLine, configColorMatch,
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
import GHC.Conc ( forkIO )
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
import Control.Concurrent.Async (forConcurrently, forConcurrently_, mapConcurrently_)

import qualified CGrep.Strategy.BoyerMoore       as BoyerMoore
import qualified CGrep.Strategy.Levenshtein      as Levenshtein
import qualified CGrep.Strategy.Regex            as Regex
import qualified CGrep.Strategy.Tokenizer        as Tokenizer
import qualified CGrep.Strategy.Semantic         as Semantic
import Control.Monad.Catch ( SomeException, MonadCatch(catch) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.Language ( Language )
import CGrep.LanguagesMap
    ( languageInfoLookup, languageLookup, LanguageInfo )

import Control.Monad.Loops ( whileM_ )
import Control.DeepSeq as DS ( force )

withRecursiveContents :: Options
                      -> RawFilePath
                      -> [Language]
                      -> [RawFilePath]
                      -> Set.Set RawFilePath
                      -> IORef Int
                      -> ([RawFilePath] -> IO ()) -> IO ()
withRecursiveContents opt@Options{..} dir langs pdirs visited cnt action = do
    xs <- getDirectoryContents dir

    let (dirs, files) = partition ((== dtDir) . fst) xs

    -- filter the list of files

    let files' = (dir </>) . snd <$> filter (\f -> fileFilter opt langs (snd f) && (not skip_test || isNotTestFile (snd f))) files
    let dirs'  = (dir </>) . snd <$> dirs

    -- run action

    blen <- batchLen cnt 64

    unless (null files') $ do
        mapM_ action (chunksOf blen files')

    -- process dirs recursively

    forM_ dirs' $ \dirPath -> do
        unless (isPrunableDir dirPath pdirs) $ -- this is a good directory (unless already visited)!
                 -- this is a good directory (unless already visited)!
                makeRawAbsolute dirPath >>= \cpath ->
                    unless (cpath `Set.member` visited) $
                        withRecursiveContents opt dirPath langs pdirs (Set.insert cpath visited) cnt action


parallelSearch :: [RawFilePath] -> [C.ByteString] -> [Language] -> Bool -> ReaderIO ()
parallelSearch paths patterns langs isTermIn = do

    Env{..} <- ask

    let Config{..} = conf
        Options{..} = opt

    -- create channels ...

    fileCh <- liftIO $ newChan 8192
    outCh  <- liftIO $ newChan 8192

    -- recursively traverse the filesystem ...

    _ <- liftIO . forkIO $ do

        cnt <- newIORef (0 :: Int)

        if recursive || follow
            then forM_ (if null paths then ["."] else paths) $ \p ->
                    doesDirectoryExist p >>= \case
                        True -> withRecursiveContents opt p langs
                                (mkPrunableDirName <$> configPruneDirs <> (C.pack <$> prune_dir)) (Set.singleton p) cnt $ do
                                    (\a -> writeChan (fst fileCh) a *> atomicModifyIORef' cnt (\x -> (x+1, ())))
                        _     ->  writeChan (fst fileCh) [p] *> atomicModifyIORef' cnt (\x -> (x+1, ()))
            else forM_ (if null paths && not isTermIn
                        then [("", 0)]
                        else paths `zip` [0..]) (\(p, idx) -> writeChan (fst fileCh) [p] )

        -- enqueue EOF messages...
        forM_ ([0..jobs-1] :: [Int]) $ \idx -> writeChan (fst fileCh) []
        hPutStrLn stderr "Search done!"

    -- launch the worker threads...

    matchingFiles <- liftIO $ newIORef Set.empty

    let env = Env conf opt
        runSearch = getSearcher env

    forM_ ([0 .. jobs-1] :: [Int]) $ \idx -> liftIO . forkIO $ do
        void $ runExceptT . forever $ do
            fs <- liftIO $ readChan (snd fileCh)
            liftIO $ E.catch (
                case fs of
                    [] -> liftIO $ writeChan (fst outCh) []
                    fs -> runReaderT (do
                        out <- catMaybes <$> forM fs (\f -> do
                                out' <- take max_count <$> runSearch (languageInfoLookup opt f) f patterns
                                when (vim || editor) $
                                    liftIO $ mapM_ (modifyIORef matchingFiles . Set.insert . (outFilePath &&& outLineNumb)) out'
                                putOutputElements out')
                        liftIO $ unless (null out) $
                            writeChan (fst outCh) out
                        ) env
                ) (\e -> let msg = show (e :: SomeException) in C.hPutStrLn stderr (showFileName conf opt (getTargetName (head fs)) <> ": error: " <> C.pack (takeN 80 msg)))

            when (null fs) $ do
                liftIO $ B.hPutStr stderr "worker done!\n"
                throwE ()

    -- dump output until workers are done

    liftIO $ do
        hSetBuffering stdout (BlockBuffering $ Just 8192)
        hSetBinaryMode stdout True

        totalDone <- newIORef (0 :: Int)

        whileM_ (readIORef totalDone >>= \n -> pure (n < jobs)) $ do
            readChan (snd outCh) >>= \case
                [] -> modifyIORef' totalDone (+1)
                out -> mapM_ (B.hPutBuilder stdout) out

        -- fix (\action !n -> unless (n == jobs) $ do
        --     readChan (snd outCh) >>= \case
        --         [] -> action (n+1)
        --         out -> mapM_ (\b -> B.hPut stdout b *> putChar '\n') out *> action n) 0

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


{-# NOINLINE toLazyByteString #-}
toLazyByteString :: B.Builder -> LB.ByteString
toLazyByteString =
  B.toLazyByteStringWith (B.untrimmedStrategy 128 B.smallChunkSize) LB.empty


getSearcher :: Env -> (Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output])
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


batchLen :: IORef Int -> Int -> IO Int
batchLen cnt maxblen =
    readIORef cnt >>=
        \round -> modifyIORef' cnt (+1) $> min maxblen (1 + round)
{-# INLINE  batchLen #-}


fileFilter :: Options -> [Language] -> RawFilePath -> Bool
fileFilter opt langs filename = maybe False (liftA2 (||) (const $ null langs) (`elem` langs)) (languageLookup opt filename)
{-# INLINE fileFilter #-}


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

hasLanguage :: RawFilePath -> Options -> [Language] -> Bool
hasLanguage path opt xs = isJust $ languageLookup opt path >>= (`elemIndex` xs)
{-# INLINE hasLanguage #-}

hasTokenizerOpt :: Options -> Bool
hasTokenizerOpt Options{..} =
  identifier ||
  keyword    ||
  number     ||
  string     ||
  operator

isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt || regex_pcre opt
{-# INLINE isRegexp #-}
