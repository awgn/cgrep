--
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
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List ( isSuffixOf, (\\), isInfixOf, nub, sort, union, isPrefixOf, genericLength )
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Data.Maybe ( catMaybes, fromJust )
import Data.Char ( toLower )
import Data.Data()

import Data.IORef ( modifyIORef, newIORef, readIORef )
import Data.Version(showVersion)
import Data.Function ( fix )
import qualified Data.Set as Set
import Paths_cgrep ( version )

import Control.Exception as E ( catch, SomeException )
import Control.Concurrent ( forkIO )
import Control.Concurrent.Async ( forConcurrently_, forConcurrently )
import Control.Monad.STM ( atomically )
import Control.Concurrent.STM.TQueue
    ( newTQueueIO, readTQueue, writeTQueue )

import Control.Monad ( when, forM_, forever, replicateM_, unless, void, forM )
import Control.Monad.Trans ( MonadIO(liftIO) )
import Control.Monad.Trans.Except ( runExceptT, throwE )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import Control.Applicative
    ( Applicative(liftA2), Alternative((<|>)) )

import System.Console.CmdArgs ( cmdArgsRun )
import System.Directory
    ( canonicalizePath, doesDirectoryExist, listDirectory )
import System.Environment ( lookupEnv, withArgs )
import System.PosixCompat.Files as PosixCompat
    ( getSymbolicLinkStatus, isSymbolicLink )
import System.IO
    ( stdout, stdin, hIsTerminalDevice, stderr, hPutStrLn, hSetBuffering, hSetBinaryMode, BufferMode (BlockBuffering) )
import System.Exit ( exitSuccess )
import System.Process (readProcess, runProcess, waitForProcess)

import CGrep.Search ( isRegexp, runSearch )
import CGrep.Language ( Language, splitLanguagesList)
import CGrep.LanguagesMap ( languagesMap, languageLookup, dumpLanguagesMap)

import CGrep.Output
    ( Output(..),
      putOutput,
      showFileName )
import CGrep.Common ( takeN, trim8, getTargetName )
import CGrep.Parser.Atom ( wildCardMap )

import CmdOptions ( options )
import Options ( Options(..) )
import Util ( partitionM, notNull )
import Verbose ( putStrLn1 )
import Config
    ( Config(Config, configFileLine, configColorMatch, configColorFile,
             configPruneDirs, configColors, configLanguages),
      getConfig, dumpPalette )
import Reader ( OptionIO, Env (..) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Data.Tuple.Extra ( (&&&) )
import System.FilePath.Posix ( (</>), takeBaseName )
import GHC.Conc ( getNumCapabilities )
import qualified Data.Bifunctor

-- push file names in Chan...

withRecursiveContents :: Options -> FilePath -> [Language] -> [String] -> Set.Set FilePath -> ([FilePath] -> IO ()) -> IO ()
withRecursiveContents opt@Options{..} dir langs pdirs visited action = do
    xs <- listDirectory dir

    (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs]

    magics <- if null magic_filter || null files
               then return []
               else getFilesMagic files

    -- filter the list of files

    let files' = if null magics
                   then  filter (fileFilter opt langs) files
                   else  catMaybes $ zipWith (\f m ->  if any (`isInfixOf` m) magic_filter then Just f else Nothing) files magics

        files'' = filter (\f -> not skip_test || isNotTestFile f) files'

    -- run action

    unless (null files'') $ do
        let bl = length files'' `div` jobs
            batches = chunksOf (if bl == 0 then 1 else bl) files''
        mapM_ action batches

    -- process dirs recursively

    forConcurrently_ dirs $ \path -> do
         lstatus <- getSymbolicLinkStatus path
         when (deference_recursive || not (PosixCompat.isSymbolicLink lstatus)) $
             unless (isPruneableDir path pdirs) $ do -- this is a good directory (unless already visited)!
                 canonicalizePath path >>= \cpath -> unless (cpath `Set.member` visited) $
                    withRecursiveContents opt path langs pdirs (Set.insert cpath visited) action

-- read patterns from file

readPatternsFromFile :: FilePath -> IO [C.ByteString]
readPatternsFromFile f =
    if null f then return []
              else map trim8 . C.lines <$> C.readFile f

getFilePaths :: Bool        ->     -- pattern(s) from file
                [String]    ->     -- list of patterns and files
                [String]
getFilePaths False xs = if length xs == 1 then [] else tail xs
getFilePaths True  xs = xs


parallelSearch :: [FilePath] -> [C.ByteString] -> [Language] -> Bool -> OptionIO ()
parallelSearch paths patterns langs isTermIn = do

    Env{..} <- ask

    let Config{..} = conf
        Options{..} = opt

    -- create Transactional Chan and Vars...

    in_chan  <- liftIO newTQueueIO
    out_chan <- liftIO newTQueueIO

    -- push the files to for...

    _ <- liftIO . forkIO $ do
        if recursive || deference_recursive
            then forM_ (if null paths then ["."] else paths) $ \p -> do
                    isDir <- doesDirectoryExist p
                    if isDir
                        then withRecursiveContents opt p langs
                                (mkPrunableDirName <$> configPruneDirs <> prune_dir) (Set.singleton p) (atomically . writeTQueue in_chan)
                        else atomically . writeTQueue in_chan $ [p]

            else forM_ (if null paths && not isTermIn
                        then [""]
                        else paths) (\p -> atomically . writeTQueue in_chan $ [p] )

        -- enqueue EOF messages:
        replicateM_ jobs ((atomically . writeTQueue in_chan) [])


    -- launch worker threads...

    matchingFiles <- liftIO $ newIORef Set.empty

    forM_ [1 .. jobs] $ \_ -> liftIO . forkIO $ do
        void $ runExceptT . forever $ do
            fs <- liftIO . atomically $ readTQueue in_chan
            liftIO $ E.catch (
                case fs of
                    [] -> atomically $ writeTQueue out_chan []
                    xs -> (if asynch then forConcurrently_
                                     else forM_) xs $ \x -> do

                            out <- fmap (take max_count)
                                (runReaderT (do
                                    out' <- runSearch x patterns
                                    when (vim || editor) $
                                        liftIO $ mapM_ (modifyIORef matchingFiles . Set.insert . (outFilePath &&& outLineNumb)) out'
                                    putOutput out'
                                ) (Env conf opt Nothing Nothing))

                            unless (null out) $
                                atomically $ writeTQueue out_chan ((<> B.char8 '\n') <$> out)

                )  (\e -> let msg = show (e :: SomeException) in
                        hPutStrLn stderr (showFileName conf opt (getTargetName (head fs)) <> ": error: " <> takeN 80 msg))

            when (null fs) $ throwE ()

    -- dump output until workers are done

    let stop = jobs

    -- setup stdout...

    liftIO $ do
        hSetBuffering stdout (BlockBuffering $ Just 8192)
        hSetBinaryMode stdout True
        fix (\action (!n) m ->
         unless (n == stop) $ atomically (readTQueue out_chan) >>= \case
                      [] -> action (n+1) m
                      out -> mapM_ (B.hPutBuilder stdout) out *> action n True
            )  0 False

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


main :: IO ()
main = do
    -- check whether this is a terminal device

    isTermIn  <- hIsTerminalDevice stdin
    isTermOut <- hIsTerminalDevice stdout

    -- read Cgrep config options

    (conf, _)  <- getConfig

    -- read command-line options

    opt  <- (if isTermOut
                then \o -> o { color = color o || configColors conf }
                else id) <$> cmdArgsRun options

    -- check for multiple backends...

    when (length (catMaybes [
                if json opt then Just "" else Nothing
               ]) > 1)
        $ error "you can use one back-end at time!"


    -- display lang-map and exit...

    when (language_map opt) $
        dumpLanguagesMap languagesMap >> exitSuccess

    when (show_palette opt) $
        dumpPalette >> exitSuccess

    -- check whether the pattern list is empty, display help message if it's the case

    when (null (others opt) && isTermIn && null (file opt)) $
        withArgs ["--help"] $ void (cmdArgsRun options)

    -- load patterns:

    patterns <- if null (file opt) then return $ map (C.pack . UC.encodeString) (((:[]).head.others) opt)
                                    else readPatternsFromFile $ file opt

    let patterns' = map (if ignore_case opt then ic else id) patterns
            where ic | (not . isRegexp) opt && semantic opt = C.unwords . map (\p -> if p `elem` wildCardTokens then p else C.map toLower p) . C.words
                     | otherwise = C.map toLower
                        where wildCardTokens = "OR" : M.keys wildCardMap   -- "OR" is not included in wildCardMap

    -- display the configuration in use

    -- when (isJust confpath) $
    --    hPutStrLn stderr $ showBold opt ("Using '" <> fromJust confpath <> "' configuration file...")

    -- load files to parse:

    let paths = getFilePaths (notNull (file opt)) (others opt)

    -- parse cmd line language list:

    let (l0, l1, l2) = splitLanguagesList (language_filter opt)

    -- language enabled:

    let langs = (if null l0 then configLanguages conf else l0 `union` l1) \\ l2

    runReaderT (do putStrLn1 $ "Cgrep " <> showVersion version <> "!"
                   putStrLn1 $ "options   : " <> show opt
                   putStrLn1 $ "config    : " <> show conf
                   putStrLn1 $ "languages : " <> show langs
                   putStrLn1 $ "pattern   : " <> show patterns'
                   putStrLn1 $ "files     : " <> show paths
                   putStrLn1 $ "isTermIn  : " <> show isTermIn
                   putStrLn1 $ "isTermOut : " <> show isTermOut
        ) (Env conf opt Nothing Nothing)

    -- specify number of cores

    njobs <- if jobs opt /= 0
                then return (jobs opt)
                else getNumCapabilities

    -- run search

    runReaderT (parallelSearch paths patterns' langs isTermIn) (Env conf opt { jobs = njobs} Nothing Nothing)


fileFilter :: Options -> [Language] -> FilePath -> Bool
fileFilter opt langs filename = maybe False (liftA2 (||) (const $ null langs) (`elem` langs)) (languageLookup opt filename)
{-# INLINE fileFilter #-}

getFilesMagic :: [FilePath] -> IO [String]
getFilesMagic filenames = lines <$> readProcess "/usr/bin/file" ("-b" : filenames) []
{-# INLINE getFilesMagic #-}

isNotTestFile :: FilePath -> Bool
isNotTestFile  f =
    let fs = [("_test" `isSuffixOf`), ("-test" `isSuffixOf`), ("test-" `isPrefixOf`), ("test_" `isPrefixOf`), ("test" == )]
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
