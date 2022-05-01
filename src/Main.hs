--
-- Copyright (c) 2013-2019 Nicola Bonelli <nicola@pfq.io>
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

module Main where

import Data.List ( isSuffixOf, (\\), isInfixOf, nub, sort, union, isPrefixOf )
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
import Control.Concurrent ( forkIO, setNumCapabilities )
import Control.Concurrent.Async ( mapConcurrently )
import Control.Monad.STM ( atomically )
import Control.Concurrent.STM.TChan
    ( newTChanIO, readTChan, writeTChan )

import Control.Monad
    ( when, forM_, forever, replicateM_, unless, void, forM )
import Control.Monad.Trans ( MonadIO(liftIO), MonadTrans(lift) )
import Control.Monad.Trans.Except ( runExceptT, throwE )
import Control.Monad.Trans.Reader ( ReaderT(runReaderT), ask )
import Control.Applicative
    ( Applicative(liftA2), Alternative((<|>)) )

import System.Console.CmdArgs ( cmdArgsRun )
import System.Directory
    ( canonicalizePath, doesDirectoryExist, getDirectoryContents )
import System.Environment ( lookupEnv, withArgs )
import System.PosixCompat.Files as PosixCompat
    ( getSymbolicLinkStatus, isSymbolicLink )
import System.IO
    ( stdout, stdin, hIsTerminalDevice, stderr, hPutStrLn )
import System.Exit ( exitSuccess )
import System.Process (readProcess, runProcess, waitForProcess)

import CGrep.CGrep ( sanitizeOptions, isRegexp, runCgrep )
import CGrep.Lang
    ( Lang, langMap, getFileLang, dumpLangMap, splitLangList )
import CGrep.Output
    ( Output(outLine, outTokens, outFilePath, outLineNo),
      putPrettyHeader,
      putPrettyFooter,
      prettyOutput,
      showFileName )
import CGrep.Common ( takeN, trim8, getTargetName )
import CGrep.Parser.WildCard ( wildCardMap )

import CmdOptions ( options )
import Options ( Options(..) )
import Util ( partitionM, notNull )
import Verbose ( putStrLn1 )
import Config
    ( Config(Config, configFileLine, configColorMatch, configColorFile,
             configPruneDirs, configColors, configLanguages),
      getConfig )
import Reader ( OptionT )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Data.Tuple.Extra ( (&&&) )
import System.FilePath.Posix

fileFilter :: Options -> [Lang] -> FilePath -> Bool
fileFilter opts langs filename = maybe False (liftA2 (||) (const $ null langs) (`elem` langs)) (getFileLang opts filename)
{-# INLINE fileFilter #-}

getFilesMagic :: [FilePath] -> IO [String]
getFilesMagic filenames = lines <$> readProcess "/usr/bin/file" ("-b" : filenames) []
{-# INLINE getFilesMagic #-}

isNotTestFile :: FilePath -> Bool
isNotTestFile  f =
    let fs = [("_test" `isSuffixOf`), ("-test" `isSuffixOf`), ("test-" `isPrefixOf`), ("test_" `isPrefixOf`), ("test" == )]
        in not $ any ($ takeBaseName f) fs
{-# INLINE isNotTestFile #-}

-- push file names in Chan...

withRecursiveContents :: Options -> FilePath -> [Lang] -> [String] -> Set.Set FilePath -> ([FilePath] -> IO ()) -> IO ()
withRecursiveContents opts@Options{..} dir langs pdirs visited action = do
    isDir <-  doesDirectoryExist dir
    if isDir then do
               xs <- getDirectoryContents dir

               (dirs,files) <- partitionM doesDirectoryExist [dir </> x | x <- xs, x `notElem` [".", ".."]]

               magics <- if null magic_filter || null files
                          then return []
                          else getFilesMagic files

               -- filter the list of files
               --
               let files' = if null magics
                              then  filter (fileFilter opts langs) files
                              else  catMaybes $ zipWith (\f m ->  if any (`isInfixOf` m) magic_filter then Just f else Nothing ) files magics

                   files'' = filter (\f -> not skip_test || isNotTestFile f) files'

               unless (null files'') $
                    let chunks = chunksOf chunk files'' in
                    forM_ chunks $ \b -> action b

               -- process dirs
               --
               forM_ dirs $ \path -> do
                    lstatus <- getSymbolicLinkStatus path
                    when ( deference_recursive || not (PosixCompat.isSymbolicLink lstatus)) $
                        unless (isPruneableDir path pdirs) $ do -- this is a good directory (unless already visited)!
                            cpath <- canonicalizePath path
                            unless (cpath `Set.member` visited) $
                                withRecursiveContents opts path langs pdirs (Set.insert cpath visited) action
             else action [dir]


isPruneableDir:: FilePath -> [FilePath] -> Bool
isPruneableDir dir = any (`isSuffixOf` pdir)
    where pdir = mkPrunableDirName dir
{-# INLINE isPruneableDir #-}

mkPrunableDirName :: FilePath -> FilePath
mkPrunableDirName xs | "/" `isSuffixOf` xs = xs
                     | otherwise           = xs ++ "/"
{-# INLINE mkPrunableDirName #-}

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


parallelSearch :: [FilePath] -> [C.ByteString] -> [Lang] -> (Bool, Bool) -> OptionT IO ()
parallelSearch paths patterns langs (isTermIn, _) = do

    (conf@Config{..}, opts@Options{..}) <- ask

    -- create Transactional Chan and Vars...

    in_chan  <- liftIO newTChanIO
    out_chan <- liftIO newTChanIO


    -- launch worker threads...

    forM_ [1 .. jobs] $ \_ -> liftIO . forkIO $
        void $ runExceptT . forever $ do
            fs <- lift $ atomically $ readTChan in_chan
            lift $
                E.catch (
                    case fs of
                        [] -> atomically $ writeTChan out_chan []
                        xs -> void $ (if asynch then flip mapConcurrently
                                                else forM) xs $ \x -> do
                                out <- fmap (take max_count ) (runReaderT (runCgrep conf opts x patterns) (conf, sanitizeOptions x opts))
                                unless (null out) $ atomically $ writeTChan out_chan out)
                       (\e -> let msg = show (e :: SomeException) in
                            hPutStrLn stderr (showFileName conf opts (getTargetName (head fs))
                                ++ ": exception: " ++ takeN 80 msg))
            when (null fs) $ throwE ()


    -- push the files to grep for...

    _ <- liftIO . forkIO $ do

        if recursive || deference_recursive
            then forM_ (if null paths then ["."] else paths) $ \p ->
                    withRecursiveContents opts p langs
                        (mkPrunableDirName <$> configPruneDirs ++ prune_dir) (Set.singleton p) (atomically . writeTChan in_chan)

            else forM_ (if null paths && not isTermIn then [""] else paths) (atomically . writeTChan in_chan . (:[]))

        -- enqueue EOF messages:

        replicateM_ jobs ((atomically . writeTChan in_chan) [])

    -- dump output until workers are done

    putPrettyHeader

    let stop = jobs

    matchingFiles <- liftIO $ newIORef Set.empty

    fix (\action n m ->
         unless (n == stop) $ do
                 out <- liftIO $ atomically $ readTChan out_chan
                 case out of
                      [] -> action (n+1) m
                      _  -> do
                          case () of
                            _ | json -> when m $ liftIO $ putStrLn ","
                              | otherwise -> return ()
                          let out' = map (\p -> p {outTokens = map (\(off, s) -> (length $ UC.decode $ B.unpack $ C.take off $ outLine p, UC.decodeString s)) $ outTokens p}) out
                          prettyOutput out' >>= mapM_ (liftIO . putStrLn)
                          liftIO $ when (vim || editor) $
                                    mapM_ (modifyIORef matchingFiles . Set.insert . (outFilePath &&& outLineNo)) out
                          action n True
        )  0 False


    putPrettyFooter

    -- run editor...

    when (vim || editor ) $ liftIO $ do

        editor' <- if vim
                    then return (Just "vim")
                    else lookupEnv "EDITOR"

        files   <- Set.toList <$> readIORef matchingFiles

        let editFiles = (if fileline || configFileLine
                            then fmap (\(a,b) -> a ++ ":" ++ show b)
                            else nub . sort . fmap fst) files

        putStrLn $ "cgrep: open files " ++ unwords editFiles ++ "..."
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

    opts  <- (if isTermOut
                then \o -> o { color = color o || configColors conf }
                else id) <$> cmdArgsRun options

    -- check for multiple backends...

    when (length (catMaybes [
#ifdef ENABLE_HINT
                hint opts,
#endif
                format opts,
                if xml opts  then Just "" else Nothing,
                if json opts then Just "" else Nothing
               ]) > 1)
        $ error "you can use one back-end at time!"


    -- display lang-map and exit...

    when (language_map opts) $
        dumpLangMap langMap >> exitSuccess

    -- check whether the pattern list is empty, display help message if it's the case

    when (null (others opts) && isTermIn && null (file opts)) $
        withArgs ["--help"] $ void (cmdArgsRun options)

    -- load patterns:

    patterns <- if null (file opts) then return $ map (C.pack . UC.encodeString) (((:[]).head.others) opts)
                                    else readPatternsFromFile $ file opts

    let patterns' = map (if ignore_case opts then ic else id) patterns
            where ic | (not . isRegexp) opts && semantic opts = C.unwords . map (\p -> if C.unpack p `elem` wildCardTokens then p else C.map toLower p) . C.words
                     | otherwise = C.map toLower
                        where wildCardTokens = "OR" : M.keys wildCardMap   -- "OR" is not included in wildCardMap

    -- display the configuration in use

    -- when (isJust confpath) $
    --    hPutStrLn stderr $ showBold opts ("Using '" ++ fromJust confpath ++ "' configuration file...")

    -- load files to parse:

    let paths = getFilePaths (notNull (file opts)) (others opts)

    -- parse cmd line language list:

    let (l0, l1, l2) = splitLangList (language_filter opts)

    -- language enabled:

    let langs = (if null l0 then configLanguages conf else l0 `union` l1) \\ l2

    runReaderT (do putStrLn1 $ "Cgrep " ++ showVersion version ++ "!"
                   putStrLn1 $ "options   : " ++ show opts
                   putStrLn1 $ "config    : " ++ show conf
                   putStrLn1 $ "languages : " ++ show langs
                   putStrLn1 $ "pattern   : " ++ show patterns'
                   putStrLn1 $ "files     : " ++ show paths
                   putStrLn1 $ "isTermIn  : " ++ show isTermIn
                   putStrLn1 $ "isTermOut : " ++ show isTermOut
        ) (conf, opts)

    -- specify number of cores

    when (cores opts /= 0) $ setNumCapabilities (cores opts)

    -- run search

    runReaderT (parallelSearch paths patterns' langs (isTermIn, isTermOut)) (conf, opts)
