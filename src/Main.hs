--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

module Main where

import Data.List
import qualified Data.Set as Set
import Data.Maybe
import Data.Char
import Data.Data()
import Data.Function

import Control.Exception as E
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Control.Applicative

import System.Console.CmdArgs
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.Environment
import System.Posix.Files
import System.IO
import System.Exit

import CGrep.CGrep
import CGrep.Lang
import CGrep.Output

import CmdOptions
import Options
import Util
import Debug
import Config

import qualified Data.ByteString.Char8 as C

-- push file names in TChan...

putRecursiveContents :: Options -> TChan (Maybe FilePath) -> FilePath -> [Lang] -> [String] -> Set.Set FilePath -> IO ()
putRecursiveContents opts inchan topdir langs prunedir visited = do
    dir <-  doesDirectoryExist topdir
    if dir
        then do
            names <- liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents topdir
            forM_ names $ \n -> E.catch ( do
                    let path = topdir </> n
                    let filename = takeFileName path
                    cpath  <- canonicalizePath path
                    status  <- getFileStatus path
                    lstatus <- getSymbolicLinkStatus path
                    unless (cpath `Set.member` visited) $
                        if isDirectory status && (not (isSymbolicLink lstatus) || deference_recursive opts)
                           then unless (filename `elem` prunedir)  $
                               putRecursiveContents opts inchan path langs prunedir (Set.insert cpath visited)
                           else case getLang opts filename >>= (\f -> f `elemIndex` langs <|> toMaybe 0 (null langs) ) of
                                   Nothing -> return ()
                                   _       -> atomically $ writeTChan inchan (Just path)
                ) (\e -> let msg = show (e :: SomeException) in hPutStrLn stderr ("cgrep: " ++ msg))
        else atomically $ writeTChan inchan (Just topdir)


-- read patterns from file

readPatternsFromFile :: FilePath -> IO [C.ByteString]
readPatternsFromFile f =
    if null f then return []
              else liftM C.words $ C.readFile f

main :: IO ()
main = do

    -- check whether this is a terminal device

    isTermIn  <- hIsTerminalDevice stdin
    isTermOut <- hIsTerminalDevice stdout

    -- read Cgrep config options

    conf  <- getConfig

    -- read command-line options

    opts  <- (if isTermOut then (\o@Options{color = c} -> o { color = c || configAutoColor conf})
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

    -- display lang-map...

    when (lang_maps opts) $ dumpLangMap langMap >> exitSuccess

    -- check whether patterns list is empty, display help message if it's the case

    when (null $ others opts) $ withArgs ["--help"] $ void (cmdArgsRun options)

    -- load patterns:

    patterns <- if null $ file opts then return [C.pack $ head $ others opts]
                                    else readPatternsFromFile $ file opts

    let patternsLc = map (if ignore_case opts then C.map toLower else id) patterns

    -- retrieve files to parse

    let tailOpts = tail $ others opts

    let paths = if null $ file opts then if null tailOpts && isTermIn then ["."] else tailOpts
                                    else others opts

    -- parse cmd line language list:

    let (l0, l1, l2) = splitLangList (lang opts)

    -- language enabled:

    let lang_enabled = (if null l0 then configLanguages conf else l0 `union` l1) \\ l2

    putStrLevel1 (debug opts) $ "Cgrep " ++ version ++ "!"
    putStrLevel1 (debug opts) $ "options   : " ++ show opts
    putStrLevel1 (debug opts) $ "languages : " ++ show lang_enabled
    putStrLevel1 (debug opts) $ "pattern   : " ++ show patterns
    putStrLevel1 (debug opts) $ "files     : " ++ show paths
    putStrLevel1 (debug opts) $ "isTermIn  : " ++ show isTermIn
    putStrLevel1 (debug opts) $ "isTermOut : " ++ show isTermOut

    -- create Transactional Chan and Vars...

    in_chan  <- newTChanIO
    out_chan <- newTChanIO

    -- launch worker threads...

    forM_ [1 .. jobs opts] $ \_ -> forkIO $ do
        _ <- runEitherT $ forever $ do
            f <- lift $ atomically $ readTChan in_chan
            lift $ E.catch (case f of
                    Nothing -> atomically $ writeTChan out_chan []
                    Just x  -> do
                        out <- let op = sanitizeOptions x opts in
                                            liftM (take (max_count opts)) $ cgrepDispatch op x op patternsLc $ guard (x /= "") >> f
                        unless (null out) $ atomically $ writeTChan out_chan out
                   )
                   (\e -> let msg = show (e :: SomeException) in hPutStrLn stderr (showFile opts (fromMaybe "<STDIN>" f) ++ ": exception: " ++ if length msg > 80 then take 80 msg ++ "..." else msg))
            when (isNothing f) $ left ()
        return ()


    -- push the files to grep for...

    _ <- forkIO $ do

        if recursive opts || deference_recursive opts
            then forM_ paths $ \p -> putRecursiveContents opts in_chan p lang_enabled (configPruneDirs conf) (Set.singleton p)
            else do
                files <- liftM (\l -> if null l && not isTermIn then [""] else l) $ filterM doesFileExist paths
                forM_ files (atomically . writeTChan in_chan . Just)

        -- enqueue EOF messages:

        replicateM_ (jobs opts) ((atomically . writeTChan in_chan) Nothing)

    -- dump output until workers are done

    putPrettyHeader opts

    let stop = jobs opts

    fix (\action n m ->
         unless (n == stop) $ do
                 out <- atomically $ readTChan out_chan
                 case out of
                      [] -> action (n+1) m
                      _  -> do
                          case () of
                            _ | json opts -> when m $ putStrLn ","
                              | otherwise -> return ()
                          prettyOutput opts out >>= mapM_ putStrLn
                          action n True
        )  0 False

    putPrettyFooter opts

