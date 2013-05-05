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
import Data.Char
import Data.Data()
import Data.Function

import Control.Exception as E
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Control.Monad 
import Control.Applicative

import System.Console.CmdArgs
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.Environment
import System.IO
import System.Exit

import CGrep.Output
import CGrep.CGrep
import CGrep.Lang

import CmdOptions
import Options
import Util

import qualified Data.ByteString.Char8 as C

-- from Realworld in Haskell...

getRecursiveContents :: FilePath -> [Lang] -> [String] -> IO [FilePath]

getRecursiveContents topdir langs prunedir = 
  liftM concat $ do
    dir <-  doesDirectoryExist topdir 
    if dir then do
            names <- liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents topdir
            forM names $ \n -> do
                let path = topdir </> n
                let filename = takeFileName path
                isDirectory <- doesDirectoryExist path
                if isDirectory
                    then if filename `elem` prunedir
                         then return []
                         else getRecursiveContents path langs prunedir
                    else case lookupLang filename >>= (\f -> f `elemIndex` langs <|> toMaybe 0 (null langs) ) of 
                            Nothing -> return []
                            _       -> return [path]
           else return [[topdir]]


-- read patterns from file


readPatternsFromFile :: FilePath -> IO [C.ByteString]
readPatternsFromFile f = 
    if null f then return []
              else liftM C.words $ C.readFile f 



main :: IO ()
main = do

    -- read command-line options 
    opts  <- cmdArgsRun options
    
    -- display lang-map...
    when (lang_map opts) $ dumpLangMap langMap >> exitSuccess 

    -- check whether patterns list is empty, display help message if it's the case
    when (null $ others opts) $ withArgs ["--help"] $ void (cmdArgsRun options)  

    -- read Cgrep config options
    conf  <- getCgrepOptions 

    -- load patterns:
    patterns <- (if null $ file opts then return [C.pack $ head $ others opts]
                                     else readPatternsFromFile $ file opts ) >>= \ps ->
                    return $ if ignore_case opts 
                                then map (C.map toLower) ps 
                                else ps


    -- check whether is a terminal device 
    isTerm <- hIsTerminalDevice stdin

    -- retrieve files to parse
    let paths = if null $ file opts then tail $ others opts
                                    else others opts

    -- parse cmd line language list:

    let (l0, l1, l2) = parseLangList (lang opts)

    -- language enabled:

    let lang_enabled = (if null l0 then language conf else l0 `union` l1) \\ l2

    when (debug opts) $ 
        putStrLn $ "languages : " ++ show lang_enabled

    -- retrieve the list of files to parse

    files <- liftM (\l -> if null l && not isTerm then [""] else l) $
                if recursive opts 
                    then liftM concat $ forM paths $ \p -> getRecursiveContents p lang_enabled (pruneDir conf)
                    else filterM doesFileExist paths

    -- debug
   
    when (debug opts) $ do
        putStrLn ("pattern   : " ++ show patterns)
        putStrLn ("files     : " ++ show files) 

    -- create Transactional Chan and Vars...
    --
   
    in_chan  <- newTChanIO 
    out_chan <- newTChanIO

    -- Launch worker threads...

    forM_ [1 .. jobs opts] $ \_ -> forkIO $ 
        fix (\action -> do 
                f <- atomically $ readTChan in_chan
                case f of 
                     Nothing -> atomically $ writeTChan out_chan []
                     Just f' -> do
                        out <- let op = sanitizeOptions f' opts in cgrepDispatch op op patterns f'
                        unless (null out) $ atomically $ writeTChan out_chan out 
                        action
                `E.catch` (\ex -> putStrLn ("parse error: " ++ show (ex :: SomeException)) >> action)
            )   


    -- This thread push files name in in_chan:
    
    mapM_ (atomically . writeTChan in_chan . Just) files 


    -- Enqueue finish message:
   
    mapM_ (atomically . writeTChan in_chan) $ replicate (jobs opts) Nothing 
   

    -- Dump output until workers are running  

    let stop = jobs opts

    fix (\action n -> 
         unless (n == stop) $ do
                 out <- atomically $ readTChan out_chan
                 case out of
                      [] -> action $ n+1
                      _  -> forM_ out (putStrLn . showOutput opts) >> action n
        )  0

