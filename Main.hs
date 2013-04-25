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
import Data.Maybe
import Data.Data()
import Data.Function

import Control.Exception as E
import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan

import Control.Monad 
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.Console.CmdArgs
import System.Environment
import System.IO
import System.Exit

import CGrep.Options
import CGrep.Output
import CGrep.CGrep
import CGrep.Lang

import qualified Data.ByteString.Char8 as C

cgreprc :: FilePath
cgreprc = "cgreprc.hs" 

version :: String
version = "2.3"

options = cmdArgsMode $ Options 
          {
                file  = ""  &= typ "FILE"  &= help "read PATTERNs from file" &= groupname "Pattern",
                word  = False              &= help "force word matching",
                regex = False              &= help "regex matching" &= explicit &= name "G" &=name "regex",
                ignore_case = False        &= help "ignore case distinctions",

                code = False               &= help "grep in source code"     &= explicit &= name "c" &= name "code" &= groupname "Context filters",
                comment = False            &= help "grep in comments"        &= explicit &= name "m" &= name "comment",
                literal = False            &= help "grep in string literals" &= explicit &= name "l" &= name "literal",

                identifier = False         &= help "identifiers" &= explicit &= name "identifier" &= groupname "C/C++ language",
                keyword = False            &= help "keywords" &= explicit &= name "keyword",
                directive = False          &= help "preproc directives" &= explicit &= name "directive",
                header = False             &= help "headers name" &= explicit &= name "header",
                number = False             &= help "literal numbers" &= explicit &= name "number",
                string = False             &= help "literal strings" &= explicit &= name "string",
                char = False               &= help "literal chars" &= explicit &= name "char",
                oper = False               &= help "operators" &= explicit &= name "oper",
                
                no_filename = False        &= help "suppress the file name prefix on output"  &= explicit &= name "h" &= name "no-filename" &= groupname "Output control",
                no_linenumber= False       &= help "suppress the line number on output lines" &= explicit &= name "N" &= name "no-line-umber",
                lang_map = False           &= help "show language -> ext map",

                jobs   = 1                 &= help "number of jobs" &= groupname "General",
                multiline = False          &= help "enable multi-line matching",
                recursive = False          &= help "enable recursive search",
                invert_match = False       &= help "select non-matching lines" &= explicit &= name "invert-match", 
                debug = False              &= help "debug mode",
                others = []                &= args

          } &= summary ("Cgrep " ++ version ++ ". Usage: cgrep [OPTION] [PATTERN] files...") &= program "cgrep"


data  CgrepOptions = CgrepOptions
                    {
                        language :: [Lang],
                        pruneDir :: [String]
                    } deriving (Show,Read)


-- remove # comment lines from config file
--

rmCommentLines :: String -> String
rmCommentLines =  unlines . filter (not . isPrefixOf "#" . dropWhile isSpace) . lines


-- parse CgrepOptions from ~/.cgreprc, or /etc/cgreprc 
--

getCgrepOptions :: IO CgrepOptions
getCgrepOptions = do
    home <- getHomeDirectory
    conf <- liftM msum $ forM [ home </> "." ++ cgreprc, "/etc" </> cgreprc ] $ \f ->
                doesFileExist f >>= \b -> if b then return (Just f) else return Nothing
    if isJust conf then readFile (fromJust conf) >>= \xs -> return (read (rmCommentLines xs) :: CgrepOptions) 
                   else return $ CgrepOptions [] []


-- from Realworld in Haskell...

getRecursiveContents :: FilePath -> [Lang] -> [String] -> IO [FilePath]

getRecursiveContents topdir langs prunedir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \fname -> do
    let path = topdir </> fname
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then if takeFileName path `elem` prunedir
           then return []
           else getRecursiveContents path langs prunedir
      else case lookupLang path >>= (`elemIndex` langs) of 
            Nothing -> return []
            _       -> return [path]
  return (concat paths)


-- read patterns from file


readPatternsFromFile :: FilePath -> IO [C.ByteString]
readPatternsFromFile f = 
    if null f then return []
              else liftM C.words $ C.readFile f 


isCppIdentifier :: C.ByteString -> Bool
isCppIdentifier = C.all (\c -> isAlphaNum c || c == '_') 


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
                    if ignore_case opts 
                      then return $ map (C.map toLower) ps 
                      else return ps


    -- check whether patterns require regex
    -- opts' <- if (not $ all isCppIdentifier patterns) then putStrLn "cgrep: pattern(s) require regex search -> forced." >> return opts{ regex = True }
    --                                                 else return opts
    
    -- check whether is a terminal device 
    isTerm <- hIsTerminalDevice stdin

    -- retrieve files to parse
    let paths = if null $ file opts then tail $ others opts
                                    else others opts


    -- retrieve the list of files to parse

    files <- liftM (\l -> if null l && not isTerm then [""] else l) $
                if recursive opts 
                    then liftM concat $ forM paths $ \p -> getRecursiveContents p (language conf) (pruneDir conf)
                    else filterM doesFileExist paths

    -- debug
   
    when (debug opts) $ do
        putStrLn ("pattern: " ++ show patterns)
        putStrLn ("files  : " ++ show files) 

    -- create Transactional Chan and Vars...
    --
   
    in_chan  <- newTChanIO 
    out_chan <- newTChanIO

    -- Launch worker threads...

    forM_ [1 .. jobs opts] $ \_ -> forkIO $ 
        fix (\action -> do 
                f <- atomically $ readTChan in_chan
                case f of 
                     Nothing -> do   
                         atomically $ writeTChan out_chan []
                         return ()
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

    fix (\action n -> 
        case n == jobs opts of 
             True -> return ()
             _ -> do
                 out <- atomically $ readTChan out_chan
                 case out of
                      [] -> action $ n+1
                      _  -> forM_ out (putStrLn . showOutput opts) >> action n
        )  0

