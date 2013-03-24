{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data
import Data.List
import Data.Char
import Data.Maybe

import Control.Monad (forM,liftM,when,msum)
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.Console.CmdArgs
import System.Environment
import System.IO

cgreprc :: FilePath
cgreprc = "cgreprc2" 

data Options = Options 
               {
                -- Pattern:
                file    :: String,
                word    :: Bool,
                regex   :: Bool,
                icase   :: Bool,
                -- Context:
                code    :: Bool,
                comment :: Bool,
                string  :: Bool,
                others  :: [String],
                -- General:
                jobs      :: Int,
                multiline :: Bool,
                recursive :: Bool

               } deriving (Data, Typeable, Show)


options = cmdArgsMode $ Options {
                                    file  = ""       &= typ "FILE"  &= help "read PATTERNs from file" &= groupname "Pattern",
                                    word  = False                   &= help "force word matching",
                                    regex = False                   &= help "regex matching",
                                    icase = False                   &= help "ignore case distinctions",

                                    code = False                    &= help "grep in valid c/c+ source code" &= groupname "Context",
                                    comment = False                 &= help "grep in comments",
                                    string = False                  &= help "grep in string literals",
                                    jobs   = 1                      &= help "number of jobs" &= groupname "General",
                                    multiline = False               &= help "enable multi-line matching",
                                    recursive = False               &= help "enable recursive search",
                                    others = []                     &= args
                                } &= summary "Cgrep. Usage: cgrep [OPTION] [PATTERN] files..." &= program "cgrep"


data  CgrepOptions = CgrepOptions
                    {
                        fileType :: [String],
                        pruneDir :: [String]
                    } deriving (Show,Read)


-- remove # comment lines from config file
--

rmCommentLines :: String -> String
rmCommentLines =  unlines . (filter $ not . isPrefixOf "#" . dropWhile isSpace) . lines


-- parse CgrepOptions from ~/.cgreprc, or /etc/cgreprc 
--

getCgrepOptions :: IO CgrepOptions
getCgrepOptions = do
    home <- getHomeDirectory
    conf <- liftM msum $ forM [ home </> "." ++ cgreprc, "/etc" </> cgreprc ] $ \f ->  do
                doesFileExist f >>= \b -> if b then return (Just f) else return Nothing
    if (isJust conf) then (readFile $ fromJust conf) >>= \xs -> return (read (rmCommentLines xs) :: CgrepOptions) 
                     else return $ CgrepOptions [] []


-- from Realworld in Haskell...

getRecursiveContents :: FilePath -> [String] -> IO [FilePath]

getRecursiveContents topdir prune = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \fname -> do
    let path = topdir </> fname
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then if (takeFileName path `elem` prune)
           then return []
           else getRecursiveContents path prune
      else return [path]
  return (concat paths)


-- read patterns from file
-- TODO: add support for info

readPatternsFromFile :: String -> IO [String]
readPatternsFromFile f = do
    if null f then return []
              else liftM words $ readFile f 


isCppIdentifier :: String -> Bool
isCppIdentifier = all (\c -> isAlphaNum c || c == '_') 


-- main
--

main :: IO ()
main = do

    -- read command-line and file options
    opts  <- cmdArgsRun options
    fopts <- getCgrepOptions 
    files <- getRecursiveContents "." (pruneDir fopts)

    -- load patterns:
    patterns <- if (null $ file opts) then return $ others opts
                                      else readPatternsFromFile $ file opts

    -- check whether patterns list is empty, display help message if it's the case
    when (null patterns) $ withArgs ["--help"] $ cmdArgsRun options >> return ()  

    -- check whether patterns require regex
    opts' <- if (not $ all isCppIdentifier patterns) then putStrLn "cgrep: pattern(s) require regex search -> forced." >> return opts{ regex = True }
                                                     else return opts
    
    -- check whether is a terminal device 
    isTerm <- hIsTerminalDevice stdin

    print opts'  
    print fopts
    print files
    print patterns
    print isTerm
