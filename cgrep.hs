{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data
import Data.List
import Data.Char

import Control.Monad (forM)
import System.Directory
import System.FilePath ((</>), takeFileName)
import System.Console.CmdArgs


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
                others  :: [String]
               } deriving (Data, Typeable, Show)


options = cmdArgsMode $ Options {
                                    file = ""       &= typ "FILE"   &= help "read PATTERNs from file"   &= groupname "Pattern:",
                                    word = False                    &= help "force word matching",
                                    regex = False                   &= help "regex matching",
                                    icase = False                   &= help "ignore case distinctions",

                                    code = False                    &= help "grep in valid c/c+ source code" &= groupname "Context:",
                                    comment = False                 &= help "grep in comments",
                                    string = False                  &= help "grep in string literals",
                                    others = []                     &= args
                                } &= summary "Cgrep. Usage: cgreap [OPTION] [PATTERN] files..." &= program "cgrep"


data  CgrepOptions = CgrepOptions
                    {
                        fileType :: [String],
                        pruneDir :: [String]
                    } deriving (Show,Read)


-- remove # comment lines from file
--

uncomment :: String -> String
uncomment =  unlines . (dropWhile $ isPrefixOf "#" . dropWhile isSpace) . lines

-- parse CgrepOptions from ~/.cgreprc, or /etc/cgreprc 
--

getCgrepOptions :: IO CgrepOptions
getCgrepOptions = do
    home <- getHomeDirectory
    let f1 = home </> "." ++ cgreprc
    let f2 = "/etc" </> cgreprc
    b1 <- doesFileExist f1
    b2 <- doesFileExist f2 
    if (b1)
    then readFile f1 >>= \s -> return (read (uncomment s) :: CgrepOptions)
    else if (b2)
         then readFile f2 >>= \s -> return (read (uncomment s) :: CgrepOptions)
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


main = do
    -- read command-line and file options
    opts  <- cmdArgsRun options
    fopts <- getCgrepOptions 
    
    files <- getRecursiveContents "." (pruneDir fopts)

    print opts 
    print fopts
    print files
