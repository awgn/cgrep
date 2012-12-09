{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Data
import Data.List
import Data.Char

import Control.Monad (forM)
import System.Directory
import System.FilePath ((</>))
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


uncomment :: String -> String
uncomment =  unlines . (dropWhile $ isPrefixOf "#" . dropWhile isSpace) . lines


getCgrepOptions :: IO CgrepOptions
getCgrepOptions = do
    home <- getHomeDirectory
    let f1 = home </> "." ++ cgreprc
    b1 <- doesFileExist f1
    b2 <- doesFileExist $ "/etc" </> cgreprc
    if (b1)
    then readFile f1 >>= \s -> return (read (uncomment s) :: CgrepOptions)
    else if (b2)
         then readFile f1 >>= \s -> return (read (uncomment s) :: CgrepOptions)
         else return $ CgrepOptions [] []


-- from Realworld in Haskell...

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return (concat paths)


main = do
    -- read command-line and file options
    opts  <- cmdArgsRun options
    fopts <- getCgrepOptions 
    
    files <- getRecursiveContents "."

    print opts 
    print fopts
    print files
