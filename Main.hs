
module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Data()

import Control.Concurrent.Async

import Control.Monad (forM,forM_,liftM,filterM,when,msum)
import System.Directory
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Console.CmdArgs
import System.Environment
import System.IO

import Options
import Output
import Cgrep

cgreprc :: FilePath
cgreprc = "cgreprc2" 


options = cmdArgsMode $ Options 
          {
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

getRecursiveContents :: FilePath -> [String] -> [String] -> IO [FilePath]

getRecursiveContents topdir filetype prunedir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \fname -> do
    let path = topdir </> fname
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then if (takeFileName path `elem` prunedir)
           then return []
           else getRecursiveContents path filetype prunedir
      else if (takeExtension path `elem` filetype)
           then return [path]
           else return []
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

    -- read command-line options 
    opts  <- cmdArgsRun options

    -- check whether patterns list is empty, display help message if it's the case
    when (null $ others opts) $ withArgs ["--help"] $ cmdArgsRun options >> return ()  

    -- read Cgrep config options
    conf  <- getCgrepOptions 

    -- load patterns:
    patterns <- if (null $ file opts) then return $ [head $ others opts]
                                      else readPatternsFromFile $ file opts

    -- check whether patterns require regex
    opts' <- if (not $ all isCppIdentifier patterns) then putStrLn "cgrep: pattern(s) require regex search -> forced." >> return opts{ regex = True }
                                                     else return opts
    
    -- check whether is a terminal device 
    isTerm <- hIsTerminalDevice stdin

    -- retrieve files to parse
    let paths = if (null $ file opts) then tail $ others opts
                                       else others opts

    -- retrieve the list of files to parse
    files <- if (recursive opts) then liftM concat $ forM paths $ \p -> getRecursiveContents p (map ("." ++) $ fileType conf) (pruneDir conf)
                                 else filterM doesFileExist paths

    -- run cgrep threads
    futures <- mapM (async . ((cgrep opts') opts' patterns)) files

    -- putStrLn $ "opts :" ++ show opts'  
    -- putStrLn $ "conf :" ++ show conf
    -- putStrLn $ "pat  :" ++ show patterns
    -- putStrLn $ "paths:" ++ show paths
    -- putStrLn $ "files:" ++ show files
    
    -- wait for threads results
    results <- sequence $ map wait futures
    
    forM_ (concat results) $ \o -> do
        putStrLn $ showOutput opts' o

