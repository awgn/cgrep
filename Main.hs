
module Main where

import Data.List
import Data.Char
import Data.Maybe
import Data.Data()
import Data.Function

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

-- import Control.Concurrent.Async

import Control.Monad (forM,forM_,liftM,filterM,when,msum)
import System.Directory
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Console.CmdArgs
import System.Environment
import System.IO

import CGrep.Options
import CGrep.Output
import CGrep.CGrep

import qualified Data.ByteString.Char8 as C

cgreprc :: FilePath
cgreprc = "cgreprc-new" 


options = cmdArgsMode $ Options 
          {
                file  = ""  &= typ "FILE"  &= help "read PATTERNs from file" &= groupname "Pattern",
                word  = False              &= help "force word matching",
                regex = False              &= help "regex matching" &= explicit &= name "G" &=name "regex",
                ignore_case = False        &= help "ignore case distinctions",

                code = False               &= help "grep in C/C++ source code" &= explicit &= name "c" &= name "code" &= groupname "Context",
                comment = False            &= help "grep in comments"          &= explicit &= name "m" &= name "comment",
                string = False             &= help "grep in string literals"   &= explicit &= name "s" &= name "string",
                
                no_filename = False        &= help "suppress the file name prefix on output"  &= explicit &= name "h" &= name "no-filename" &= groupname "Output control",
                no_linenumber= False       &= help "suppress the line number on output lines" &= explicit &= name "N" &= name "no-line-umber",

                jobs   = 1                 &= help "number of jobs" &= groupname "General",
                multiline = False          &= help "enable multi-line matching",
                recursive = False          &= help "enable recursive search",
                invert_match = False       &= help "select non-matching lines" &= explicit &= name "invert-match", 
                others = []                &= args
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


readPatternsFromFile :: FilePath -> IO [C.ByteString]
readPatternsFromFile f = do
    if null f then return []
              else liftM C.words $ C.readFile f 


isCppIdentifier :: C.ByteString -> Bool
isCppIdentifier xs = C.all (\c -> isAlphaNum c || c == '_') xs


main :: IO ()
main = do

    -- read command-line options 
    opts  <- cmdArgsRun options

    -- check whether patterns list is empty, display help message if it's the case
    when (null $ others opts) $ withArgs ["--help"] $ cmdArgsRun options >> return ()  

    -- read Cgrep config options
    conf  <- getCgrepOptions 

    -- load patterns:
    patterns <- if (null $ file opts) then return $ [C.pack $ head $ others opts]
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

    -- create Transactional Chan and Vars...
    --
   
    running <- newTVarIO (jobs opts')

    in_chan  <- newTChanIO 
    out_chan <- newTChanIO

    -- Launch worker threads...

    forM_ [1 .. jobs opts] $ \_ -> forkIO $ fix (\action -> do
        f <- atomically $ readTChan in_chan
        case f of 
             "" -> do
                 atomically (modifyTVar' running (subtract 1 )) 
                 atomically $ writeTChan out_chan [] 
                 return ()
             _  -> do
                out <- (cgrep opts') opts' patterns f
                when (not $ null out) $ atomically $ writeTChan out_chan out 
                action
        )

    -- This thread push files name in in_chan:
    
    mapM_ (\f -> atomically $ writeTChan in_chan f ) files 

    -- Enqueue finish message:
   
    mapM_ (\f -> atomically $ writeTChan in_chan f ) $ replicate (jobs opts') "" 
   

    -- Dump output until workers are running  

    fix (\action n -> do
        (empty, run) <- atomically $ do 
            e <- isEmptyTChan out_chan
            r <- readTVar running
            return (e,r)
        case empty of 
             True -> if (run == 0 && n == jobs opts') then return ()
                                                      else threadDelay 1 >> action n
             _    -> do
                 out <- atomically $ readTChan out_chan
                 forM_ out $ \line -> putStrLn $ showOutput opts' line 
                 case out of
                      [] -> action $ n+1
                      _  -> action n
        )  0

