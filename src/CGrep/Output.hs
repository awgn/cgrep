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

{-# LANGUAGE CPP #-}

module CGrep.Output (Output(..),
                     mkOutput,
                     putPrettyHeader,
                     putPrettyFooter,
                     prettyOutput,
                     showFileName,
                     showFile,
                     showBold) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Codec.Binary.UTF8.String as UC

import Text.Show.Unicode
import System.Console.ANSI

#ifdef ENABLE_HINT
import Language.Haskell.Interpreter
#endif

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Data.Maybe
import Data.List
import Data.Function

import CGrep.Types
import CGrep.Token

import Options
import Util
import Config
import Reader
import Safe


data Output = Output
    { outFilePath :: FilePath
    , outLineNo   :: Int
    , outLine     :: Text8
    , outTokens   :: [Token]
    }
    deriving (Show)


getOffsetsLines :: Text8 -> [Int]
getOffsetsLines txt = let l = C.length txt in filter (<(l-1)) $ C.elemIndices '\n' txt


getOffset2d :: [OffsetLine] -> Offset -> Offset2d
getOffset2d idx off =
  let prc = filter (< off) idx
      (len_prc, last_prc) = foldl' (\(len,_) cur -> (len + 1, cur)) (0,off) prc
  in (len_prc, off - last_prc - 1)

mkOutput :: (Monad m) => FilePath -> Text8 -> Text8 -> [Token] -> OptionT m [Output]
mkOutput f text multi ts = do
    invert <- reader (invert_match . snd)
    return $ if invert then map (\(n, xs) -> Output f n (ls !! (n-1)) xs) . invertMatchLines (length ls) $ mkMatchLines multi ts
                       else map (\(n, xs) -> Output f n (ls !! (n-1)) xs) $ mkMatchLines multi ts
    where ls = C.lines text


mkMatchLines :: Text8 -> [Token] -> [MatchLine]
mkMatchLines _ [] = []
mkMatchLines text ts = map mergeGroup $ groupBy ((==) `on` fst) $
    sortBy (compare `on` fst) $ map (\t -> let (r,c) = getOffset2d ols (fst t) in (1 + r, [(c, snd t)])) ts
    where mergeGroup ls = (fst $ head ls, foldl (\l m -> l ++ snd m) [] ls)
          ols = getOffsetsLines text


invertMatchLines :: Int -> [MatchLine] -> [MatchLine]
invertMatchLines n xs =  filter (\(i,_) ->  i `notElem` idx ) $ take n [ (i, []) | i <- [1..]]
    where idx = map fst xs


putPrettyHeader :: OptionT IO ()
putPrettyHeader = do
    (_,opt) <- ask
    case () of
      _  | json opt  -> liftIO $ putStrLn "["
         | xml  opt  -> liftIO $ putStrLn "<?xml version=\"1.0\"?>" >> putStrLn "<cgrep>"
         | otherwise -> return ()


putPrettyFooter :: OptionT IO ()
putPrettyFooter = do
    (_,opt) <- ask
    case () of
      _  | json opt  -> liftIO $ putStrLn "]"
         | xml  opt  -> liftIO $ putStrLn "</cgrep>"
         | otherwise -> return ()


prettyOutput :: (Monad m) => [Output] -> OptionT m [String]
prettyOutput out = do
    (_,opt) <- ask
    case () of
        _ | isJust $ format opt -> mapM formatOutput out
          | filename_only opt   -> filenameOutput out
          | json opt            -> jsonOutput out
          | xml opt             -> xmlOutput  out
#ifdef ENABLE_HINT
          | isJust $ hint opt   -> hintOputput out
#endif
          | otherwise           -> defaultOutput out

defaultOutput :: (Monad m) => [Output] -> OptionT m [String]
defaultOutput xs = do
    (conf,opt) <- ask
    case () of
        _ |  Options{ no_filename = False, no_numbers = False , count = False } <- opt
                -> return $ map (\out -> concat $ [showFile conf, showSep ":", showLineCol, showSep ":", showTokens, showLine conf] <*> [opt] <*> [out]) xs
          |  Options{ no_filename = False, no_numbers = True  , count = False } <- opt
                -> return $ map (\out -> concat $ [showFile conf, showSep ":", showTokens,  showLine conf] <*> [opt] <*> [out] ) xs
          |  Options{ no_filename = True , no_numbers = False , count = False } <- opt
                -> return $ map (\out -> concat $ [showLineCol, showSep ":",  showTokens, showLine conf] <*> [opt] <*> [out] ) xs
          |  Options{ no_filename = True , no_numbers = True  , count = False } <- opt
                -> return $ map (\out -> concat $ [showTokens, showLine conf] <*> [opt] <*>  [out]) xs
          |  Options{ count = True } <- opt -> do let gs = groupBy (\(Output f1 _ _ _) (Output f2 _ _ _) -> f1 == f2) xs
                                                  return $ map (\ys@(y:_) -> showFile conf opt y ++ ":" ++ show (length ys)) gs
          |  otherwise -> undefined


jsonOutput :: (Monad m) => [Output] -> OptionT m [String]
jsonOutput outs = return $
    [" { \"file\": " ++ show fname ++ ", \"matches\": ["] ++
    [ intercalate "," (foldl mkMatch [] outs) ] ++
    ["] }"]
        where fname | (Output f _ _ _) <- head outs = f
              mkToken (n, xs) = "{ \"col\": " ++ show n ++ ", \"token\": " ++ show xs ++ " }"
              mkMatch xs (Output _ n l ts) = xs ++ [ "{ \"row\": " ++ show n ++ ", \"tokens\": [" ++ intercalate "," (map mkToken ts) ++ "], \"line\":" ++ show l ++ "}" ]


filenameOutput :: (Monad m) => [Output] -> OptionT m [String]
filenameOutput outs = return $ nub $ map (\(Output fname _ _ _) -> fname) outs


xmlOutput :: (Monad m) => [Output] -> OptionT m [String]
xmlOutput outs = return $
    ["<file name=" ++ show fname ++ ">" ] ++
    ["<matches>" ] ++
    [foldl mkMatch "" outs] ++
    ["</matches>"] ++
    ["</file>"]
        where fname | (Output f _ _ _) <- head outs = f
              mkToken (n, xs) = "<token col=\"" ++ show n ++ "\" >" ++ xs ++ "</token>"
              mkMatch xs (Output _ n l ts) = xs ++  "<match line=" ++ show l ++ " row=\"" ++ show n ++ "\">" ++
                                                    unwords (map mkToken ts) ++
                                                    "</match>"


formatOutput :: (Monad m) => Output -> OptionT m String
formatOutput out = do
    (conf,opt) <- ask
    return $ replace (fromJust $ format opt)
        [
            ("#f", showFile conf opt out),
            ("#n", showLineCol opt out),
            ("#l", showLine conf opt out),
            ("#t", ushow ts'),
            ("##", unwords ts'),
            ("#,", intercalate "," ts'),
            ("#;", intercalate ";" ts'),
            ("#0", atDef "" ts' 0),
            ("#1", atDef "" ts' 1),
            ("#2", atDef "" ts' 2),
            ("#3", atDef "" ts' 3),
            ("#4", atDef "" ts' 4),
            ("#5", atDef "" ts' 5),
            ("#6", atDef "" ts' 6),
            ("#7", atDef "" ts' 7),
            ("#8", atDef "" ts' 8),
            ("#9", atDef "" ts' 9)
        ]
    where ts' = map snd (outTokens out)


replace :: String -> [(String, String)] -> String
replace ys@(x:xs) pats =
  let pats' = filter ((`isPrefixOf` ys) . fst) pats  in
  if null pats' then x : replace xs pats
                else let new = head pats' in snd new ++ replace (drop (length(fst new) - 1) xs) pats
replace [] _ = []


#ifdef ENABLE_HINT
hintOputput :: [Output] -> OptionT IO [String]
hintOputput outs = do
    (_,opt) <- ask
    let cmds = map mkCmd outs
    out <- runInterpreter $ setImports ["Prelude", "Data.List"] >> mapM (`interpret` (as :: String)) cmds
    return $ either ((:[]) . show) id out
        where mkCmd out@(Output f n l ts) = "let a # b = a !! b " ++
                                             "; file   = " ++ show (showFile opt out) ++
                                             "; row    = " ++ show n ++
                                             "; line   = " ++ show (showLine conf opt ts l) ++
                                             "; tokens = " ++ ushow (map snd ts) ++ " in " ++
                                            (fromJust $ hint opt)
#endif

bold, resetTerm :: String
bold      = setSGRCode [SetConsoleIntensity BoldIntensity]
resetTerm = setSGRCode []


type ColorString = String


showSep  :: String -> Options -> Output -> String
showSep xs _ _ = xs


showFile :: Config -> Options -> Output -> String
showFile conf opt = showFileName conf opt . outFilePath


showLineCol :: Options -> Output -> String
showLineCol Options{no_numbers = True } _ = ""
showLineCol Options{no_numbers = False, no_column = True  } (Output _ n _ _)  = show n
showLineCol Options{no_numbers = False, no_column = False } (Output _ n _ []) = show n 
showLineCol Options{no_numbers = False, no_column = False } (Output _ n _ ts) = show n ++ ":" ++ show ((+1) . fst . head $ ts)


showTokens :: Options -> Output -> String
showTokens Options { show_match = st } out
    | st        = ushow (map snd (outTokens out))
    | otherwise = ""


showLine :: Config -> Options -> Output -> String
showLine conf Options { color = c, no_color = c' } out
    | c && not c'= hilightLine conf (sortBy (flip compare `on` (length . snd )) (outTokens out)) line
    | otherwise  = line
    where line = UC.decode $ B.unpack $ outLine out


showFileName :: Config -> Options -> String -> String
showFileName conf opt = showColoredAs opt $ setSGRCode (configColorFile conf)


showBold :: Options -> String -> String
showBold opt = showColoredAs opt bold


showColoredAs :: Options -> ColorString -> String -> String
showColoredAs Options { color = c, no_color = c'} colorCode str
    | c && not c'= colorCode ++ str ++ resetTerm
    | otherwise  = str


hilightLine :: Config -> [Token] -> String -> String
hilightLine conf ts =  hilightLine' (hilightIndicies ts, 0, 0)
    where hilightLine' :: ([(Int, Int)], Int, Int) -> String -> String
          hilightLine'  _ [] = []
          hilightLine' (ns, n, bs) s@(x:_) = (case () of
                                                  _ | check && bs' == 0 -> if fst stack > 0 then colorMatch ++ [x] ++ resetTerm
                                                                                            else x : resetTerm
                                                    | check && bs' > 0 -> colorMatch ++ [x]
                                                    | otherwise -> next
                                             ) ++ hilightLine' (ns, n + nn, bs') rest
            where stack = foldr (\(a, b) (c, d) -> (c + fromEnum (a == n), d + fromEnum (b == n))) (0, 0) ns
                  check = fst stack > 0 || snd stack > 0
                  colorMatch = setSGRCode (configColorMatch conf)
                  bs' = bs + fst stack - snd stack
                  plain = nub . sort $ foldr (\(a, b) acc -> a : b : acc) [] ns
                  nn | check = 1
                     | null plain' = length s
                     | otherwise = head plain' - n
                         where plain' = dropWhile (<=n) plain
                  (next, rest) = splitAt nn s


hilightIndicies :: [Token] -> [(Int, Int)]
hilightIndicies = foldr (\t a -> let b = fst t in (b, b + length (snd t) - 1) : a) [] . filter (notNull . snd)

