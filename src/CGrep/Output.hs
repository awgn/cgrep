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
       

module CGrep.Output (Output(..),  
                     mkOutput, 
                     prettyOutputList, 
                     showFile) where
 
import qualified Data.ByteString.Char8 as C

import System.Console.ANSI
import Language.Haskell.Interpreter

import Control.Monad
import Debug.Trace
import Data.List
import Data.Function

import CGrep.Types
import Options


getOffsetsLines :: Text8 -> [Int]
getOffsetsLines = C.elemIndices '\n' 


getOffset2d :: [OffsetLine] -> Offset -> Offset2d
getOffset2d idx off = let prc =  fst $ partition (< off) idx in
        case prc of
          [] -> (0, off)
          _  -> (length prc, off - last prc - 1)


mkOutput :: Options -> FilePath -> Text8 -> [Token] -> [Output]
mkOutput Options { invert_match = invert } f text ts
    | invert    = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) . invertMatchLines (length ls) $ mkMatchLines text ts
    | otherwise = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) $ mkMatchLines text ts
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


prettyOutputList :: Options -> [Output] -> IO [String] 
prettyOutputList opt out 
    | null $ hint opt = forM out $ staticOutput opt
    | otherwise       = dynamicOutputs opt out 


staticOutput :: Options -> Output -> IO String
staticOutput opt@ Options { no_filename = False, no_linenumber = False , count = False } (Output f n l ts) = 
    return $ showFile opt f ++ ":" ++ show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
staticOutput opt@ Options { no_filename = False, no_linenumber = True  , count = False } (Output f _ l ts) = 
    return $ showFile opt f ++ ":" ++ showTokens opt ts ++ showLine opt ts l
staticOutput opt@ Options { no_filename = True , no_linenumber = False , count = False } (Output _ n l ts) = 
    return $ show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
staticOutput opt@ Options { no_filename = True , no_linenumber = True  , count = False } (Output _ _ l ts) = 
    return $ showTokens opt ts ++ showLine opt ts l
staticOutput opt@ Options { count = True } (Output f n _ _) = 
    return $ showFile opt f ++ ":" ++ show n


dynamicOutputs :: Options -> [Output] -> IO [String]
dynamicOutputs opt outs = do
    let cmds = map mkCmd outs 
    out <- runInterpreter $ setImports ["Prelude", "Data.List"] >> mapM (`interpret` (as :: String)) cmds
    return $ either ((:[]) . show) id out 
        where mkCmd (Output f n l ts) = "let a # b = a !! b " ++
                                          "; file   = " ++ show (showFile opt f) ++ 
                                          "; row    = " ++ show n ++ 
                                          "; line   = " ++ show (showLine opt ts l) ++  
                                          "; tokens = " ++ show (map snd ts) ++ " in " ++ 
                                         hint opt

blue, bold, resetTerm :: String

blue      = setSGRCode [SetColor Foreground Vivid Blue]    
bold      = setSGRCode [SetConsoleIntensity BoldIntensity] 
resetTerm = setSGRCode []                                  


showTokens :: Options -> [Token] -> String
showTokens Options { show_match = st } xs
    | st        = show (map snd xs) 
    | otherwise = ""


showFile :: Options -> String -> String
showFile Options { color = c } f 
    | c         = bold ++ blue ++ f ++ resetTerm
    | otherwise = f 


showLine :: Options -> [Token] -> Line8 -> String
showLine Options { color = c } ts l
    | c         = hilightLine (sortBy (flip compare `on` (length . snd )) ts) (C.unpack l) 
    | otherwise = C.unpack l 


hilightLine :: [Token] -> String -> String
hilightLine ts =  hilightLine' (hilightIndicies ts, 0)
    where hilightLine' :: ([Int],Int) -> String -> String
          hilightLine'  _ [] = []
          hilightLine' (ns,n) (x:xs) = (if n `elem` ns then bold ++ [x] ++ resetTerm 
                                                       else [x]) ++ hilightLine' (ns, n+1) xs


hilightIndicies :: [Token] -> [Int]
hilightIndicies = concatMap (\(o, s) -> take (length s) [o..])


