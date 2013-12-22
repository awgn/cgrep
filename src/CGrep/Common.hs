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
       
{-# LANGUAGE FlexibleContexts #-} 

module CGrep.Common (Output(..), CgrepFunction, MatchLine, Text8, 
                     getFileName, getText, expandMultiline, mkOutput, 
                     ignoreCase, prettyOutput, showFile, spanGroup) where
 
import qualified Data.ByteString.Char8 as C

import System.Console.ANSI

import Control.Monad (liftM)
import Data.List
import Data.Char
import Data.Function

import CGrep.Types
import Options


type CgrepFunction = Options -> [Text8] -> Maybe FilePath -> IO [Output] 


getOffsetsLines :: Text8 -> [Int]
getOffsetsLines = C.elemIndices '\n' 


getOffset2d :: [OffsetLine] -> Offset -> Offset2d
getOffset2d idx off = let prc =  fst $ partition (< off) idx in
        case prc of
          [] -> (0, off)
          _  -> (length prc, off - last prc - 1)


getFileName :: Maybe FilePath -> String
getFileName Nothing = "<STDIN>"
getFileName (Just name) = name


getText :: Maybe FilePath -> IO Text8
getText  = maybe C.getContents C.readFile


ignoreCase :: Options -> Text8 -> Text8
ignoreCase Options { ignore_case = icase } 
    | icase  =  C.map toLower 
    | otherwise = id


expandMultiline :: Options -> Text8 -> Text8
expandMultiline Options { multiline = n } xs 
    | n == 1 = xs
    | otherwise = C.unlines $ map C.unwords $ spanGroup n (C.lines xs) 
 

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


spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs = take n xs : spanGroup n (tail xs)


prettyOutput :: Options -> Output -> String
prettyOutput opt@ Options { no_filename = False, no_linenumber = False , count = False } (Output f n l ts) = showFile opt f ++ ":" ++ show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { no_filename = False, no_linenumber = True  , count = False } (Output f _ l ts) = showFile opt f ++ ":" ++ showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { no_filename = True , no_linenumber = False , count = False } (Output _ n l ts) = show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { no_filename = True , no_linenumber = True  , count = False } (Output _ _ l ts) = showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { count = True } (Output f n _ _) = showFile opt f ++ ":" ++ show n


blue, bold, reset :: String

blue    = setSGRCode [SetColor Foreground Vivid Blue]    
red     = setSGRCode [SetColor Foreground Vivid Red]    
bold    = setSGRCode [SetConsoleIntensity BoldIntensity] 
reset   = setSGRCode []                                  


showTokens :: Options -> [Token] -> String
showTokens Options { show_match = st } xs
    | st        = show (map snd xs) 
    | otherwise = ""


showFile :: Options -> String -> String
showFile Options { color = c } f 
    | c         = bold ++ red ++ f ++ reset
    | otherwise = f 


showLine :: Options -> [Token] -> Line8 -> String
showLine Options { color = c } ts l
    | c         = hilightLine (sortBy (flip compare `on` (length . snd )) ts) (C.unpack l) 
    | otherwise = C.unpack l 


hilightLine :: [Token] -> String -> String
hilightLine ts =  hilightLine' (hilightIndicies ts, 0)
    where hilightLine' :: ([Int],Int) -> String -> String
          hilightLine'  _ [] = []
          hilightLine' (ns,n) (x:xs) = (if n `elem` ns then bold ++ [x] ++ reset 
                                                       else [x]) ++ hilightLine' (ns, n+1) xs


hilightIndicies :: [Token] -> [Int]
hilightIndicies = concatMap (\(o, s) -> take (length s) [o..])


