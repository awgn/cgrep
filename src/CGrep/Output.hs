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

{-# LANGUAGE ExistentialQuantification #-} 

module CGrep.Output (Output(..), Match, mergeMatches, mkOutput, showOutput) where
 
import System.Console.ANSI
import Data.String.Utils
import Data.List
import Data.Function

import CGrep.StringLike
import Options

type Match  = (Int, [String])

data Output = forall a. (StringLike a) => Output FilePath Int a [String]


mergeMatches :: [Match] -> [Match] 
mergeMatches [] = []
mergeMatches xs = map mergeGroup $ groupBy ((==) `on` fst) xs
    where mergeGroup ls = (fst $ head ls, foldl (\l m -> l ++ snd m) [] ls)  


mkOutput :: (StringLike a) => FilePath -> a -> [Match] -> [Output]
mkOutput f source = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) 
    where ls = slLines source  


showOutput :: Options -> Output -> String
showOutput opt@ Options { no_filename = False, no_linenumber = False , count = False } (Output f n l ts) = showFile opt f ++ ":" ++ show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
showOutput opt@ Options { no_filename = False, no_linenumber = True  , count = False } (Output f _ l ts) = showFile opt f ++ ":" ++ showTokens opt ts ++ showLine opt ts l
showOutput opt@ Options { no_filename = True , no_linenumber = False , count = False } (Output _ n l ts) = show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
showOutput opt@ Options { no_filename = True , no_linenumber = True  , count = False } (Output _ _ l ts) = showTokens opt ts ++ showLine opt ts l
showOutput opt@ Options { count = True } (Output f n _ _) = showFile opt f ++ ":" ++ show n

blue, bold, reset :: String

blue    = setSGRCode [SetColor Foreground Vivid Blue]    
bold    = setSGRCode [SetConsoleIntensity BoldIntensity] 
reset   = setSGRCode []                                  


showTokens :: Options -> [String] -> String
showTokens Options { show_match = st } xs
    | st        = show xs 
    | otherwise = ""


showFile :: Options -> String -> String
showFile Options { color = c } f 
    | c         = bold ++ blue ++ f ++ reset
    | otherwise = f 


showLine :: (StringLike a) => Options -> [String] -> a -> String
showLine Options { color = c } ts l
    | c         = hilightLine ts (slToString l) 
    | otherwise = slToString l 


hilightLine :: [String] -> String -> String
hilightLine []      l = l
hilightLine (t:ts)  l = hilightLine ts $ replace t (hilight t) l 
    where hilight token = bold ++ token ++ reset


