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
{-# LANGUAGE ExistentialQuantification #-} 

module CGrep.Common (Output(..), CgrepFunction, MatchLine, Text8, 
                     getFileName, getText, getMultiLine, mkOutput, prettyOutput, spanGroup) where
 
import qualified Data.ByteString.Char8 as C

import System.Console.ANSI

import Control.Monad (liftM)
-- import Data.Maybe
import Data.List
import Data.Char
import Data.Function

import CGrep.Token

import Options


type CgrepFunction = Options -> [Text8] -> Maybe FilePath -> IO [Output] 

type Text8  = C.ByteString

type Token = (Offset, String)
type MatchLine  = (Int, [String])

data Output = Output FilePath Int Text8 [String]
                                  

getFileName :: Maybe FilePath -> String
getFileName Nothing = "<STDIN>"
getFileName (Just name) = name


getText :: Bool -> Maybe FilePath -> IO Text8
getText icase filename 
    | icase = liftM (C.map toLower) content
    | otherwise =  content
        where content = maybe C.getContents C.readFile filename                  
 

mkOutput :: Options -> FilePath -> Text8 -> [Token] -> [Output]
mkOutput Options { invert_match = invert } f source 
    | invert    = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) . invertMatchLines (length ls) . mkMatchLines source
    | otherwise = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) . mkMatchLines source
        where ls = C.lines source  


mkMatchLines :: Text8 -> [Token] -> [MatchLine] 
mkMatchLines _ [] = []
mkMatchLines src ts = map mergeGroup $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ map (\(o, t) -> (1 + offsetToLineNo src o, [t])) ts 
    where mergeGroup ls = (fst $ head ls, foldl (\l m -> l ++ snd m) [] ls)  


offsetToLineNo :: Text8 -> Int -> Int
offsetToLineNo text = \off -> length . fst $ partition (< off) crs 
    where crs = C.elemIndices '\n' text 


invertMatchLines :: Int -> [MatchLine] -> [MatchLine]
invertMatchLines n xs =  filter (\(i,_) ->  i `notElem` idx ) $ take n [ (i, []) | i <- [1..]] 
    where idx = map fst xs


prettyOutput :: Options -> Output -> String
prettyOutput opt@ Options { no_filename = False, no_linenumber = False , count = False } (Output f n l ts) = showFile opt f ++ ":" ++ show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { no_filename = False, no_linenumber = True  , count = False } (Output f _ l ts) = showFile opt f ++ ":" ++ showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { no_filename = True , no_linenumber = False , count = False } (Output _ n l ts) = show n ++ ":" ++ showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { no_filename = True , no_linenumber = True  , count = False } (Output _ _ l ts) = showTokens opt ts ++ showLine opt ts l
prettyOutput opt@ Options { count = True } (Output f n _ _) = showFile opt f ++ ":" ++ show n


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


showLine :: Options -> [String] -> Text8 -> String
showLine Options { color = c } ts l
    | c         = hilightLine (sortBy (flip compare `on` length) ts) (C.unpack l) 
    | otherwise = C.unpack l 


substrIndex :: String -> String -> (Int, [Int], Int, String)
substrIndex reverseword = foldl step (0,[],1,"") 
    where step (x,ys,i,matcher) c | c:matcher == reverseword = (x+1,i-length reverseword:ys,i+1,"")
                                  | (c:matcher) `isSuffixOf` reverseword = (x,ys,i+1,c:matcher)
                                  | otherwise = (x,ys,i+1,"")


substrIndices :: String -> String -> [Int]
substrIndices word content = (\(_,x,_,_)-> reverse x) $ substrIndex (reverse word) content


substrAllIndices :: [String] -> String -> [Int]
substrAllIndices ts xs = nub $ sort $ concatMap (`substrAllIndices'` xs) ts
    where substrAllIndices' :: String -> String -> [Int]
          substrAllIndices' t xs' = concatMap (\n -> take len [n..]) $ substrIndices t xs'
            where len = length t
        

hilightLine :: [String] -> String -> String
hilightLine ts line =  hilightLine' (idx, 0) line
    where idx = substrAllIndices ts line
          hilightLine' :: ([Int],Int) -> String -> String
          hilightLine'  _ [] = []
          hilightLine' (ns,n) (x:xs) = (if n `elem` ns then bold ++ [x] ++ reset 
                                                       else [x]) ++ hilightLine' (ns, n+1) xs


spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs = take n xs : spanGroup n (tail xs)


getMultiLine :: Int -> Text8 -> Text8
getMultiLine 1 xs = xs
getMultiLine n xs = C.unlines $ map C.unwords $ spanGroup n (C.lines xs) 
 
 
