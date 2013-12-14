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

module CGrep.Common (Output(..), CgrepFunction, Match, mergeMatches, mkOutput, showOutput, spanGroup, spanMultiLine, offsetToLine, basicGrep) where
 
import qualified Data.ByteString.Char8 as C

import System.Console.ANSI

import Data.List
import Data.Function

import Text.Regex.Posix
import Options

import CGrep.StringLike


type CgrepFunction = Options -> [C.ByteString] -> FilePath -> IO [Output] 

type Match  = (Int, [String])

data Output = forall a. (StringLike a) => Output FilePath Int a [String]


mergeMatches :: [Match] -> [Match] 
mergeMatches [] = []
mergeMatches xs = map mergeGroup $ groupBy ((==) `on` fst) xs
    where mergeGroup ls = (fst $ head ls, foldl (\l m -> l ++ snd m) [] ls)  


invertMatches :: Int -> [Match] -> [Match]
invertMatches n xs =  filter (\(i,_) ->  i `notElem` idx ) $ take n [ (i, []) | i <- [1..]] 
    where idx = map fst xs


mkOutput :: (StringLike a) => Options -> FilePath -> a -> [Match] -> [Output]
mkOutput Options { invert_match = invert } f source 
    | invert    = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) . invertMatches (length ls)
    | otherwise = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) 
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
    | c         = hilightLine (sortBy (flip compare `on` length) ts) (slToString l) 
    | otherwise = slToString l 


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


spanMultiLine :: Int -> C.ByteString -> C.ByteString
spanMultiLine 1 xs = xs
spanMultiLine n xs = C.unlines $ map C.unwords $ spanGroup n (C.lines xs) 
 
 
offsetToLine :: C.ByteString -> Int -> Int
offsetToLine text = (\off -> length . fst $ partition (\n -> n < off) crs) 
    where crs = C.elemIndices '\n' text 


basicGrep :: (StringLike a) => Options -> [a] -> (Int, a) -> [Match]
basicGrep opt patterns (n, line) 
    | null  pfilt = []
    | otherwise   = [ (n, map slToString pfilt) ]
        where pfilt = slSearch (word_match opt) patterns line
 

