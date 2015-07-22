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

module CGrep.Common (CgrepFunction, Text8,
                     getFileName,
                     getText,
                     quickSearch,
                     expandMultiline,
                     ignoreCase,
                     spanGroup,
                     trim,
                     trim8,
                     unquotes) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Search as SC

import Data.Char
import Data.Array.Unboxed

import CGrep.Types
import CGrep.Output
import Options


type CgrepFunction = Options -> [Text8] -> Maybe FilePath -> IO [Output]


getFileName :: Maybe FilePath -> String
getFileName Nothing = "<STDIN>"
getFileName (Just name) = name


getText :: Maybe FilePath -> IO Text8
getText  = maybe C.getContents C.readFile


quickSearch :: Options -> [Text8] -> Text8 -> Maybe Bool
quickSearch opt ps text
    | no_turbo opt        = Nothing
    | otherwise           = Just $ any has_pattern ps
    where has_pattern pat = not . null $ pat `SC.nonOverlappingIndices` text


toLowercase :: Char -> Char
toLowercase x = ctypeLowercase ! x
    where ctypeLowercase = listArray ('\0','\255') (map toLower ['\0'..'\255']) :: UArray Char Char


ignoreCase :: Options -> Text8 -> Text8
ignoreCase Options { ignore_case = icase }
    | icase  =  C.map toLowercase
    | otherwise = id


expandMultiline :: Options -> Text8 -> Text8
expandMultiline Options { multiline = n } xs
    | n == 1 = xs
    | otherwise = C.unlines $ map C.unwords $ spanGroup n (C.lines xs)


spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs = take n xs : spanGroup n (tail xs)


trim :: String -> String
trim = (dropWhile isSpace . reverse) . dropWhile isSpace . reverse


trim8 :: Text8 -> Text8
trim8 = (C.dropWhile isSpace . C.reverse) . C.dropWhile isSpace . C.reverse


unquotes :: String -> String
unquotes []   = []
unquotes [x]  = [x]
unquotes y@(x:xs)
    | x == '"' || x == '\'' =  if x == last xs then init xs
                                               else y
    | otherwise = y

