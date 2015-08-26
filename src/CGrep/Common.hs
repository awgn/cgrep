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

module CGrep.Common (SearchFunction, Text8,
                     getTargetName,
                     getTargetContents,
                     quickSearch,
                     expandMultiline,
                     ignoreCase,
                     trim, trim8) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Search as SC
import Data.Char

import CGrep.Types
import CGrep.Output

import Options
import Util


type SearchFunction = Options -> [Text8] -> FilePath -> IO [Output]


trim :: String -> String
trim = (dropWhile isSpace . reverse) . dropWhile isSpace . reverse


trim8 :: Text8 -> Text8
trim8 = (C.dropWhile isSpace . C.reverse) . C.dropWhile isSpace . C.reverse


getTargetName :: FilePath -> String
getTargetName [] = "<STDIN>"
getTargetName name = name


getTargetContents :: FilePath -> IO Text8
getTargetContents [] = C.getContents
getTargetContents xs = C.readFile xs


quickSearch :: Options -> [Text8] -> Text8 -> Maybe Bool
quickSearch opt ps text
    | no_turbo opt        = Nothing
    | otherwise           = Just $ any has_pattern ps || null ps
    where has_pattern pat = notNull $ pat `SC.nonOverlappingIndices` text


expandMultiline :: Options -> Text8 -> Text8
expandMultiline Options { multiline = n } xs
    | n == 1 = xs
    | otherwise = C.unlines $ map C.unwords $ spanGroup n (C.lines xs)


ignoreCase :: Options -> Text8 -> Text8
ignoreCase Options { ignore_case = icase }
    | icase  =  C.map toLowercase
    | otherwise = id

