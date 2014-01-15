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
                     expandMultiline, 
                     ignoreCase, 
                     spanGroup) where
 
import qualified Data.ByteString.Char8 as C

import Data.Char

import CGrep.Types
import Options


type CgrepFunction = Options -> [Text8] -> Maybe FilePath -> IO [Output] 


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
 

spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs = take n xs : spanGroup n (tail xs)


