--
-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE  ViewPatterns #-}
{-# LANGUAGE  OverloadedStrings #-}

module CGrep.Common ( Text8
                    , getTargetName
                    , getTargetContents
                    , stringSearch
                    , runSearch
                    , expandMultiline
                    , ignoreCase
                    , quickMatch
                    , trim
                    , trim8
                    , takeN) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Search as BM
import qualified Data.ByteString.Search.DFA as DFA

import Data.Char ( isSpace, isSpace, toLower )

import CGrep.Types ( Text8 )
import CGrep.Output ( Output, mkOutputElements )

import Options
    ( Options(Options, no_shallow, multiline, ignore_case) )
import Reader ( ReaderIO )
import Util ( spanGroup, notNull )
import Data.Int (Int64)
import System.IO.MMap ( mmapFileByteString )

import System.Posix.FilePath ( RawFilePath )

takeN :: Int -> String -> String
takeN n xs | length xs > n = take n xs <> "..."
          | otherwise     = xs
{-# INLINE takeN #-}


trim :: String -> String
trim = (dropWhile isSpace . reverse) . dropWhile isSpace . reverse
{-# INLINE trim #-}

trim8 :: Text8 -> Text8
trim8 = (C.dropWhile isSpace . C.reverse) . C.dropWhile isSpace . C.reverse
{-# INLINE trim8 #-}


getTargetName :: RawFilePath -> RawFilePath
getTargetName (C.null -> True )= "<STDIN>"
getTargetName name = name
{-# INLINE getTargetName #-}


getTargetContents :: RawFilePath -> IO Text8
getTargetContents (C.null -> True) = C.getContents
getTargetContents xs = mmapFileByteString (C.unpack xs) Nothing
{-# INLINE getTargetContents #-}


stringSearch :: [Text8] -> Text8 -> [[Int64]]
stringSearch ps text | maximum (C.length <$> ps) > 4 = ps >>= (\p -> [fromIntegral <$> p `BM.nonOverlappingIndices` text])
                     | otherwise = ps >>= (\p -> [fromIntegral <$> p `DFA.nonOverlappingIndices` text])
{-# INLINE stringSearch #-}


quickMatch :: [a] -> [[Int64]] -> Bool
quickMatch [_] = all notNull
quickMatch _   = any notNull
{-# INLINE quickMatch #-}

runSearch :: Options
          -> RawFilePath
          -> Bool
          -> ReaderIO [Output]
          -> ReaderIO [Output]
runSearch opt filename shallowTest doSearch =
    if shallowTest || no_shallow opt
        then doSearch
        else mkOutputElements filename C.empty C.empty []


expandMultiline :: Options -> Text8 -> Text8
expandMultiline Options { multiline = n } xs
    | n == 1 = xs
    | otherwise = C.unlines $ map C.unwords $ spanGroup n (C.lines xs)
{-# INLINE expandMultiline #-}


ignoreCase :: Options -> Text8 -> Text8
ignoreCase opt
    | ignore_case opt =  C.map toLower
    | otherwise = id
{-# INLINE ignoreCase #-}
