--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.Common (
    Text8,
    getTargetName,
    getTargetContents,
    expandMultiline,
    ignoreCase,
    subText,
    trim,
    trim8,
    takeN,
) where

import CGrep.Parser.Char (isSpace)
import CGrep.Types (Offset, Text8)
import Data.Char (toLower)

import Options (
    Options (Options, ignore_case, multiline, no_shallow),
 )

import Data.Int (Int64)
import System.IO.MMap (mmapFileByteString)
import Util (spanGroup)

import Data.List (group, groupBy, sort, sortOn)
import qualified Data.Vector.Unboxed as UV
import System.OsPath
import qualified System.OsString as OS

import GHC.Exts (groupWith)

import qualified Data.ByteString.Char8 as C

takeN :: Int -> String -> String
takeN n xs
    | length xs > n = take n xs <> "..."
    | otherwise = xs
{-# INLINE takeN #-}

trim :: String -> String
trim = (dropWhile isSpace . reverse) . dropWhile isSpace . reverse
{-# INLINE trim #-}

trim8 :: Text8 -> Text8
trim8 = (C.dropWhile isSpace . C.reverse) . C.dropWhile isSpace . C.reverse
{-# INLINE trim8 #-}

getTargetName :: OsPath -> OsPath
getTargetName (OS.null -> True) = unsafeEncodeUtf "<STDIN>"
getTargetName name = name
{-# INLINE getTargetName #-}

getTargetContents :: OsPath -> IO Text8
getTargetContents (OS.null -> True) = C.getContents
getTargetContents xs = decodeUtf xs >>= \fp -> mmapFileByteString fp Nothing
{-# INLINE getTargetContents #-}

expandMultiline :: Options -> Text8 -> Text8
expandMultiline Options{multiline = n} xs
    | n == 1 = xs
    | otherwise = C.unlines $ map C.unwords $ spanGroup n (C.lines xs)
{-# INLINE expandMultiline #-}

ignoreCase :: Options -> Text8 -> Text8
ignoreCase opt
    | ignore_case opt = C.map toLower
    | otherwise = id
{-# INLINE ignoreCase #-}

subText :: [[Offset]] -> Text8 -> Text8
subText [] txt = txt
subText indices txt = case C.elemIndex '\n' (C.drop maxOff txt) of
    Nothing -> txt
    (Just n) -> C.take (maxOff + n) txt
  where
    maxOff = fromIntegral $ maximum (lastDef 0 <$> indices)
    lastDef def xs = if null xs then def else last xs
{-# INLINE subText #-}
