--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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
    runSearch,
    getTargetName,
    getTargetContents,
    ignoreCase,
    subText,
    trim,
    trimT,
    takeN,
)
where

import CGrep.Line (LineIndex)
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Char (isSpace)
import CGrep.Parser.Chunk (Chunk)
import qualified Data.Text as T
import qualified Data.Text.IO.Utf8 as TIO
import qualified Data.Text.Unsafe as TU
import Reader (ReaderIO)
import System.OsPath
import qualified System.OsString as OS
import Options (Options (..))

takeN :: Int -> String -> String
takeN n xs
    | length xs > n = take n xs <> "..."
    | otherwise = xs
{-# INLINE takeN #-}

trim :: String -> String
trim = (dropWhile isSpace . reverse) . dropWhile isSpace . reverse
{-# INLINE trim #-}

trimT :: T.Text -> T.Text
trimT = T.stripEnd . T.stripStart

getTargetName :: OsPath -> OsPath
getTargetName (OS.null -> True) = unsafeEncodeUtf "<STDIN>"
getTargetName name = name
{-# INLINE getTargetName #-}

getTargetContents :: OsPath -> IO T.Text
getTargetContents (OS.null -> True) = TIO.getContents
getTargetContents xs = decodeUtf xs >>= TIO.readFile
{-# INLINE getTargetContents #-}

ignoreCase :: Options -> T.Text -> T.Text
ignoreCase opt
    | ignore_case opt = T.toLower
    | otherwise = id
{-# INLINE ignoreCase #-}

subText :: [[Int]] -> T.Text -> T.Text
subText [] txt = txt
subText indices txt = case T.findIndex (== '\n') (TU.dropWord8 maxOff txt) of
    Nothing -> txt
    (Just n) -> TU.takeWord8 (maxOff + n) txt
  where
    maxOff = maximum (lastDef 0 <$> indices)
    lastDef def xs = if null xs then def else last xs
{-# INLINE subText #-}

runSearch ::
    LineIndex ->
    OsPath ->
    Bool ->
    ReaderIO [Match] ->
    ReaderIO [Match]
runSearch lindex filename eligible doSearch =
    if eligible
        then doSearch
        else mkMatches lindex filename T.empty ([] :: [Chunk])
