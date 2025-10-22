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
    runSearch,
    eligibleForSearch,
    getTargetName,
    getTargetContents,
    expandMultiline,
    ignoreCase,
    subText,
    trim,
    trimT,
    takeN,
)
where

import CGrep.Parser.Char (isSpace)
import Data.Char (toLower)
import Data.Int (Int64)
import Data.List (group, groupBy, sort, sortOn)
import qualified Data.Vector.Unboxed as UV
import GHC.Exts (groupWith)
import Options (
    Options (Options, ignore_case, multiline, no_shallow),
 )
import System.OsPath
import qualified System.OsString as OS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Util (spanGroup)
import Data.List.Extra (notNull)
import CGrep.Line (LineIndex)
import Reader (ReaderIO)
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Chunk (Chunk)

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

expandMultiline :: Options -> T.Text -> T.Text
expandMultiline Options{multiline = n} xs
    | n == 1 = xs
    | otherwise = T.unlines $ map T.unwords $ spanGroup n (T.lines xs)
{-# INLINE expandMultiline #-}

ignoreCase :: Options -> T.Text -> T.Text
ignoreCase opt
    | ignore_case opt = T.map toLower
    | otherwise = id
{-# INLINE ignoreCase #-}

subText :: [[Int]] -> T.Text -> T.Text
subText [] txt = txt
subText indices txt = case T.findIndex (== '\n') (T.drop maxOff txt) of
    Nothing -> txt
    (Just n) -> T.take (maxOff + n) txt
  where
    maxOff = fromIntegral $ maximum (lastDef 0 <$> indices)
    lastDef def xs = if null xs then def else last xs
{-# INLINE subText #-}

eligibleForSearch :: [a] -> [[Int]] -> Bool
eligibleForSearch [_] = all notNull
eligibleForSearch _ = any notNull
{-# INLINE eligibleForSearch #-}


runSearch ::
    Options ->
    LineIndex ->
    OsPath ->
    Bool ->
    ReaderIO [Match] ->
    ReaderIO [Match]
runSearch opt lindex filename eligible doSearch =
    if eligible || no_shallow opt
        then doSearch
        else mkMatches lindex filename T.empty ([] :: [Chunk])
