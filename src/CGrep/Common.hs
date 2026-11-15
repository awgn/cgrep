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
    ignoreCaseRef,
    textContainsOneOfOpt,
    sliceToMaxIndex,
    sliceToLastMatch,
    trim,
    trimT,
    takeN,
)
where

import CGrep.Line (LineIndex)
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Char (isSpace)
import CGrep.Parser.Chunk (Chunk)
import CGrep.Text (charUtf8Length, firstIndex, isAsciiText, lastIndex, textContainsAsciiCI, textContainsOneOf)
import Control.Monad.ST (ST, runST)
import Data.Bits (unsafeShiftR, (.&.), (.|.))
import Data.Char (ord, toLower)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.IO.Utf8 as TIO
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import Options (Options (..))
import Reader (ReaderIO)
import System.OsPath
import qualified System.OsString as OS

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

toLowerSameWidth :: Char -> Char
toLowerSameWidth c =
    let !lc = toLower c
     in if charUtf8Length lc == charUtf8Length c then lc else c
{-# INLINE toLowerSameWidth #-}

ignoreCase :: Options -> T.Text -> T.Text
ignoreCase opt
    | ignore_case opt = ignoreCaseText
    | otherwise = id
{-# INLINE ignoreCase #-}

ignoreCaseRef :: Options -> T.Text -> T.Text
ignoreCaseRef opt
    | ignore_case opt = T.map toLowerSameWidth
    | otherwise = id
{-# INLINE ignoreCaseRef #-}

ignoreCaseText :: T.Text -> T.Text
ignoreCaseText txt@(TI.Text srcArr srcOff byteLen)
    | byteLen <= 0 = T.empty
    | otherwise =
        case findFirstChange srcOff of
            -1 -> txt
            firstIx ->
                let outArr = runST $ do
                        marr <- A.new byteLen
                        A.copyI byteLen marr 0 srcArr srcOff
                        lowerLoop marr firstIx
                        A.unsafeFreeze marr
                 in TI.Text outArr 0 byteLen
  where
    !endOff = srcOff + byteLen

    findFirstChange :: Int -> Int
    findFirstChange !ix
        | ix >= endOff = -1
        | otherwise =
            let !b = A.unsafeIndex srcArr ix
             in if b < 0x80
                    then
                        if (b - 0x41) <= 25
                            then ix
                            else findFirstChange (ix + 1)
                    else
                        let TU.Iter !c !delta = TU.iter txt (ix - srcOff)
                            !lc = toLower c
                         in if lc /= c && charUtf8Length lc == delta
                                then ix
                                else findFirstChange (ix + delta)

    lowerLoop :: forall s. A.MArray s -> Int -> ST s ()
    lowerLoop !marr !ix
        | ix >= endOff = pure ()
        | otherwise =
            let !b = A.unsafeIndex srcArr ix
             in if b < 0x80
                    then do
                        if (b - 0x41) <= 25
                            then A.unsafeWrite marr (ix - srcOff) (b .|. 0x20)
                            else pure ()
                        lowerLoop marr (ix + 1)
                    else do
                        let !relIx = ix - srcOff
                            TU.Iter !c !delta = TU.iter txt relIx
                            !lc = toLower c
                        if lc /= c && charUtf8Length lc == delta
                            then writeUtf8Multi marr relIx delta (ord lc)
                            else pure ()
                        lowerLoop marr (ix + delta)

    writeUtf8Multi :: forall s. A.MArray s -> Int -> Int -> Int -> ST s ()
    writeUtf8Multi !marr !dst !delta !cp =
        case delta of
            2 -> do
                A.unsafeWrite marr dst (fromIntegral (0xC0 .|. (cp `unsafeShiftR` 6)))
                A.unsafeWrite marr (dst + 1) (fromIntegral (0x80 .|. (cp .&. 0x3F)))
            3 -> do
                A.unsafeWrite marr dst (fromIntegral (0xE0 .|. (cp `unsafeShiftR` 12)))
                A.unsafeWrite marr (dst + 1) (fromIntegral (0x80 .|. ((cp `unsafeShiftR` 6) .&. 0x3F)))
                A.unsafeWrite marr (dst + 2) (fromIntegral (0x80 .|. (cp .&. 0x3F)))
            _ -> do
                A.unsafeWrite marr dst (fromIntegral (0xF0 .|. (cp `unsafeShiftR` 18)))
                A.unsafeWrite marr (dst + 1) (fromIntegral (0x80 .|. ((cp `unsafeShiftR` 12) .&. 0x3F)))
                A.unsafeWrite marr (dst + 2) (fromIntegral (0x80 .|. ((cp `unsafeShiftR` 6) .&. 0x3F)))
                A.unsafeWrite marr (dst + 3) (fromIntegral (0x80 .|. (cp .&. 0x3F)))

-- | Shallow pre-filter equivalent to @textContainsOneOf (ignoreCase opt <$> ps) (ignoreCase opt txt)@
-- without allocating a lowercased copy of @txt@ when patterns are ASCII.
textContainsOneOfOpt :: Options -> [T.Text] -> T.Text -> Bool
textContainsOneOfOpt _ [] _ = True
textContainsOneOfOpt opt ps txt
    | not (ignore_case opt) = textContainsOneOf ps txt
    | otherwise =
        let psLc = map ignoreCaseText ps
         in if all isAsciiText psLc
                then any (`textContainsAsciiCI` txt) psLc
                else textContainsOneOf psLc (ignoreCaseText txt)
{-# INLINE textContainsOneOfOpt #-}

sliceToMaxIndex
 :: [[Int]] -> T.Text -> T.Text
sliceToMaxIndex
 [] txt = txt
sliceToMaxIndex
 indices txt = case firstIndex (T.singleton '\n') (TU.dropWord8 maxOff txt) of
    Nothing -> txt
    (Just n) -> TU.takeWord8 (maxOff + n) txt
  where
    maxOff = maximum (lastDef 0 <$> indices)
    lastDef def xs = if null xs then def else last xs
{-# INLINE sliceToMaxIndex #-}

-- | Slice @txt@ up to the newline following the rightmost occurrence of any pattern in @ps@,
-- scanning backwards from the end of @txt@ with zero intermediate list allocation.
sliceToLastMatch :: [T.Text] -> T.Text -> T.Text
sliceToLastMatch [] txt = txt
sliceToLastMatch ps txt =
    let !maxOff =
            foldl'
                ( \ !acc p ->
                    case lastIndex p txt of
                        Just o ->
                            let !endPos = o + max 0 (TU.lengthWord8 p - 1)
                             in if endPos > acc then endPos else acc
                        Nothing -> acc
                )
                (-1)
                ps
     in if maxOff < 0
            then T.empty
            else case firstIndex (T.singleton '\n') (TU.dropWord8 maxOff txt) of
                Nothing -> txt
                Just n -> TU.takeWord8 (maxOff + n) txt
{-# INLINE sliceToLastMatch #-}

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
