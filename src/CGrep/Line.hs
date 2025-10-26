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

module CGrep.Line (
    LineIndex,
    buildIndex,
    totalLines,
    lookupLineAndPosition,
    getLineByOffset',
    ------------------------
    getLineOffsets,
    getLineByOffset,
    lowerBound,
) where

import CGrep.Text (textSlice)
import Data.Bits
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector.Unboxed as UV

-- A LineIndex holds the original text and a vector of line start offsets.
data LineIndex
    = LineIndex
        T.Text
        (UV.Vector (Int))
    deriving stock (Show)

totalLines :: LineIndex -> Int
totalLines (LineIndex _ vec) = UV.length vec
{-# INLINE totalLines #-}

-- | Build a LineIndex from the given Text.
buildIndex :: T.Text -> LineIndex
buildIndex txt = LineIndex txt $ UV.fromList $ 0 : (map (+ 1) $ T.indices (T.singleton '\n') txt)
{-# INLINE buildIndex #-}

-- | Given a LineIndex and a 0-based offset, return the (1-based) line number and column number.
lookupLineAndPosition :: LineIndex -> Int -> (# Int, Int #)
lookupLineAndPosition (LineIndex _ vec) !queryOffset
    | UV.null vec = (# 1, queryOffset + 1 #) -- Edge case: indice vuoto
    | otherwise =
        -- 1. find the 0-based index of the line.
        let !lineIndex = findLineIndex vec queryOffset
         in if lineIndex < 0
                then (# 1, queryOffset + 1 #)
                else
                    let !lineStartOffset = vec `UV.unsafeIndex` lineIndex
                        !lineNum = lineIndex + 1 -- 1-based
                        !colNum = (queryOffset - lineStartOffset) + 1 -- 1-based
                     in (# lineNum, colNum #)

-- | Given a LineIndex and a 0-based offset, return the line Text.
getLineByOffset' :: LineIndex -> Int -> T.Text
getLineByOffset' (LineIndex originalText vec) !offset
    | UV.null vec = T.empty
    | otherwise =
        let !lineIndex = findLineIndex vec offset
         in if lineIndex < 0
                then T.empty
                else
                    let !offsetStart = vec `UV.unsafeIndex` lineIndex
                        !numLines = UV.length vec
                     in if lineIndex == numLines - 1
                            then
                                let !len = T.length originalText - offsetStart
                                 in textSlice originalText offsetStart len
                            else
                                let !offsetNext = vec `UV.unsafeIndex` (lineIndex + 1)
                                    !len = offsetNext - offsetStart - 1
                                 in textSlice originalText offsetStart len

-- Binary search to find the greatest index i such that vec[i] <= v
findLineIndex :: UV.Vector Int -> Int -> Int
findLineIndex vec v = findLineIndexGo vec v 0 (UV.length vec - 1)
{-# INLINE findLineIndex #-}

findLineIndexGo :: UV.Vector Int -> Int -> Int -> Int -> Int
findLineIndexGo vec v !left !right
    | left > right = right
    | otherwise = case v `compare` midValue of
        LT -> findLineIndexGo vec v left (mid - 1)
        EQ -> mid
        GT -> findLineIndexGo vec v (mid + 1) right
  where
    !mid = (left + right) `div` 2
    !midValue = vec `UV.unsafeIndex` mid

--------------------------------------------------------------------------------------------

--- >>> getLineOffsets "Hello ©\nWorld\nThis is a test\n"
-- [0,9,15,30]
getLineOffsets :: T.Text -> UV.Vector Int
getLineOffsets txt = UV.fromList $ 0 : (map (+ 1) $ T.indices (T.singleton '\n') txt)
{-# INLINE getLineOffsets #-}

getLineByOffset :: Int -> T.Text -> UV.Vector Int -> (# T.Text, Int #)
getLineByOffset off text vec = (# line, lb #)
  where
    !lb = lowerBound vec off
    !dropped = TU.dropWord8 lb text
    !line = T.takeWhile (/= '\n') dropped
{-# INLINE getLineByOffset #-}

lowerBound :: UV.Vector Int -> Int -> Int
lowerBound vec v
    | UV.null vec = 0 -- caso edge
    | otherwise = lowerBoundGo vec v 0 (UV.length vec - 1)
{-# INLINE lowerBound #-}

lowerBoundGo :: UV.Vector Int -> Int -> Int -> Int -> Int
lowerBoundGo vec v !left !right
    | left > right = vec `UV.unsafeIndex` right
    | midValue > v = lowerBoundGo vec v left (mid - 1)
    | midValue == v = midValue
    | otherwise = lowerBoundGo vec v (mid + 1) right
  where
    !mid = left + ((right - left) `shiftR` 1)
    !midValue = vec `UV.unsafeIndex` mid
