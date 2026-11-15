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
    buildIndexRef,
    lineOffsetsOf,
    totalLines,
    lookupLineAndPosition,
    getLineByOffset',
    getLineByNumber,
    sliceLineAt,
    ------------------------
    getLineOffsets,
    getLineOffsetsRef,
    getLineByOffset,
    lowerBound,
) where

import CGrep.Text (textSlice)
import Control.Monad.ST (runST)
import Data.Bits (unsafeShiftL, unsafeShiftR)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import Data.Text.Internal.ArrayUtils (memchr)
import qualified Data.Text.Internal.Search as TIS
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Word (Word8)

newlineByte :: Word8
newlineByte = 10

-- A LineIndex holds the original text and a (lazily evaluated) vector of 0-based line start Word8 offsets.
data LineIndex
    = LineIndex
        T.Text
        (UV.Vector Int)
    deriving stock (Show)

lineOffsetsOf :: LineIndex -> UV.Vector Int
lineOffsetsOf (LineIndex _ vec) = vec
{-# INLINE lineOffsetsOf #-}

totalLines :: LineIndex -> Int
totalLines (LineIndex txt vec)
    | T.null txt = 0
    | UV.null vec = 0
    | UV.last vec == TU.lengthWord8 txt = UV.length vec - 1
    | otherwise = UV.length vec
{-# INLINE totalLines #-}

getLineByNumber :: LineIndex -> Int -> T.Text
getLineByNumber lindex@(LineIndex _ vec) !lineNum
    | lineNum < 1 || lineNum > UV.length vec = T.empty
    | otherwise = getLineByOffset' lindex (vec `UV.unsafeIndex` (lineNum - 1))
{-# INLINE getLineByNumber #-}

-- | Build a LineIndex from the given Text using SIMD memchr directly into an unboxed Vector.
buildIndex :: T.Text -> LineIndex
buildIndex txt = LineIndex txt (getLineOffsets txt)
{-# INLINE buildIndex #-}

-- | Reference LineIndex builder retained for differential testing.
buildIndexRef :: T.Text -> LineIndex
buildIndexRef txt = LineIndex txt (getLineOffsetsRef txt)
{-# INLINE buildIndexRef #-}

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

-- | Slice the line containing @relOffset@ from @txt@ in O(log L) using precomputed @vec@ offsets.
sliceLineAt :: UV.Vector Int -> T.Text -> Int -> T.Text
sliceLineAt vec txt !relOffset
    | UV.null vec = T.empty
    | otherwise =
        let !lineIndex = findLineIndex vec relOffset
         in if lineIndex < 0
                then T.empty
                else
                    let !offsetStart = vec `UV.unsafeIndex` lineIndex
                        !numLines = UV.length vec
                     in if lineIndex == numLines - 1
                            then
                                let !len = TU.lengthWord8 txt - offsetStart
                                 in textSlice txt offsetStart len
                            else
                                let !offsetNext = vec `UV.unsafeIndex` (lineIndex + 1)
                                    !len = offsetNext - offsetStart - 1
                                 in textSlice txt offsetStart len
{-# INLINE sliceLineAt #-}

-- | Given a LineIndex and a 0-based offset, return the line Text.
getLineByOffset' :: LineIndex -> Int -> T.Text
getLineByOffset' (LineIndex originalText vec) !offset =
    sliceLineAt vec originalText offset
{-# INLINE getLineByOffset' #-}

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
    !mid = left + ((right - left) `unsafeShiftR` 1)
    !midValue = vec `UV.unsafeIndex` mid

--------------------------------------------------------------------------------------------

--- >>> getLineOffsets "Hello ©\nWorld\nThis is a test\n"
-- [0,9,15,30]
getLineOffsets :: T.Text -> UV.Vector Int
getLineOffsets (TI.Text (A.ByteArray arr#) srcOff byteLen)
    | byteLen <= 0 = UV.singleton 0
    | otherwise = runST $ do
        let !endOff = srcOff + byteLen
            !initCap = max 8 ((byteLen `unsafeShiftR` 5) + 4)
        mvec0 <- UMV.unsafeNew initCap
        UMV.unsafeWrite mvec0 0 0
        let loop !scanIx !cnt !cap !mvec
                | scanIx >= endOff = UV.unsafeFreeze (UMV.unsafeSlice 0 cnt mvec)
                | otherwise =
                    case memchr arr# scanIx (endOff - scanIx) newlineByte of
                        -1 -> UV.unsafeFreeze (UMV.unsafeSlice 0 cnt mvec)
                        relNl -> do
                            let !nextLineAbs = scanIx + relNl + 1
                                !relLineStart = nextLineAbs - srcOff
                            if cnt < cap
                                then do
                                    UMV.unsafeWrite mvec cnt relLineStart
                                    loop nextLineAbs (cnt + 1) cap mvec
                                else do
                                    let !newCap = cap `unsafeShiftL` 1
                                    mvec' <- UMV.unsafeGrow mvec cap
                                    UMV.unsafeWrite mvec' cnt relLineStart
                                    loop nextLineAbs (cnt + 1) newCap mvec'
        loop srcOff 1 initCap mvec0

getLineOffsetsRef :: T.Text -> UV.Vector Int
getLineOffsetsRef txt = UV.fromList $ 0 : (map (+ 1) $ TIS.indices (T.singleton '\n') txt)
{-# INLINE getLineOffsetsRef #-}

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
    !mid = left + ((right - left) `unsafeShiftR` 1)
    !midValue = vec `UV.unsafeIndex` mid
