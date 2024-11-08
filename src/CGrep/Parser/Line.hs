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

module CGrep.Parser.Line (
    getLineOffsets,
    getAllLineOffsets,
    getLineByOffset,
    lowerBound,
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Types (LText8, Offset, Text8)
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Unsafe as BU
import Data.Int (Int64)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as UV

-- Returns a vector of offsets for a given character in a ByteString, up to the given maximum offset.
charOffsets :: Char -> Int64 -> C.ByteString -> UV.Vector Int64
charOffsets c maxOff bs = UV.unfoldrN (fromIntegral maxOff) (findOffsets bs maxOff) 0
  where
    findOffsets :: C.ByteString -> Int64 -> Int64 -> Maybe (Int64, Int64)
    findOffsets bs' maxOff' i
        | i >= maxOff' = Nothing
        | BU.unsafeIndex bs' (fromIntegral i) == c2w c = Just (fromIntegral i, i + 1)
        | otherwise = findOffsets bs' maxOff' (i + 1)

getLineOffsets :: Int64 -> Text8 -> UV.Vector Offset
getLineOffsets maxOff text =
    let idx = nlOffsets (fromIntegral maxOff) text
     in if UV.null idx
            then idx
            else
                if UV.last idx == fromIntegral (C.length text - 1)
                    then UV.init idx
                    else idx

{-# INLINE nlOffsets #-}
nlOffsets :: Int -> Text8 -> UV.Vector Int64
nlOffsets maxOff' bs' = UV.unfoldrN maxOff' (findOffsets maxOff' bs') (-1)

findOffsets :: Int -> Text8 -> Int -> Maybe (Int64, Int)
findOffsets max ts !i
    | i == -1 = Just (0, 0)
    | i >= max = Nothing
    | BU.unsafeIndex ts (fromIntegral i) == c2w '\n' = Just (fromIntegral i + 1, i + 1)
    | otherwise = findOffsets max ts (i + 1)

getAllLineOffsets :: Text8 -> UV.Vector Offset
getAllLineOffsets ts = getLineOffsets (fromIntegral $ C.length ts) ts
{-# INLINE getAllLineOffsets #-}

lowerBound :: UV.Vector Int64 -> Int64 -> Int64
lowerBound vec v = lowerBoundGo vec v 0 (UV.length vec - 1)

lowerBoundGo :: UV.Vector Int64 -> Int64 -> Int -> Int -> Int64
lowerBoundGo vec v !left !right
    | left > right = if right >= 0 then vec `UV.unsafeIndex` right else -1
    | otherwise = case v `compare` midValue of
        LT -> lowerBoundGo vec v left (mid - 1)
        EQ -> midValue
        _ -> lowerBoundGo vec v (mid + 1) right
  where
    mid = (left + right) `div` 2
    midValue = vec `UV.unsafeIndex` mid

getLineByOffset :: Offset -> Text8 -> UV.Vector Int64 -> (# Text8, Offset #)
getLineByOffset off text vec = (# (head . C.lines) (C.drop (fromIntegral lb) text), lb #)
  where
    lb = lowerBound vec off
{-# INLINE getLineByOffset #-}
