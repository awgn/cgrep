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

module CGrep.Line (
    getLineOffsets,
    getLineByOffset,
    lowerBound,
) where

import qualified Data.DList as DL
import Data.Int (Int64)
import qualified Data.Text as T
import Data.Text.Internal (Text (..))
import qualified Data.Text.Internal as TI
import qualified Data.Text.Internal.Search as T
import qualified Data.Text.Unsafe as TU
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as UV
import GHC.Stack (HasCallStack)

splitIdx :: (Char -> Bool) -> Text -> [(Int64, Text)]
splitIdx p t@(Text _ toff _)
    | T.null t = [(fromIntegral 0, T.empty)]
    | otherwise = loop t
  where
    loop :: Text -> [(Int64, Text)]
    loop s
        | T.null s' = [(fromIntegral (off-toff), l)]
        | otherwise = (fromIntegral (off-toff), l) : loop (TU.unsafeTail s')
      where
        (# l@(Text _ off _), s' #) = span_ (not . p) s

span_ :: (Char -> Bool) -> Text -> (# Text, Text #)
span_ p t@(Text arr off len) = (# hd, tl #)
  where
    hd = TI.text arr off k
    tl = TI.text arr (off + k) (len - k)
    !k = loop 0
    loop !i
        | i < len && p c = loop (i + d)
        | otherwise = i
      where
        TU.Iter c d = TU.iter t i
{-# INLINE span_ #-}


getLineOffsets :: T.Text -> UV.Vector Int
getLineOffsets txt = UV.fromList $ 0 : (map (+ 1) $ T.indices (T.singleton '\n') txt)
{-# INLINE getLineOffsets #-}

lowerBound :: UV.Vector Int -> Int -> Int
lowerBound vec v = lowerBoundGo vec v 0 (UV.length vec - 1)
{-# INLINE lowerBound #-}

getLineByOffset :: Int -> T.Text -> UV.Vector Int -> (# T.Text, Int #)
getLineByOffset off text vec = (# getFirstLine (T.drop (fromIntegral lb) text), lb #)
  where
    lb = lowerBound vec off
    getFirstLine bs = case T.lines bs of
        [] -> T.empty
        (line : _) -> line
{-# INLINE getLineByOffset #-}

lowerBoundGo :: UV.Vector Int -> Int -> Int -> Int -> Int
lowerBoundGo vec v !left !right
    | left > right = if right >= 0 then vec `UV.unsafeIndex` right else -1
    | otherwise = case v `compare` midValue of
        LT -> lowerBoundGo vec v left (mid - 1)
        EQ -> midValue
        _ -> lowerBoundGo vec v (mid + 1) right
  where
    mid = (left + right) `div` 2
    midValue = vec `UV.unsafeIndex` mid
