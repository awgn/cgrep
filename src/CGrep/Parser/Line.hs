{-# LANGUAGE FlexibleInstances #-}

module CGrep.Parser.Line (
      getLineOffsets
    , getAllLineOffsets
    , getLineByOffset
    , lowerBound
    ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import qualified Data.Vector.Unboxed as UV
import Data.Vector.Unboxed ((!))
import qualified Data.ByteString.Unsafe as BU
import Data.ByteString.Internal (c2w)
import CGrep.Types ( Offset, Text8, LText8 )
import Data.Int ( Int64 )


-- Returns a vector of offsets for a given character in a ByteString, up to the given maximum offset.
charOffsets :: Char -> Int64 -> C.ByteString -> UV.Vector Int64
charOffsets c maxOff bs = UV.unfoldrN (fromIntegral maxOff) findOffsets 0
  where
    !target = c2w c
    findOffsets :: Int64 -> Maybe (Int64, Int64)
    findOffsets i
      | i >= maxOff = Nothing
      | BU.unsafeIndex bs (fromIntegral i) == target = Just (fromIntegral i, i + 1)
      | otherwise = findOffsets (i + 1)


getLineOffsets :: Int64 -> Text8 -> UV.Vector Offset
getLineOffsets maxOff text =
    let l = C.length text
        idx = nlOffsets '\n' (fromIntegral maxOff) text
    in if UV.null idx
        then idx
        else if UV.last idx == fromIntegral (l-1)
            then UV.init idx
            else idx
    where nlOffsets :: Char -> Int -> Text8 -> UV.Vector Int64
          nlOffsets c maxOff bs = UV.unfoldrN maxOff findOffsets 0
            where
              !target = c2w c
              findOffsets :: Int -> Maybe (Int64, Int)
              findOffsets !i
                | i == 0 = Just (0, 1)
                | i >= maxOff = Nothing
                | BU.unsafeIndex bs (fromIntegral i) == target = Just (fromIntegral i + 1, i + 1)
                | otherwise = findOffsets (i + 1)


getAllLineOffsets :: Text8 -> UV.Vector Offset
getAllLineOffsets ts = getLineOffsets (fromIntegral $ C.length ts) ts
{-# INLINE getAllLineOffsets #-}


lowerBound :: UV.Vector Int64 -> Int64 -> Int64
lowerBound vec v = go 0 (UV.length vec-1)
  where
   go !left !right
      | left > right = if right >= 0 then vec `UV.unsafeIndex` right else -1
      | otherwise = case v `compare` midValue of
          LT -> go left (mid - 1)
          EQ -> midValue
          _  -> go (mid + 1) right
      where
        mid = (left + right) `div` 2
        midValue = vec `UV.unsafeIndex` mid


getLineByOffset :: Offset -> Text8 -> UV.Vector Int64 -> (# Text8, Offset #)
getLineByOffset off text vec = (# (head . C.lines) (C.drop (fromIntegral lb) text), lb #)
        where lb = lowerBound vec off
{-# INLINE getLineByOffset #-}
