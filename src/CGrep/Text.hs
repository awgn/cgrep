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

module CGrep.Text (
    iterM,
    textIndices,
    textSlice,
    textOffsetWord8,
    textContainsOneOf,
    firstIndex,
    charUtf8Length,
    blankByWidth,
) where

import Data.Bits (unsafeShiftL, (.&.), (.|.))
import Data.Char (ord)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import Data.Text.Internal.ArrayUtils (memchr)
import qualified Data.Text.Internal.Search as TIS
import qualified Data.Text.Unsafe as TU
import Data.Word (Word64)
import GHC.Word (Word8)

iterM :: (Monad m) => T.Text -> ((# Char, Int, Int #) -> m ()) -> m ()
iterM txt f = go 0 (return ())
  where
    !len = TU.lengthWord8 txt
    go !off !cont
        | off >= len = cont
        | otherwise =
            let TU.Iter !c !delta = TU.iter txt off
             in f (# c, off, delta #) >> go (off + delta) cont
{-# INLINE iterM #-}

textIndices :: [T.Text] -> T.Text -> [[Int]]
textIndices ps text = (`TIS.indices` text) <$> ps
{-# INLINE textIndices #-}

textContainsOneOf :: [T.Text] -> T.Text -> Bool
textContainsOneOf [] _ = True
textContainsOneOf ps text = any isJust ((`firstIndex` text) <$> ps)
{-# INLINE textContainsOneOf #-}

textSlice :: T.Text -> Int -> Int -> T.Text
textSlice txt start len = TU.takeWord8 len $ TU.dropWord8 start txt
{-# INLINE textSlice #-}

textOffsetWord8 :: T.Text -> Int
textOffsetWord8 (TI.Text _ off _) = off
{-# INLINE textOffsetWord8 #-}

charUtf8Length :: Char -> Int
charUtf8Length c
    | n <= 0x7F = 1 -- 0-127
    | n <= 0x7FF = 2 -- 128-2047
    | n <= 0xFFFF = 3 -- 2048-65535
    | otherwise = 4 -- 65536-1114111 (max Unicode code point)
  where
    -- Get the integer Unicode code point from the Char
    n = ord c
{-# INLINE charUtf8Length #-}

blankByWidth :: Char -> Char
blankByWidth c
    | n <= 0x7F = '\x0020' -- SPACE
    | n <= 0x7FF = '\x00A0' -- NO-BREAK SPACE
    | n <= 0xFFFF = '\x3000' -- IDEOGRAPHIC SPACE
    | otherwise = '\x100000' -- (Placeholder PUA)
  where
    -- Get the integer Unicode code point from the Char
    n = ord c
{-# INLINE blankByWidth #-}

data T = {-# UNPACK #-} !Word64 :* {-# UNPACK #-} !Int

{- | /O(n+m)/ Find the offsets of all non-overlapping indices of
@needle@ within @haystack@.

In (unlikely) bad cases, this algorithm's complexity degrades
towards /O(n*m)/.
-}
firstIndex ::
    -- | Substring to search for (@needle@)
    T.Text ->
    -- | Text to search in (@haystack@)
    T.Text ->
    Maybe Int
firstIndex needle@(TI.Text narr noff nlen)
    | nlen == 1 = scanOne (A.unsafeIndex narr noff)
    | nlen <= 0 = const Nothing
    | otherwise = firstIndex' needle
{-# INLINE firstIndex #-}

-- | nlen must be >= 2, otherwise nindex causes access violation
firstIndex' :: T.Text -> T.Text -> Maybe Int
firstIndex' (TI.Text narr noff nlen) (TI.Text harr@(A.ByteArray harr#) hoff hlen) = loop (hoff + nlen)
  where
    nlast = nlen - 1
    !z = nindex nlast
    nindex k = A.unsafeIndex narr (noff + k)
    buildTable !i !msk !skp
        | i >= nlast = (msk .|. swizzle z) :* skp
        | otherwise = buildTable (i + 1) (msk .|. swizzle c) skp'
      where
        !c = nindex i
        skp'
            | c == z = nlen - i - 2
            | otherwise = skp
    !(mask :* skip) = buildTable 0 0 (nlen - 2)

    swizzle :: Word8 -> Word64
    swizzle !k = 1 `unsafeShiftL` (word8ToInt k .&. 0x3f)

    loop !i
        | i > hlen + hoff =
            Nothing
        | A.unsafeIndex harr (i - 1) == z =
            if A.equal narr noff harr (i - nlen) nlen
                then Just $ i - nlen - hoff
                else loop (i + skip + 1)
        | i == hlen + hoff =
            Nothing
        | mask .&. swizzle (A.unsafeIndex harr i) == 0 =
            loop (i + nlen + 1)
        | otherwise =
            case memchr harr# i (hlen + hoff - i) z of
                -1 -> Nothing
                x -> loop (i + x + 1)
{-# INLINE firstIndex' #-}

scanOne :: Word8 -> T.Text -> Maybe Int
scanOne c (TI.Text harr hoff hlen) = loop 0
  where
    loop !i
        | i >= hlen = Nothing
        | A.unsafeIndex harr (hoff + i) == c = Just i
        | otherwise = loop (i + 1)
{-# INLINE scanOne #-}

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
