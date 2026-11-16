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

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module CGrep.Text (
    iterM,
    isAsciiText,
    textIndices,
    textIndicesRef,
    indicesFast,
    indicesFastAsciiCI,
    countNewlinesBetween,
    sliceLineRangeAt,
    textSlice,
    textOffsetWord8,
    textContainsOneOf,
    textContainsAsciiCI,
    textIsInfixOf,
    textEqCI,
    textIsPrefixOfCI,
    textIsSuffixOfCI,
    textIsInfixOfCI,
    firstIndex,
    lastIndex,
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
import Foreign.C.Types (CPtrdiff (..), CSize (..))
import GHC.Exts (ByteArray#)
import GHC.Word (Word8)

foreign import ccall unsafe "cgrep_count_newlines"
    c_count_newlines :: ByteArray# -> CSize -> CSize -> CSize

foreign import ccall unsafe "cgrep_find_ascii_ci"
    c_find_ascii_ci :: ByteArray# -> CSize -> CSize -> CSize -> ByteArray# -> CSize -> CSize -> CPtrdiff

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
textIndices ps text = (`indicesFast` text) <$> ps
{-# INLINE textIndices #-}

textIndicesRef :: [T.Text] -> T.Text -> [[Int]]
textIndicesRef ps text = (\p -> if T.null p then [] else p `TIS.indices` text) <$> ps
{-# INLINE textIndicesRef #-}

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
scanOne !c (TI.Text (A.ByteArray harr#) hoff hlen)
    | hlen <= 0 = Nothing
    | otherwise =
        case memchr harr# hoff hlen c of
            -1 -> Nothing
            rel -> Just rel
{-# INLINE scanOne #-}

scanAllOne :: Word8 -> T.Text -> [Int]
scanAllOne !c (TI.Text (A.ByteArray harr#) hoff hlen) = go hoff
  where
    !end = hoff + hlen
    go !cur
        | cur >= end = []
        | otherwise =
            case memchr harr# cur (end - cur) c of
                -1 -> []
                rel ->
                    let !hitAbs = cur + rel
                     in (hitAbs - hoff) : go (hitAbs + 1)
{-# INLINE scanAllOne #-}

indicesFast :: T.Text -> T.Text -> [Int]
indicesFast needle@(TI.Text narr noff nlen)
    | nlen == 1 = scanAllOne (A.unsafeIndex narr noff)
    | nlen <= 0 = const []
    | otherwise = indicesFast' needle
{-# INLINE indicesFast #-}

indicesFast' :: T.Text -> T.Text -> [Int]
indicesFast' (TI.Text narr noff nlen) (TI.Text harr@(A.ByteArray harr#) hoff hlen)
    | hlen < nlen = []
    | otherwise = loop (hoff + nlen)
  where
    !end = hoff + hlen
    !nlast = nlen - 1
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
        | i > end = []
        | A.unsafeIndex harr (i - 1) == z =
            if A.equal narr noff harr (i - nlen) nlen
                then (i - nlen - hoff) : loop (i + nlen)
                else loop (i + skip + 1)
        | i == end = []
        | mask .&. swizzle (A.unsafeIndex harr i) == 0 =
            loop (i + nlen + 1)
        | otherwise =
            case memchr harr# i (end - i) z of
                -1 -> []
                x -> loop (i + x + 1)
{-# INLINE indicesFast' #-}

word8ToInt :: Word8 -> Int
word8ToInt = fromIntegral
{-# INLINE word8ToInt #-}

asciiLowerByte :: Word8 -> Word8
asciiLowerByte !b = if (b - 0x41) <= 25 then b .|. 0x20 else b
{-# INLINE asciiLowerByte #-}

isAsciiText :: T.Text -> Bool
isAsciiText (TI.Text arr off len) = go off
  where
    !end = off + len
    go !i
        | i >= end = True
        | A.unsafeIndex arr i < 0x80 = go (i + 1)
        | otherwise = False
{-# INLINE isAsciiText #-}

-- | Zero-allocation substring check replacing @Data.Text.isInfixOf@ (which allocates
-- a 64-entry skip table via @Data.Text.Internal.Search.indices@ on every call).
textIsInfixOf :: T.Text -> T.Text -> Bool
textIsInfixOf needle@(TI.Text narr noff nlen) haystack@(TI.Text harr@(A.ByteArray harr#) hoff hlen)
    | nlen <= 0 = True
    | nlen > hlen = False
    | nlen == hlen = A.equal narr noff harr hoff nlen
    | nlen == 1 = memchr harr# hoff hlen (A.unsafeIndex narr noff) >= 0
    | hlen <= 64 =
        let !b0 = A.unsafeIndex narr noff
            !z = A.unsafeIndex narr (noff + nlen - 1)
            !lastStart = hoff + hlen - nlen
            scanShort !cur
                | cur > lastStart = False
                | otherwise =
                    case memchr harr# cur (lastStart - cur + 1) b0 of
                        -1 -> False
                        rel ->
                            let !pos = cur + rel
                             in ( A.unsafeIndex harr (pos + nlen - 1) == z
                                    && A.equal narr noff harr pos nlen
                                )
                                    || scanShort (pos + 1)
         in scanShort hoff
    | otherwise = isJust (firstIndex' needle haystack)
{-# INLINE textIsInfixOf #-}

-- | Compare a pre-lowercased pattern (@patLc@, with @isAsciiPat = isAsciiText patLc@)
-- against a candidate 'T.Text' under Unicode @T.toLower@ semantics without allocating
-- when both slices are ASCII.
textEqCI :: Bool -> T.Text -> T.Text -> Bool
textEqCI !isAsciiPat patLc@(TI.Text parr poff plen) cand@(TI.Text carr coff clen)
    | isAsciiPat && isAsciiText cand =
        plen == clen && eqAsciiLowerSlice parr poff carr coff plen
    | otherwise = patLc == T.toLower cand
{-# INLINE textEqCI #-}

textIsPrefixOfCI :: Bool -> T.Text -> T.Text -> Bool
textIsPrefixOfCI !isAsciiPat patLc@(TI.Text parr poff plen) cand@(TI.Text carr coff clen)
    | isAsciiPat && isAsciiText cand =
        plen <= clen && eqAsciiLowerSlice parr poff carr coff plen
    | otherwise = patLc `T.isPrefixOf` T.toLower cand
{-# INLINE textIsPrefixOfCI #-}

textIsSuffixOfCI :: Bool -> T.Text -> T.Text -> Bool
textIsSuffixOfCI !isAsciiPat patLc@(TI.Text parr poff plen) cand@(TI.Text carr coff clen)
    | isAsciiPat && isAsciiText cand =
        plen <= clen && eqAsciiLowerSlice parr poff carr (coff + clen - plen) plen
    | otherwise = patLc `T.isSuffixOf` T.toLower cand
{-# INLINE textIsSuffixOfCI #-}

textIsInfixOfCI :: Bool -> T.Text -> T.Text -> Bool
textIsInfixOfCI !isAsciiPat patLc@(TI.Text parr poff plen) cand@(TI.Text carr coff clen)
    | isAsciiPat && isAsciiText cand =
        if plen <= 0
            then True
            else
                if plen > clen
                    then False
                    else
                        let !b0 = A.unsafeIndex parr poff
                            !z = A.unsafeIndex parr (poff + plen - 1)
                            !lastStart = coff + clen - plen
                            go !pos
                                | pos > lastStart = False
                                | asciiLowerByte (A.unsafeIndex carr (pos + plen - 1)) == z
                                    && asciiLowerByte (A.unsafeIndex carr pos) == b0
                                    && eqAsciiLowerSlice parr poff carr pos plen =
                                    True
                                | otherwise = go (pos + 1)
                         in go coff
    | otherwise = patLc `textIsInfixOf` T.toLower cand
{-# INLINE textIsInfixOfCI #-}

eqAsciiLowerSlice :: A.Array -> Int -> A.Array -> Int -> Int -> Bool
eqAsciiLowerSlice !parr !poff !carr !coff !len = go 0
  where
    go !k
        | k >= len = True
        | A.unsafeIndex parr (poff + k) == asciiLowerByte (A.unsafeIndex carr (coff + k)) = go (k + 1)
        | otherwise = False
{-# INLINE eqAsciiLowerSlice #-}

-- | Zero-allocation check for whether a pre-lowercased ASCII needle @patLc@ occurs
-- anywhere in @haystack@ under @ignoreCaseText@ semantics (where multi-byte UTF-8
-- codepoints never fold to 1-byte ASCII).
textContainsAsciiCI :: T.Text -> T.Text -> Bool
textContainsAsciiCI (TI.Text parr poff plen) (TI.Text harr@(A.ByteArray harr#) hoff hlen)
    | plen <= 0 = False
    | plen > hlen = False
    | plen == 1 =
        let !b0 = A.unsafeIndex parr poff
         in if b0 >= 0x61 && b0 <= 0x7A
                then scanByteCI hoff (hoff + hlen) b0
                else memchr harr# hoff hlen b0 >= 0
    | otherwise =
        let !nlast = plen - 1
            !z = A.unsafeIndex parr (poff + nlast)
            !end = hoff + hlen
            buildTbl !i !msk !skp
                | i >= nlast = (msk .|. swzCI z) :* skp
                | otherwise =
                    let !c = A.unsafeIndex parr (poff + i)
                        !skp' = if c == z then plen - i - 2 else skp
                     in buildTbl (i + 1) (msk .|. swzCI c) skp'
            !(mask :* skip) = buildTbl 0 0 (plen - 2)
            swzCI :: Word8 -> Word64
            swzCI !k = 1 `unsafeShiftL` (word8ToInt (asciiLowerByte k) .&. 0x3f)
            loop !i
                | i > end = False
                | asciiLowerByte (A.unsafeIndex harr (i - 1)) == z =
                    if eqAsciiLowerSlice parr poff harr (i - plen) plen
                        then True
                        else loop (i + skip + 1)
                | i == end = False
                | mask .&. swzCI (A.unsafeIndex harr i) == 0 =
                    loop (i + plen + 1)
                | z < 0x61 || z > 0x7A =
                    case memchr harr# i (end - i) z of
                        -1 -> False
                        rel -> loop (i + rel + 1)
                | otherwise = loop (i + 1)
         in loop (hoff + plen)
  where
    scanByteCI !cur !end !b0
        | cur >= end = False
        | asciiLowerByte (A.unsafeIndex harr cur) == b0 = True
        | otherwise = scanByteCI (cur + 1) end b0

-- | Find the byte offset of the last (rightmost) occurrence of @needle@ in @haystack@
-- by scanning backwards from the end of @haystack@ with zero heap allocation.
lastIndex :: T.Text -> T.Text -> Maybe Int
lastIndex (TI.Text narr noff nlen) (TI.Text harr hoff hlen)
    | nlen <= 0 = Nothing
    | nlen > hlen = Nothing
    | nlen == 1 =
        let !b0 = A.unsafeIndex narr noff
            go1 !i
                | i < hoff = Nothing
                | A.unsafeIndex harr i == b0 = Just (i - hoff)
                | otherwise = go1 (i - 1)
         in go1 (hoff + hlen - 1)
    | otherwise =
        let !b0 = A.unsafeIndex narr noff
            !z = A.unsafeIndex narr (noff + nlen - 1)
            goN !i
                | i < hoff = Nothing
                | A.unsafeIndex harr (i + nlen - 1) == z
                    && A.unsafeIndex harr i == b0
                    && A.equal narr noff harr i nlen =
                    Just (i - hoff)
                | otherwise = goN (i - 1)
         in goN (hoff + hlen - nlen)
{-# INLINE lastIndex #-}

-- | Zero-allocation non-overlapping ASCII case-insensitive substring search for a
-- pre-lowercased ASCII @needle@ directly on @haystack@ (avoiding @ignoreCase@ full-file copy).
indicesFastAsciiCI :: T.Text -> T.Text -> [Int]
indicesFastAsciiCI (TI.Text (A.ByteArray narr#) noff nlen) (TI.Text (A.ByteArray harr#) hoff hlen)
    | nlen <= 0 || nlen > hlen = []
    | otherwise = go 0
  where
    !limit = hlen - nlen
    go !relStart
        | relStart > limit = []
        | otherwise =
            case c_find_ascii_ci
                harr#
                (fromIntegral hoff)
                (fromIntegral hlen)
                (fromIntegral relStart)
                narr#
                (fromIntegral noff)
                (fromIntegral nlen) of
                -1 -> []
                hitRel ->
                    let !h = fromIntegral hitRel :: Int
                     in h : go (h + nlen)
{-# INLINE indicesFastAsciiCI #-}

-- | Vectorized count of @'\n'@ bytes in @txt@ over relative byte range @[relStart .. relEnd)@.
countNewlinesBetween :: T.Text -> Int -> Int -> Int
countNewlinesBetween (TI.Text (A.ByteArray arr#) baseOff totalLen) !relStart !relEnd =
    let !s = max 0 relStart
        !e = min totalLen relEnd
     in if e <= s
            then 0
            else fromIntegral (c_count_newlines arr# (fromIntegral (baseOff + s)) (fromIntegral (e - s)))
{-# INLINE countNewlinesBetween #-}

-- | Find the 0-based relative @[lineStart .. lineEnd)@ byte bounds of the line containing
-- @relOff@ in @txt@ without building a @LineIndex@ vector.
sliceLineRangeAt :: T.Text -> Int -> (# Int, Int #)
sliceLineRangeAt (TI.Text arr@(A.ByteArray arr#) baseOff totalLen) !relOff
    | totalLen <= 0 = (# 0, 0 #)
    | otherwise =
        let !clamped = max 0 (min totalLen relOff)
            -- Note: if relOff points directly at a '\n' byte, that byte belongs to the line ending at relOff.
            !ls = findStart (baseOff + clamped - 1)
            !le =
                if clamped >= totalLen
                    then totalLen
                    else case memchr arr# (baseOff + clamped) (totalLen - clamped) (0x0A :: Word8) of
                        -1 -> totalLen
                        d -> clamped + d
         in (# ls, le #)
  where
    findStart !ix
        | ix < baseOff = 0
        | A.unsafeIndex arr ix == 0x0A = (ix + 1) - baseOff
        | otherwise = findStart (ix - 1)
{-# INLINE sliceLineRangeAt #-}
