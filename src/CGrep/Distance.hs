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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module CGrep.Distance (
    distance,
    distanceRef,
    (~==),
    approxEqRef,
    textApproxEq,
    EditPattern,
    mkEditPattern,
    matchEditPattern,
) where

import Control.Monad.ST (runST)
import Data.Bits (Bits (complement, unsafeShiftL, xor, (.&.), (.|.)))
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Word (Word64)

-- | Maximum inclusive edit distance allowed by cgrep's approximate matching rule
-- for @len = min (length a) (length b)@:
--   * @len < 5@  -> @dist < 3@ (@dist <= 2@)
--   * @len >= 5@ -> @dist < (len * 40 `div` 100)@ (@dist <= (len * 40 `quot` 100) - 1@)
allowedMaxDist :: Int -> Int
allowedMaxDist !len
    | len < 5 = 2
    | otherwise = ((len * 40) `quot` 100) - 1
{-# INLINE allowedMaxDist #-}

min3 :: Int -> Int -> Int -> Int
min3 !x !y !z = if x < y then min x z else min y z
{-# INLINE min3 #-}

isAsciiSlice :: A.Array -> Int -> Int -> Bool
isAsciiSlice !arr !off !len = go off
  where
    !end = off + len
    go !i
        | i >= end = True
        | A.unsafeIndex arr i < 0x80 = go (i + 1)
        | otherwise = False
{-# INLINE isAsciiSlice #-}

-- | Precompiled pattern for O(N * ceil(M/64)) zero-allocation Myers bit-parallel
-- approximate matching on ASCII slices, with automatic Unicode/long-pattern fallback.
data EditPattern = EditPattern
    { epText :: !T.Text
    , epCharLen :: {-# UNPACK #-} !Int
    , epMinCharLen :: {-# UNPACK #-} !Int
    , epMaxCharLen :: {-# UNPACK #-} !Int
    , epMaxBytes :: {-# UNPACK #-} !Int
    , epIsFastMyers :: !Bool
    , epHighBit :: {-# UNPACK #-} !Word64
    , epPeq :: !(UV.Vector Word64)
    , epChars :: !(UV.Vector Char)
    }

mkEditPattern :: T.Text -> EditPattern
mkEditPattern pat@(TI.Text patArr patOff patByteLen) =
    let !ascii = isAsciiSlice patArr patOff patByteLen
        !m = if ascii then patByteLen else T.length pat
        !maxCharLen = m + allowedMaxDist m
        !minCharLen = findMinLen 0 m
        !maxBytes = maxCharLen `unsafeShiftL` 2
        !fastMyers = ascii && m >= 1 && m <= 64
        !highBit = if fastMyers then (1 :: Word64) `unsafeShiftL` (m - 1) else 0
        !peq = if fastMyers then buildPeq patArr patOff m else UV.empty
        !chars = UV.fromListN m (T.unpack pat)
     in EditPattern
            { epText = pat
            , epCharLen = m
            , epMinCharLen = minCharLen
            , epMaxCharLen = maxCharLen
            , epMaxBytes = maxBytes
            , epIsFastMyers = fastMyers
            , epHighBit = highBit
            , epPeq = peq
            , epChars = chars
            }
  where
    findMinLen !cand !m
        | cand >= m = m
        | m - cand <= allowedMaxDist cand = cand
        | otherwise = findMinLen (cand + 1) m

    buildPeq :: A.Array -> Int -> Int -> UV.Vector Word64
    buildPeq !arr !off !m = runST $ do
        v <- UMV.replicate 128 0
        let go !i
                | i >= m = UV.unsafeFreeze v
                | otherwise = do
                    let !b = fromIntegral (A.unsafeIndex arr (off + i)) :: Int
                        !bit = (1 :: Word64) `unsafeShiftL` i
                    old <- UMV.unsafeRead v b
                    UMV.unsafeWrite v b (old .|. bit)
                    go (i + 1)
        go 0

matchEditPattern :: EditPattern -> T.Text -> Bool
matchEditPattern EditPattern{..} candidate@(TI.Text barr boff blen)
    | blen < epMinCharLen || blen > epMaxBytes = False
    | epIsFastMyers && isAsciiSlice barr boff blen =
        if blen > epMaxCharLen
            then False
            else
                let !maxDist = allowedMaxDist (min epCharLen blen)
                    !(TI.Text patArr patOff _) = epText
                 in if blen == epCharLen && A.equal patArr patOff barr boff blen
                        then True
                        else myersBoundedAscii epPeq epCharLen epHighBit maxDist barr boff blen
    | otherwise = matchUnicodeFallback epChars epCharLen candidate
{-# INLINE matchEditPattern #-}

-- | Myers (1999) bit-parallel global Levenshtein distance (with Hyyro's global boundary
-- shift @phShift = (ph << 1) | 1@) operating strictly in 64-bit CPU registers with zero
-- heap allocation and early termination when remaining characters cannot reach @maxDist@.
myersBoundedAscii :: UV.Vector Word64 -> Int -> Word64 -> Int -> A.Array -> Int -> Int -> Bool
myersBoundedAscii !peq !m !highBit !maxDist !barr !boff !blen
    | blen == 0 = m <= maxDist
    | otherwise = loop 0 (complement 0) 0 m
  where
    loop :: Int -> Word64 -> Word64 -> Int -> Bool
    loop !idx !pv !mv !score
        | idx >= blen = score <= maxDist
        | otherwise =
            let !b = fromIntegral (A.unsafeIndex barr (boff + idx)) :: Int
                !eq = peq `UV.unsafeIndex` b
                !xv = eq .|. mv
                !xh = (((eq .&. pv) + pv) `xor` pv) .|. eq
                !ph = mv .|. complement (xh .|. pv)
                !mh = pv .&. xh
                !inc = if (ph .&. highBit) /= 0 then 1 else 0
                !dec = if (mh .&. highBit) /= 0 then 1 else 0
                !score' = score + inc - dec
                !remChars = blen - 1 - idx
             in if score' - remChars > maxDist
                    then False
                    else
                        let !phShift = (ph `unsafeShiftL` 1) .|. 1
                            !mhShift = mh `unsafeShiftL` 1
                            !pv' = mhShift .|. complement (xv .|. phShift)
                            !mv' = phShift .&. xv
                         in loop (idx + 1) pv' mv' score'
{-# INLINE myersBoundedAscii #-}

matchUnicodeFallback :: UV.Vector Char -> Int -> T.Text -> Bool
matchUnicodeFallback !aChars !m !bText =
    let !bChars = UV.fromList (T.unpack bText)
        !n = UV.length bChars
        !maxDist = allowedMaxDist (min m n)
     in if abs (m - n) > maxDist
            then False
            else
                if m == 0
                    then n <= maxDist
                    else
                        if n == 0
                            then m <= maxDist
                            else
                                if aChars == bChars
                                    then True
                                    else boundedVectorDist aChars bChars maxDist

boundedVectorDist :: UV.Vector Char -> UV.Vector Char -> Int -> Bool
boundedVectorDist !xs !ys !maxDist = runST $ do
    let !m = UV.length xs
        !n = UV.length ys
    row <- UMV.unsafeNew (n + 1)
    let initRow !j
            | j > n = pure ()
            | otherwise = UMV.unsafeWrite row j j >> initRow (j + 1)
    initRow 0
    let outerLoop !i
            | i > m = do
                finalD <- UMV.unsafeRead row n
                pure (finalD <= maxDist)
            | otherwise = do
                let !x = xs `UV.unsafeIndex` (i - 1)
                prevDiag0 <- UMV.unsafeRead row 0
                UMV.unsafeWrite row 0 i
                let innerLoop !j !diag !rowMin
                        | j > n = pure rowMin
                        | otherwise = do
                            let !y = ys `UV.unsafeIndex` (j - 1)
                            up <- UMV.unsafeRead row j
                            left <- UMV.unsafeRead row (j - 1)
                            let !cur =
                                    if x == y
                                        then diag
                                        else 1 + min3 left diag up
                                !rowMin' = min rowMin cur
                            UMV.unsafeWrite row j cur
                            innerLoop (j + 1) up rowMin'
                rMin <- innerLoop 1 prevDiag0 i
                if rMin > maxDist
                    then pure False
                    else outerLoop (i + 1)
    outerLoop 1

textApproxEq :: T.Text -> T.Text -> Bool
textApproxEq a b = matchEditPattern (mkEditPattern a) b
{-# INLINE textApproxEq #-}

-- | Strict unboxed single-row Wagner-Fischer Levenshtein distance.
distance :: (Eq a) => [a] -> [a] -> Int
distance [] ys = length ys
distance xs [] = length xs
distance xs ys = runST $ do
    let !n = length ys
    row <- UMV.unsafeNew (n + 1)
    let initRow !j
            | j > n = pure ()
            | otherwise = UMV.unsafeWrite row j j >> initRow (j + 1)
    initRow 0
    let outerLoop !_ [] = UMV.unsafeRead row n
        outerLoop !i (x : restX) = do
            prevDiag0 <- UMV.unsafeRead row 0
            UMV.unsafeWrite row 0 i
            let innerLoop !_ !_ [] = pure ()
                innerLoop !j !diag (y : restY) = do
                    up <- UMV.unsafeRead row j
                    left <- UMV.unsafeRead row (j - 1)
                    let !cur =
                            if x == y
                                then diag
                                else 1 + min3 left diag up
                    UMV.unsafeWrite row j cur
                    innerLoop (j + 1) up restY
            innerLoop 1 prevDiag0 ys
            outerLoop (i + 1) restX
    outerLoop 1 xs

(~==) :: String -> String -> Bool
a ~== b = T.pack a `textApproxEq` T.pack b
{-# INLINE (~==) #-}

-- ---------------------------------------------------------------------------
-- Reference implementation retained for differential QuickCheck & corpus tests
-- ---------------------------------------------------------------------------

distanceRef :: (Eq a) => [a] -> [a] -> Int
distanceRef a b =
    last
        ( if lab == 0
            then mainDiag
            else
                if lab > 0
                    then lowers !! (lab - 1)
                    else uppers !! (-1 - lab)
        )
  where
    mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
    uppers = eachDiag a b (mainDiag : uppers)
    lowers = eachDiag b a (mainDiag : lowers)
    eachDiag _a [] _diags = []
    eachDiag a' (_bch : bs) (lastDiag : diags) = oneDiag a' bs nextDiag lastDiag : eachDiag a' bs diags
      where
        nextDiag = head (tail diags)
    eachDiag _ _ [] = undefined
    oneDiag a' b' diagAbove diagBelow = thisdiag
      where
        doDiag [] _b _nw _n _w = []
        doDiag _a [] _nw _n _w = []
        doDiag (ach : as) (bch : bs) nw n w = me : doDiag as bs me (tail n) (tail w)
          where
            me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
        firstelt = 1 + head diagBelow
        thisdiag = firstelt : doDiag a' b' firstelt diagAbove (tail diagBelow)
    lab = length a - length b

approxEqRef :: String -> String -> Bool
approxEqRef a b
    | len < 5 = dist < 3
    | otherwise = dist < (len * 40 `div` 100)
  where
    len = length a `min` length b
    dist = distanceRef a b
