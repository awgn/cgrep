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
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module CGrep.Semantic.ContextFilter (
    FilterFunction,
    ContextFilter (..),
    contextFilterAll,
    isContextFilterAll,
    contextBitCode,
    contextBitComment,
    contextBitLiteral,
    mkContextFilter,
    mkParConfig,
    (~!),
    (~?),
    runContextFilter,
    runContextFilterRef,
    startLiteralMarker,
    endLiteralMarker,
) where

import CGrep.Boundary (Boundary (..), BoundaryGuard (..))
import CGrep.Parser.Char (chr, isAlphaNum, isSpace, ord)
import CGrep.Text (blankByWidth)
import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Bits (Bits (complement, unsafeShiftL, (.&.), (.|.)))
import Data.Int (Int32)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import Data.Text.Internal.ArrayUtils (memchr)
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector as V
import Data.Word (Word64, Word8)
import GHC.Exts (Char (C#), Int (I#), chr#)
import Options (Options (..))

startLiteralMarker :: Char
startLiteralMarker = chr 2

endLiteralMarker :: Char
endLiteralMarker = chr 3

startLiteralMarkerByte :: Word8
startLiteralMarkerByte = 2

endLiteralMarkerByte :: Word8
endLiteralMarkerByte = 3

unsafeChr8 :: Word8 -> Char
unsafeChr8 !w =
    let !(I# i#) = fromIntegral w
     in C# (chr# i#)
{-# INLINE unsafeChr8 #-}

type FilterFunction = ContextFilter -> T.Text -> T.Text

newtype ContextBit = ContextBit Int32
    deriving stock (Show)
    deriving newtype (Eq, Bits)

contextBitEmpty :: ContextBit
contextBitEmpty = ContextBit 0

contextBitCode :: ContextBit
contextBitCode = ContextBit 0x1

contextBitComment :: ContextBit
contextBitComment = ContextBit 0x2

contextBitLiteral :: ContextBit
contextBitLiteral = ContextBit 0x4

(~=) :: ContextBit -> Bool -> ContextBit
b ~= True = b
_ ~= False = contextBitEmpty
{-# INLINE (~=) #-}

(~?) :: ContextFilter -> ContextBit -> Bool
f ~? b = (unFilter f .&. b) /= contextBitEmpty
{-# INLINE (~?) #-}

(~!) :: ContextFilter -> ContextBit -> ContextFilter
a ~! b = ContextFilter $ unFilter a .&. complement b
{-# INLINE (~!) #-}

newtype ContextFilter = ContextFilter {unFilter :: ContextBit}
    deriving stock (Show)
    deriving newtype (Eq, Bits)

contextFilterAll :: ContextFilter
contextFilterAll = ContextFilter (contextBitCode .|. contextBitComment .|. contextBitLiteral)
{-# NOINLINE contextFilterAll #-}

isContextFilterAll :: ContextFilter -> Bool
isContextFilterAll f = f == contextFilterAll
{-# INLINE isContextFilterAll #-}

codeFilter :: ContextFilter -> Bool
codeFilter f = (unFilter f .&. contextBitCode) /= contextBitEmpty
{-# INLINE codeFilter #-}

commentFilter :: ContextFilter -> Bool
commentFilter f = (unFilter f .&. contextBitComment) /= contextBitEmpty
{-# INLINE commentFilter #-}

literalFilter :: ContextFilter -> Bool
literalFilter f = (unFilter f .&. contextBitLiteral) /= contextBitEmpty
{-# INLINE literalFilter #-}

data RegionBoundary = RegionBoundary
    { rbBegin :: !(# Char, T.Text #)
    , rbBeginByte0 :: {-# UNPACK #-} !Word8
    , rbBeginRest :: {-# UNPACK #-} !T.Text
    , rbBeginLen :: {-# UNPACK #-} !Int
    , rbEnd :: !(# Char, T.Text #)
    , rbEndByte0 :: {-# UNPACK #-} !Word8
    , rbEndLastChar :: {-# UNPACK #-} !Char
    , rbEndRest :: {-# UNPACK #-} !T.Text
    , rbEndLen :: {-# UNPACK #-} !Int
    , rbEndUniform :: !Bool
    , rbGuard :: !BoundaryGuard
    }

isAsciiRegion :: RegionBoundary -> Bool
isAsciiRegion RegionBoundary{rbBegin = (# bc, bs #), rbEnd = (# ec, es #), ..} =
    bc < '\x80'
        && T.all (< '\x80') bs
        && ec < '\x80'
        && T.all (< '\x80') es
        && rbBeginLen == 1 + TU.lengthWord8 bs
        && rbEndLen == 1 + TU.lengthWord8 es
{-# INLINE isAsciiRegion #-}

toRegionBoundary :: Boundary -> Maybe RegionBoundary
toRegionBoundary Boundary{..}
    | Just (!bc, !bs) <- T.uncons bBegin
    , Just (!ec, !es) <- T.uncons bEnd =
        Just
            RegionBoundary
                { rbBegin = (# bc, bs #)
                , rbBeginByte0 = fromIntegral (ord bc)
                , rbBeginRest = bs
                , rbBeginLen = bBeginLen
                , rbEnd = (# ec, es #)
                , rbEndByte0 = fromIntegral (ord ec)
                , rbEndLastChar = T.last bEnd
                , rbEndRest = es
                , rbEndLen = bEndLen
                , rbEndUniform = bEndLen >= 3 && T.all (== ec) es
                , rbGuard = bGuard
                }
    | otherwise = Nothing

data ParConfig = ParConfig
    { commBound :: !(V.Vector RegionBoundary)
    , litrBound :: !(V.Vector RegionBoundary)
    , rawBound :: !(V.Vector RegionBoundary)
    , chrBound :: !(V.Vector RegionBoundary)
    , initMaskLo :: {-# UNPACK #-} !Word64
    , initMaskHi :: {-# UNPACK #-} !Word64
    , inits :: T.Text
    , pcAllAscii :: !Bool
    , useMakers :: !Bool
    }

addMaskChar :: (Word64, Word64) -> RegionBoundary -> (Word64, Word64)
addMaskChar (!lo, !hi) RegionBoundary{rbBegin = (# c, _ #)} =
    let !o = ord c
     in if o < 64
            then (lo .|. (1 `unsafeShiftL` o), hi)
            else
                if o < 128
                    then (lo, hi .|. (1 `unsafeShiftL` (o - 64)))
                    else (lo, hi)
{-# INLINE addMaskChar #-}

testAsciiInit :: Word64 -> Word64 -> Word8 -> Bool
testAsciiInit !lo !hi !b =
    let !i = fromIntegral b :: Int
     in if i < 64
            then (lo .&. (1 `unsafeShiftL` i)) /= 0
            else (hi .&. (1 `unsafeShiftL` (i - 64))) /= 0
{-# INLINE testAsciiInit #-}

mkParConfig :: [Boundary] -> [Boundary] -> [Boundary] -> [Boundary] -> Bool -> ParConfig
mkParConfig cs ls rs chs ab =
    let sortBounds = sortBy (comparing (Down . rbBeginLen)) . mapMaybe toRegionBoundary
        !cs' = sortBounds cs
        !ls' = sortBounds ls
        !rs' = sortBounds rs
        !chs' = sortBounds chs
        !m0 = foldl' addMaskChar (0, 0) cs'
        !m1 = foldl' addMaskChar m0 ls'
        !m2 = foldl' addMaskChar m1 rs'
        !(maskLo, maskHi) = foldl' addMaskChar m2 chs'
        !allAscii =
            all isAsciiRegion cs'
                && all isAsciiRegion ls'
                && all isAsciiRegion rs'
                && all isAsciiRegion chs'
        firstChar RegionBoundary{rbBegin = (# c, _ #)} = c
     in ParConfig
            { commBound = V.fromList cs'
            , litrBound = V.fromList ls'
            , rawBound = V.fromList rs'
            , chrBound = V.fromList chs'
            , initMaskLo = maskLo
            , initMaskHi = maskHi
            , inits =
                T.pack . Set.toList . Set.fromList $
                    (firstChar <$> cs')
                        <> (firstChar <$> ls')
                        <> (firstChar <$> rs')
                        <> (firstChar <$> chs')
            , pcAllAscii = allAscii
            , useMakers = ab
            }

mkContextFilter :: Options -> ContextFilter
mkContextFilter Options{..} =
    if not (code || comment || literal)
        then contextFilterAll
        else ContextFilter $ contextBitCode ~= code .|. contextBitComment ~= comment .|. contextBitLiteral ~= literal

blankChar :: Char -> Char
blankChar x = if isSpace x then x else blankByWidth x
{-# INLINE blankChar #-}

emitIf :: Bool -> Char -> Char
emitIf True x = x
emitIf False x = blankChar x
{-# INLINE emitIf #-}

-- | Fast in-place ST byte-array implementation preserving exact UTF-8 Word8 length.
-- Falls back to the character-by-character reference state machine only if non-ASCII
-- delimiter boundaries are ever supplied via 'mkParConfig'.
runContextFilter :: ParConfig -> ContextFilter -> T.Text -> T.Text
runContextFilter conf f txt@(TI.Text srcArr@(A.ByteArray srcArr#) srcOff byteLen)
    | byteLen == 0 = T.empty
    | isContextFilterAll f && not (useMakers conf) = txt
    | not (pcAllAscii conf) = runContextFilterRef conf f txt
    | otherwise = runST $ do
        marr <- A.new byteLen
        A.copyI byteLen marr 0 srcArr srcOff
        runFastLoops marr
        frozen <- A.unsafeFreeze marr
        pure (TI.Text frozen 0 byteLen)
  where
    !codeDisp = codeFilter f
    !commDisp = commentFilter f
    !litrDisp = literalFilter f
    !markerDisp = codeDisp || litrDisp
    !markers = useMakers conf
    !maskLo = initMaskLo conf
    !maskHi = initMaskHi conf
    !cBounds = commBound conf
    !rBounds = rawBound conf
    !lBounds = litrBound conf
    !chBounds = chrBound conf

    {-# INLINE blankAsciiByte #-}
    blankAsciiByte :: A.MArray s -> Word8 -> Int -> ST s ()
    blankAsciiByte marr !b !p =
        when ((b - 9) > 4) $
            A.unsafeWrite marr p 0x20

    {-# INLINE blankAsciiAt #-}
    blankAsciiAt :: A.MArray s -> Int -> ST s ()
    blankAsciiAt marr !p = do
        let !b = A.unsafeIndex srcArr (srcOff + p)
        blankAsciiByte marr b p

    {-# INLINE blankAsciiSpan #-}
    blankAsciiSpan :: A.MArray s -> Int -> Int -> ST s ()
    blankAsciiSpan marr = goSpan
      where
        goSpan !p !endP
            | p >= endP = pure ()
            | otherwise = do
                blankAsciiAt marr p
                goSpan (p + 1) endP

    {-# INLINE blankMultiByte #-}
    blankMultiByte :: A.MArray s -> Word8 -> Int -> ST s ()
    blankMultiByte marr !b0 !p
        | b0 < 0xE0 = do
            A.unsafeWrite marr p 0xC2
            A.unsafeWrite marr (p + 1) 0xA0
        | b0 < 0xF0 = do
            A.unsafeWrite marr p 0xE3
            A.unsafeWrite marr (p + 1) 0x80
            A.unsafeWrite marr (p + 2) 0x80
        | otherwise = do
            A.unsafeWrite marr p 0xF4
            A.unsafeWrite marr (p + 1) 0x80
            A.unsafeWrite marr (p + 2) 0x80
            A.unsafeWrite marr (p + 3) 0x80

    {-# INLINE utf8Width #-}
    utf8Width :: Word8 -> Int
    utf8Width !b0
        | b0 < 0x80 = 1
        | b0 < 0xE0 = 2
        | b0 < 0xF0 = 3
        | otherwise = 4

    blankSpan :: A.MArray s -> Int -> Int -> ST s ()
    blankSpan marr = goBlank
      where
        goBlank !p !endP
            | p >= endP = pure ()
            | otherwise = do
                let !b0 = A.unsafeIndex srcArr (srcOff + p)
                if b0 < 0x80
                    then do
                        blankAsciiByte marr b0 p
                        goBlank (p + 1) endP
                    else do
                        blankMultiByte marr b0 p
                        goBlank (p + utf8Width b0) endP

    runFastLoops :: forall s. A.MArray s -> ST s ()
    runFastLoops marr = loopCode 0 '\0' False
      where
        {-# INLINE applyEnterLiteral #-}
        applyEnterLiteral !pos !beginLen
            | markers = do
                let !lastPos = pos + beginLen - 1
                when (beginLen > 1) $
                    blankAsciiSpan marr pos lastPos
                if markerDisp
                    then A.unsafeWrite marr lastPos startLiteralMarkerByte
                    else blankAsciiAt marr lastPos
            | not litrDisp = blankAsciiSpan marr pos (pos + beginLen)
            | otherwise = pure ()

        {-# INLINE applyExitLiteral #-}
        applyExitLiteral !pos !endLen
            | markers = do
                if markerDisp
                    then A.unsafeWrite marr pos endLiteralMarkerByte
                    else blankAsciiAt marr pos
                when (endLen > 1) $
                    blankAsciiSpan marr (pos + 1) (pos + endLen)
            | not litrDisp = blankAsciiSpan marr pos (pos + endLen)
            | otherwise = pure ()

        loopCode :: Int -> Char -> Bool -> ST s ()
        loopCode !pos !prevCh !prevEsc
            | pos >= byteLen = pure ()
            | otherwise = do
                let !b0 = A.unsafeIndex srcArr (srcOff + pos)
                if b0 < 0x80
                    then do
                        let !c = unsafeChr8 b0
                            !nextEsc = (b0 == 0x5C) && not prevEsc
                        if testAsciiInit maskLo maskHi b0
                            then case findPrefixFast b0 prevCh prevEsc pos cBounds of
                                (# _, Just !rb #) -> do
                                    let !blen = rbBeginLen rb
                                    when (not commDisp) $
                                        blankAsciiSpan marr pos (pos + blen)
                                    loopComment rb (pos + blen)
                                _ -> case findPrefixFast b0 prevCh prevEsc pos rBounds of
                                    (# _, Just !rb #) -> do
                                        let !blen = rbBeginLen rb
                                        applyEnterLiteral pos blen
                                        loopRaw rb (pos + blen)
                                    _ -> case findPrefixFast b0 prevCh prevEsc pos lBounds of
                                        (# _, Just !rb #) -> do
                                            let !blen = rbBeginLen rb
                                            applyEnterLiteral pos blen
                                            loopLiteral rb (pos + blen)
                                        _ -> case findCharPrefixFast b0 prevCh prevEsc pos chBounds of
                                            (# _, Just !rb #) -> do
                                                let !blen = rbBeginLen rb
                                                applyEnterLiteral pos blen
                                                loopChar rb (pos + blen)
                                            _ -> do
                                                when (not codeDisp) $
                                                    blankAsciiByte marr b0 pos
                                                loopCode (pos + 1) c nextEsc
                            else do
                                when (not codeDisp) $
                                    blankAsciiByte marr b0 pos
                                loopCode (pos + 1) c nextEsc
                    else do
                        let TU.Iter !c !d = TU.iter txt pos
                        when (not codeDisp) $
                            blankMultiByte marr b0 pos
                        loopCode (pos + d) c False

        loopComment :: RegionBoundary -> Int -> ST s ()
        loopComment !rb@RegionBoundary{..} !pos
            | pos >= byteLen = pure ()
            | otherwise =
                case memchr srcArr# (srcOff + pos) (byteLen - pos) rbEndByte0 of
                    -1 ->
                        when (not commDisp) $
                            blankSpan marr pos byteLen
                    relOff -> do
                        let !hitPos = pos + relOff
                        when (not commDisp && hitPos > pos) $
                            blankSpan marr pos hitPos
                        if matchRestFast rbEndRest srcArr srcOff byteLen (hitPos + 1)
                            then do
                                let !afterEnd = hitPos + rbEndLen
                                when (not commDisp) $
                                    blankAsciiSpan marr hitPos afterEnd
                                loopCode afterEnd rbEndLastChar False
                            else do
                                when (not commDisp) $
                                    blankAsciiByte marr rbEndByte0 hitPos
                                loopComment rb (hitPos + 1)

        loopRaw :: RegionBoundary -> Int -> ST s ()
        loopRaw !rb@RegionBoundary{..} !pos
            | pos >= byteLen = pure ()
            | otherwise =
                case memchr srcArr# (srcOff + pos) (byteLen - pos) rbEndByte0 of
                    -1 ->
                        when (not litrDisp) $
                            blankSpan marr pos byteLen
                    relOff -> do
                        let !hitPos = pos + relOff
                        when (not litrDisp && hitPos > pos) $
                            blankSpan marr pos hitPos
                        if isClosingBoundaryFast rb srcArr srcOff byteLen hitPos
                            then do
                                applyExitLiteral hitPos rbEndLen
                                loopCode (hitPos + rbEndLen) rbEndLastChar False
                            else do
                                when (not litrDisp) $
                                    blankAsciiByte marr rbEndByte0 hitPos
                                loopRaw rb (hitPos + 1)

        loopLiteral :: RegionBoundary -> Int -> ST s ()
        loopLiteral !rb@RegionBoundary{..} !pos
            | pos >= byteLen = pure ()
            | otherwise = do
                let !b0 = A.unsafeIndex srcArr (srcOff + pos)
                if b0 == 0x5C
                    then do
                        when (not litrDisp) $
                            A.unsafeWrite marr pos 0x20
                        let !p1 = pos + 1
                        if p1 >= byteLen
                            then pure ()
                            else do
                                let !b1 = A.unsafeIndex srcArr (srcOff + p1)
                                    !d1 = utf8Width b1
                                when (not litrDisp) $
                                    if b1 < 0x80
                                        then blankAsciiByte marr b1 p1
                                        else blankMultiByte marr b1 p1
                                loopLiteral rb (p1 + d1)
                    else
                        if b0 == rbEndByte0 && isClosingBoundaryFast rb srcArr srcOff byteLen pos
                            then do
                                applyExitLiteral pos rbEndLen
                                loopCode (pos + rbEndLen) rbEndLastChar False
                            else
                                if b0 < 0x80
                                    then do
                                        when (not litrDisp) $
                                            blankAsciiByte marr b0 pos
                                        loopLiteral rb (pos + 1)
                                    else do
                                        when (not litrDisp) $
                                            blankMultiByte marr b0 pos
                                        loopLiteral rb (pos + utf8Width b0)

        loopChar :: RegionBoundary -> Int -> ST s ()
        loopChar !rb@RegionBoundary{..} !pos
            | pos >= byteLen = pure ()
            | otherwise = do
                let !b0 = A.unsafeIndex srcArr (srcOff + pos)
                if b0 == 0x5C
                    then do
                        when (not litrDisp) $
                            A.unsafeWrite marr pos 0x20
                        let !p1 = pos + 1
                        if p1 >= byteLen
                            then pure ()
                            else do
                                let !b1 = A.unsafeIndex srcArr (srcOff + p1)
                                    !d1 = utf8Width b1
                                when (not litrDisp) $
                                    if b1 < 0x80
                                        then blankAsciiByte marr b1 p1
                                        else blankMultiByte marr b1 p1
                                loopChar rb (p1 + d1)
                    else
                        if b0 == rbEndByte0 && matchRestFast rbEndRest srcArr srcOff byteLen (pos + 1)
                            then do
                                applyExitLiteral pos rbEndLen
                                loopCode (pos + rbEndLen) rbEndLastChar False
                            else
                                if b0 < 0x80
                                    then do
                                        when (not litrDisp) $
                                            blankAsciiByte marr b0 pos
                                        loopChar rb (pos + 1)
                                    else do
                                        when (not litrDisp) $
                                            blankMultiByte marr b0 pos
                                        loopChar rb (pos + utf8Width b0)

    {-# INLINE findPrefixFast #-}
    findPrefixFast :: Word8 -> Char -> Bool -> Int -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
    findPrefixFast !b0 !prevCh !prevEsc !pos !vb = goB 0
      where
        !len = V.length vb
        goB !i
            | i >= len = (# 0, Nothing #)
            | otherwise =
                let !rb@RegionBoundary{..} = V.unsafeIndex vb i
                 in if rbBeginByte0 == b0
                        && matchRestFast rbBeginRest srcArr srcOff byteLen (pos + 1)
                        && checkBoundaryGuardFast rbGuard rbBeginLen prevCh prevEsc txt srcArr srcOff byteLen pos
                        then (# i, Just rb #)
                        else goB (i + 1)

    {-# INLINE findCharPrefixFast #-}
    findCharPrefixFast :: Word8 -> Char -> Bool -> Int -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
    findCharPrefixFast !b0 !prevCh !prevEsc !pos !vb = goC 0
      where
        !len = V.length vb
        goC !i
            | i >= len = (# 0, Nothing #)
            | otherwise =
                let !rb@RegionBoundary{..} = V.unsafeIndex vb i
                 in if rbBeginByte0 == b0
                        && matchRestFast rbBeginRest srcArr srcOff byteLen (pos + 1)
                        && checkBoundaryGuardFast rbGuard rbBeginLen prevCh prevEsc txt srcArr srcOff byteLen pos
                        && hasMatchingCharEndFast rb txt srcArr srcOff byteLen (pos + rbBeginLen)
                        then (# i, Just rb #)
                        else goC (i + 1)

matchRestFast :: T.Text -> A.Array -> Int -> Int -> Int -> Bool
matchRestFast (TI.Text pArr pOff pLen) srcArr !srcOff !byteLen !pos
    | pLen == 0 = True
    | pos + pLen > byteLen = False
    | pLen == 1 = A.unsafeIndex pArr pOff == A.unsafeIndex srcArr (srcOff + pos)
    | otherwise = A.equal pArr pOff srcArr (srcOff + pos) pLen
{-# INLINE matchRestFast #-}

countExtraBytes :: A.Array -> Int -> Int -> Int -> Word8 -> Int
countExtraBytes srcArr !srcOff !byteLen !startPos !targetByte = go startPos 0
  where
    go !p !acc
        | p < byteLen && A.unsafeIndex srcArr (srcOff + p) == targetByte =
            go (p + 1) (acc + 1)
        | otherwise = acc
{-# INLINE countExtraBytes #-}

isClosingBoundaryFast :: RegionBoundary -> A.Array -> Int -> Int -> Int -> Bool
isClosingBoundaryFast RegionBoundary{..} srcArr !srcOff !byteLen !pos =
    matchRestFast rbEndRest srcArr srcOff byteLen (pos + 1)
        && ( not rbEndUniform
                || let !extra = countExtraBytes srcArr srcOff byteLen (pos + rbEndLen) rbEndByte0
                    in extra == 0 || extra >= rbEndLen
           )
{-# INLINE isClosingBoundaryFast #-}

checkBoundaryGuardFast :: BoundaryGuard -> Int -> Char -> Bool -> T.Text -> A.Array -> Int -> Int -> Int -> Bool
checkBoundaryGuardFast NoBoundaryGuard _ _ _ _ _ _ _ _ = True
checkBoundaryGuardFast GuardShellComment _ !prevCh !prevEsc _ _ _ _ _ =
    not prevEsc && isShellCommentDelimiter prevCh
checkBoundaryGuardFast GuardNotPrecededBySlash _ _ !prevEsc _ _ _ _ _ =
    not prevEsc
checkBoundaryGuardFast (GuardNotFollowedBy !forbidden) !beginLen _ _ txt srcArr !srcOff !byteLen !pos =
    let !afterBegin = pos + beginLen
     in if afterBegin >= byteLen
            then True
            else
                let !nb = A.unsafeIndex srcArr (srcOff + afterBegin)
                    !nc =
                        if nb < 0x80
                            then unsafeChr8 nb
                            else let TU.Iter !ch _ = TU.iter txt afterBegin in ch
                 in nc /= forbidden
{-# INLINE checkBoundaryGuardFast #-}

hasMatchingCharEndFast :: RegionBoundary -> T.Text -> A.Array -> Int -> Int -> Int -> Bool
hasMatchingCharEndFast RegionBoundary{rbEnd = (# !ecChar, _ #), ..} txt srcArr !srcOff !byteLen !bodyPos
    | bodyPos >= byteLen = False
    | otherwise =
        let !b0 = A.unsafeIndex srcArr (srcOff + bodyPos)
         in if b0 == 0x5C
                then
                    let !afterSlash = bodyPos + 1
                     in if afterSlash >= byteLen
                            then False
                            else
                                let TU.Iter !esc1 !d1 = TU.iter txt afterSlash
                                 in esc1 /= '\n' && esc1 /= '\r' && scanEscAt 10 (afterSlash + d1)
                else
                    if b0 < 0x80
                        then
                            let !y = unsafeChr8 b0
                             in y /= '\n' && y /= '\r' && y /= ecChar && matchEndAt (bodyPos + 1)
                        else
                            let TU.Iter !y !dy = TU.iter txt bodyPos
                             in y /= '\n' && y /= '\r' && y /= ecChar && matchEndAt (bodyPos + dy)
  where
    matchEndAt !p =
        p < byteLen
            && A.unsafeIndex srcArr (srcOff + p) == rbEndByte0
            && matchRestFast rbEndRest srcArr srcOff byteLen (p + 1)

    scanEscAt :: Int -> Int -> Bool
    scanEscAt !remSteps !p
        | matchEndAt p = True
        | remSteps <= 0 || p >= byteLen = False
        | otherwise =
            let !b = A.unsafeIndex srcArr (srcOff + p)
             in b < 0x80
                    && let !ch = unsafeChr8 b
                        in (isAlphaNum ch || ch == '{' || ch == '}')
                            && scanEscAt (remSteps - 1) (p + 1)

-- ---------------------------------------------------------------------------
-- Reference character-by-character state machine (kept for non-ASCII custom
-- boundaries and QuickCheck differential testing).
-- ---------------------------------------------------------------------------

data SkipKind
    = NoSkip
    | SkipBlank
    | SkipDisplay !Bool
    | SkipToStartMarker !Bool
    deriving stock (Show, Eq)

data ParState = ParState
    { parCtxState :: !ContextState
    , parNextState :: !ContextState
    , parSkipKind :: !SkipKind
    , parSkip :: {-# UNPACK #-} !Int
    , parPrevChar :: {-# UNPACK #-} !Char
    , parEscaped :: !Bool
    , parText :: !T.Text
    }
    deriving stock (Show)

data ContextState
    = CodeState
    | CommState {-# UNPACK #-} !Int
    | ChrState {-# UNPACK #-} !Int
    | LitrState {-# UNPACK #-} !Int
    | RawState {-# UNPACK #-} !Int
    deriving stock (Show, Eq, Ord)

runContextFilterRef :: ParConfig -> ContextFilter -> T.Text -> T.Text
runContextFilterRef conf f txt = T.unfoldr stepContext initialState
  where
    !codeDisp = codeFilter f
    !commDisp = commentFilter f
    !litrDisp = literalFilter f
    !markerDisp = codeDisp || litrDisp
    !markers = useMakers conf

    !initialState =
        ParState
            { parCtxState = CodeState
            , parNextState = CodeState
            , parSkipKind = NoSkip
            , parSkip = 0
            , parPrevChar = '\0'
            , parEscaped = False
            , parText = txt
            }

    stepContext :: ParState -> Maybe (Char, ParState)
    stepContext !s@ParState{..} = case T.uncons parText of
        Nothing -> Nothing
        Just (!c, !cont) ->
            let !(# !outChar, !nextState #) = stepChar s c cont
             in Just (outChar, nextState)
    {-# INLINE stepContext #-}

    stepChar :: ParState -> Char -> T.Text -> (# Char, ParState #)
    stepChar !s@ParState{..} !c !cont
        | parSkip > 0 =
            let !newSkip = parSkip - 1
                finishOrCont !outCh
                    | newSkip == 0 = (# outCh, ParState parNextState parNextState NoSkip 0 c False cont #)
                    | otherwise = (# outCh, s{parSkip = newSkip, parPrevChar = c, parEscaped = False, parText = cont} #)
             in case parSkipKind of
                    SkipDisplay disp -> finishOrCont (emitIf disp c)
                    SkipBlank -> finishOrCont (blankChar c)
                    SkipToStartMarker mDisp
                        | newSkip == 0 ->
                            let !outCh = if mDisp then startLiteralMarker else blankChar c
                             in (# outCh, ParState parNextState parNextState NoSkip 0 c False cont #)
                        | otherwise ->
                            (# blankChar c, s{parSkip = newSkip, parPrevChar = c, parEscaped = False, parText = cont} #)
                    NoSkip -> finishOrCont (blankChar c)
        | CodeState <- parCtxState = handleCodeState s c cont
        | CommState n <- parCtxState = handleCommentState s n c cont
        | LitrState n <- parCtxState = handleLiteralState s n c cont
        | ChrState n <- parCtxState = handleCharState s n c cont
        | RawState n <- parCtxState = handleRawState s n c cont
    {-# INLINE stepChar #-}

    {-# INLINE enterComment #-}
    enterComment !i !beginLen !c !cont =
        let !outCh = emitIf commDisp c
            !skip = beginLen - 1
            !target = CommState i
         in if skip == 0
                then (# outCh, ParState target target NoSkip 0 c False cont #)
                else (# outCh, ParState CodeState target (SkipDisplay commDisp) skip c False cont #)

    {-# INLINE enterLiteral #-}
    enterLiteral !target !beginLen !c !cont
        | markers =
            let !skip = beginLen - 1
             in if skip == 0
                    then
                        let !outCh = if markerDisp then startLiteralMarker else blankChar c
                         in (# outCh, ParState target target NoSkip 0 c False cont #)
                    else (# blankChar c, ParState CodeState target (SkipToStartMarker markerDisp) skip c False cont #)
        | otherwise =
            let !outCh = emitIf litrDisp c
                !skip = beginLen - 1
             in if skip == 0
                    then (# outCh, ParState target target NoSkip 0 c False cont #)
                    else (# outCh, ParState CodeState target (SkipDisplay litrDisp) skip c False cont #)

    {-# INLINE exitLiteral #-}
    exitLiteral !currSt !endLen !c !cont
        | markers =
            let !outCh = if markerDisp then endLiteralMarker else blankChar c
                !skip = endLen - 1
             in if skip == 0
                    then (# outCh, ParState CodeState CodeState NoSkip 0 c False cont #)
                    else (# outCh, ParState currSt CodeState SkipBlank skip c False cont #)
        | otherwise =
            let !outCh = emitIf litrDisp c
                !skip = endLen - 1
             in if skip == 0
                    then (# outCh, ParState CodeState CodeState NoSkip 0 c False cont #)
                    else (# outCh, ParState currSt CodeState (SkipDisplay litrDisp) skip c False cont #)

    {-# INLINE handleCodeState #-}
    handleCodeState !s@ParState{..} !c !cont =
        let !nextEsc = (c == '\\') && not parEscaped
         in if c `T.elem` inits conf
                then case findPrefixBoundary c cont parPrevChar parEscaped (commBound conf) of
                    (# i, Just !b #) -> enterComment i (rbBeginLen b) c cont
                    _ -> case findPrefixBoundary c cont parPrevChar parEscaped (rawBound conf) of
                        (# i, Just !b #) -> enterLiteral (RawState i) (rbBeginLen b) c cont
                        _ -> case findPrefixBoundary c cont parPrevChar parEscaped (litrBound conf) of
                            (# i, Just !b #) -> enterLiteral (LitrState i) (rbBeginLen b) c cont
                            _ -> case findPrefixBoundary' c cont parPrevChar parEscaped (chrBound conf) of
                                (# i, Just !b #) -> enterLiteral (ChrState i) (rbBeginLen b) c cont
                                _ -> (# emitIf codeDisp c, s{parPrevChar = c, parEscaped = nextEsc, parText = cont} #)
                else (# emitIf codeDisp c, s{parPrevChar = c, parEscaped = nextEsc, parText = cont} #)

    {-# INLINE handleCommentState #-}
    handleCommentState !s !n !c !cont =
        let !RegionBoundary{..} = V.unsafeIndex (commBound conf) n
            (# ec, es #) = rbEnd
            !outCh = emitIf commDisp c
         in if c == ec && es `T.isPrefixOf` cont
                then
                    let !skip = rbEndLen - 1
                     in if skip == 0
                            then (# outCh, ParState CodeState CodeState NoSkip 0 c False cont #)
                            else (# outCh, ParState (CommState n) CodeState (SkipDisplay commDisp) skip c False cont #)
                else (# outCh, s{parText = cont} #)

    {-# INLINE isClosingBoundary #-}
    isClosingBoundary !RegionBoundary{..} !c !cont =
        let (# ec, es #) = rbEnd
         in c == ec
                && es `T.isPrefixOf` cont
                && ( not rbEndUniform
                        || let !extra = T.length (T.takeWhile (== ec) (T.drop (rbEndLen - 1) cont))
                            in extra == 0 || extra >= rbEndLen
                   )

    {-# INLINE handleLiteralState #-}
    handleLiteralState !s !n !c !cont =
        if c == '\\'
            then (# emitIf litrDisp c, ParState (LitrState n) (LitrState n) (SkipDisplay litrDisp) 1 c False cont #)
            else
                let !rb@RegionBoundary{..} = V.unsafeIndex (litrBound conf) n
                 in if isClosingBoundary rb c cont
                        then exitLiteral (LitrState n) rbEndLen c cont
                        else (# emitIf litrDisp c, s{parText = cont} #)

    {-# INLINE handleCharState #-}
    handleCharState !s !n !c !cont =
        if c == '\\'
            then (# emitIf litrDisp c, ParState (ChrState n) (ChrState n) (SkipDisplay litrDisp) 1 c False cont #)
            else
                let !RegionBoundary{..} = V.unsafeIndex (chrBound conf) n
                    (# ec, es #) = rbEnd
                 in if c == ec && es `T.isPrefixOf` cont
                        then exitLiteral (ChrState n) rbEndLen c cont
                        else (# emitIf litrDisp c, s{parText = cont} #)

    {-# INLINE handleRawState #-}
    handleRawState !s !n !c !cont =
        let !rb@RegionBoundary{..} = V.unsafeIndex (rawBound conf) n
         in if isClosingBoundary rb c cont
                then exitLiteral (RawState n) rbEndLen c cont
                else (# emitIf litrDisp c, s{parText = cont} #)

isShellCommentDelimiter :: Char -> Bool
isShellCommentDelimiter ch =
    ch == '\0'
        || isSpace ch
        || ch == ';'
        || ch == '&'
        || ch == '|'
        || ch == '('
        || ch == ')'
        || ch == '<'
        || ch == '>'
{-# INLINE isShellCommentDelimiter #-}

checkBoundaryGuard :: BoundaryGuard -> Int -> Char -> Bool -> T.Text -> Bool
checkBoundaryGuard NoBoundaryGuard _ _ _ _ = True
checkBoundaryGuard GuardShellComment _ prevCh prevEsc _ =
    not prevEsc && isShellCommentDelimiter prevCh
checkBoundaryGuard GuardNotPrecededBySlash _ _ prevEsc _ =
    not prevEsc
checkBoundaryGuard (GuardNotFollowedBy forbidden) beginLen _ _ xs =
    case T.uncons (T.drop (beginLen - 1) xs) of
        Just (!nc, _) -> nc /= forbidden
        Nothing -> True
{-# INLINE checkBoundaryGuard #-}

findPrefixBoundary :: Char -> T.Text -> Char -> Bool -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
findPrefixBoundary !x !xs !prevCh !prevEsc !vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !rb@RegionBoundary{..} = V.unsafeIndex vb i
                (# !c, !cont #) = rbBegin
             in if c == x
                    then
                        if cont `T.isPrefixOf` xs && checkBoundaryGuard rbGuard rbBeginLen prevCh prevEsc xs
                            then (# i, Just rb #)
                            else go (i + 1)
                    else go (i + 1)
{-# INLINE findPrefixBoundary #-}

findPrefixBoundary' :: Char -> T.Text -> Char -> Bool -> V.Vector RegionBoundary -> (# Int, Maybe RegionBoundary #)
findPrefixBoundary' !x !xs !prevCh !prevEsc !vb = go 0
  where
    !len = V.length vb
    go !i
        | i >= len = (# 0, Nothing #)
        | otherwise =
            let !rb@RegionBoundary{..} = V.unsafeIndex vb i
                (# !c, !cont #) = rbBegin
             in if c /= x || not (cont `T.isPrefixOf` xs) || not (checkBoundaryGuard rbGuard rbBeginLen prevCh prevEsc xs)
                    then go (i + 1)
                    else
                        if hasMatchingCharEnd rb (T.drop (rbBeginLen - 1) xs)
                            then (# i, Just rb #)
                            else go (i + 1)

    hasMatchingCharEnd :: RegionBoundary -> T.Text -> Bool
    hasMatchingCharEnd RegionBoundary{rbEnd = (# !e, !es #)} body =
        let matchEnd t = case T.uncons t of
                Just (!z, !zs) -> z == e && es `T.isPrefixOf` zs
                Nothing -> False
            scanEsc :: Int -> T.Text -> Bool
            scanEsc !remSteps t
                | matchEnd t = True
                | remSteps <= 0 = False
                | Just (!ch, !rest) <- T.uncons t
                , isAlphaNum ch || ch == '{' || ch == '}' =
                    scanEsc (remSteps - 1) rest
                | otherwise = False
         in case T.uncons body of
                Just ('\\', afterSlash) ->
                    case T.uncons afterSlash of
                        Just (!esc1, !rest1)
                            | esc1 /= '\n' && esc1 /= '\r' -> scanEsc 10 rest1
                        _ -> False
                Just (!y, !ys)
                    | y /= '\n' && y /= '\r' && y /= e -> matchEnd ys
                _ -> False
