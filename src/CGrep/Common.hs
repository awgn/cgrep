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
    getTargetContentsRef,
    RawPrefilter (..),
    defaultMmapThreshold,
    mkAsciiRawPrefilter,
    evalRawPrefilterBS,
    readTargetFiltered,
    readTargetFilteredWith,
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
import qualified Control.Exception as E
import Control.Monad.ST (ST, runST)
import Data.Bits (unsafeShiftL, unsafeShiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (ord, toLower)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Internal as TI
import qualified Data.Text.Unsafe as TU
import Data.Word (Word64, Word8)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr, castPtr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff)
import Options (Options (..))
import Reader (ReaderIO)
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.IO.MMap (Mode (ReadOnly), mmapWithFilePtr)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.OsPath
import qualified System.OsString as OS
import System.PosixCompat.Files (fileSize, getFileStatus, isRegularFile)

foreign import ccall unsafe "string.h memchr"
    c_memchr :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp"
    c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

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

-- | Files at or above this size (64 KiB) are mapped read-only via @mmapWithFilePtr@
-- rather than read into a pinned heap @ByteString@, avoiding GHC large-object heap
-- allocations and kernel-to-user @memcpy@ on non-matching files while avoiding VM
-- map lock / TLB shootdown contention on small files.
defaultMmapThreshold :: Int
defaultMmapThreshold = 64 * 1024

-- | Sound pre-decode raw byte filter evaluated directly on @mmap@ pages or raw @ByteString@
-- buffers before @decodeUtf8Lenient@.
data RawPrefilter
    = RawAcceptAll
    | RawRejectAll
    | RawRequireAnyCS ![BS.ByteString]
    | RawRequireAnyCI ![BS.ByteString]
    deriving stock (Eq, Show)

mkAsciiRawPrefilter :: Bool -> Bool -> [T.Text] -> RawPrefilter
mkAsciiRawPrefilter !forbidSpace !ic needles
    | null needles = RawAcceptAll
    | any T.null needles = RawAcceptAll
    | not (all isAsciiText needles) = RawAcceptAll
    | forbidSpace && any (T.elem ' ') needles = RawAcceptAll
    | ic = RawRequireAnyCI (nub (map (TE.encodeUtf8 . T.toLower) needles))
    | otherwise = RawRequireAnyCS (nub (map TE.encodeUtf8 needles))

evalRawPrefilterBS :: RawPrefilter -> BS.ByteString -> Bool
evalRawPrefilterBS RawAcceptAll _ = True
evalRawPrefilterBS RawRejectAll _ = False
evalRawPrefilterBS pf bs =
    unsafeDupablePerformIO $
        BSU.unsafeUseAsCStringLen bs $ \(ptr, len) ->
            evalRawPrefilterPtr pf (castPtr ptr) len
{-# INLINE evalRawPrefilterBS #-}

evalRawPrefilterPtr :: RawPrefilter -> Ptr Word8 -> Int -> IO Bool
evalRawPrefilterPtr RawAcceptAll !_ !_ = pure True
evalRawPrefilterPtr RawRejectAll !_ !_ = pure False
evalRawPrefilterPtr (RawRequireAnyCS ns) !hptr !hlen = go ns
  where
    go [] = pure False
    go (n : rest) = do
        ok <- rawMemContainsCS hptr hlen n
        if ok then pure True else go rest
evalRawPrefilterPtr (RawRequireAnyCI ns) !hptr !hlen = go ns
  where
    go [] = pure False
    go (n : rest) = do
        ok <- rawMemContainsCI hptr hlen n
        if ok then pure True else go rest

peekW8 :: Ptr Word8 -> Int -> IO Word8
peekW8 = peekByteOff
{-# INLINE peekW8 #-}

rawMemContainsCS :: Ptr Word8 -> Int -> BS.ByteString -> IO Bool
rawMemContainsCS !hptr !hlen needle =
    BSU.unsafeUseAsCStringLen needle $ \(nptr0, nlen) ->
        if nlen <= 0 || nlen > hlen
            then pure False
            else do
                let !nptr = castPtr nptr0 :: Ptr Word8
                !b0 <- peek nptr
                if nlen == 1
                    then do
                        hit <- c_memchr hptr (fromIntegral b0) (fromIntegral hlen)
                        pure (hit /= nullPtr)
                    else do
                        !bLast <- peekW8 nptr (nlen - 1)
                        let !endStart = hptr `plusPtr` (hlen - nlen)
                            scan !cur =
                                let !remStarts = (endStart `minusPtr` cur) + 1
                                 in if remStarts <= 0
                                        then pure False
                                        else do
                                            hit <- c_memchr cur (fromIntegral b0) (fromIntegral remStarts)
                                            if hit == nullPtr
                                                then pure False
                                                else do
                                                    !candLast <- peekW8 hit (nlen - 1)
                                                    if candLast /= bLast
                                                        then scan (hit `plusPtr` 1)
                                                        else
                                                            if nlen == 2
                                                                then pure True
                                                                else do
                                                                    cmp <-
                                                                        c_memcmp
                                                                            (hit `plusPtr` 1)
                                                                            (nptr `plusPtr` 1)
                                                                            (fromIntegral (nlen - 2))
                                                                    if cmp == 0
                                                                        then pure True
                                                                        else scan (hit `plusPtr` 1)
                        scan hptr

rawMemContainsCI :: Ptr Word8 -> Int -> BS.ByteString -> IO Bool
rawMemContainsCI !hptr !hlen needle =
    BSU.unsafeUseAsCStringLen needle $ \(nptr0, nlen) ->
        if nlen <= 0 || nlen > hlen
            then pure False
            else do
                let !nptr = castPtr nptr0 :: Ptr Word8
                if nlen == 1
                    then do
                        !b0 <- peek nptr
                        p1 <- c_memchr hptr (fromIntegral b0) (fromIntegral hlen)
                        if p1 /= nullPtr
                            then pure True
                            else
                                if b0 >= 0x61 && b0 <= 0x7A
                                    then do
                                        p2 <- c_memchr hptr (fromIntegral (b0 - 0x20)) (fromIntegral hlen)
                                        pure (p2 /= nullPtr)
                                    else pure False
                    else do
                        let !nlast = nlen - 1
                            asciiLowerW8 :: Word8 -> Word8
                            asciiLowerW8 !w = if w - 0x41 <= 25 then w .|. 0x20 else w
                            swzCI :: Word8 -> Word64
                            swzCI !w = 1 `unsafeShiftL` (fromIntegral (asciiLowerW8 w) .&. 0x3F)
                        !z <- peekByteOff nptr nlast
                        let buildTbl :: Int -> Word64 -> Int -> IO (Word64, Int)
                            buildTbl !i !msk !skp
                                | i >= nlast = pure (msk .|. swzCI z, skp)
                                | otherwise = do
                                    !c <- peekByteOff nptr i
                                    let !skp' = if c == z then nlen - i - 2 else skp
                                    buildTbl (i + 1) (msk .|. swzCI c) skp'
                        (!mask, !skip) <- buildTbl 0 0 (nlen - 2)
                        let eqSliceCI :: Int -> Int -> IO Bool
                            eqSliceCI !hOff !k
                                | k >= nlen = pure True
                                | otherwise = do
                                    !pByte <- peekByteOff nptr k
                                    !hByte <- peekByteOff hptr (hOff + k)
                                    if pByte == asciiLowerW8 hByte
                                        then eqSliceCI hOff (k + 1)
                                        else pure False
                            loop :: Int -> IO Bool
                            loop !i
                                | i > hlen = pure False
                                | otherwise = do
                                    !lastCand <- peekByteOff hptr (i - 1)
                                    if asciiLowerW8 lastCand == z
                                        then do
                                            matched <- eqSliceCI (i - nlen) 0
                                            if matched
                                                then pure True
                                                else loop (i + skip + 1)
                                        else
                                            if i == hlen
                                                then pure False
                                                else do
                                                    !nextByte <- peekByteOff hptr i
                                                    if mask .&. swzCI nextByte == 0
                                                        then loop (i + nlen + 1)
                                                        else
                                                            if z < 0x61 || z > 0x7A
                                                                then do
                                                                    hit <-
                                                                        c_memchr
                                                                            (hptr `plusPtr` i)
                                                                            (fromIntegral z)
                                                                            (fromIntegral (hlen - i))
                                                                    if hit == nullPtr
                                                                        then pure False
                                                                        else loop ((hit `minusPtr` hptr) + 1)
                                                                else loop (i + 1)
                        loop nlen

decodeIfEligible :: RawPrefilter -> BS.ByteString -> Maybe T.Text
decodeIfEligible prefilt bs
    | evalRawPrefilterBS prefilt bs =
        let !txt = TE.decodeUtf8Lenient bs
         in Just txt
    | otherwise = Nothing
{-# INLINE decodeIfEligible #-}

readTargetFiltered :: RawPrefilter -> OsPath -> IO (Maybe T.Text)
readTargetFiltered = readTargetFilteredWith defaultMmapThreshold
{-# INLINE readTargetFiltered #-}

readTargetFilteredWith :: Int -> RawPrefilter -> OsPath -> IO (Maybe T.Text)
readTargetFilteredWith !_ RawRejectAll _ = pure Nothing
readTargetFilteredWith !mmapThresh prefilt path
    | OS.null path = decodeIfEligible prefilt <$> BS.getContents
    | otherwise = do
        fp <- decodeFS path
        st <- getFileStatus fp
        if not (isRegularFile st)
            then decodeIfEligible prefilt <$> BS.readFile fp
            else
                let !sz = fromIntegral (fileSize st) :: Int
                 in if
                        | sz <= 0 -> pure $! decodeIfEligible prefilt BS.empty
                        | sz < mmapThresh -> do
                            bs <- withBinaryFile fp ReadMode (`BS.hGet` sz)
                            pure $! decodeIfEligible prefilt bs
                        | otherwise ->
                            E.catch
                                ( mmapWithFilePtr fp ReadOnly (Just (0, sz)) $ \(ptr, mappedSz) -> do
                                    let !wptr = castPtr ptr :: Ptr Word8
                                    ok <- evalRawPrefilterPtr prefilt wptr mappedSz
                                    if not ok
                                        then pure Nothing
                                        else do
                                            bs <- BSU.unsafePackCStringLen (castPtr ptr, mappedSz)
                                            let !txt = TE.decodeUtf8Lenient bs
                                            pure (Just txt)
                                )
                                (\(_ :: E.IOException) -> decodeIfEligible prefilt <$> BS.readFile fp)

getTargetContents :: OsPath -> IO T.Text
getTargetContents xs = fromMaybe T.empty <$> readTargetFiltered RawAcceptAll xs
{-# INLINE getTargetContents #-}

-- | Reference file reader (@BS.readFile@ + @TE.decodeUtf8Lenient@) retained for differential tests & benchmarks.
getTargetContentsRef :: OsPath -> IO T.Text
getTargetContentsRef (OS.null -> True) = TE.decodeUtf8Lenient <$> BS.getContents
getTargetContentsRef xs = decodeFS xs >>= fmap TE.decodeUtf8Lenient . BS.readFile
{-# INLINE getTargetContentsRef #-}

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
