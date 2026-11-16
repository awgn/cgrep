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

module CGrep.Common (
    runSearch,
    getTargetName,
    getTargetContents,
    getTargetContentsRef,
    RawPrefilter (..),
    CPrefilter,
    WorkerBuffer,
    compileCPrefilter,
    withWorkerBuffer,
    readTargetFilteredBuf,
    scandirBuf,
    dirInodeBuf,
    joinPosixPath,
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
import qualified Data.ByteString.Short as SBS
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
import Foreign.Ptr (castPtr)
import Options (Options (..))
import qualified OsPath as OSP
import Reader (ReaderIO)
import System.IO (IOMode (ReadMode), withBinaryFile)
import System.IO.MMap (Mode (ReadOnly), mmapWithFilePtr)
import System.OsPath
import qualified System.OsString as OS
import System.OsString.Internal.Types (OsString (OsString), PosixString (PosixString))

#ifdef ENABLE_CBITS
import Data.ByteString.Short.Internal (ShortByteString (SBS))
import Foreign.C.Types (CChar (..), CInt (..), CSize (..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca, free, mallocBytes)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (FunPtr, Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekByteOff, poke, pokeByteOff)
import GHC.Exts (
    Int (I#),
    Ptr (Ptr),
    copyAddrToByteArray#,
    copyByteArrayToAddr#,
    newByteArray#,
    unsafeFreezeByteArray#,
 )
import GHC.IO (IO (IO))
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.PosixCompat.Files (fileSize, getFileStatus, isRegularFile)
#else
import Data.ByteString.Short (ShortByteString)
import System.PosixCompat.Files (deviceID, fileID, fileSize, getFileStatus, isRegularFile)
#endif

#ifdef ENABLE_CBITS
foreign import ccall unsafe "string.h memchr"
    c_memchr :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp"
    c_memcmp :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

data CGrepPrefilterC

foreign import ccall unsafe "cgrep_pf_create"
    c_pf_create :: CInt -> CSize -> Ptr Word8 -> Ptr CSize -> IO (Ptr CGrepPrefilterC)

foreign import ccall unsafe "&cgrep_pf_free"
    c_pf_free_ptr :: FunPtr (Ptr CGrepPrefilterC -> IO ())

#ifndef mingw32_HOST_OS
foreign import ccall unsafe "cgrep_read_prefilter"
    c_read_prefilter ::
        Ptr CChar ->
        Ptr CGrepPrefilterC ->
        Ptr Word8 ->
        CSize ->
        Ptr CSize ->
        Ptr CInt ->
        Ptr (Ptr Word8) ->
        IO CInt

foreign import ccall unsafe "cgrep_munmap"
    c_munmap :: Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "cgrep_scandir"
    c_scandir ::
        Ptr CChar ->
        CInt ->
        Ptr Word8 ->
        CSize ->
        Ptr CSize ->
        IO CInt

foreign import ccall unsafe "cgrep_dir_inode"
    c_dir_inode :: Ptr CChar -> Ptr Word64 -> Ptr Word64 -> IO CInt
#endif
#endif

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

#ifdef ENABLE_CBITS
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
#else
evalRawPrefilterBS :: RawPrefilter -> BS.ByteString -> Bool
evalRawPrefilterBS RawAcceptAll !_ = True
evalRawPrefilterBS RawRejectAll !_ = False
evalRawPrefilterBS (RawRequireAnyCS ns) !bs = any (bsContainsCS bs) ns
evalRawPrefilterBS (RawRequireAnyCI ns) !bs = any (bsContainsCI bs) ns
{-# INLINE evalRawPrefilterBS #-}

bsContainsCS :: BS.ByteString -> BS.ByteString -> Bool
bsContainsCS !hay !needle
    | nlen <= 0 = False
    | nlen > hlen = False
    | nlen == 1 = BS.elem (BSU.unsafeHead needle) hay
    | otherwise =
        let !b0 = BSU.unsafeHead needle
            !bLast = BSU.unsafeIndex needle (nlen - 1)
            !maxStart = hlen - nlen
            scan !pos
                | pos > maxStart = False
                | otherwise =
                    case BS.elemIndex b0 (BSU.unsafeTake (maxStart - pos + 1) (BSU.unsafeDrop pos hay)) of
                        Nothing -> False
                        Just rel ->
                            let !hit = pos + rel
                             in ( BSU.unsafeIndex hay (hit + nlen - 1) == bLast
                                    && BSU.unsafeTake nlen (BSU.unsafeDrop hit hay) == needle
                                )
                                    || scan (hit + 1)
         in scan 0
  where
    !hlen = BS.length hay
    !nlen = BS.length needle

bsContainsCI :: BS.ByteString -> BS.ByteString -> Bool
bsContainsCI !hay !needle
    | nlen <= 0 || nlen > hlen = False
    | nlen == 1 =
        let !b0 = BSU.unsafeHead needle
         in BS.elem b0 hay || (b0 >= 0x61 && b0 <= 0x7A && BS.elem (b0 - 0x20) hay)
    | otherwise =
        let !nlast = nlen - 1
            asciiLowerW8 :: Word8 -> Word8
            asciiLowerW8 !w = if w - 0x41 <= 25 then w .|. 0x20 else w
            swzCI :: Word8 -> Word64
            swzCI !w = 1 `unsafeShiftL` (fromIntegral (asciiLowerW8 w) .&. 0x3F)
            !z = BSU.unsafeIndex needle nlast
            buildTbl :: Int -> Word64 -> Int -> (Word64, Int)
            buildTbl !i !msk !skp
                | i >= nlast = (msk .|. swzCI z, skp)
                | otherwise =
                    let !c = BSU.unsafeIndex needle i
                        !skp' = if c == z then nlen - i - 2 else skp
                     in buildTbl (i + 1) (msk .|. swzCI c) skp'
            (!mask, !skip) = buildTbl 0 0 (nlen - 2)
            eqSliceCI !hOff !k
                | k >= nlen = True
                | BSU.unsafeIndex needle k == asciiLowerW8 (BSU.unsafeIndex hay (hOff + k)) = eqSliceCI hOff (k + 1)
                | otherwise = False
            loop !i
                | i > hlen = False
                | asciiLowerW8 (BSU.unsafeIndex hay (i - 1)) == z =
                    eqSliceCI (i - nlen) 0 || loop (i + skip + 1)
                | i == hlen = False
                | mask .&. swzCI (BSU.unsafeIndex hay i) == 0 =
                    loop (i + nlen + 1)
                | z < 0x61 || z > 0x7A =
                    case BS.elemIndex z (BSU.unsafeDrop i hay) of
                        Nothing -> False
                        Just rel -> loop (i + rel + 1)
                | otherwise = loop (i + 1)
         in loop nlen
  where
    !hlen = BS.length hay
    !nlen = BS.length needle
#endif

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
#ifdef ENABLE_CBITS
                                    let !wptr = castPtr ptr :: Ptr Word8
                                    ok <- evalRawPrefilterPtr prefilt wptr mappedSz
                                    if not ok
                                        then pure Nothing
                                        else do
                                            bs <- BSU.unsafePackCStringLen (castPtr ptr, mappedSz)
                                            let !txt = TE.decodeUtf8Lenient bs
                                            pure (Just txt)
#else
                                    bs <- BSU.unsafePackCStringLen (castPtr ptr, mappedSz)
                                    pure $! decodeIfEligible prefilt bs
#endif
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

slashSBS :: ShortByteString
slashSBS = SBS.singleton 0x2F
{-# NOINLINE slashSBS #-}

joinPosixPath :: OsPath -> OsPath -> OsPath
joinPosixPath dirPath baseName
    | OS.null dirPath = baseName
    | OS.null baseName = dirPath
    | otherwise =
        let sbsDir = OSP.toShortByteString dirPath
            sbsBase = OSP.toShortByteString baseName
            !dLen = SBS.length sbsDir
         in if SBS.index sbsDir (dLen - 1) == 0x2F
                then OsString (PosixString (sbsDir <> sbsBase))
                else OsString (PosixString (sbsDir <> slashSBS <> sbsBase))
{-# INLINE joinPosixPath #-}

#ifdef ENABLE_CBITS
newtype CPrefilter = CPrefilter (ForeignPtr CGrepPrefilterC)

compileCPrefilter :: RawPrefilter -> IO CPrefilter
compileCPrefilter RawAcceptAll = do
    p <- c_pf_create 0 0 nullPtr nullPtr
    CPrefilter <$> newForeignPtr c_pf_free_ptr p
compileCPrefilter RawRejectAll = do
    p <- c_pf_create 1 0 nullPtr nullPtr
    CPrefilter <$> newForeignPtr c_pf_free_ptr p
compileCPrefilter (RawRequireAnyCS ns) = compileNeedlesC 2 ns
compileCPrefilter (RawRequireAnyCI ns) = compileNeedlesC 3 ns

compileNeedlesC :: CInt -> [BS.ByteString] -> IO CPrefilter
compileNeedlesC !mode ns = do
    let !packed = BS.concat ns
        !lens = map (fromIntegral . BS.length) ns :: [CSize]
        !cnt = fromIntegral (length ns) :: CSize
    BSU.unsafeUseAsCStringLen packed $ \(cptr, _) ->
        withArray lens $ \lenPtr -> do
            p <- c_pf_create mode cnt (castPtr cptr) lenPtr
            CPrefilter <$> newForeignPtr c_pf_free_ptr p

data WorkerBuffer = WorkerBuffer
    { wbScratchPtr :: !(Ptr Word8)
    , wbScratchCap :: {-# UNPACK #-} !Int
    , wbPathPtr :: !(Ptr CChar)
    , wbPathCap :: {-# UNPACK #-} !Int
    , wbScandirPtr :: !(Ptr Word8)
    , wbScandirCap :: {-# UNPACK #-} !Int
    , wbOutSize :: !(Ptr CSize)
    , wbOutAscii :: !(Ptr CInt)
    , wbOutMmap :: !(Ptr (Ptr Word8))
    }

withWorkerBuffer :: (WorkerBuffer -> IO a) -> IO a
withWorkerBuffer action =
    let !sCap = defaultMmapThreshold
        !pCap = 4096
        !dCap = 131072
     in E.bracket
            ( do
                sPtr <- mallocBytes sCap
                pPtr <- mallocBytes pCap
                dPtr <- mallocBytes dCap
                oSz <- mallocBytes 8
                oAsc <- mallocBytes 8
                oMm <- mallocBytes 8
                pure (WorkerBuffer sPtr sCap pPtr pCap dPtr dCap oSz oAsc oMm)
            )
            ( \WorkerBuffer{..} -> do
                free wbScratchPtr
                free wbPathPtr
                free wbScandirPtr
                free wbOutSize
                free wbOutAscii
                free wbOutMmap
            )
            action

writeOsPathCString :: OsPath -> Ptr CChar -> Int -> IO Bool
writeOsPathCString path dst@(Ptr dstAddr#) !cap =
    let !sbs@(SBS ba#) = OSP.toShortByteString path
        !len@(I# len#) = SBS.length sbs
        hasNul !i
            | i >= len = False
            | SBS.index sbs i == 0 = True
            | otherwise = hasNul (i + 1)
     in if len <= 0 || len >= cap || hasNul 0
            then pure False
            else do
                IO $ \s0 -> (# copyByteArrayToAddr# ba# 0# dstAddr# len# s0, () #)
                pokeByteOff dst len (0 :: Word8)
                pure True
{-# INLINE writeOsPathCString #-}

copyPtrToAsciiText :: Ptr Word8 -> Int -> IO T.Text
copyPtrToAsciiText !_ 0 = pure T.empty
copyPtrToAsciiText (Ptr addr#) !sz@(I# sz#) =
    IO $ \s0 ->
        case newByteArray# sz# s0 of
            (# s1, mba# #) ->
                case copyAddrToByteArray# addr# mba# 0# sz# s1 of
                    s2 ->
                        case unsafeFreezeByteArray# mba# s2 of
                            (# s3, ba# #) -> (# s3, TI.Text (A.ByteArray ba#) 0 sz #)
{-# INLINE copyPtrToAsciiText #-}

decodePtrBuf :: Ptr Word8 -> Int -> CInt -> IO T.Text
decodePtrBuf !ptr !sz !isAsc
    | sz <= 0 = pure T.empty
    | isAsc /= 0 = copyPtrToAsciiText ptr sz
    | otherwise = do
        bs <- BSU.unsafePackCStringLen (castPtr ptr, sz)
        let !txt = TE.decodeUtf8Lenient bs
        pure txt
{-# INLINE decodePtrBuf #-}

readTargetFilteredBuf :: WorkerBuffer -> CPrefilter -> RawPrefilter -> OsPath -> IO (Maybe T.Text)
readTargetFilteredBuf !_ !_ RawRejectAll _ = pure Nothing
#ifdef mingw32_HOST_OS
readTargetFilteredBuf _ _ prefilt path = readTargetFiltered prefilt path
#else
readTargetFilteredBuf WorkerBuffer{..} (CPrefilter pfFPtr) prefilt path
    | OS.null path = readTargetFiltered prefilt path
    | otherwise = do
        okPath <- writeOsPathCString path wbPathPtr wbPathCap
        if not okPath
            then readTargetFiltered prefilt path
            else withForeignPtr pfFPtr $ \pfPtr -> do
                poke wbOutMmap nullPtr
                rc <-
                    c_read_prefilter
                        wbPathPtr
                        pfPtr
                        wbScratchPtr
                        (fromIntegral wbScratchCap)
                        wbOutSize
                        wbOutAscii
                        wbOutMmap
                case rc of
                    0 -> pure Nothing
                    1 -> do
                        !sz <- fromIntegral <$> peek wbOutSize
                        !isAsc <- peek wbOutAscii
                        Just <$> decodePtrBuf wbScratchPtr sz isAsc
                    2 -> do
                        !szC <- peek wbOutSize
                        let !sz = fromIntegral szC :: Int
                        !isAsc <- peek wbOutAscii
                        !mptr <- peek wbOutMmap
                        Just <$> E.finally (decodePtrBuf mptr sz isAsc) (c_munmap mptr szC)
                    _ -> readTargetFiltered prefilt path
#endif

ptrToOsPath :: Ptr Word8 -> Int -> IO OsPath
ptrToOsPath !_ 0 = pure mempty
ptrToOsPath (Ptr addr#) !(I# sz#) =
    IO $ \s0 ->
        case newByteArray# sz# s0 of
            (# s1, mba# #) ->
                case copyAddrToByteArray# addr# mba# 0# sz# s1 of
                    s2 ->
                        case unsafeFreezeByteArray# mba# s2 of
                            (# s3, ba# #) -> (# s3, OsString (PosixString (SBS ba#)) #)
{-# INLINE ptrToOsPath #-}

-- | Fast POSIX directory scan returning @Just ([(subdirName, isSymlinkDir)], [regFileName])@,
-- or @Nothing@ if fallback to @System.Directory.OsPath@ is required.
scandirBuf :: WorkerBuffer -> Bool -> OsPath -> IO (Maybe ([(OsPath, Bool)], [OsPath]))
#ifdef mingw32_HOST_OS
scandirBuf _ _ _ = pure Nothing
#else
scandirBuf WorkerBuffer{..} !followSymlinks dirPath = do
    okPath <- writeOsPathCString dirPath wbPathPtr wbPathCap
    if not okPath
        then pure Nothing
        else do
            let !fFlag = if followSymlinks then 1 else 0
            rc <- c_scandir wbPathPtr fFlag wbScandirPtr (fromIntegral wbScandirCap) wbOutSize
            if
                | rc == 0 -> do
                    !used <- fromIntegral <$> peek wbOutSize
                    Just <$> unpackScandir wbScandirPtr used
                | rc == (-2) -> do
                    let !bigCap = 2 * 1024 * 1024
                    E.bracket (mallocBytes bigCap) free $ \bigPtr -> do
                        rc2 <- c_scandir wbPathPtr fFlag bigPtr (fromIntegral bigCap) wbOutSize
                        if rc2 == 0
                            then do
                                !used <- fromIntegral <$> peek wbOutSize
                                Just <$> unpackScandir bigPtr used
                            else pure Nothing
                | otherwise -> pure (Just ([], []))
  where
    unpackScandir :: Ptr Word8 -> Int -> IO ([(OsPath, Bool)], [OsPath])
    unpackScandir !buf !totalBytes = go 0 [] []
      where
        go !off dirsAcc filesAcc
            | off + 3 > totalBytes = pure (reverse dirsAcc, reverse filesAcc)
            | otherwise = do
                !kind <- peekW8 buf off
                !lo <- peekW8 buf (off + 1)
                !hi <- peekW8 buf (off + 2)
                let !nlen = fromIntegral lo + (fromIntegral hi `unsafeShiftL` 8)
                    !nextOff = off + 3 + nlen
                if nextOff > totalBytes
                    then pure (reverse dirsAcc, reverse filesAcc)
                    else do
                        !nm <- ptrToOsPath (buf `plusPtr` (off + 3)) nlen
                        case kind of
                            1 -> go nextOff dirsAcc (nm : filesAcc)
                            2 -> go nextOff ((nm, False) : dirsAcc) filesAcc
                            3 -> go nextOff ((nm, True) : dirsAcc) filesAcc
                            _ -> go nextOff dirsAcc filesAcc
#endif

dirInodeBuf :: WorkerBuffer -> OsPath -> IO (Maybe (Word64, Word64))
#ifdef mingw32_HOST_OS
dirInodeBuf _ _ = pure Nothing
#else
dirInodeBuf WorkerBuffer{..} dirPath = do
    okPath <- writeOsPathCString dirPath wbPathPtr wbPathCap
    if not okPath
        then pure Nothing
        else alloca $ \pDev ->
            alloca $ \pIno -> do
                rc <- c_dir_inode wbPathPtr pDev pIno
                if rc == 0
                    then do
                        !d <- peek pDev
                        !i <- peek pIno
                        pure (Just (d, i))
                    else pure Nothing
#endif
#else
data CPrefilter = CPrefilter

compileCPrefilter :: RawPrefilter -> IO CPrefilter
compileCPrefilter !_ = pure CPrefilter
{-# INLINE compileCPrefilter #-}

data WorkerBuffer = WorkerBuffer

withWorkerBuffer :: (WorkerBuffer -> IO a) -> IO a
withWorkerBuffer action = action WorkerBuffer
{-# INLINE withWorkerBuffer #-}

readTargetFilteredBuf :: WorkerBuffer -> CPrefilter -> RawPrefilter -> OsPath -> IO (Maybe T.Text)
readTargetFilteredBuf !_ !_ = readTargetFiltered
{-# INLINE readTargetFilteredBuf #-}

scandirBuf :: WorkerBuffer -> Bool -> OsPath -> IO (Maybe ([(OsPath, Bool)], [OsPath]))
scandirBuf !_ !_ !_ = pure Nothing
{-# INLINE scandirBuf #-}

dirInodeBuf :: WorkerBuffer -> OsPath -> IO (Maybe (Word64, Word64))
dirInodeBuf !_ dirPath =
    E.catch
        ( do
            fp <- decodeFS dirPath
            st <- getFileStatus fp
            pure (Just (fromIntegral (deviceID st), fromIntegral (fileID st)))
        )
        (\(_ :: E.IOException) -> pure Nothing)
#endif

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
