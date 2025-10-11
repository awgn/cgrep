module OsPath (
    toShortByteString,
    toByteString,
    toFilePath,
    fromByteString,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import System.OsPath (OsPath, decodeUtf)
import System.OsString.Data.ByteString.Short (fromShort)
import System.OsString.Internal.Types

toShortByteString :: OsString -> ShortByteString
toShortByteString = getPosixString . getOsString
{-# INLINE toShortByteString #-}

toByteString :: OsString -> ByteString
toByteString = fromShort . getPosixString . getOsString
{-# INLINE toByteString #-}

toFilePath :: OsString -> FilePath
toFilePath = show . getPosixString . getOsString
{-# INLINE toFilePath #-}

fromByteString :: ByteString -> OsString
fromByteString bs = OsString $ PosixString (toShort bs)
{-# INLINE fromByteString #-}
