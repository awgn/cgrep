module OsPath (
    fromByteString,
    fromText,
    toShortByteString,
    toText,
    toFilePath,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString, toShort)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.OsString.Data.ByteString.Short (fromShort)
import System.OsString.Internal.Types

toText :: OsString -> T.Text
toText = TE.decodeUtf8Lenient . fromShort . getPosixString . getOsString
{-# INLINE toText #-}

toShortByteString :: OsString -> ShortByteString
toShortByteString = getPosixString . getOsString
{-# INLINE toShortByteString #-}

toFilePath :: OsString -> FilePath
toFilePath = show . getPosixString . getOsString
{-# INLINE toFilePath #-}

fromByteString :: ByteString -> OsString
fromByteString bs = OsString $ PosixString (toShort bs)
{-# INLINE fromByteString #-}

fromText :: T.Text -> OsString
fromText = OsString . PosixString . toShort . TE.encodeUtf8
{-# INLINE fromText #-}
