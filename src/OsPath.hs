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
--
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
