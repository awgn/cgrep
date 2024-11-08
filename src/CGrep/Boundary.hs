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

module CGrep.Boundary (
    Boundary (..),
    BoundaryType (..),
    pattern Begin,
    pattern End,
) where

import qualified Data.ByteString.Char8 as C
import Data.Word (Word8)

data Boundary = Boundary
    { bBegin :: C.ByteString
    , bEnd :: C.ByteString
    }
    deriving stock (Show, Eq)

newtype BoundaryType = BoundaryType {unpackBoundaryType :: Word8}
    deriving newtype (Eq, Ord)

instance Show BoundaryType where
    show Begin = "begin"
    show End = "end"

pattern Begin :: BoundaryType
pattern Begin = BoundaryType 0
pattern End :: BoundaryType
pattern End = BoundaryType 1

{-# COMPLETE Begin, End #-}