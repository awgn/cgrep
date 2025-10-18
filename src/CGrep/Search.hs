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

module CGrep.Search (
    searchStringIndices,
    searchStringTaggedIndices,
    eligibleForSearch,
    TaggedIx (..),
) where

import Data.Int (Int64)
import GHC.Exts (groupWith)
import Data.List.Extra (notNull)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as TS

searchStringIndices :: [T.Text] -> T.Text -> [[Int]]
searchStringIndices ps text = (`TS.indices` text) <$> ps
{-# INLINE searchStringIndices #-}

data TaggedIx a = TaggedIx
    { index :: {-# UNPACK #-} !Int
    , tags :: [a]
    }
    deriving stock (Show)

instance Eq (TaggedIx a) where
    (TaggedIx i1 _) == (TaggedIx i2 _) = i1 == i2

instance Ord (TaggedIx a) where
    compare (TaggedIx i1 _) (TaggedIx i2 _) = compare i1 i2

searchStringTaggedIndices :: [(T.Text, a)] -> T.Text -> [TaggedIx a]
searchStringTaggedIndices ps text =
    let res =
            ps >>= \p ->
                let pat = fst p
                    tag = snd p
                    ids = TS.indices pat text
                 in (\i -> TaggedIx (fromIntegral i) [tag]) <$> ids
     in mapMaybe fuseGroup (groupWith index res)
  where
    {-# INLINE fuseGroup #-}
    fuseGroup :: [TaggedIx a] -> Maybe (TaggedIx a)
    fuseGroup [] = Nothing
    fuseGroup (x:xs) = Just $ TaggedIx (index x) $ concatMap tags (x:xs)

eligibleForSearch :: [a] -> [[Int]] -> Bool
eligibleForSearch [_] = all notNull
eligibleForSearch _ = any notNull
{-# INLINE eligibleForSearch #-}
