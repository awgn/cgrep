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

import CGrep.Types (Text8)
import Data.Int (Int64)
import GHC.Exts (groupWith)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Search as BM
import qualified Data.ByteString.Search.DFA as DFA

import qualified Data.ByteString.Lazy.Search as LBM
import qualified Data.ByteString.Lazy.Search.DFA as LDFA
import Data.List.Extra (notNull)

findIndices :: Text8 -> Text8 -> [Int]
findIndices p =
    if C.length p <= 3
        then DFA.indices p
        else BM.indices p
{-# INLINE findIndices #-}

searchStringIndices :: [Text8] -> Text8 -> [[Int64]]
searchStringIndices ps text = ps >>= \p -> [fromIntegral <$> p `findIndices` text]
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

-- >>> searchStringTaggedIndices [("a",2),("b",1),("a",0), ("he", 42)] "aheba"
-- [TaggedIx {index = 0, tags = [2,0]},TaggedIx {index = 1, tags = [42]},TaggedIx {index = 3, tags = [1]},TaggedIx {index = 4, tags = [2,0]}]

searchStringTaggedIndices :: [(Text8, a)] -> Text8 -> [TaggedIx a]
searchStringTaggedIndices ps text =
    let res =
            ps >>= \p ->
                let pat = fst p
                    tag = snd p
                    ids = findIndices pat text
                 in (\i -> TaggedIx (fromIntegral i) [tag]) <$> ids
     in fuseGroup <$> groupWith index res
  where
    {-# INLINE fuseGroup #-}
    fuseGroup :: [TaggedIx a] -> TaggedIx a
    fuseGroup xs = TaggedIx (index $ head xs) $ concatMap tags xs

eligibleForSearch :: [a] -> [[Int64]] -> Bool
eligibleForSearch [_] = all notNull
eligibleForSearch _ = any notNull
{-# INLINE eligibleForSearch #-}
