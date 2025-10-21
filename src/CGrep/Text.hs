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

module CGrep.Text (
    iterM,
    textIndices,
) where

import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Internal.Search as TIS
import Data.Int (Int64)

iterM :: (Monad m) => T.Text -> ((# Char, Int #) -> m ()) -> m ()
iterM txt f = go 0
  where
    len = TU.lengthWord8 txt
    go !off
        | off >= len = return ()
        | otherwise = do
            let TU.Iter c delta = TU.iter txt off
            f (# c, off #)
            go (off + delta)


textIndices :: [T.Text] -> T.Text -> [[Int]]
textIndices ps text = (`TIS.indices` text) <$> ps
{-# INLINE textIndices #-}
