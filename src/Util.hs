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

module Util where

import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence as S
import Text.Read (readMaybe)
import qualified Data.Text as T

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x : xs) = do
    res <- f x
    (as, bs) <- partitionM f xs
    return ([x | res] <> as, [x | not res] <> bs)
{-# INLINE partitionM #-}

xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b
{-# INLINE xor #-}

prettyRead :: (Read a) => String -> String -> a
prettyRead xs err =
    case readMaybe xs of
        Just v -> v
        _ -> errorWithoutStackTrace $ err <> ": parse error near '" <> take 40 xs <> "'"

spanGroup :: Int -> [a] -> [[a]]
spanGroup n xs
  | length xs < n = [] -- Stop if the remaining list is shorter than n
  | otherwise     = take n xs : spanGroup n (drop 1 xs)

spanGroupSeq :: Int -> S.Seq a -> [S.Seq a]
spanGroupSeq _ S.Empty = []
spanGroupSeq 1 xs = [xs]
spanGroupSeq n xs = S.take n xs : spanGroupSeq n (S.drop 1 xs)
{-# INLINE spanGroupSeq #-}

-- | Removes a single pair of matching quotes ('"' or '\'')
-- | from the beginning and end of a Text.
unquoteT :: T.Text -> T.Text
unquoteT txt =
    case T.uncons txt of
        -- Check if 'x' is a quote we care about
        Just (x, xs) | x == '"' || x == '\'' ->
            case T.unsnoc xs of
                Just (inner, y) | x == y -> inner
                _ -> txt
        _ -> txt -- Text was empty or first char wasn't a quote
{-# INLINE unquoteT #-}

mapMaybe' :: (Foldable f) => (a -> Maybe b) -> f a -> [b]
mapMaybe' f = foldr g []
  where
    g x rest
        | Just y <- f x = y : rest
        | otherwise = rest

findWithIndex :: forall a. (a -> Bool) -> [a] -> (# Int, Maybe a #)
findWithIndex predicate = go predicate 0
  where
    go :: (a -> Bool) -> Int -> [a] -> (# Int, Maybe a #)
    go _ _ [] = (# 0, Nothing #)
    go p !index (x : xs)
        | p x = (# index, Just x #)
        | otherwise = go p (index + 1) xs
