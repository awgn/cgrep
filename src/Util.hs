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

import qualified Data.ByteString.Char8 as C
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)), (|>))
import qualified Data.Sequence as S
import Text.Read (readMaybe)

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
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n l@(x : xs) = take n l : spanGroup n xs
{-# INLINE spanGroup #-}

spanGroupSeq :: Int -> S.Seq a -> [S.Seq a]
spanGroupSeq _ S.Empty = []
spanGroupSeq 1 xs = [xs]
spanGroupSeq n xs = S.take n xs : spanGroupSeq n (S.drop 1 xs)
{-# INLINE spanGroupSeq #-}

rmQuote :: String -> String
rmQuote [] = []
rmQuote [x] = [x]
rmQuote y@(x : xs)
    | x == '"' || x == '\'' =
        if x == last xs
            then init xs
            else y
    | otherwise = y
{-# INLINE rmQuote #-}

rmQuote8 :: C.ByteString -> C.ByteString
rmQuote8 b
    | C.length b < 2 = b
    | otherwise =
        case C.uncons b of
            Just (x, xs) -> if (x == '"' || x == '\'') && (x == C.last b) then C.init xs else b
            _ -> b
{-# INLINE rmQuote8 #-}

mapMaybe' :: (Foldable f) => (a -> Maybe b) -> f a -> [b]
mapMaybe' f = foldr g []
  where
    g x rest
        | Just y <- f x = y : rest
        | otherwise = rest

findWithIndex :: forall a. (a -> Bool) -> [a] -> (# Int, Maybe a #)
findWithIndex predicate = go predicate 0
  where
    go :: forall a. (a -> Bool) -> Int -> [a] -> (# Int, Maybe a #)
    go p _ [] = (# 0, Nothing #)
    go p !index (x : xs)
        | p x = (# index, Just x #)
        | otherwise = go p (index + 1) xs
