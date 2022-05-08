--
-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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

import Data.Array.Unboxed ( (!), listArray, UArray )

import Data.Maybe ( listToMaybe )
import Data.Char ( toLower )

-- from hlint :-)

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)
{-# INLINE partitionM #-}


notNull :: [a] -> Bool
notNull = not . null
{-# INLINE notNull #-}

xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b
{-# INLINE xor #-}

prettyRead :: Read a => String -> String -> a
prettyRead xs err =
    case readMaybe xs of
        Just v -> v
        _      -> error $ err ++ ": parse error near " ++ show (take 40 xs)


readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads
{-# INLINE readMaybe #-}


spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs = take n xs : spanGroup n (tail xs)
{-# INLINE spanGroup #-}


toLowercase :: Char -> Char
toLowercase x = ctypeLowercase ! x
    where ctypeLowercase = listArray ('\0','\255') (map toLower ['\0'..'\255']) :: UArray Char Char
{-# INLINE  toLowercase #-}


rmQuote :: String -> String
rmQuote []   = []
rmQuote [x]  = [x]
rmQuote y@(x:xs)
    | x == '"' || x == '\'' =  if x == last xs then init xs
                                               else y
    | otherwise = y
{-# INLINE  rmQuote #-}
