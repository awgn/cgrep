--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Array.Unboxed

import Data.Maybe
import Data.Char

-- from hlint :-)


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = return ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    return ([x | res]++as, [x | not res]++bs)


notNull :: [a] -> Bool
notNull = not . null


xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b


prettyRead :: Read a => String -> String -> a
prettyRead xs err =
    case readMaybe xs of
        Just v -> v
        _      -> error $ err ++ ": parse error near " ++ show (take 40 xs)


readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads


spanGroup :: Int -> [a] -> [[a]]
spanGroup _ [] = []
spanGroup 1 xs = map (: []) xs
spanGroup n xs = take n xs : spanGroup n (tail xs)


toStrict :: LC.ByteString -> C.ByteString
toStrict = C.concat . LC.toChunks


toLowercase :: Char -> Char
toLowercase x = ctypeLowercase ! x
    where ctypeLowercase = listArray ('\0','\255') (map toLower ['\0'..'\255']) :: UArray Char Char


rmQuote :: String -> String
rmQuote []   = []
rmQuote [x]  = [x]
rmQuote y@(x:xs)
    | x == '"' || x == '\'' =  if x == last xs then init xs
                                               else y
    | otherwise = y

