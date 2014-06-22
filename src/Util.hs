--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the StopNerds Public License as published by
-- the StopNerds Foundation; either version 1 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- StopNerds Public License for more details.
--
-- You should have received a copy of the StopNerds Public License
-- along with this program; if not, see <http://stopnerds.org/license/>.
--

module Util where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import Data.Maybe


toStrict :: LC.ByteString -> C.ByteString
toStrict = C.concat . LC.toChunks


toMaybe :: a -> Bool -> Maybe a
toMaybe a True  = Just a
toMaybe _ False = Nothing


notNull :: [a] -> Bool
notNull = not . null


xor :: Bool -> Bool -> Bool
a `xor` b = a && not b || not a && b


prettyRead :: Read a => String -> String -> a
prettyRead xs err =
    case value of
        Just v -> v
        _      -> error $ err ++ ": parse error near " ++ show(take 40 xs)
        where value = readMaybe xs


readMaybe :: Read a => String -> Maybe a
readMaybe = fmap fst . listToMaybe . reads



