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

{-# LANGUAGE ExistentialQuantification #-} 

module CGrep.Output (Output(..), Match, mkOutput, showOutput) where

import CGrep.StringLike
import Options

type Match  = (Int, [String])

data Output = forall a. (StringLike a) => Output FilePath Int a [String]

mkOutput :: (StringLike a) => FilePath -> a -> [Match] -> [Output]
mkOutput f source ms = map (\(n, xs) -> Output f n (ls !! (n-1)) xs) ms 
    where ls = slLines source  


showOutput :: Options -> Output -> String
showOutput Options { no_filename = False, no_linenumber = False , count = False } (Output f n l _) = f ++ ":" ++ show n ++ ":" ++ slToString l
showOutput Options { no_filename = False, no_linenumber = True  , count = False } (Output f _ l _) = f ++ ":" ++ slToString l
showOutput Options { no_filename = True , no_linenumber = False , count = False } (Output _ n l _) = show n ++ ":" ++ slToString l
showOutput Options { no_filename = True , no_linenumber = True  , count = False } (Output _ _ l _) = slToString l
showOutput Options { count = True } (Output f n _ _) = f ++ ":" ++ show n


