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


module CGrep.Distance (distance, (~==)) where

-- from http://www.haskell.org/haskellwiki/Edit_distance
--

distance :: Eq a => [a] -> [a] -> Int
distance a b
    = last (if lab == 0 then mainDiag
        else if lab > 0 then lowers !! (lab - 1)
         else {- < 0 -} uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag _a [] _diags = []
          eachDiag a' (_bch:bs) (lastDiag:diags) = oneDiag a' bs nextDiag lastDiag : eachDiag a' bs diags
              where nextDiag = head (tail diags)
          eachDiag _ _ [] = undefined -- the original implementation does not cover this case...
          oneDiag a' b' diagAbove diagBelow = thisdiag
              where doDiag [] _b _nw _n _w = []
                    doDiag _a [] _nw _n _w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : doDiag as bs me (tail n) (tail w)
                        where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
                    firstelt = 1 + head diagBelow
                    thisdiag = firstelt : doDiag a' b' firstelt diagAbove (tail diagBelow)
          lab = length a - length b
          min3 x y z = if x < y then x else min y z


(~==) :: String -> String -> Bool
a ~== b |  len < 5  = dist < 3
        | otherwise = dist < (len * 40 `div` 100)
    where len  = fromIntegral (length a `min` length b)
          dist = distance a b

