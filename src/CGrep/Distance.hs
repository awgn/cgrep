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


module CGrep.Distance (distance, (~==)) where

-- from http://www.haskell.org/haskellwiki/Edit_distance
--

distance :: Eq a => [a] -> [a] -> Int
distance a b
    = last (if lab == 0 then mainDiag
                        else if lab > 0 then lowers !! (lab - 1)
                                        else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
          uppers = eachDiag a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag b a (mainDiag : lowers) -- lower diagonals
          eachDiag _ [] _ = []
          eachDiag a' (_:bs) (lastDiag:diags) = oneDiag a' bs nextDiag lastDiag : eachDiag a' bs diags
              where nextDiag = head (tail diags)
          oneDiag a' b' diagAbove diagBelow = thisdiag
              where doDiag [] _ _ _ _ = []
                    doDiag _ [] _ _ _ = []
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
