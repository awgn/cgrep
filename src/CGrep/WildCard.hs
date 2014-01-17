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

module CGrep.WildCard (WildCard(..), GenericToken(..), 
                       mkWildCard, 
                       getWildCardSubsequence,
                       filterWildCardToken) where

import qualified Data.Map as M

import CGrep.Common

import Data.List
import Options 


class (Show t, Ord t) => GenericToken t where
    isIdentifier    :: t -> Bool
    getString       :: t -> String
    wildCardMatch :: Options ->  WildCard t -> t -> Bool


data WildCard a =  TokenCard a          |
                   AnyCard              |
                   KeyWordCard          |
                   NumberCard           |
                   OctCard              |
                   HexCard              |
                   StringCard           |
                   CharCard             |
                   IdentifCard String 
                       deriving (Show, Eq, Ord)      


wildCardMap :: M.Map String (WildCard a) 
wildCardMap = M.fromList 
            [
                ("ANY", AnyCard    ),
                ("KEY", KeyWordCard),
                ("OCT", OctCard    ),
                ("HEX", HexCard    ),
                ("NUM", NumberCard ),
                ("CHR", CharCard   ),
                ("STR", StringCard )
            ]


mkWildCard :: (GenericToken a) => a -> WildCard a
mkWildCard  t
    | isIdentifier t = case () of 
                        _  |  Just wc <-  M.lookup str wildCardMap -> wc
                           | ('$':_)  <- getString t               -> IdentifCard str
                           | ('_':_)  <- getString t               -> IdentifCard str
                           | otherwise                             -> TokenCard t
    | otherwise      = TokenCard t
        where str = getString t


getWildCardSubsequence :: [WildCard a] -> [[WildCard a]]
getWildCardSubsequence wc = map (`filterCardIndicies` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $ 
                findIndices (\w -> case w of 
                                    IdentifCard ('$':_) -> True  
                                    _ -> False) wc


filterCardIndicies :: [Int] -> [(Int,WildCard a)] -> [WildCard a]
filterCardIndicies ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps


filterWildCardToken :: (GenericToken a) => Options -> FilePath -> [[WildCard a]] -> [a] -> [a]
filterWildCardToken _ _ [] _ = []
filterWildCardToken opt f (g:gs) ts = 
    concatMap (take grpLen . (`drop` ts)) (findIndices (groupCompare opt g) grp) ++ 
        filterWildCardToken opt f gs ts 
    where grp    = spanGroup grpLen ts
          grpLen = length g


groupCompare :: (GenericToken a) => Options -> [WildCard a] -> [a] -> Bool
groupCompare opt l r = 
    groupCompareAll ts && groupCompareOptional ts
        where ts = tokensGroupCompare opt l r               


{-# INLINE groupCompareAll #-}

groupCompareAll :: [(Bool, (WildCard a, [String]))] -> Bool
groupCompareAll = all fst 


{-# INLINE groupCompareOptional #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in the relative occurrences  
--       

groupCompareOptional :: (GenericToken a) => [(Bool, (WildCard a, [String]))] -> Bool
groupCompareOptional ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m  
        where m =  M.mapWithKey (\k xs -> case k of 
                                                AnyCard          -> [] 
                                                NumberCard       -> [] 
                                                KeyWordCard      -> []
                                                StringCard       -> []
                                                CharCard         -> []
                                                HexCard          -> []
                                                OctCard          -> []
                                                IdentifCard "_"  -> []
                                                IdentifCard "$"  -> []
                                                _                -> xs
                                  ) $ M.fromListWith (++) (map snd ts)
        

tokensGroupCompare :: (GenericToken a) => Options -> [WildCard a] -> [a] -> [(Bool, (WildCard a, [String]))]
tokensGroupCompare opt ls rs 
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise = [ (False, (AnyCard, [])) ]


tokensZip :: (GenericToken a) => Options -> WildCard a -> a -> (Bool, (WildCard a, [String]))
tokensZip opt l r 
    |  wildCardMatch opt l r = (True, (l, [getString r]))
    |  otherwise             = (False,(AnyCard,[] ))

