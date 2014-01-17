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
                       getOptionalSubsequence,
                       filterTokensWithWildCards) where

import qualified Data.Map as M

import CGrep.Common
import CGrep.Distance

import Data.Char
import Data.List
import Options 

class (Show t, Ord t) => GenericToken t where
    tkIsIdentifier :: t -> Bool
    tkIsString     :: t -> Bool
    tkIsChar       :: t -> Bool
    tkIsNumber     :: t -> Bool
    tkIsKeyword    :: t -> Bool
    tkToString     :: t -> String
    tkEquivalent   :: t -> t -> Bool


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


getOptionalSubsequence :: [WildCard a] -> [[WildCard a]]
getOptionalSubsequence wc = map (`filterCardIndicies` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $ 
                findIndices (\w -> case w of 
                                    IdentifCard ('$':_) -> True  
                                    _ -> False) wc


filterCardIndicies :: [Int] -> [(Int,WildCard a)] -> [WildCard a]
filterCardIndicies ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps


filterTokensWithWildCards :: (GenericToken a) => Options -> [[WildCard a]] -> [a] -> [a]
filterTokensWithWildCards _ [] _ = []
filterTokensWithWildCards opt (g:gs) ts = 
    concatMap (take grpLen . (`drop` ts)) (findIndices (groupCompare opt g) grp) ++ 
        filterTokensWithWildCards opt gs ts 
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
    |  wildCardMatch opt l r = (True, (l, [tkToString r]))
    |  otherwise             = (False,(AnyCard,[] ))


wildCardMatch :: (GenericToken t) => Options ->  WildCard t -> t -> Bool

wildCardMatch _  AnyCard _         = True
wildCardMatch _  (IdentifCard _) t = tkIsIdentifier t
wildCardMatch _  KeyWordCard t     = tkIsKeyword t
wildCardMatch _  StringCard  t     = tkIsString t
wildCardMatch _  CharCard    t     = tkIsChar t 
wildCardMatch _  NumberCard  t     = tkIsNumber t
wildCardMatch _  OctCard     t     = tkIsNumber t && case (tkToString t) of ('0':d: _)  -> isDigit d; _ -> False
wildCardMatch _  HexCard     t     = tkIsNumber t && case (tkToString t) of ('0':'x':_) -> True; _     -> False

wildCardMatch opt (TokenCard l) r
    | tkIsIdentifier l && tkIsIdentifier r = case () of
                                                _ | edit_dist  opt -> tkToString l ~== tkToString r
                                                  | word_match opt -> tkToString l ==  tkToString r
                                                  | otherwise      -> tkToString l `isInfixOf` tkToString r
    | tkIsString l && tkIsString r = case () of 
                                        _ | edit_dist  opt -> tkToString l ~== tkToString r
                                          | word_match opt -> tkToString l ==  tkToString r
                                          | otherwise      -> trim (tkToString l) `isInfixOf` tkToString r
    | otherwise  = l `tkEquivalent` r 
        where trim = init . tail

