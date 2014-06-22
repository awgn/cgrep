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

module CGrep.Semantic.WildCard (WildCard(..), MultiCard,
                                filterTokensWithMultiCards,
                                wildCardMatch,
                                multiCardMatch) where

import qualified Data.Map as M

import CGrep.Common
import CGrep.Distance
import CGrep.Semantic.Token

import Data.Char
import Data.List
import Options


data WildCard a =  TokenCard a          |
                   AnyCard              |
                   KeyWordCard          |
                   NumberCard           |
                   OctCard              |
                   HexCard              |
                   StringCard           |
                   LiteralCard          |
                   CharCard             |
                   IdentifCard String
                       deriving (Show, Eq, Ord)

type MultiCard a = [WildCard a]


filterTokensWithMultiCards :: (SemanticToken a) => Options -> [MultiCard a] -> [a] -> [a]
filterTokensWithMultiCards opt ws = filterTokensWithMultiCards' opt (spanOptionalCards ws)


filterTokensWithMultiCards' :: (SemanticToken a) => Options -> [[MultiCard a]] -> [a] -> [a]
filterTokensWithMultiCards' _ [] _ = []
filterTokensWithMultiCards' opt (g:gs) ts =
    concatMap (take grpLen . (`drop` ts)) (findIndices (multiCardCompare opt g) grp) ++
        filterTokensWithMultiCards' opt gs ts
    where grp    = spanGroup grpLen ts
          grpLen = length g


spanOptionalCards :: [MultiCard a] -> [[MultiCard a]]
spanOptionalCards wc = map (`filterCardIndicies` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $
                findIndices (\w -> case w of
                                    [IdentifCard ('$':_)] -> True
                                    _ -> False) wc


filterCardIndicies :: [Int] -> [(Int, MultiCard a)] -> [MultiCard a]
filterCardIndicies ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps


multiCardCompare :: (SemanticToken a) => Options -> [MultiCard a] -> [a] -> Bool
multiCardCompare opt l r =
    multiCardCompareAll ts && multiCardCheckOccurences ts
        where ts = multiCardGroupCompare opt l r


{-# INLINE multiCardCompareAll #-}

multiCardCompareAll :: [(Bool, (MultiCard a, [String]))] -> Bool
multiCardCompareAll = all fst


{-# INLINE multiCardCheckOccurences #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in the relative occurrences
--

multiCardCheckOccurences :: (SemanticToken a) => [(Bool, (MultiCard a, [String]))] -> Bool
multiCardCheckOccurences ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m
        where m =  M.mapWithKey (\k xs -> case k of
                                                [IdentifCard "_0"]  -> xs
                                                [IdentifCard "_1"]  -> xs
                                                [IdentifCard "_2"]  -> xs
                                                [IdentifCard "_3"]  -> xs
                                                [IdentifCard "_4"]  -> xs
                                                [IdentifCard "_5"]  -> xs
                                                [IdentifCard "_6"]  -> xs
                                                [IdentifCard "_7"]  -> xs
                                                [IdentifCard "_8"]  -> xs
                                                [IdentifCard "_9"]  -> xs
                                                [IdentifCard "$0"]  -> xs
                                                [IdentifCard "$1"]  -> xs
                                                [IdentifCard "$2"]  -> xs
                                                [IdentifCard "$3"]  -> xs
                                                [IdentifCard "$4"]  -> xs
                                                [IdentifCard "$5"]  -> xs
                                                [IdentifCard "$6"]  -> xs
                                                [IdentifCard "$7"]  -> xs
                                                [IdentifCard "$8"]  -> xs
                                                [IdentifCard "$9"]  -> xs
                                                _                   -> []
                                  ) $ M.fromListWith (++) (map snd ts)


multiCardGroupCompare :: (SemanticToken a) => Options -> [MultiCard a] -> [a] -> [(Bool, (MultiCard a, [String]))]
multiCardGroupCompare opt ls rs
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise              = [ (False, ([AnyCard], [])) ]


tokensZip :: (SemanticToken a) => Options -> MultiCard a -> a -> (Bool, (MultiCard a, [String]))
tokensZip opt l r
    |  multiCardMatch opt l r = (True,  (l, [tkToString r]))
    |  otherwise             =  (False, ([AnyCard],[] ))


multiCardMatch :: (SemanticToken t) => Options ->  MultiCard t -> t -> Bool
multiCardMatch opt m t = any (\w -> wildCardMatch opt w t) m


wildCardMatch :: (SemanticToken t) => Options ->  WildCard t -> t -> Bool
wildCardMatch _  AnyCard _          = True
wildCardMatch _  (IdentifCard _) t  = tkIsIdentifier t
wildCardMatch _  KeyWordCard     t  = tkIsKeyword t
wildCardMatch _  StringCard      t  = tkIsString t
wildCardMatch _  CharCard        t  = tkIsChar t
wildCardMatch _  LiteralCard     t  = tkIsString t || tkIsChar t
wildCardMatch _  NumberCard      t  = tkIsNumber t
wildCardMatch _  OctCard         t  = tkIsNumber t && case tkToString t of ('0':d: _)  -> isDigit d; _ -> False
wildCardMatch _  HexCard         t  = tkIsNumber t && case tkToString t of ('0':'x':_) -> True; _     -> False

wildCardMatch opt (TokenCard l) r
    | tkIsIdentifier l && tkIsIdentifier r = case () of
                                                _ | edit_dist  opt   -> tkToString l ~== tkToString r
                                                  | word_match opt   -> tkToString l ==  tkToString r
                                                  | prefix_match opt -> tkToString l `isPrefixOf`  tkToString r
                                                  | suffix_match opt -> tkToString l `isSuffixOf`  tkToString r
                                                  | otherwise        -> tkToString l `isInfixOf` tkToString r
    | tkIsString l && tkIsString r = case () of
                                        _ | edit_dist  opt   -> ls ~== rs
                                          | word_match opt   -> ls ==  rs
                                          | prefix_match opt -> ls `isPrefixOf` rs
                                          | suffix_match opt -> ls `isSuffixOf` rs
                                          | otherwise        -> ls `isInfixOf`  rs
                                          where ls = unquotes $ trim (tkToString l)
                                                rs = unquotes $ trim (tkToString r)
    | otherwise  = l `tkEquivalent` r

