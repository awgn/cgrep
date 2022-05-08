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
{-# LANGUAGE MultiWayIf #-}

module CGrep.Parser.WildCard (WildCard(..), MultiCard,
                                mkWildCardFromToken,
                                combineMultiCard,
                                filterTokensWithMultiCards,
                                wildCardMap,
                                wildCardMatch,
                                multiCardMatch) where

import qualified Data.Map as M

import CGrep.Common ( trim )
import CGrep.Distance ( (~==) )
import CGrep.Parser.SemanticToken ( SemanticToken(..) )

import Data.Char ( isNumber, isDigit )
import Data.List
    ( isSuffixOf, findIndices, isInfixOf, isPrefixOf, subsequences )
import Options
    ( Options(edit_dist, word_match, prefix_match, suffix_match) )
import Util ( spanGroup, rmQuote )


data WildCard a =
    TokenCard a        |
    AnyCard            |
    KeyWordCard        |
    NumberCard         |
    OctCard            |
    HexCard            |
    StringCard         |
    LiteralCard        |
    CharCard           |
    IdentifCard String
        deriving (Show, Eq, Ord)



type MultiCard a = [WildCard a]


wildCardMap :: M.Map String (WildCard a)
wildCardMap = M.fromList
            [
                ("ANY", AnyCard     ),
                ("KEY", KeyWordCard ),
                ("OCT", OctCard     ),
                ("HEX", HexCard     ),
                ("NUM", NumberCard  ),
                ("CHR", CharCard    ),
                ("STR", StringCard  ),
                ("LIT", StringCard  )
            ]


mkWildCardFromToken :: (SemanticToken a) => a -> WildCard a
mkWildCardFromToken t
    | tkIsIdentifier t = case () of
        _ | Just wc <- M.lookup str wildCardMap -> wc
          | isWildCardIdentif str               -> IdentifCard str
          | otherwise                           -> TokenCard $ tkToIdentif (rmWildCardEscape str) (tkToOffset t)
            where str = tkToString t
    | otherwise = TokenCard t


combineMultiCard :: (SemanticToken a) => [MultiCard a] -> [MultiCard a]
combineMultiCard (m1:r@(m2:m3:ms))
    | [TokenCard b] <- m2, tkToString b == "OR" = combineMultiCard $ (m1++m3):ms
    | otherwise          =  m1 : combineMultiCard r
combineMultiCard [m1,m2] =  [m1,m2]
combineMultiCard [m1]    =  [m1]
combineMultiCard []      =  []


filterTokensWithMultiCards :: (SemanticToken a) => Options -> [MultiCard a] -> [a] -> [a]
filterTokensWithMultiCards opt ws = filterTokensWithMultiCards' opt (spanOptionalCards ws)
{-# INLINE filterTokensWithMultiCards #-}


filterTokensWithMultiCards' :: (SemanticToken a) => Options -> [[MultiCard a]] -> [a] -> [a]
filterTokensWithMultiCards' _ [] _ = []
filterTokensWithMultiCards' opt (g:gs) ts =
    concatMap (take grpLen . (`drop` ts)) (findIndices (multiCardCompare opt g) grp) ++
        filterTokensWithMultiCards' opt gs ts
    where grp    = spanGroup grpLen ts
          grpLen = length g


spanOptionalCards :: [MultiCard a] -> [[MultiCard a]]
spanOptionalCards wc = map (`filterCardIndices` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $
                findIndices (\case
                                [IdentifCard ('$':_)] -> True
                                _ -> False) wc


filterCardIndices :: [Int] -> [(Int, MultiCard a)] -> [MultiCard a]
filterCardIndices ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps
{-# INLINE filterCardIndices #-}


multiCardCompare :: (SemanticToken a) => Options -> [MultiCard a] -> [a] -> Bool
multiCardCompare opt l r =
    multiCardCompareAll ts && multiCardCheckOccurences ts
        where ts = multiCardGroupCompare opt l r
{-# INLINE multiCardCompare #-}


isWildCardIdentif :: String -> Bool
isWildCardIdentif s =
        if | (x:y:_) <- s  -> wprefix x && isNumber y
           | [x]     <- s  -> wprefix x
           | otherwise     -> error "isWildCardIdentif"
    where wprefix x = x == '$' || x == '_'


rmWildCardEscape :: String -> String
rmWildCardEscape ('$':xs) = xs
rmWildCardEscape ('_':xs) = xs
rmWildCardEscape xs = xs
{-# INLINE rmWildCardEscape #-}


multiCardCompareAll :: [(Bool, (MultiCard a, [String]))] -> Bool
multiCardCompareAll = all fst
{-# INLINE multiCardCompareAll #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in the respective occurrences

multiCardCheckOccurences :: (SemanticToken a) => [(Bool, (MultiCard a, [String]))] -> Bool
multiCardCheckOccurences ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m
    where m =  M.mapWithKey (\k xs ->
                case k of
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
{-# INLINE multiCardCheckOccurences #-}



multiCardGroupCompare :: (SemanticToken a) => Options -> [MultiCard a] -> [a] -> [(Bool, (MultiCard a, [String]))]
multiCardGroupCompare opt ls rs
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise              = [ (False, ([AnyCard], [])) ]
{-# INLINE multiCardGroupCompare #-}


tokensZip :: (SemanticToken a) => Options -> MultiCard a -> a -> (Bool, (MultiCard a, [String]))
tokensZip opt l r
    |  multiCardMatch opt l r = (True,  (l, [tkToString r]))
    |  otherwise              = (False, ([AnyCard],[] ))
{-# INLINE tokensZip #-}


multiCardMatch :: (SemanticToken t) => Options ->  MultiCard t -> t -> Bool
multiCardMatch opt m t = any (\w -> wildCardMatch opt w t) m
{-# INLINE multiCardMatch #-}

wildCardMatch :: (SemanticToken t) => Options ->  WildCard t -> t -> Bool
wildCardMatch _  AnyCard _          = True
wildCardMatch _  (IdentifCard _) t  = tkIsIdentifier t
wildCardMatch _  KeyWordCard     t  = tkIsKeyword t
wildCardMatch _  StringCard      t  = tkIsString t
wildCardMatch _  CharCard        t  = tkIsChar t
wildCardMatch _  LiteralCard     t  = tkIsString t || tkIsChar t
wildCardMatch _  NumberCard      t  = tkIsNumber t
wildCardMatch _  OctCard         t  = tkIsNumber t && case tkToString t of ('0':d: _)  -> isDigit d; _ -> False
wildCardMatch _  HexCard         t  = tkIsNumber t && case tkToString t of ('0':'x':_) -> True; _      -> False
wildCardMatch opt (TokenCard l) r
    | tkIsIdentifier l && tkIsIdentifier r =
        if | edit_dist  opt   -> tkToString l ~== tkToString r
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
            where ls = rmQuote $ trim (tkToString l)
                  rs = rmQuote $ trim (tkToString r)
    | otherwise  = l `tkEqual` r
