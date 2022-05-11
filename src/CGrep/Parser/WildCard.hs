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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module CGrep.Parser.WildCard (WildCard(..), MultiCard,
                                mkWildCardFromToken,
                                combineMultiCard,
                                filterTokensWithMultiCards,
                                wildCardMap,
                                wildCardMatch,
                                multiCardMatch) where

import qualified Data.Map as M

import CGrep.Common ( trim, trim8 )
import CGrep.Distance ( (~==) )

import Data.Char ( isNumber, isDigit )
import Data.List
    ( isSuffixOf, findIndices, isInfixOf, isPrefixOf, subsequences )
import Options
    ( Options(edit_dist, word_match, prefix_match, suffix_match) )
import Util ( spanGroup, rmQuote, rmQuote8 )

import qualified CGrep.Parser.Token as T
import qualified Data.ByteString.Char8 as C

data WildCard =
    TokenCard T.Token  |
    AnyCard            |
    KeyWordCard        |
    NumberCard         |
    OctCard            |
    HexCard            |
    StringCard         |
    LiteralCard        |
    IdentifCard  C.ByteString
        deriving (Show, Eq, Ord)


type MultiCard = [WildCard]


wildCardMap :: M.Map C.ByteString WildCard
wildCardMap = M.fromList
            [
                ("ANY", AnyCard     ),
                ("KEY", KeyWordCard ),
                ("OCT", OctCard     ),
                ("HEX", HexCard     ),
                ("NUM", NumberCard  ),
                ("STR", StringCard  ),
                ("LIT", StringCard  )
            ]


mkWildCardFromToken :: T.Token -> WildCard
mkWildCardFromToken t
    | T.isIdentifier t = case () of
        _ | Just wc <- M.lookup str wildCardMap -> wc
          | isWildCardIdentif str               -> IdentifCard str
          | otherwise                           -> TokenCard $ T.TokenIdentifier (rmWildCardEscape str) (T.toOffset t)
            where str = T.toString t
    | otherwise = TokenCard t


combineMultiCard :: [MultiCard] -> [MultiCard]
combineMultiCard (m1:r@(m2:m3:ms))
    | [TokenCard b] <- m2, T.toString b == "OR" = combineMultiCard $ (m1++m3):ms
    | otherwise          =  m1 : combineMultiCard r
combineMultiCard [m1,m2] =  [m1,m2]
combineMultiCard [m1]    =  [m1]
combineMultiCard []      =  []


filterTokensWithMultiCards :: Options -> [MultiCard] -> [T.Token] -> [T.Token]
filterTokensWithMultiCards opt ws = filterTokensWithMultiCards' opt (spanOptionalCards ws)
{-# INLINE filterTokensWithMultiCards #-}


filterTokensWithMultiCards' :: Options -> [[MultiCard]] -> [T.Token] -> [T.Token]
filterTokensWithMultiCards' _ [] _ = []
filterTokensWithMultiCards' opt (g:gs) ts =
    concatMap (take grpLen . (`drop` ts)) (findIndices (multiCardCompare opt g) grp) ++
        filterTokensWithMultiCards' opt gs ts
    where grp    = spanGroup grpLen ts
          grpLen = length g


spanOptionalCards :: [MultiCard] -> [[MultiCard]]
spanOptionalCards wc = map (`filterCardIndices` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $
                findIndices (\case
                                [IdentifCard (C.uncons -> Just ('$', _))] -> True
                                _ -> False) wc


filterCardIndices :: [Int] -> [(Int, MultiCard)] -> [MultiCard]
filterCardIndices ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps
{-# INLINE filterCardIndices #-}


multiCardCompare :: Options -> [MultiCard] -> [T.Token] -> Bool
multiCardCompare opt l r =
    multiCardCompareAll ts && multiCardCheckOccurences ts
        where ts = multiCardGroupCompare opt l r
{-# INLINE multiCardCompare #-}


isWildCardIdentif :: C.ByteString -> Bool
isWildCardIdentif s =
        if | Just (x, C.uncons -> Just (y, xs)) <- C.uncons s -> wprefix x && isNumber y
           | Just (x, "")                       <- C.uncons s -> wprefix x
           | otherwise                                        -> error "isWildCardIdentif"
    where wprefix x = x == '$' || x == '_'


rmWildCardEscape :: C.ByteString -> C.ByteString
rmWildCardEscape (C.uncons -> Just ('$',xs)) = xs
rmWildCardEscape (C.uncons -> Just ('_',xs)) = xs
rmWildCardEscape xs = xs
{-# INLINE rmWildCardEscape #-}


multiCardCompareAll :: [(Bool, (MultiCard, [C.ByteString]))] -> Bool
multiCardCompareAll = all fst
{-# INLINE multiCardCompareAll #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in the respective occurrences

multiCardCheckOccurences :: [(Bool, (MultiCard, [C.ByteString]))] -> Bool
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



multiCardGroupCompare :: Options -> [MultiCard] -> [T.Token] -> [(Bool, (MultiCard, [C.ByteString]))]
multiCardGroupCompare opt ls rs
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise              = [ (False, ([AnyCard], [])) ]
{-# INLINE multiCardGroupCompare #-}


tokensZip :: Options -> MultiCard -> T.Token -> (Bool, (MultiCard, [C.ByteString]))
tokensZip opt l r
    |  multiCardMatch opt l r = (True,  (l, [T.toString r]))
    |  otherwise              = (False, ([AnyCard],[] ))
{-# INLINE tokensZip #-}


multiCardMatch :: Options ->  MultiCard -> T.Token -> Bool
multiCardMatch opt m t = any (\w -> wildCardMatch opt w t) m
{-# INLINE multiCardMatch #-}

wildCardMatch :: Options -> WildCard -> T.Token -> Bool
wildCardMatch _  AnyCard _          = True
wildCardMatch _  (IdentifCard _) t  = T.isIdentifier t
wildCardMatch _  KeyWordCard     t  = T.isKeyword t
wildCardMatch _  StringCard      t  = T.isString t
wildCardMatch _  LiteralCard     t  = T.isString t
wildCardMatch _  NumberCard      t  = T.isNumber t
wildCardMatch _  OctCard         t  = T.isNumber t && case C.uncons (T.toString t) of Just ('0', C.uncons -> Just (d, _))  -> isDigit d; _ -> False
wildCardMatch _  HexCard         t  = T.isNumber t && case C.uncons (T.toString t) of Just ('0', C.uncons -> Just ('x',_))-> True; _      -> False
wildCardMatch opt (TokenCard l) r
    | T.isIdentifier l && T.isIdentifier r =
        if | edit_dist  opt   -> (C.unpack . T.toString) l ~== C.unpack (T.toString r)
           | word_match opt   -> T.toString l ==  T.toString r
           | prefix_match opt -> T.toString l `C.isPrefixOf`  T.toString r
           | suffix_match opt -> T.toString l `C.isSuffixOf`  T.toString r
           | otherwise        -> T.toString l `C.isInfixOf` T.toString r
    | T.isString l && T.isString r = case () of
        _ | edit_dist  opt   -> C.unpack ls ~== C.unpack rs
          | word_match opt   -> ls ==  rs
          | prefix_match opt -> ls `C.isPrefixOf` rs
          | suffix_match opt -> ls `C.isSuffixOf` rs
          | otherwise        -> ls `C.isInfixOf`  rs
            where ls = rmQuote8 $ trim8 (T.toString l)
                  rs = rmQuote8 $ trim8 (T.toString r)
    | otherwise  = l `T.tokenEqual` r
