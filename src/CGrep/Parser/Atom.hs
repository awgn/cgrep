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

module CGrep.Parser.Atom (Atom(..), Atoms,
                                mkAtomFromToken,
                                combineAtoms,
                                filterTokensWithAtoms,
                                wildCardMap,
                                wildCardMatch,
                                wildCardsMatch) where

import qualified Data.Map as M

import CGrep.Common ( trim, trim8 )
import CGrep.Distance ( (~==) )
import CGrep.Parser.Char ( isDigit )

import Data.List
    ( isSuffixOf, findIndices, isInfixOf, isPrefixOf, subsequences )
import Options
    ( Options(edit_dist, word_match, prefix_match, suffix_match) )
import Util ( spanGroup, rmQuote8 )

import qualified CGrep.Parser.Token as T
import qualified Data.ByteString.Char8 as C

data Atom =
    Token T.Token  |
    Any            |
    Keyword        |
    Number         |
    Oct            |
    Hex            |
    String         |
    Literal        |
    Identifier  C.ByteString
        deriving (Show, Eq, Ord)


type Atoms = [Atom]


wildCardMap :: M.Map C.ByteString Atom
wildCardMap = M.fromList
            [
                ("ANY", Any     ),
                ("KEY", Keyword ),
                ("OCT", Oct     ),
                ("HEX", Hex     ),
                ("NUM", Number  ),
                ("STR", String  ),
                ("LIT", String  )
            ]


mkAtomFromToken :: T.Token -> Atom
mkAtomFromToken t
    | T.isIdentifier t = case () of
        _ | Just wc <- M.lookup str wildCardMap -> wc
          | isAtomIdentifier str                -> Identifier str
          | otherwise                           -> Token $ T.TokenIdentifier (rmAtomEscape str) (T.toOffset t)
            where str = T.toString t
    | otherwise = Token t


combineAtoms :: [Atoms] -> [Atoms]
combineAtoms (m1:r@(m2:m3:ms))
    | [Token b] <- m2, T.toString b == "OR" = combineAtoms $ (m1<>m3):ms
    | otherwise      =  m1 : combineAtoms r
combineAtoms [m1,m2] =  [m1,m2]
combineAtoms [m1]    =  [m1]
combineAtoms []      =  []


{-# INLINE filterTokensWithAtoms #-}
filterTokensWithAtoms :: Options -> [Atoms] -> [T.Token] -> [T.Token]
filterTokensWithAtoms opt ws ts = go opt (spanOptionalCards ws) ts
    where go  :: Options -> [[Atoms]] -> [T.Token] -> [T.Token]
          go  _ [] _ = []
          go opt (g:gs) ts =
              {-# SCC "atomConcatMap" #-} concatMap (take grpLen . (`drop` ts)) ({-# SCC "atomicFindIndices" #-} findIndices (wildCardsCompare opt g) grp) <> go opt gs ts
              where grp    = {-# SCC "atomSpanGroup" #-} spanGroup grpLen ts
                    grpLen = length g


spanOptionalCards :: [Atoms] -> [[Atoms]]
spanOptionalCards wc = map (`filterCardIndices` wc') idx
    where wc' = zip [0..] wc
          idx = subsequences $
                findIndices (\case
                                [Identifier (C.uncons -> Just ('$', _))] -> True
                                _ -> False) wc


filterCardIndices :: [Int] -> [(Int, Atoms)] -> [Atoms]
filterCardIndices ns ps = map snd $ filter (\(n, _) -> n `notElem` ns) ps
{-# INLINE filterCardIndices #-}


wildCardsCompare :: Options -> [Atoms] -> [T.Token] -> Bool
wildCardsCompare opt l r =
    wildCardsCompareAll ts && wildCardsCheckOccurrences ts
        where ts = wildCardsGroupCompare opt l r
{-# INLINE wildCardsCompare #-}


isAtomIdentifier :: C.ByteString -> Bool
isAtomIdentifier s =
        if | Just (x, C.uncons -> Just (y, xs)) <- C.uncons s -> wprefix x && isDigit y
           | Just (x, "")                       <- C.uncons s -> wprefix x
           | otherwise                                        -> error "isAtomIdentifier"
    where wprefix x = x == '$' || x == '_'


rmAtomEscape :: C.ByteString -> C.ByteString
rmAtomEscape (C.uncons -> Just ('$',xs)) = xs
rmAtomEscape (C.uncons -> Just ('_',xs)) = xs
rmAtomEscape xs = xs
{-# INLINE rmAtomEscape #-}


wildCardsCompareAll :: [(Bool, (Atoms, [C.ByteString]))] -> Bool
wildCardsCompareAll = all fst
{-# INLINE wildCardsCompareAll #-}

-- Note: pattern $ and _ match any token, whereas $1 $2 (_1 _2 etc.) match tokens
--       that must compare equal in the respective occurrences

wildCardsCheckOccurrences :: [(Bool, (Atoms, [C.ByteString]))] -> Bool
wildCardsCheckOccurrences ts =  M.foldr (\xs r -> r && all (== head xs) xs) True m
    where m =  M.mapWithKey (\k xs ->
                case k of
                    [Identifier "_0"]  -> xs
                    [Identifier "_1"]  -> xs
                    [Identifier "_2"]  -> xs
                    [Identifier "_3"]  -> xs
                    [Identifier "_4"]  -> xs
                    [Identifier "_5"]  -> xs
                    [Identifier "_6"]  -> xs
                    [Identifier "_7"]  -> xs
                    [Identifier "_8"]  -> xs
                    [Identifier "_9"]  -> xs
                    [Identifier "$0"]  -> xs
                    [Identifier "$1"]  -> xs
                    [Identifier "$2"]  -> xs
                    [Identifier "$3"]  -> xs
                    [Identifier "$4"]  -> xs
                    [Identifier "$5"]  -> xs
                    [Identifier "$6"]  -> xs
                    [Identifier "$7"]  -> xs
                    [Identifier "$8"]  -> xs
                    [Identifier "$9"]  -> xs
                    _                  -> []
                ) $ M.fromListWith (<>) (map snd ts)
{-# INLINE wildCardsCheckOccurrences #-}


wildCardsGroupCompare :: Options -> [Atoms] -> [T.Token] -> [(Bool, (Atoms, [C.ByteString]))]
wildCardsGroupCompare opt ls rs
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise              = [ (False, ([Any], [])) ]
{-# INLINE wildCardsGroupCompare #-}


tokensZip :: Options -> Atoms -> T.Token -> (Bool, (Atoms, [C.ByteString]))
tokensZip opt l r
    |  wildCardsMatch opt l r = (True,  (l, [T.toString r]))
    |  otherwise              = (False, ([Any],[] ))
{-# INLINE tokensZip #-}


wildCardsMatch :: Options ->  Atoms -> T.Token -> Bool
wildCardsMatch opt m t = any (\w -> wildCardMatch opt w t) m
{-# INLINE wildCardsMatch #-}

wildCardMatch :: Options -> Atom -> T.Token -> Bool
wildCardMatch _  Any _          = True
wildCardMatch _  (Identifier _) t  = T.isIdentifier t
wildCardMatch _  Keyword     t  = T.isKeyword t
wildCardMatch _  String      t  = T.isString t
wildCardMatch _  Literal     t  = T.isString t
wildCardMatch _  Number      t  = T.isNumber t
wildCardMatch _  Oct         t  = T.isNumber t && case C.uncons (T.toString t) of Just ('0', C.uncons -> Just (d, _))  -> isDigit d; _ -> False
wildCardMatch _  Hex         t  = T.isNumber t && case C.uncons (T.toString t) of Just ('0', C.uncons -> Just ('x',_)) -> True; _      -> False
wildCardMatch opt (Token l) r
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
