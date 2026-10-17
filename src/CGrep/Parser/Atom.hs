--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.Parser.Atom (
    Atom (..),
    mkAtomFromToken,
    findAllMatches,
    wildCardMap,
) where

import qualified Data.Map as M

import CGrep.Common (trimT)
import CGrep.Distance ((~==))
import CGrep.Parser.Char (isDigit)

import Data.List (tails)

import Options (
    Options (edit_dist, prefix_match, suffix_match, word_match),
 )
import Util (unquoteT)

import qualified CGrep.Parser.Token as T

import qualified Data.Text as T

data Atom
    = Any
    | Keyword
    | NativeType
    | Operator
    | Number
    | Oct
    | Hex
    | String
    | Literal
    | Placeholder T.Text
    | Exact T.Token
    deriving stock (Eq, Ord, Show)

wildCardMap :: M.Map T.Text Atom
wildCardMap =
    M.fromList
        [ ("ANY", Any)
        , ("KEY", Keyword)
        , ("TYP", NativeType)
        , ("OP", Operator)
        , ("OCT", Oct)
        , ("HEX", Hex)
        , ("NUM", Number)
        , ("STR", String)
        , ("LIT", Literal)
        ]

mkAtomFromToken :: T.Token -> Atom
mkAtomFromToken t
    | Just wc <- M.lookup txt wildCardMap = wc
    | T.isTokenIdentifier t =
        if isAtomPlaceholder txt
            then Placeholder txt
            else Exact $ T.mkTokenIdentifier (unescapeAtom txt)
    | otherwise = Exact t
  where
    txt = T.tToken t

{-# INLINE isAtomPlaceholder #-}
isAtomPlaceholder :: T.Text -> Bool
isAtomPlaceholder s =
    case T.uncons s of
        Just ('_', "") -> True
        Just ('_', xs) -> T.all isDigit xs
        _ -> False

unescapeAtom :: T.Text -> T.Text
unescapeAtom s@(T.uncons -> Just ('_', xs))
    | M.member xs wildCardMap || isAtomPlaceholder xs = xs
    | otherwise = s
unescapeAtom xs = xs
{-# INLINE unescapeAtom #-}

findAllMatches :: Options -> [[Atom]] -> [T.Token] -> [T.Token]
findAllMatches opt ws ts = concatMap (\w -> findAllMatches' opt w ts) ws
{-# INLINE findAllMatches #-}

findAllMatches' :: Options -> [Atom] -> [T.Token] -> [T.Token]
findAllMatches' opt as ts =
    let indices = findIndicesBy (doesAtomMatchToken opt) as ts
     in concatMap
            ( \i ->
                let s = extractSlice i (length as) ts
                 in if atomsCheckOccurrences as s
                        then s
                        else []
            )
            indices

extractSlice :: Int -> Int -> [a] -> [a]
extractSlice i n xs = take n (drop i xs)
{-# INLINE extractSlice #-}

-- The pattern _ matches any identifier token, whereas _0, _1, _2, etc. match
-- identifier tokens that must be equal across their respective occurrences.

atomsCheckOccurrences :: [Atom] -> [T.Token] -> Bool
atomsCheckOccurrences as ts = M.foldr checkAndFold True m
  where
    checkAndFold xs acc =
        acc && case xs of
            [] -> True
            (y : ys) -> all (== y) ys
    m =
        M.mapWithKey
            ( \k xs ->
                case k of
                    Placeholder p | p /= "_" -> xs
                    _ -> []
            )
            $ M.fromListWith (<>)
            $ zip as (take (length as) (map (: []) ts))

doesAtomMatchToken :: Options -> Atom -> T.Token -> Bool
doesAtomMatchToken opt (Exact l) r
    | T.isTokenIdentifier l && T.isTokenIdentifier r =
        if
            | word_match opt -> T.tToken l == T.tToken r
            | prefix_match opt -> T.tToken l `T.isPrefixOf` T.tToken r
            | suffix_match opt -> T.tToken l `T.isSuffixOf` T.tToken r
            | edit_dist opt -> (T.unpack . T.tToken) l ~== T.unpack (T.tToken r)
            | otherwise -> T.tToken l `T.isInfixOf` T.tToken r
    | T.isTokenString l && T.isTokenString r =
        if
            | word_match opt -> ls == rs
            | prefix_match opt -> ls `T.isPrefixOf` rs
            | suffix_match opt -> ls `T.isSuffixOf` rs
            | edit_dist opt -> T.unpack ls ~== T.unpack rs
            | otherwise -> ls `T.isInfixOf` rs
    | otherwise = l `T.eqToken` r
  where
    ls = (unquoteT . trimT) (T.tToken l)
    rs = (unquoteT . trimT) (T.tToken r)
doesAtomMatchToken _ Any _ = True
doesAtomMatchToken _ (Placeholder _) t = T.isTokenIdentifier t
doesAtomMatchToken _ Keyword t = T.isTokenKeyword t
doesAtomMatchToken _ NativeType t = T.isTokenNativeType t
doesAtomMatchToken _ Operator t = T.isTokenOperator t
doesAtomMatchToken _ String t = T.isTokenString t
doesAtomMatchToken _ Literal t = T.isTokenString t || T.isTokenNumber t
doesAtomMatchToken _ Number t = T.isTokenNumber t
doesAtomMatchToken _ Oct t = T.isTokenNumber t && case T.uncons (T.tToken t) of Just ('0', T.uncons -> Just (d, _)) -> isDigit d; _ -> False
doesAtomMatchToken _ Hex t = T.isTokenNumber t && case T.uncons (T.tToken t) of Just ('0', T.uncons -> Just ('x', _)) -> True; _ -> False

isPrefixOfBy :: (a -> b -> Bool) -> [a] -> [b] -> Bool
isPrefixOfBy _ [] _ = True
isPrefixOfBy _ (_ : _) [] = False
isPrefixOfBy p (x : xs) (y : ys) = p x y && isPrefixOfBy p xs ys
{-# INLINEABLE isPrefixOfBy #-}

findIndicesBy :: (a -> b -> Bool) -> [a] -> [b] -> [Int]
findIndicesBy p needle haystack =
    [i | (i, tail') <- zip [0 ..] (tails haystack), isPrefixOfBy p needle tail']
{-# INLINE findIndicesBy #-}
