--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

import CGrep.Common (trim, trimT)
import CGrep.Distance ((~==))
import CGrep.Parser.Char (isDigit)

import Data.List (
    findIndices,
    isInfixOf,
    isPrefixOf,
    isSuffixOf,
    sort,
    subsequences,
    tails,
 )
import Options (
    Options (edit_dist, prefix_match, suffix_match, word_match),
 )
import Util (unquoteT, spanGroup)

import qualified CGrep.Parser.Chunk as T
import qualified CGrep.Parser.Token as T

import Data.Containers.ListUtils (nubOrd)
import Data.Function (on)
import Data.List (groupBy)
import Data.List.Extra (sortOn)
import Debug.Trace
import GHC.Stack (errorWithStackTrace)
import qualified Data.Text as T

data Atom
    = Any
    | Keyword
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
        , ("OCT", Oct)
        , ("HEX", Hex)
        , ("NUM", Number)
        , ("STR", String)
        , ("LIT", String)
        ]

mkAtomFromToken :: T.Token -> Atom
mkAtomFromToken t
    | T.isTokenIdentifier t = case () of
        _
            | Just wc <- M.lookup str wildCardMap -> wc
            | isAtomPlaceholder str -> Placeholder str
            | otherwise -> Exact $ T.mkTokenIdentifier (unescapeAtom str) (T.tOffset t)
          where
            str = T.tToken t
    | otherwise = Exact t

{-# INLINE isAtomPlaceholder #-}
isAtomPlaceholder :: T.Text -> Bool
isAtomPlaceholder s =
    if
        | Just (x, T.uncons -> Just (y, xs)) <- T.uncons s -> wprefix x && isDigit y
        | Just (x, "") <- T.uncons s -> wprefix x
        | otherwise -> errorWithoutStackTrace "CGrep: isAtomIdentifier"
  where
    wprefix x = x == '$' || x == '_'

unescapeAtom :: T.Text -> T.Text
unescapeAtom (T.uncons -> Just ('$', xs)) = xs
unescapeAtom (T.uncons -> Just ('_', xs)) = xs
unescapeAtom xs = xs
{-# INLINE unescapeAtom #-}

findAllMatches :: Options -> [[Atom]] -> [T.Token] -> [T.Token]
findAllMatches opt ws ts = nubOrd . nubOrd $ concatMap (\w -> findAllMatches' opt w ts) ws
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

-- The pattern _ matches any token, whereas _1, _2, etc. match tokens that must be equal across their respective occurrences.

atomsCheckOccurrences :: [Atom] -> [T.Token] -> Bool
atomsCheckOccurrences as ts = M.foldr checkAndFold True (m)
  where
    checkAndFold xs acc =
        acc && case xs of
            [] -> True
            (y : ys) -> all (== y) ys
    m =
        M.mapWithKey
            ( \k xs ->
                case k of
                    Placeholder "_0" -> xs
                    Placeholder "_1" -> xs
                    Placeholder "_2" -> xs
                    Placeholder "_3" -> xs
                    Placeholder "_4" -> xs
                    Placeholder "_5" -> xs
                    Placeholder "_6" -> xs
                    Placeholder "_7" -> xs
                    Placeholder "_8" -> xs
                    Placeholder "_9" -> xs
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
doesAtomMatchToken _ String t = T.isTokenString t
doesAtomMatchToken _ Literal t = T.isTokenString t
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
    [i | (i, tail) <- zip [0 ..] (tails haystack), isPrefixOfBy p needle tail]
{-# INLINE findIndicesBy #-}
