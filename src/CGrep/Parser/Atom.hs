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
    mkAtomsFromTokens,
    findAllMatches,
    wildCardMap,
) where

import qualified Data.Map.Strict as M

import CGrep.Common (trimT)
import CGrep.Distance ((~==))
import CGrep.Parser.Char (isDigit)

import Data.List (tails)
import Data.Maybe (mapMaybe)

import Options (
    Options (edit_dist, prefix_match, suffix_match, word_match),
 )
import Util (unquoteT)

import qualified CGrep.Parser.Token as T

import qualified Data.Text as T

data Atom
    = Any
    | Ellipsis
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

-- | Converts a sequence of pattern tokens into semantic Atoms, recognizing
-- three contiguous dots `...` (or a single `"..."` token) as `Ellipsis`.
mkAtomsFromTokens :: [T.Token] -> [Atom]
mkAtomsFromTokens [] = []
mkAtomsFromTokens (t1 : t2 : t3 : rest)
    | T.tToken t1 == "."
    , T.tToken t2 == "."
    , T.tToken t3 == "."
    , T.tOffset t2 == T.tOffset t1 + 1
    , T.tOffset t3 == T.tOffset t2 + 1 =
        collapseEllipsis (Ellipsis : mkAtomsFromTokens rest)
mkAtomsFromTokens (t : rest)
    | T.tToken t == "..." =
        collapseEllipsis (Ellipsis : mkAtomsFromTokens rest)
    | otherwise =
        mkAtomFromToken t : mkAtomsFromTokens rest

collapseEllipsis :: [Atom] -> [Atom]
collapseEllipsis (Ellipsis : Ellipsis : xs) = collapseEllipsis (Ellipsis : xs)
collapseEllipsis xs = xs

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

maxEllipsisTokens :: Int
maxEllipsisTokens = 64

findAllMatches :: Options -> [[Atom]] -> [T.Token] -> [T.Token]
findAllMatches opt ws ts = concatMap (\w -> findAllMatches' opt w ts) ws
{-# INLINE findAllMatches #-}

findAllMatches' :: Options -> [Atom] -> [T.Token] -> [T.Token]
findAllMatches' _ [] _ = []
findAllMatches' opt as ts =
    concat $ mapMaybe (matchAtoms opt M.empty as) (tails ts)

-- | Matches a pattern of `Atom`s against the prefix of a token stream,
-- verifying placeholder unification (`_0`, `_1`, ...) on the fly and
-- supporting bracket-balanced `Ellipsis` (`...`) matching.
matchAtoms :: Options -> M.Map T.Text T.Token -> [Atom] -> [T.Token] -> Maybe [T.Token]
matchAtoms _ _ [] _ = Just []
matchAtoms _ _ [Ellipsis] _ = Just []
matchAtoms opt bindings (Ellipsis : as) ts =
    matchEllipsis 0 maxEllipsisTokens [] ts
  where
    matchEllipsis :: Int -> Int -> [T.Token] -> [T.Token] -> Maybe [T.Token]
    matchEllipsis !depth !remaining acc curTs =
        let tryHere =
                if depth == 0
                    then case matchAtoms opt bindings as curTs of
                        Just restMatched -> Just (reverse acc ++ restMatched)
                        Nothing -> Nothing
                    else Nothing
         in case tryHere of
                Just matched -> Just matched
                Nothing
                    | remaining <= 0 -> Nothing
                    | otherwise ->
                        case curTs of
                            [] -> Nothing
                            (t : rest)
                                | T.isTokenBracket t && isOpenBracket (T.tToken t) ->
                                    matchEllipsis (depth + 1) (remaining - 1) (t : acc) rest
                                | T.isTokenBracket t && isCloseBracket (T.tToken t) ->
                                    if depth == 0
                                        then Nothing
                                        else matchEllipsis (depth - 1) (remaining - 1) (t : acc) rest
                                | depth == 0 && T.isTokenOperator t && T.tToken t == ";" ->
                                    Nothing
                                | otherwise ->
                                    matchEllipsis depth (remaining - 1) (t : acc) rest

    isOpenBracket :: T.Text -> Bool
    isOpenBracket b = b == "(" || b == "[" || b == "{"

    isCloseBracket :: T.Text -> Bool
    isCloseBracket b = b == ")" || b == "]" || b == "}"
matchAtoms _ _ (_ : _) [] = Nothing
matchAtoms opt bindings (Placeholder p : as) (t : ts)
    | not (T.isTokenIdentifier t) = Nothing
    | p == "_" = (t :) <$> matchAtoms opt bindings as ts
    | otherwise =
        case M.lookup p bindings of
            Just boundTok
                | boundTok == t -> (t :) <$> matchAtoms opt bindings as ts
                | otherwise -> Nothing
            Nothing ->
                (t :) <$> matchAtoms opt (M.insert p t bindings) as ts
matchAtoms opt bindings (a : as) (t : ts)
    | doesAtomMatchToken opt a t = (t :) <$> matchAtoms opt bindings as ts
    | otherwise = Nothing

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
doesAtomMatchToken _ Ellipsis _ = True
doesAtomMatchToken _ (Placeholder _) t = T.isTokenIdentifier t
doesAtomMatchToken _ Keyword t = T.isTokenKeyword t
doesAtomMatchToken _ NativeType t = T.isTokenNativeType t
doesAtomMatchToken _ Operator t = T.isTokenOperator t
doesAtomMatchToken _ String t = T.isTokenString t
doesAtomMatchToken _ Literal t = T.isTokenString t || T.isTokenNumber t
doesAtomMatchToken _ Number t = T.isTokenNumber t
doesAtomMatchToken _ Oct t = T.isTokenNumber t && case T.uncons (T.tToken t) of Just ('0', T.uncons -> Just (d, _)) -> isDigit d; _ -> False
doesAtomMatchToken _ Hex t = T.isTokenNumber t && case T.uncons (T.tToken t) of Just ('0', T.uncons -> Just ('x', _)) -> True; _ -> False
