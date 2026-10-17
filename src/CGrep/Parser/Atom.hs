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
    extractAtomMatchers,
    findAllMatches,
    wildCardMap,
) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M

import CGrep.Common (trimT)
import CGrep.Distance ((~==))
import CGrep.Parser.Char (isDigit)
import CGrep.Parser.Chunk (Chunk (..), pattern ChunkString)

import Data.List (tails)
import Data.Maybe (isNothing, mapMaybe)

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
    | Not Atom
    | Or Atom Atom
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

isCombinatorKeyword :: T.Text -> Bool
isCombinatorKeyword txt = txt == "NOT" || txt == "OR"

-- | Extracts literal text matchers from a pattern for shallow file pre-filtering (`textContainsOneOf`).
-- Note: `Not` atoms are excluded so negated tokens are not required to exist in the file.
-- `Or` atoms contribute matchers only if both branches are `Exact` (when there are no other
-- mandatory `Exact` tokens in the pattern).
extractAtomMatchers :: [Atom] -> [T.Text]
extractAtomMatchers atoms =
    let mandatory = mapMaybe exactText atoms
     in if not (null mandatory)
            then mandatory
            else concatMap orTexts atoms
  where
    exactText :: Atom -> Maybe T.Text
    exactText (Exact (T.Token (Chunk ChunkString xs))) = Just ((unquoteT . trimT) xs)
    exactText (Exact t) = Just (T.tToken t)
    exactText _ = Nothing

    orTexts :: Atom -> [T.Text]
    orTexts (Or a b) =
        let la = orBranchTexts a
            lb = orBranchTexts b
         in if not (null la) && not (null lb) then la ++ lb else []
    orTexts _ = []

    orBranchTexts :: Atom -> [T.Text]
    orBranchTexts (Or a b) = orBranchTexts a ++ orBranchTexts b
    orBranchTexts a = case exactText a of
        Just txt -> [txt]
        Nothing -> []

-- | Converts a sequence of pattern tokens into semantic Atoms, recognizing:
-- * `...` as `Ellipsis`
-- * `NOT <atom>` as `Not <atom>`
-- * `<atom1> OR <atom2>` as `Or <atom1> <atom2>`
mkAtomsFromTokens :: [T.Token] -> [Atom]
mkAtomsFromTokens = collapseEllipsis . parseOrTokens . parseNotAndBaseTokens
  where
    -- Step 1: Parse Ellipsis (`...`), `NOT`, and base Atoms, tagging `"OR"` tokens via `Left ()`
    parseNotAndBaseTokens :: [T.Token] -> [Either () Atom]
    parseNotAndBaseTokens [] = []
    parseNotAndBaseTokens (t1 : t2 : t3 : rest)
        | T.tToken t1 == "."
        , T.tToken t2 == "."
        , T.tToken t3 == "."
        , T.tOffset t2 == T.tOffset t1 + 1
        , T.tOffset t3 == T.tOffset t2 + 1 =
            Right Ellipsis : parseNotAndBaseTokens rest
    parseNotAndBaseTokens (t : rest)
        | T.tToken t == "..." =
            Right Ellipsis : parseNotAndBaseTokens rest
        | T.tToken t == "OR" =
            Left () : parseNotAndBaseTokens rest
        | T.tToken t == "NOT" =
            case parseNotAndBaseTokens rest of
                (Right nextAtom : remAtoms) | nextAtom /= Ellipsis ->
                    Right (Not nextAtom) : remAtoms
                other ->
                    Right (Exact (T.mkTokenIdentifier "NOT")) : other
        | otherwise =
            Right (mkAtomFromToken t) : parseNotAndBaseTokens rest

    -- Step 2: Combine `Right a : Left () : ...` into right-associative `Or a b`
    parseOrTokens :: [Either () Atom] -> [Atom]
    parseOrTokens [] = []
    parseOrTokens (Right a : Left () : rest) =
        case parseOrTokens rest of
            (b : remAtoms) | a /= Ellipsis && b /= Ellipsis ->
                Or a b : remAtoms
            other ->
                a : Exact (T.mkTokenIdentifier "OR") : other
    parseOrTokens (Left () : rest) =
        Exact (T.mkTokenIdentifier "OR") : parseOrTokens rest
    parseOrTokens (Right a : rest) =
        a : parseOrTokens rest

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
    | M.member xs wildCardMap || isCombinatorKeyword xs || isAtomPlaceholder xs = xs
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
-- verifying placeholder unification (`_0`, `_1`, ...), `Not`, `Or`, and
-- bracket-balanced `Ellipsis` (`...`) matching on the fly.
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
matchAtoms opt bindings (a : as) (t : ts) = do
    bindings' <- matchSingleAtom opt bindings a t
    (t :) <$> matchAtoms opt bindings' as ts

-- | Matches a single non-Ellipsis Atom against a single Token, updating placeholder bindings.
matchSingleAtom :: Options -> M.Map T.Text T.Token -> Atom -> T.Token -> Maybe (M.Map T.Text T.Token)
matchSingleAtom _ bindings (Placeholder p) t
    | not (T.isTokenIdentifier t) = Nothing
    | p == "_" = Just bindings
    | otherwise =
        case M.lookup p bindings of
            Just boundTok
                | boundTok == t -> Just bindings
                | otherwise -> Nothing
            Nothing -> Just (M.insert p t bindings)
matchSingleAtom opt bindings (Not a) t
    | isNothing (matchSingleAtom opt bindings a t) = Just bindings
    | otherwise = Nothing
matchSingleAtom opt bindings (Or a b) t =
    matchSingleAtom opt bindings a t <|> matchSingleAtom opt bindings b t
matchSingleAtom opt bindings a t
    | doesAtomMatchToken opt a t = Just bindings
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
doesAtomMatchToken opt (Not a) t = not (doesAtomMatchToken opt a t)
doesAtomMatchToken opt (Or a b) t = doesAtomMatchToken opt a t || doesAtomMatchToken opt b t
