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
    extractAtomAnchors,
    findAllMatches,
    findAllMatchesRef,
    wildCardMap,
) where

import Control.Applicative ((<|>))
import qualified Data.Map.Strict as M

import CGrep.Common (trimT)
import CGrep.Distance (EditPattern, matchEditPattern, mkEditPattern, textApproxEq)
import CGrep.Parser.Char (isAlphaNum, isDigit)
import CGrep.Parser.Chunk (Chunk (..), ChunkType, pattern ChunkString)
import CGrep.Text (
    isAsciiText,
    textEqCI,
    textIsInfixOf,
    textIsInfixOfCI,
    textIsPrefixOfCI,
    textIsSuffixOfCI,
 )

import Data.List (maximumBy, tails)
import Data.Maybe (isNothing, mapMaybe)
import Data.Ord (comparing)

import Options (
    Options (edit_dist, ignore_case, prefix_match, suffix_match, word_match),
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

-- | Extracts literal text matchers from a pattern for line-slicing (@sliceToLastMatch@).
-- Note: `Not` atoms are excluded so negated tokens are not required to exist in the file.
-- `Or` atoms contribute matchers only if both branches are `Exact` (when there are no other
-- mandatory `Exact` tokens in the pattern).
extractAtomMatchers :: [Atom] -> [T.Text]
extractAtomMatchers atoms =
    let mandatory = mapMaybe exactAtomText atoms
     in if not (null mandatory)
            then mandatory
            else concatMap orAtomTexts atoms

-- | Extracts the single most selective sound disjunctive requirement @[T.Text]@ for file
-- eligibility pre-filtering (@eligibleForSearch@ / @RawPrefilter@).
-- Every @Exact@ atom in @atoms@ is a mandatory requirement (the file MUST contain ALL of them),
-- so requiring the single highest-selectivity @Exact@ token (preferring identifier/word characters
-- over punctuation like @"("@ or @"*"@, and longer byte length) avoids false-positive wakeups
-- on ubiquitous operators/brackets while remaining 100% sound.
extractAtomAnchors :: [Atom] -> [T.Text]
extractAtomAnchors atoms =
    let candidates =
            [[t] | t <- mapMaybe exactAtomText atoms]
                ++ filter (not . null) (map orAtomTexts atoms)
     in case candidates of
            [] -> []
            cs -> maximumBy (comparing groupScore) cs
  where
    anchorScore :: T.Text -> (Bool, Int)
    anchorScore t = (T.any (\c -> isAlphaNum c || c == '_') t, T.length t)

    groupScore :: [T.Text] -> ((Bool, Int), Int)
    groupScore g = (minimum (map anchorScore g), negate (length g))

exactAtomText :: Atom -> Maybe T.Text
exactAtomText (Exact (T.Token (Chunk ChunkString xs))) =
    let s = (unquoteT . trimT) xs
     in if T.null s then Nothing else Just s
exactAtomText (Exact t) =
    let s = T.tToken t
     in if T.null s then Nothing else Just s
exactAtomText _ = Nothing

orAtomTexts :: Atom -> [T.Text]
orAtomTexts (Or a b) =
    let la = orBranchTexts a
        lb = orBranchTexts b
     in if not (null la) && not (null lb) then la ++ lb else []
  where
    orBranchTexts (Or x y) = orBranchTexts x ++ orBranchTexts y
    orBranchTexts x = case exactAtomText x of
        Just txt -> [txt]
        Nothing -> []
orAtomTexts _ = []

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

data SliceMatcher
    = SMWordCS !T.Text
    | SMWordCI !Bool !T.Text
    | SMPrefixCS !T.Text
    | SMPrefixCI !Bool !T.Text
    | SMSuffixCS !T.Text
    | SMSuffixCI !Bool !T.Text
    | SMEdit !Bool !EditPattern
    | SMInfixCS !T.Text
    | SMInfixCI !Bool !T.Text

mkSliceMatcher :: Options -> T.Text -> SliceMatcher
mkSliceMatcher opt raw =
    let !ic = ignore_case opt
        !pat = if ic then T.toLower raw else raw
        !asc = isAsciiText pat
     in if
            | word_match opt -> if ic then SMWordCI asc pat else SMWordCS pat
            | prefix_match opt -> if ic then SMPrefixCI asc pat else SMPrefixCS pat
            | suffix_match opt -> if ic then SMSuffixCI asc pat else SMSuffixCS pat
            | edit_dist opt -> SMEdit ic (mkEditPattern pat)
            | otherwise -> if ic then SMInfixCI asc pat else SMInfixCS pat

evalSliceMatcher :: SliceMatcher -> T.Text -> Bool
evalSliceMatcher m cand =
    case m of
        SMWordCS pat -> pat == cand
        SMWordCI asc pat -> textEqCI asc pat cand
        SMPrefixCS pat -> pat `T.isPrefixOf` cand
        SMPrefixCI asc pat -> textIsPrefixOfCI asc pat cand
        SMSuffixCS pat -> pat `T.isSuffixOf` cand
        SMSuffixCI asc pat -> textIsSuffixOfCI asc pat cand
        SMEdit ic ep -> matchEditPattern ep (if ic then T.toLower cand else cand)
        SMInfixCS pat -> pat `textIsInfixOf` cand
        SMInfixCI asc pat -> textIsInfixOfCI asc pat cand
{-# INLINE evalSliceMatcher #-}

data CompiledAtom
    = CAny
    | CEllipsis
    | CWildIdent
    | CNamedPlaceholder !T.Text
    | CKeyword
    | CNativeType
    | COperator
    | CString
    | CLiteral
    | CNumber
    | COct
    | CHex
    | CExactIdentCS !SliceMatcher
    | CExactIdentCI !Bool !T.Text !SliceMatcher
    | CExactStringEmpty
    | CExactString !SliceMatcher
    | CExactWordCI !Bool !T.Text
    | CExactTypedCI !ChunkType !Bool !T.Text
    | CExactRaw !ChunkType !T.Text
    | CNot !CompiledAtom
    | COr !CompiledAtom !CompiledAtom

isCEllipsis :: CompiledAtom -> Bool
isCEllipsis CEllipsis = True
isCEllipsis _ = False
{-# INLINE isCEllipsis #-}

hasNamedPlaceholder :: CompiledAtom -> Bool
hasNamedPlaceholder (CNamedPlaceholder _) = True
hasNamedPlaceholder (CNot a) = hasNamedPlaceholder a
hasNamedPlaceholder (COr a b) = hasNamedPlaceholder a || hasNamedPlaceholder b
hasNamedPlaceholder _ = False

compileAtom :: Options -> Atom -> CompiledAtom
compileAtom opt = go
  where
    !ic = ignore_case opt
    go Any = CAny
    go Ellipsis = CEllipsis
    go Keyword = CKeyword
    go NativeType = CNativeType
    go Operator = COperator
    go Number = CNumber
    go Oct = COct
    go Hex = CHex
    go String = CString
    go Literal = CLiteral
    go (Placeholder p)
        | p == "_" = CWildIdent
        | otherwise = CNamedPlaceholder p
    go (Not a) = CNot (go a)
    go (Or a b) = COr (go a) (go b)
    go (Exact l)
        | T.isTokenIdentifier l =
            let raw = T.tToken l
                !sm = mkSliceMatcher opt raw
             in if ic
                    then
                        let !lc = T.toLower raw
                         in CExactIdentCI (isAsciiText lc) lc sm
                    else CExactIdentCS sm
        | T.isTokenString l =
            let stripped = (unquoteT . trimT) (T.tToken l)
             in if T.null stripped
                    then CExactStringEmpty
                    else CExactString (mkSliceMatcher opt stripped)
        | ic && (T.isTokenKeyword l || T.isTokenNativeType l) =
            let !lc = T.toLower (T.tToken l)
             in CExactWordCI (isAsciiText lc) lc
        | ic =
            let !lc = T.toLower (T.tToken l)
             in CExactTypedCI (T.tTyp l) (isAsciiText lc) lc
        | otherwise =
            CExactRaw (T.tTyp l) (T.tToken l)

-- | Pure O(1) / zero-allocation token matcher for atoms without named placeholders.
matchPureAtom :: CompiledAtom -> T.Token -> Bool
matchPureAtom CAny _ = True
matchPureAtom CEllipsis _ = True
matchPureAtom CWildIdent t = T.isTokenIdentifier t
matchPureAtom (CNamedPlaceholder _) t = T.isTokenIdentifier t
matchPureAtom CKeyword t = T.isTokenKeyword t
matchPureAtom CNativeType t = T.isTokenNativeType t
matchPureAtom COperator t = T.isTokenOperator t
matchPureAtom CString t = T.isTokenString t
matchPureAtom CLiteral t = T.isTokenString t || T.isTokenNumber t
matchPureAtom CNumber t = T.isTokenNumber t
matchPureAtom COct t =
    T.isTokenNumber t
        && case T.uncons (T.tToken t) of
            Just ('0', T.uncons -> Just (d, _)) -> isDigit d
            _ -> False
matchPureAtom CHex t =
    T.isTokenNumber t
        && case T.uncons (T.tToken t) of
            Just ('0', T.uncons -> Just (x, _)) -> x == 'x' || x == 'X'
            _ -> False
matchPureAtom (CExactIdentCS sm) r =
    T.isTokenIdentifier r && evalSliceMatcher sm (T.tToken r)
matchPureAtom (CExactIdentCI asc lc sm) r
    | T.isTokenIdentifier r = evalSliceMatcher sm (T.tToken r)
    | T.isTokenKeyword r || T.isTokenNativeType r = textEqCI asc lc (T.tToken r)
    | otherwise = False
matchPureAtom CExactStringEmpty r =
    T.isTokenString r && T.null ((unquoteT . trimT) (T.tToken r))
matchPureAtom (CExactString sm) r =
    T.isTokenString r && evalSliceMatcher sm ((unquoteT . trimT) (T.tToken r))
matchPureAtom (CExactWordCI asc lc) r =
    (T.isTokenIdentifier r || T.isTokenKeyword r || T.isTokenNativeType r)
        && textEqCI asc lc (T.tToken r)
matchPureAtom (CExactTypedCI typ asc lc) r =
    T.tTyp r == typ && textEqCI asc lc (T.tToken r)
matchPureAtom (CExactRaw typ txt) r =
    T.tTyp r == typ && T.tToken r == txt
matchPureAtom (CNot a) r = not (matchPureAtom a r)
matchPureAtom (COr a b) r = matchPureAtom a r || matchPureAtom b r

data BoundIdent = BoundIdent
    { biName :: !T.Text
    , biTok :: !T.Token
    , biLc :: !T.Text
    , biAscii :: !Bool
    }

type Bindings = [BoundIdent]

lookupBinding :: T.Text -> Bindings -> Maybe BoundIdent
lookupBinding _ [] = Nothing
lookupBinding name (b : bs)
    | biName b == name = Just b
    | otherwise = lookupBinding name bs
{-# INLINE lookupBinding #-}

findAllMatches :: Options -> [[Atom]] -> [T.Token] -> [T.Token]
findAllMatches opt ws ts =
    let !compiled = map (map (compileAtom opt)) ws
     in concatMap (\cas -> runCompiledPattern opt cas ts) compiled
{-# INLINE findAllMatches #-}

runCompiledPattern :: Options -> [CompiledAtom] -> [T.Token] -> [T.Token]
runCompiledPattern _ [] _ = []
runCompiledPattern opt cas@(cHead : cTail) ts
    | not (any isCEllipsis cas) && not (any hasNamedPlaceholder cas) =
        case cTail of
            [] -> filter (matchPureAtom cHead) ts
            [c2] -> goPure2 cHead c2 ts
            [c2, c3] -> goPure3 cHead c2 c3 ts
            _ -> goPureN cHead cTail ts
    | not (isCEllipsis cHead) && not (hasNamedPlaceholder cHead) =
        goAnchored cHead cTail ts
    | otherwise =
        goGeneral ts
  where
    !ic = ignore_case opt

    goPure2 !c1 !c2 = loop
      where
        loop (t1 : rest@(t2 : _))
            | matchPureAtom c1 t1 && matchPureAtom c2 t2 = t1 : t2 : loop rest
            | otherwise = loop rest
        loop _ = []

    goPure3 !c1 !c2 !c3 = loop
      where
        loop (t1 : rest@(t2 : t3 : _))
            | matchPureAtom c1 t1 && matchPureAtom c2 t2 && matchPureAtom c3 t3 =
                t1 : t2 : t3 : loop rest
            | otherwise = loop rest
        loop _ = []

    goPureN !c1 restC = loop
      where
        loop [] = []
        loop (t1 : rest)
            | matchPureAtom c1 t1
            , Just tailMatched <- matchPureSeq restC rest =
                t1 : (tailMatched ++ loop rest)
            | otherwise = loop rest

    matchPureSeq :: [CompiledAtom] -> [T.Token] -> Maybe [T.Token]
    matchPureSeq [] _ = Just []
    matchPureSeq (_ : _) [] = Nothing
    matchPureSeq (c : cs) (t : rest)
        | matchPureAtom c t =
            case matchPureSeq cs rest of
                Just more -> Just (t : more)
                Nothing -> Nothing
        | otherwise = Nothing

    goAnchored !c1 restC = loop
      where
        loop [] = []
        loop (t1 : rest)
            | matchPureAtom c1 t1
            , Just tailMatched <- matchCompiledSeq ic [] restC rest =
                t1 : (tailMatched ++ loop rest)
            | otherwise = loop rest

    goGeneral [] = []
    goGeneral cur@(_ : rest) =
        case matchCompiledSeq ic [] cas cur of
            Just matched -> matched ++ goGeneral rest
            Nothing -> goGeneral rest

matchCompiledSeq :: Bool -> Bindings -> [CompiledAtom] -> [T.Token] -> Maybe [T.Token]
matchCompiledSeq _ _ [] _ = Just []
matchCompiledSeq _ _ [CEllipsis] _ = Just []
matchCompiledSeq !ic bindings (CEllipsis : as) ts0 =
    matchEllipsis 0 maxEllipsisTokens [] ts0
  where
    matchEllipsis :: Int -> Int -> [T.Token] -> [T.Token] -> Maybe [T.Token]
    matchEllipsis !depth !remaining acc curTs =
        let tryHere =
                if depth == 0
                    then case matchCompiledSeq ic bindings as curTs of
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
                                | T.isTokenBracket t && isOpenB (T.tToken t) ->
                                    matchEllipsis (depth + 1) (remaining - 1) (t : acc) rest
                                | T.isTokenBracket t && isCloseB (T.tToken t) ->
                                    if depth == 0
                                        then Nothing
                                        else matchEllipsis (depth - 1) (remaining - 1) (t : acc) rest
                                | depth == 0 && T.isTokenOperator t && T.tToken t == ";" ->
                                    Nothing
                                | otherwise ->
                                    matchEllipsis depth (remaining - 1) (t : acc) rest

    isOpenB b = b == "(" || b == "[" || b == "{"
    isCloseB b = b == ")" || b == "]" || b == "}"
matchCompiledSeq _ _ (_ : _) [] = Nothing
matchCompiledSeq !ic bindings (a : as) (t : ts)
    | not (hasNamedPlaceholder a) =
        if matchPureAtom a t
            then case matchCompiledSeq ic bindings as ts of
                Just restMatched -> Just (t : restMatched)
                Nothing -> Nothing
            else Nothing
    | otherwise =
        case matchAtomWithBindings ic bindings a t of
            Just bindings' ->
                case matchCompiledSeq ic bindings' as ts of
                    Just restMatched -> Just (t : restMatched)
                    Nothing -> Nothing
            Nothing -> Nothing

matchAtomWithBindings :: Bool -> Bindings -> CompiledAtom -> T.Token -> Maybe Bindings
matchAtomWithBindings !ic bindings (CNamedPlaceholder p) t
    | not (T.isTokenIdentifier t) = Nothing
    | otherwise =
        case lookupBinding p bindings of
            Just BoundIdent{..}
                | ic -> if textEqCI biAscii biLc (T.tToken t) then Just bindings else Nothing
                | biTok == t -> Just bindings
                | otherwise -> Nothing
            Nothing ->
                let !lc = if ic then T.toLower (T.tToken t) else T.empty
                    !asc = not ic || isAsciiText lc
                 in Just (BoundIdent p t lc asc : bindings)
matchAtomWithBindings !ic bindings (CNot a) t
    | isNothing (matchAtomWithBindings ic bindings a t) = Just bindings
    | otherwise = Nothing
matchAtomWithBindings !ic bindings (COr a b) t =
    matchAtomWithBindings ic bindings a t <|> matchAtomWithBindings ic bindings b t
matchAtomWithBindings _ bindings a t
    | matchPureAtom a t = Just bindings
    | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Reference implementation retained for differential QuickCheck & corpus tests
-- ---------------------------------------------------------------------------

findAllMatchesRef :: Options -> [[Atom]] -> [T.Token] -> [T.Token]
findAllMatchesRef opt ws ts = concatMap (\w -> findAllMatchesRef' opt w ts) ws

findAllMatchesRef' :: Options -> [Atom] -> [T.Token] -> [T.Token]
findAllMatchesRef' _ [] _ = []
findAllMatchesRef' opt as ts =
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
matchSingleAtom opt bindings (Placeholder p) t
    | not (T.isTokenIdentifier t) = Nothing
    | p == "_" = Just bindings
    | otherwise =
        case M.lookup p bindings of
            Just boundTok
                | sameIdent boundTok t -> Just bindings
                | otherwise -> Nothing
            Nothing -> Just (M.insert p t bindings)
  where
    sameIdent a b
        | ignore_case opt = T.toLower (T.tToken a) == T.toLower (T.tToken b)
        | otherwise = a == b
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
            | word_match opt -> lt == rt
            | prefix_match opt -> lt `T.isPrefixOf` rt
            | suffix_match opt -> lt `T.isSuffixOf` rt
            | edit_dist opt -> lt `textApproxEq` rt
            | otherwise -> lt `T.isInfixOf` rt
    | T.isTokenString l && T.isTokenString r =
        if T.null ls
            then T.null rs
            else
                if
                    | word_match opt -> ls == rs
                    | prefix_match opt -> ls `T.isPrefixOf` rs
                    | suffix_match opt -> ls `T.isSuffixOf` rs
                    | edit_dist opt -> ls `textApproxEq` rs
                    | otherwise -> ls `T.isInfixOf` rs
    | ignore_case opt && isWordTok l && isWordTok r = lt == rt
    | ignore_case opt = T.tTyp l == T.tTyp r && lt == rt
    | otherwise = l `T.eqToken` r
  where
    norm = if ignore_case opt then T.toLower else id
    lt = norm (T.tToken l)
    rt = norm (T.tToken r)
    ls = norm ((unquoteT . trimT) (T.tToken l))
    rs = norm ((unquoteT . trimT) (T.tToken r))
    isWordTok tok = T.isTokenIdentifier tok || T.isTokenKeyword tok || T.isTokenNativeType tok
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
doesAtomMatchToken _ Hex t = T.isTokenNumber t && case T.uncons (T.tToken t) of Just ('0', T.uncons -> Just (x, _)) -> x == 'x' || x == 'X'; _ -> False
doesAtomMatchToken opt (Not a) t = not (doesAtomMatchToken opt a t)
doesAtomMatchToken opt (Or a b) t = doesAtomMatchToken opt a t || doesAtomMatchToken opt b t
