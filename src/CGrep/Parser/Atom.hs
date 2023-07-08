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
import qualified CGrep.Parser.Chunk as T

data Atom =
    Any                         |
    Keyword                     |
    Number                      |
    Oct                         |
    Hex                         |
    String                      |
    Literal                     |
    Identifier  C.ByteString    |
    Raw T.Token
        deriving stock (Eq, Ord, Show)

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
    | T.isTokenIdentifier t = case () of
        _ | Just wc <- M.lookup str wildCardMap -> wc
          | isAtomIdentifier str                -> Identifier str
          | otherwise                           -> Raw $ T.mkTokenIdentifier (rmAtomEscape str) (T.tOffset t)
            where str = T.tToken t
    | otherwise = Raw t


combineAtoms :: [Atoms] -> [Atoms]
combineAtoms (m1:r@(m2:m3:ms))
    | [Raw b] <- m2, T.tToken b == "OR" = combineAtoms $ (m1<>m3):ms
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
              {-# SCC "atom_find_total" #-} concatMap (take grpLen . (`drop` ts)) ({-# SCC "atom_find_indices" #-} findIndices (wildCardsCompare opt g) grp) <> {-# SCC atom_find_req #-} go opt gs ts
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
{-# SCC wildCardsCompareAll #-}

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
{-# SCC wildCardsCheckOccurrences #-}


wildCardsGroupCompare :: Options -> [Atoms] -> [T.Token] -> [(Bool, (Atoms, [C.ByteString]))]
wildCardsGroupCompare opt ls rs
    | length rs >= length ls = zipWith (tokensZip opt) ls rs
    | otherwise              = [ (False, ([Any], [])) ]
{-# INLINE wildCardsGroupCompare #-}
{-# SCC wildCardsGroupCompare #-}


tokensZip :: Options -> Atoms -> T.Token -> (Bool, (Atoms, [C.ByteString]))
tokensZip opt l r
    |  wildCardsMatch opt l r = (True,  (l, [T.tToken r]))
    |  otherwise              = (False, ([Any],[] ))
{-# INLINE tokensZip #-}
{-# SCC tokensZip #-}


wildCardsMatch :: Options ->  Atoms -> T.Token -> Bool
wildCardsMatch opt m t = any (\w -> wildCardMatch opt w t) m
{-# INLINE wildCardsMatch #-}
{-# SCC wildCardsMatch #-}


{-# SCC wildCardMatch #-}
wildCardMatch :: Options -> Atom -> T.Token -> Bool
wildCardMatch opt (Raw l) r
    | T.isTokenIdentifier l && T.isTokenIdentifier r = {-# SCC wildcard_raw_0 #-}
        if | word_match opt   -> T.tToken l ==  T.tToken r
           | prefix_match opt -> T.tToken l `C.isPrefixOf`  T.tToken r
           | suffix_match opt -> T.tToken l `C.isSuffixOf`  T.tToken r
           | edit_dist  opt   -> (C.unpack . T.tToken) l ~== C.unpack (T.tToken r)
           | otherwise        -> T.tToken l `C.isInfixOf` T.tToken r
    | T.isTokenString l && T.isTokenString r = {-# SCC wildcard_raw_1 #-}
        if | word_match opt   -> ls == rs
           | prefix_match opt -> ls `C.isPrefixOf` rs
           | suffix_match opt -> ls `C.isSuffixOf` rs
           | edit_dist  opt   -> C.unpack ls ~== C.unpack rs
           | otherwise        -> ls `C.isInfixOf`  rs
    | otherwise  = {-# SCC wildcard_raw_2 #-} l `T.eqToken` r
            where ls = rmQuote8 $ trim8 (T.tToken l)
                  rs = rmQuote8 $ trim8 (T.tToken r)

wildCardMatch _  Any _             = {-# SCC wildcard_any #-} True
wildCardMatch _  (Identifier _) t  = {-# SCC wildcard_identifier #-} T.isTokenIdentifier t
wildCardMatch _  Keyword        t  = {-# SCC wildcard_keyword #-} T.isTokenKeyword t
wildCardMatch _  String         t  = {-# SCC wildcard_string #-} T.isTokenString t
wildCardMatch _  Literal        t  = {-# SCC wildcard_lit #-} T.isTokenString t
wildCardMatch _  Number         t  = {-# SCC wildcard_number #-} T.isTokenNumber t
wildCardMatch _  Oct            t  = {-# SCC wildcard_octal #-} T.isTokenNumber t && case C.uncons (T.tToken t) of Just ('0', C.uncons -> Just (d, _))  -> isDigit d; _ -> False
wildCardMatch _  Hex            t  = {-# SCC wildcard_hex #-} T.isTokenNumber t && case C.uncons (T.tToken t) of Just ('0', C.uncons -> Just ('x',_)) -> True; _      -> False
