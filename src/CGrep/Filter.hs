--
-- Copyright (c) 2012-2013 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}

module CGrep.Filter (Context(..), ContextFilter(..), contextFilter, mkContextFilter)  where

import Control.Applicative

import CGrep.Common (Text8)

import CGrep.Context
import CGrep.Lang
import Options

import Data.Maybe
import Data.Char
import Data.List
import Data.Monoid

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8   as LC
import qualified Data.ByteString.Lazy.Builder as LB

import qualified Data.Map as Map


-- filter Context:
--

mkContextFilter :: Options -> ContextFilter
mkContextFilter opt = if not (code opt || comment opt || literal opt)
                       then ContextFilter { getCode = True, getComment = True,  getLiteral = True }
                       else ContextFilter { getCode = code opt, getComment = comment opt, getLiteral = literal opt }


contextFilter :: Maybe Lang -> ContextFilter -> Text8 -> Text8
contextFilter _ (ContextFilter True True True) src = src
contextFilter Nothing _ src = src
contextFilter (Just language) filt src = LC.toStrict . LB.toLazyByteString $ contextParser parState filt src
    where parState = fromJust $ Map.lookup language parserStateMap


-- contextParser:
--

contextParser :: ParState -> ContextFilter -> C.ByteString -> LB.Builder
contextParser s  filt (C.uncons -> Just (x,xs))
    | skip s > 0 = LB.char8 (if isSpace x || display s then x else ' ') <> contextParser s { skip = skip s -1 } filt xs
    | otherwise  = let (nstate, d, n) = nextParserState s (x,xs) filt
                   in LB.char8 (if isSpace x || d then x else ' ') <> contextParser s { cxtState = nstate, display = d, skip = n } filt xs
contextParser _ _ _ = mempty


nextParserState :: ParState -> (Char,C.ByteString) -> ContextFilter -> (ContextState, Bool, Int)
nextParserState s (x,xs) (ContextFilter codefilt commfilt litrfilt)
    | x == '\\'                  = (cxtState s, False, 1)
    | CodeState    <- cxtState s = let cindex = findBoundary (x,xs) (commBound s)
                                       lindex = findBoundary (x,xs) (litrBound s)
                                   in
                                   fromJust (
                                        (cindex >>= \n -> Just (CommState n, codefilt, C.length ( fst (commBound s !! n) ) - 1 )) <|>
                                        (lindex >>= \n -> Just (LitrState n, codefilt, C.length ( fst (litrBound s !! n) ) - 1 )) <|>
                                                          Just (CodeState  , codefilt, 0))
    | CommState n <- cxtState s = let (_,end) = commBound s !! n
                                    in if C.head end == x && C.tail end `C.isPrefixOf` xs
                                            then (CodeState,   codefilt, C.length end - 1)
                                            else (CommState n, commfilt, 0)
    | LitrState n <- cxtState s = let (_,end) = litrBound s !! n
                                    in if C.head end == x && C.tail end `C.isPrefixOf` xs
                                            then (CodeState,   codefilt, C.length end - 1)
                                            else (LitrState n, litrfilt, 0)
nextParserState _ (_,_) (ContextFilter _ _ _) = undefined


{-# INLINE findBoundary #-}

findBoundary :: (Char, C.ByteString) -> [Boundary] -> Maybe Int
findBoundary (x,xs) bounds =  findIndex (\(b,_) -> (C.head b) == x && (C.tail b) `C.isPrefixOf` xs) bounds


-- filter language map:
--

parserStateMap :: Map.Map Lang ParState

type StringBoundary = (String, String)

type Boundary = (C.ByteString, C.ByteString)

data ParState =  ParState
                 {
                    commBound :: [Boundary],
                    litrBound :: [Boundary],
                    cxtState  :: !ContextState,
                    display   :: !Bool,
                    skip      :: !Int
                 }
                 deriving (Show)


mkParserState :: [StringBoundary] -> [StringBoundary] -> ParState
mkParserState cs ls = ParState (map (\(a,b) -> (C.pack a, C.pack b)) cs)
                          (map (\(a,b) -> (C.pack a, C.pack b)) ls) CodeState False 0


data ContextState = CodeState | CommState Int | LitrState Int
                        deriving (Show, Eq, Ord)


parserStateMap = Map.fromList [
    (C,          mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Cpp,        mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Csharp,     mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (D,          mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Go,         mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Java,       mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Javascript, mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (ObjectiveC, mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Scala,      mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Verilog,    mkParserState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),

    (Haskell,    mkParserState [("{-", "-}"), ("--", "\n")]         [("\"", "\""), ("[r|", "|]")] ),
    (Fsharp,     mkParserState [("(*", "*)"), ("//", "\n")]         [("\"", "\"")] ),
    (Perl,       mkParserState [("=pod", "=cut"), ("#", "\n")]      [("'", "'"), ("\"", "\"")] ),
    (Ruby,       mkParserState [("=begin", "=end"), ("#", "\n")]    [("'", "'"), ("\"", "\""), ("%|", "|"), ("%q(", ")"), ("%Q(", ")") ]),

    (CMake,      mkParserState [("#", "\n")]    [("\"", "\"")] ),
    (Awk,        mkParserState [("#", "\n")]    [("\"", "\"")] ),
    (Tcl,        mkParserState [("#", "\n")]    [("\"", "\"")] ),
    (Shell,      mkParserState [("#", "\n")]    [("'", "'"), ("\"", "\"")] ),
    (Make,       mkParserState [("#", "\n")]    [("'", "'"), ("\"", "\"")] ),

    (Css,        mkParserState [("/*", "*/")]   [("\"", "\"")] ),
    (OCaml,      mkParserState [("(*", "*)")]   [("\"", "\"")] ),
    (Python,     mkParserState [("#", "\n")]    [("\"\"\"", "\"\"\""), ("'''", "'''"), ("'", "'"), ("\"", "\"")] ),

    (Erlang,     mkParserState [("%", "\n")]    [("\"", "\"")] ),
    (Latex,      mkParserState [("%", "\n")]    [("\"", "\"")] ),

    (Html,       mkParserState [("<!--", "-->")]  [("\"", "\"")] ),
    (Vim,        mkParserState [("\"", "\n")]     [("'", "'")] ),

    (PHP,        mkParserState [("/*", "*/"), ("//", "\n"), ("#", "\n") ]  [("'", "'"), ("\"", "\"")] )
    ]

