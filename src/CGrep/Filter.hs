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

{-# LANGUAGE ViewPatterns, MagicHash #-}

module CGrep.Filter (Context(..), ContextFilter(..), contextFilter, mkContextFilter)  where

import CGrep.Common (Text8)

import CGrep.Context
import CGrep.Lang
import Options

import Data.Char
import Data.Monoid

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8   as LC
import qualified Data.ByteString.Lazy.Builder as B

import qualified Data.Map as Map

#ifdef __GLASGOW_HASKELL__

import GHC.Prim
import GHC.Exts

#endif


-- filter Context:
--

mkContextFilter :: Options -> ContextFilter
mkContextFilter opt = if not (code opt || comment opt || literal opt)
                       then ContextFilter { getCode = True, getComment = True,  getLiteral = True }
                       else ContextFilter { getCode = code opt, getComment = comment opt, getLiteral = literal opt }


type FilterFunction = ContextFilter -> Text8 -> B.Builder


contextFilter :: Maybe Lang -> ContextFilter -> Text8 -> Text8
contextFilter _ (ContextFilter True True True) src = src
contextFilter Nothing _ src = src
contextFilter (Just language) filt src
    | Just fun <- parFunc = LC.toStrict . B.toLazyByteString $ fun filt src
    | otherwise = src
        where parFunc = Map.lookup language filterFunctionMap


-- genericParser:
--

genericParser :: ParState -> ContextFilter -> Text8 -> B.Builder
genericParser _ _     (C.uncons -> Nothing) = mempty
genericParser s  filt (C.uncons -> Just (x,xs))
    | skip s > 0 = go s { skip = skip s - 1}
    | otherwise  = go $ nextParserState s (x,xs) filt
        where go t = let q = if isSpace x || display t then x else ' ' in
                        seq q $ B.char8 q <> genericParser t filt xs


nextParserState :: ParState -> (Char,Text8) -> ContextFilter -> ParState
nextParserState s (x,xs) (ContextFilter codefilt commfilt litrfilt)
    | x == '\\'                 = s { display = False, skip = 1 }
    | CodeState   <- cxtState s = let cindex = findBoundary (x,xs) (commBound s)
                                      lindex = findBoundary (x,xs) (litrBound s)
                                    in if cindex >= 0
                                       then s{ cxtState = CommState cindex, display = codefilt, skip = C.length ( fst (commBound s !! cindex) ) - 1 }
                                       else if lindex >= 0
                                       then s{ cxtState = LitrState lindex, display = codefilt, skip = C.length ( fst (litrBound s !! lindex) ) - 1 }
                                       else s{ display  = codefilt, skip = 0 }

    | CommState n <- cxtState s = let (_,end) = commBound s !! n
                                    in if C.head end == x && C.tail end `C.isPrefixOf` xs
                                            then s{ cxtState = CodeState, display = codefilt, skip = C.length end - 1}
                                            else s{ display  = commfilt, skip = 0 }
    | LitrState n <- cxtState s = let (_,end) = litrBound s !! n
                                    in if C.head end == x && C.tail end `C.isPrefixOf` xs
                                            then s{ cxtState = CodeState, display = codefilt, skip = C.length end - 1}
                                            else s{ display = litrfilt, skip = 0 }
nextParserState _ (_,_) ContextFilter {} = undefined


{-# INLINE findBoundary #-}

findBoundary :: (Char, Text8) -> [Boundary] -> Int
findBoundary (x,xs) =  findIndex' (\(b,_) -> C.head b == x && C.tail b `C.isPrefixOf` xs)


{-# INLINE findIndex' #-}

#ifdef __GLASGOW_HASKELL__

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p ls = loop 0# ls
                 where
                   loop _ [] = -1
                   loop n (x:xs) | p x       = I# n
                                 | otherwise = loop (n +# 1#) xs

#else

findIndex' :: (a -> Bool) -> [a] -> Int
findIndex' p ls = loop 0 ls
                 where
                   loop n [] = -1
                   loop n (x:xs) | p x       = n
                                 | otherwise = loop (n + 1) xs

#endif

-- filter language map:
--

filterFunctionMap :: Map.Map Lang FilterFunction

type StringBoundary = (String, String)

type Boundary = (Text8, Text8)

data ParState =  ParState
                 {
                    commBound :: [Boundary],
                    litrBound :: [Boundary],
                    cxtState  :: !ContextState,
                    display   :: !Bool,
                    skip      :: !Int
                 }
                 deriving (Show)


mkParState :: [StringBoundary] -> [StringBoundary] -> ParState
mkParState cs ls = ParState (map (\(a,b) -> (C.pack a, C.pack b)) cs)
                          (map (\(a,b) -> (C.pack a, C.pack b)) ls) CodeState False 0


data ContextState = CodeState | CommState Int | LitrState Int
                        deriving (Show, Eq, Ord)


filterFunctionMap = Map.fromList [
    (C,          genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Cpp,        genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Csharp,     genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Chapel,     genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (D,          genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Go,         genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Java,       genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Javascript, genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (ObjectiveC, genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Scala,      genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Verilog,    genericParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (VHDL,       genericParser $ mkParState [("--", "\n")] [("\"", "\"")] ),

    (Haskell,    genericParser $ mkParState [("{-", "-}"), ("--", "\n")]      [("\"", "\""), ("[r|", "|]"), ("[q|", "|]"), ("[s|", "|]"), ("[here|","|]"), ("[i|", "|]")] ),
    (Fsharp,     genericParser $ mkParState [("(*", "*)"), ("//", "\n")]      [("\"", "\"")] ),
    (Perl,       genericParser $ mkParState [("=pod", "=cut"), ("#", "\n")]   [("'", "'"), ("\"", "\"")] ),
    (Ruby,       genericParser $ mkParState [("=begin", "=end"), ("#", "\n")] [("'", "'"), ("\"", "\""), ("%|", "|"), ("%q(", ")"), ("%Q(", ")") ]),

    (CMake,      genericParser $ mkParState [("#", "\n")]    [("\"", "\"")] ),
    (Awk,        genericParser $ mkParState [("#", "\n")]    [("\"", "\"")] ),
    (Tcl,        genericParser $ mkParState [("#", "\n")]    [("\"", "\"")] ),
    (Shell,      genericParser $ mkParState [("#", "\n")]    [("'", "'"), ("\"", "\"")] ),
    (Make,       genericParser $ mkParState [("#", "\n")]    [("'", "'"), ("\"", "\"")] ),

    (Css,        genericParser $ mkParState [("/*", "*/")]   [("\"", "\"")] ),
    (OCaml,      genericParser $ mkParState [("(*", "*)")]   [("\"", "\"")] ),
    (Python,     genericParser $ mkParState [("#", "\n")]    [("\"\"\"", "\"\"\""), ("'''", "'''"), ("'", "'"), ("\"", "\"")] ),

    (Erlang,     genericParser $ mkParState [("%", "\n")]    [("\"", "\"")] ),
    (Latex,      genericParser $ mkParState [("%", "\n")]    [("\"", "\"")] ),
    (Lua,        genericParser $ mkParState [("--[[","--]]"), ("--", "\n")]    [("'", "'"), ("\"", "\""), ("[===[", "]===]"), ("[==[", "]==]"), ("[=[", "]=]"), ("[[", "]]") ] ),

    (Html,       genericParser $ mkParState [("<!--", "-->")]  [("\"", "\"")] ),
    (Vim,        genericParser $ mkParState [("\"", "\n")]     [("'", "'")] ),

    (PHP,        genericParser $ mkParState [("/*", "*/"), ("//", "\n"), ("#", "\n") ]  [("'", "'"), ("\"", "\"")] )
    ]

