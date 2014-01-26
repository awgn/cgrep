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

{-# LANGUAGE ViewPatterns, MagicHash, BangPatterns #-}

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


-- contextParser:
--

contextParser :: ParState -> ContextFilter -> Text8 -> B.Builder
contextParser _ _     (C.uncons -> Nothing)     = mempty
contextParser s  filt (C.uncons -> Just (x,xs)) =
    B.char8 c' <> contextParser s' filt xs
    where !s' = nextContextState s (x,xs) filt
          !c' = if display s' || isSpace x then x else ' '


nextContextState :: ParState -> (Char,Text8) -> ContextFilter -> ParState
nextContextState s (x,xs) (ContextFilter codefilt commfilt litrfilt)
    | x == '\\'                 = s { display = False, skip = 1 }
    | skip s > 0                = s { skip = skip s - 1 }
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
nextContextState _ (_,_) ContextFilter {} = undefined


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
mkParState cs ls =
    ParState (map (\(a,b) -> (C.pack a, C.pack b)) cs)
             (map (\(a,b) -> (C.pack a, C.pack b)) ls) CodeState False 0


data ContextState = CodeState | CommState Int | LitrState Int
                        deriving (Show, Eq, Ord)


filterFunctionMap = Map.fromList [
    (C,          contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Cpp,        contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Csharp,     contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Chapel,     contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (D,          contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Go,         contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Java,       contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Javascript, contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (ObjectiveC, contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Scala,      contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (Verilog,    contextParser $ mkParState [("/*", "*/"), ("//", "\n")]  [("\"", "\"")] ),
    (VHDL,       contextParser $ mkParState [("--", "\n")] [("\"", "\"")] ),

    (Haskell,    contextParser $ mkParState [("{-", "-}"), ("--", "\n")]      [("\"", "\""), ("[r|", "|]"), ("[q|", "|]"), ("[s|", "|]"), ("[here|","|]"), ("[i|", "|]")] ),
    (Fsharp,     contextParser $ mkParState [("(*", "*)"), ("//", "\n")]      [("\"", "\"")] ),
    (Perl,       contextParser $ mkParState [("=pod", "=cut"), ("#", "\n")]   [("'", "'"), ("\"", "\"")] ),
    (Ruby,       contextParser $ mkParState [("=begin", "=end"), ("#", "\n")] [("'", "'"), ("\"", "\""), ("%|", "|"), ("%q(", ")"), ("%Q(", ")") ]),

    (CMake,      contextParser $ mkParState [("#", "\n")]    [("\"", "\"")] ),
    (Awk,        contextParser $ mkParState [("#", "\n")]    [("\"", "\"")] ),
    (Tcl,        contextParser $ mkParState [("#", "\n")]    [("\"", "\"")] ),
    (Shell,      contextParser $ mkParState [("#", "\n")]    [("'", "'"), ("\"", "\"")] ),
    (Make,       contextParser $ mkParState [("#", "\n")]    [("'", "'"), ("\"", "\"")] ),

    (Css,        contextParser $ mkParState [("/*", "*/")]   [("\"", "\"")] ),
    (OCaml,      contextParser $ mkParState [("(*", "*)")]   [("\"", "\"")] ),
    (Python,     contextParser $ mkParState [("#", "\n")]    [("\"\"\"", "\"\"\""), ("'''", "'''"), ("'", "'"), ("\"", "\"")] ),

    (Erlang,     contextParser $ mkParState [("%", "\n")]    [("\"", "\"")] ),
    (Latex,      contextParser $ mkParState [("%", "\n")]    [("\"", "\"")] ),
    (Lua,        contextParser $ mkParState [("--[[","--]]"), ("--", "\n")]    [("'", "'"), ("\"", "\""), ("[===[", "]===]"), ("[==[", "]==]"), ("[=[", "]=]"), ("[[", "]]") ] ),

    (Html,       contextParser $ mkParState [("<!--", "-->")]  [("\"", "\"")] ),
    (Vim,        contextParser $ mkParState [("\"", "\n")]     [("'", "'")] ),

    (PHP,        contextParser $ mkParState [("/*", "*/"), ("//", "\n"), ("#", "\n") ]  [("'", "'"), ("\"", "\"")] )
    ]

