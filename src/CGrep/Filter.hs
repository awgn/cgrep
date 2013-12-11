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

module CGrep.Filter (Context(..), ContextFilter(..), filterContext, mkContextFilter)  where

import CGrep.Template
import CGrep.FilterData
import CGrep.Lang
import Options

import Data.Maybe

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map


type Source = C.ByteString

type FilterFunction = (String,Char) -> FiltState -> (Context, FiltState)

-- filter Context:
--


filterContext :: Maybe Lang -> ContextFilter -> Source -> Source

filterContext Nothing     _ src =  src 
filterContext (Just language) filt src =  
    snd $ C.mapAccumL (fromJust $ Map.lookup language filterMap) (FiltState StateCode filt []) src 


-- filter function:
--

filterFunction :: FilterFunction -> FiltState -> Char -> (FiltState, Char) 
filterFunction funFilt filtstate c = (state', charFilter (cxtFilter cxt (cfilter filtstate)) c)
                        where (cxt, state') = funFilt (pchars filtstate, c) filtstate


mkContextFilter :: Options -> ContextFilter
mkContextFilter opt = if not (code opt && comment opt && literal opt) 
                       then ContextFilter { getCode = True,     getComment = False, getLiteral = True }
                       else ContextFilter { getCode = code opt, getComment = False, getLiteral = literal opt }


-- parsers templates:
--


likeShell, likeErlang, likeLatex, likeVim, likePython, likeCSS, likeOCaml, 
    likeHtml, likeCpp, likeHaskell, likePerl, likeRuby, likeFsharp, likePHP :: FilterFunction

likeShell   =  $(parser1 ("#",  "\n"))
likeErlang  =  $(parser1 ("%",  "\n"))
likeLatex   =  $(parser1 ("%",  "\n"))
likeVim     =  $(parser1 ("\"", "\n"))
likePython  =  $(parser1 ("#",  "\n"))
likeCSS     =  $(parser1 ("/*",  "*/"))
likeOCaml   =  $(parser1 ("(*",  "*)"))
likeHtml    =  $(parser1 ("<!--","-->"))

likeCpp     =  $(parser2 ("/*", "*/") ("//", "\n"))
likeHaskell =  $(parser2 ("{-", "-}") ("--", "\n"))
likePerl    =  $(parser2 ("=pod", "=cut") ("#", "\n"))
likeRuby    =  $(parser2 ("=begin", "=end") ("#", "\n"))
likeFsharp  =  $(parser2 ("(*", "*)") ("//", "\n"))

likePHP     =  $(parser3 ("#", "\n") ("/*", "*/") ("//", "\n"))


{-# INLINE charFilter #-}

charFilter :: Bool -> Char -> Char
charFilter  _  '\n' = '\n'
charFilter  True  c =  c
charFilter  _ _     = ' '


{-# INLINE cxtFilter #-}

cxtFilter :: Context -> ContextFilter -> Bool
cxtFilter Code    = getCode 
cxtFilter Comment = getComment 
cxtFilter Literal = getLiteral 


-- filter language map:
--
    
type FilterType =  FiltState -> Char -> (FiltState, Char) 

filterMap :: Map.Map Lang FilterType

filterMap = Map.fromList [
            (Awk,        filterFunction likeShell),
            (C,          filterFunction likeCpp),
            (Cpp,        filterFunction likeCpp),
            (Csharp,     filterFunction likeCpp),
            (Css,        filterFunction likeCSS),
            (CMake,      filterFunction likeShell),
            (D,          filterFunction likeCpp),
            (Erlang,     filterFunction likeErlang),
            (Fsharp,     filterFunction likeFsharp),
            (Go,         filterFunction likeCpp),
            (Haskell,    filterFunction likeHaskell),
            (Html,       filterFunction likeHtml),
            (Java,       filterFunction likeCpp),
            (Javascript, filterFunction likeCpp),
            (Latex ,     filterFunction likeErlang),
            (Make,       filterFunction likeShell),
            (OCaml ,     filterFunction likeOCaml),
            (ObjectiveC, filterFunction likeCpp),
            (Perl,       filterFunction likePerl),
            (PHP,        filterFunction likePHP),
            (Python,     filterFunction likePython),
            (Ruby,       filterFunction likeRuby),
            (Scala,      filterFunction likeCpp),
            (Tcl,        filterFunction likeShell),
            (Shell,      filterFunction likeShell),
            (Verilog,    filterFunction likeCpp),
            (Vim,        filterFunction likeVim)
           ]


