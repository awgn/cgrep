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


module CGrep.ContextFilter (Context(..), ContextFilter(..), filterContext)  where

import CGrep.ContextParser

import CGrep.Lang
import Data.Maybe

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map


type Source = C.ByteString

filterContext :: Maybe Lang -> ContextFilter -> Source -> Source

filterContext Nothing     _ src =  src 
filterContext (Just lang) filt src =  snd $ C.mapAccumL (fromJust $ Map.lookup lang filterMap) (FiltState StateCode filt []) src 


-- filter functions:
--

filterFunction :: FilterFunction -> FiltState -> Char -> (FiltState, Char) 
filterFunction funFilt filtstate c = (state', charFilter (cxtFilter cxt (cfilter filtstate)) c)
                        where (cxt, state') = funFilt (pchar filtstate, c) filtstate


filterUndefined :: FiltState -> Char -> (FiltState, Char) 
filterUndefined s c = (s, c)


-- runFilter map:
--
    
type FilterType =  FiltState -> Char -> (FiltState, Char) 

filterMap :: Map.Map Lang FilterType

filterMap = Map.fromList [
            (Awk,        filterUndefined),
            (C,          runFilterCpp),
            (Cpp,        runFilterCpp),
            (Csharp,     runFilterCpp),
            (Css,        filterUndefined),
            (CMake,      filterUndefined),
            (D,          runFilterCpp),
            (Erlang,     filterUndefined),
            (Fsharp,     filterUndefined),
            (Go,         runFilterCpp),
            (Haskell,    filterUndefined),
            (Html,       filterUndefined),
            (Java,       runFilterCpp),
            (Javascript, runFilterCpp),
            (Latex ,     filterUndefined),
            (LUA ,       filterUndefined),
            (Make,       filterUndefined),
            (OCaml ,     filterUndefined),
            (ObjectiveC, runFilterCpp),
            (Perl,       filterUndefined),
            (PHP,        filterUndefined),
            (Python,     filterUndefined),
            (Ruby,       filterUndefined),
            (Scala,      runFilterCpp),
            (Tcl,        filterUndefined),
            (Shell,      filterUndefined),
            (Verilog,    runFilterCpp),
            (Vim,        filterUndefined)
           ]


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


