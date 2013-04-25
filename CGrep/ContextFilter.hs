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

import CGrep.Lang

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import Data.Maybe

type Source = C.ByteString


data Context = Code | Comment | Literal
                deriving (Eq, Show)


data ContextFilter = ContextFilter { getCode    :: Bool,
                                     getComment :: Bool,
                                     getLiteral :: Bool 
                     } deriving (Eq, Show)



filterContext :: Maybe Lang -> ContextFilter -> Source -> Source

filterContext Nothing     _ src =  src 
filterContext (Just lang) filt src =  snd $ C.mapAccumL (fromJust $ Map.lookup lang filterMap) (FiltState StateCode filt []) src 


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


-- States:

data FiltState = FiltState 
                 {
                    cstate  :: ContextState,
                    cfilter :: ContextFilter,
                    pchar   :: [Char]
                 } deriving (Eq, Show)



data ContextState = StateCode       | 
                    StateComment    | 
                    StateComment2   | 
                    StateLiteral    |
                    StateLiteral2
                        deriving (Eq, Show)

-- filter functions:
--

filterUndefined :: FiltState -> Char -> (FiltState, Char) 
filterUndefined s c = (s, c)


runFilterCpp :: FiltState -> Char -> (FiltState, Char) 
runFilterCpp filtstate c = (state', charFilter (cxtFilter cxt (cfilter filtstate)) c)
                        where (cxt, state') = charParserCpp (pchar filtstate, c) filtstate


charParserCpp :: (String,Char) -> FiltState -> (Context, FiltState)

charParserCpp (p,c) filtstate@(FiltState StateCode _ _) 
    | p == "/"  && c == '/'  = (Code, filtstate { cstate = StateComment2, pchar = app1 p c })
    | p == "/"  && c == '*'  = (Code, filtstate { cstate = StateComment,   pchar = app1 p c })
    |              c == '"'  = (Code, filtstate { cstate = StateLiteral,   pchar = app1 p c })
    |              c == '\'' = (Code, filtstate { cstate = StateLiteral2,   pchar = app1 p c }) 
    | p == "\\" && c == '\\' = (Code, filtstate { pchar = app1 p ' ' })
    | otherwise = (Code, filtstate { pchar = app1 p c } )
                                       
charParserCpp (_,c) filtstate@(FiltState StateComment2 _ _)
    | c == '\n' = (Comment, filtstate { cstate = StateCode, pchar = app1 [] c })
    | otherwise = (Comment, filtstate { pchar = app1 [] c })

charParserCpp (p,c) filtstate@(FiltState StateComment _ _)
    | p == "*" && c == '/'  = (Comment, filtstate { cstate = StateCode, pchar = app1 p c })
    | otherwise = (Comment, filtstate { pchar = app1 p c })

charParserCpp (p,c) filtstate@(FiltState StateLiteral _ _)
    | p /= "\\" && c == '"'  = (Code,    filtstate { cstate = StateCode, pchar = app1 p c })
    | p == "\\" && c == '\\' = (Literal, filtstate { pchar = app1 p ' '})
    | otherwise = (Literal, filtstate { pchar = app1 p c }) 

charParserCpp (p,c) filtstate@(FiltState StateLiteral2 _ _)
    | p /= "\\" && c == '\'' = (Code, filtstate { cstate = StateCode, pchar = app1 p c })
    | p == "\\" && c == '\\' = (Literal, filtstate { pchar = app1 p ' ' })
    | otherwise = (Literal, filtstate { pchar = app1 p c})


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


{-# INLINE app1 #-}
app1 :: String -> Char -> String
app1 _ c = [c]

{-# INLINE app2 #-}
app2 :: String -> Char -> String
app2 (_:x:[]) c = [x,c]
app2 xs c = xs ++ [c]

{-# INLINE app3 #-}
app3 :: String -> Char -> String
app3 (_:x:y:[]) c = [x,y,c]
app3 xs c = xs ++ [c]

{-# INLINE app4 #-}
app4 :: String -> Char -> String
app4 (_:x:y:z:[]) c = [x,y,z,c]
app4 xs c = xs ++ [c]

{-# INLINE app5 #-}
app5 :: String -> Char -> String
app5 (_:x:y:w:z:[]) c = [x,y,w,z,c]
app5 xs c = xs ++ [c]

