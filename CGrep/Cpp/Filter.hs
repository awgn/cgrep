--
-- Copyright (c) 2012 Bonelli Nicola <bonelli@antifork.org>
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
-- ass: C++11 code ass'istant 

{-# LANGUAGE ViewPatterns #-} 


module CGrep.Cpp.Filter (Context(..), ContextFilter(..), CGrep.Cpp.Filter.filter)  where

import qualified CGrep.Cpp.Source as Cpp
import qualified Data.ByteString.Lazy.Char8 as C


type Source = Cpp.Source


data Context = Code | Comment | Literal
                deriving (Eq, Show)


data ContextFilter = ContextFilter { getCode    :: Bool,
                                     getComment :: Bool,
                                     getLiteral :: Bool 
                     } deriving (Eq, Show)


filter :: ContextFilter -> Source -> Source
filter filt src =  snd $ C.mapAccumL runFilter (FiltState CodeState filt ' ') src 


-- States:

data FiltState = FiltState {
                    cstate  :: ContextState,
                    cfilter :: ContextFilter,
                    pchar   :: Char
                } deriving (Eq, Show)



data ContextState = CodeState       | 
                    CommentCState   | 
                    CommentCppState | 
                    LiteralStateS   |
                    LiteralStateC
                        deriving (Eq, Show)


runFilter :: FiltState -> Char -> (FiltState, Char) 
runFilter filtstate c = (state', charFilter (cxtFilter cxt (cfilter filtstate)) c)
                        where (cxt, state') = charParser (pchar filtstate, c) filtstate


charFilter :: Bool -> Char -> Char
charFilter  _ '\n' = '\n'
charFilter  cond c
    | cond = c
    | otherwise = ' '


cxtFilter :: Context -> ContextFilter -> Bool
cxtFilter Code    = getCode 
cxtFilter Comment = getComment 
cxtFilter Literal = getLiteral 


charParser :: (Char,Char) -> FiltState -> (Context, FiltState)

charParser (p,c) filtstate@(FiltState CodeState _ _) 
    | p == '/'  && c == '/'  = (Code, filtstate { cstate = CommentCppState, pchar = c })
    | p == '/'  && c == '*'  = (Code, filtstate { cstate = CommentCState,   pchar = c })
    | p /= '\\' && c == '"'  = (Code, filtstate { cstate = LiteralStateS,   pchar = c })
    | p /= '\\' && c == '\'' = (Code, filtstate { cstate = LiteralStateC,   pchar = c }) 
    | p == '\\' && c == '\\' = (Code, filtstate { pchar = ' ' })
    | otherwise = (Code, filtstate { pchar = c } )
                                       
charParser (_,c) filtstate@(FiltState CommentCppState _ _)
    | c == '\n' = (Comment, filtstate { cstate = CodeState, pchar = c })
    | otherwise = (Comment, filtstate { pchar = c })

charParser (p,c) filtstate@(FiltState CommentCState _ _)
    | p == '*' && c == '/'  = (Comment, filtstate { cstate = CodeState, pchar = c})
    | otherwise = (Comment, filtstate { pchar = c })

charParser (p,c) filtstate@(FiltState LiteralStateS _ _)
    | p /= '\\' && c == '"'  = (Code,    filtstate { cstate = CodeState, pchar = c})
    | p == '\\' && c == '\\' = (Literal, filtstate { pchar = ' '})
    | otherwise = (Literal, filtstate { pchar = c }) 

charParser (p,c) filtstate@(FiltState LiteralStateC _ _)
    | p /= '\\' && c == '\'' = (Code, filtstate { cstate = CodeState, pchar = c })
    | p == '\\' && c == '\\' = (Literal, filtstate { pchar = ' '})
    | otherwise = (Literal, filtstate { pchar = c})

