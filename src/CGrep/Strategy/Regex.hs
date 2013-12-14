--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

{-# LANGUAGE FlexibleContexts #-} 

module CGrep.Strategy.Regex (cgrepRegex) where

import qualified Data.ByteString.Char8 as C

import Text.Regex.Posix
import Data.Array

import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Data.Maybe

import Options 
import Debug
 
 
cgrepRegex :: CgrepFunction
cgrepRegex opt ps f = do

    source <- getText (ignore_case opt) f 
    
    let filename = if f == Nothing then "<stdin>" 
                                   else fromJust f

    putStrLevel1 (debug opt) $ "strategy  : running regex on " ++ filename ++ "..."

    let filtered = if code opt || comment opt || literal opt
                     then filterContext (lookupLang filename) ContextFilter { getCode = code opt, getComment = comment opt, getLiteral = literal opt } source
                     else source

    let multi_filtered = spanMultiLine (multiline opt) filtered


    let matchesOffsets = map (\(_, (str, (off,_) )) -> (off, [str] )) $ 
                    concatMap assocs $ concat $ map (\pat -> multi_filtered =~ pat :: [MatchText C.ByteString]) ps 

    let matches = mergeMatches $ map (\(off, xs) -> (1 + offsetToLine multi_filtered off, map C.unpack xs)) matchesOffsets

    putStrLevel2 (debug opt) $ "matches: " ++ show matches
    
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack filtered ++ "\n---"

    return $ mkOutput opt filename source matches


