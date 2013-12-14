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

module CGrep.Strategy.Context (cgrepContext) where

import qualified Data.ByteString.Char8 as C

import CGrep.StringLike
import CGrep.Filter 
import CGrep.Lang
import CGrep.Common

import Options 
import Debug

cgrepContext :: CgrepFunction
cgrepContext opt ps f = do
    
    putStrLevel1 (debug opt) $ "strategy  : running context-aware parser on " ++ f ++ "..."
    
    source <- if f == "" then slGetContents (ignore_case opt)
                         else slReadFile (ignore_case opt) f
    
    let filtered = filterContext (lookupLang f) ContextFilter { getCode = code opt, getComment = comment opt, getLiteral = literal opt } source
    
    let multi_filtered = spanMultiLine (multiline opt) filtered
    
    let content = zip [1..] $ C.lines multi_filtered
    
    let matches = concatMap (basicGrep opt ps) content

    putStrLevel2 (debug opt) $ "matches: " ++ show matches
    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack filtered ++ "\n---"

    return $ mkOutput opt f source matches 

