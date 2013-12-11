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

module CGrep.Strategy.Tokenizer (cgrepCppTokenizer) where

import qualified Data.ByteString.Char8 as C
-- import qualified Data.ByteString.Lazy.Char8 as LC

import CGrep.Function
import CGrep.Output
import CGrep.StringLike
import CGrep.Filter 
import CGrep.Lang

import Options 
import Util
import Debug

import qualified CGrep.Strategy.Cpp.Token  as Cpp


cgrepCppTokenizer :: CgrepFunction
cgrepCppTokenizer opt ps f = do

    putStrLevel1 (debug opt) $ "strategy  : running C/C++ tokenizer on " ++ f ++ "..."

    source <- if f == "" then slGetContents (ignore_case opt)  
                         else slReadFile (ignore_case opt) f

    let filtered = filterContext (lookupLang f) (mkContextFilter opt) source
    
    -- parse source code, get the Cpp.Token list...
    --
    
    let all_ts = Cpp.tokenizer filtered

    let tfilt  = filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt, 
                                                       Cpp.filtDirective  = directive opt,
                                                       Cpp.filtKeyword    = keyword opt,
                                                       Cpp.filtHeader     = header opt, 
                                                       Cpp.filtString     = string opt,
                                                       Cpp.filtNumber     = number opt,
                                                       Cpp.filtChar       = char opt,
                                                       Cpp.filtOper       = oper opt}) all_ts


    let tmatches  = tokenGrep opt f lps tfilt
    
    let matches = map (\t -> let n = fromIntegral (Cpp.lineno t) in (n+1, [])) tmatches :: [(Int, [String])]

    putStrLevel2 (debug opt) $ "tokens :  " ++ show all_ts
    putStrLevel2 (debug opt) $ "patterns: " ++ show lpt
    putStrLevel2 (debug opt) $ "matches: "  ++ show matches 

    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack filtered ++ "\n---"
    
    return $ mkOutput f source matches
        
        where  lps = map C.unpack ps
               lpt = map (Cpp.tokenizer . filterContext (Just Cpp) sourceCodeFilter) ps
    
mkContextFilter :: Options -> ContextFilter
mkContextFilter opt = if not (code opt && comment opt && literal opt) 
                       then ContextFilter { getCode = True,     getComment = False, getLiteral = True }
                       else ContextFilter { getCode = code opt, getComment = False, getLiteral = literal opt }


sourceCodeFilter :: ContextFilter 
sourceCodeFilter = ContextFilter { getCode = True, getComment = False, getLiteral = True }   


tokenGrep :: Options -> FilePath -> [String] -> [Cpp.Token] -> [Cpp.Token]
tokenGrep opt _ ps = filter (notNull . slGrep (word opt) (invert_match opt) ps . Cpp.toString) 


