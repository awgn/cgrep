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
import Control.Monad (when)

import Options 
import Util

import qualified CGrep.Strategy.Cpp.Token  as Cpp


cgrepCppTokenizer :: CgrepFunction
cgrepCppTokenizer opt ps f = do

    source <- if f == "" then slGetContents (ignore_case opt)  
                         else slReadFile (ignore_case opt) f

    let filtered = filterContext (lookupLang f) (mkContextFilter opt) source
    
    
    let tks      = filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt, 
                                                             Cpp.filtDirective  = directive opt,
                                                             Cpp.filtKeyword    = keyword opt,
                                                             Cpp.filtHeader     = header opt, 
                                                             Cpp.filtString     = string opt,
                                                             Cpp.filtNumber     = number opt,
                                                             Cpp.filtChar       = char opt,
                                                             Cpp.filtOper       = oper opt}) (Cpp.tokens source)

    when (debug opt) $ do 
        C.putStrLn filtered
        print opt
        print tks 

    let content  = C.lines source
    let tks_res  = simpleTokenGrep opt f lps tks 

    return $ map (\t -> let ln = fromIntegral (Cpp.lineno t) in Output f (ln+1) (content !! ln) [] ) tks_res
        where lps = map C.unpack ps


mkContextFilter :: Options -> ContextFilter
mkContextFilter opt = if not (code opt && comment opt && literal opt) 
                       then ContextFilter { getCode = True,     getComment = False, getLiteral = True }
                       else ContextFilter { getCode = code opt, getComment = False, getLiteral = literal opt }


simpleTokenGrep :: Options -> FilePath -> [String] -> [Cpp.Token] -> [Cpp.Token]
simpleTokenGrep opt _ ps = filter (notNull . slGrep (word opt) (invert_match opt) ps . Cpp.toString) 




