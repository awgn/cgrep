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

module CGrep.CGrep where

import CGrep.Function

import CGrep.Strategy.Regex
import CGrep.Strategy.Simple
import CGrep.Strategy.Context
import CGrep.Strategy.Tokenizer
import CGrep.Strategy.Semantic

import CGrep.Lang
              
import Data.List
import Options


sanitizeOptions  :: FilePath -> Options -> Options
sanitizeOptions path opt = case lookupLang path >>= (`elemIndex` [C, Cpp]) of
                            Nothing -> opt {identifier = False, 
                                            keyword    = False, 
                                            directive  = False, 
                                            header     = False, 
                                            string     = False, 
                                            char       = False, 
                                            oper       = False, 
                                            semantic   = False 
                                            }
                            _       -> opt

hasContextOpt :: Options -> Bool
hasContextOpt Options{ code = c, comment = m, literal = l } = c || m || l

hasRegexOpt :: Options -> Bool
hasRegexOpt Options{ regex = x } = x

hasTokenizerOpt :: Options -> Bool
hasTokenizerOpt Options{ identifier = i, 
                         keyword    = k, 
                         directive  = d, 
                         header     = h, 
                         number     = n, 
                         string     = s, 
                         char       = c, 
                         oper       = o} = i || k || d || h || n || s || c || o

hasSemanticOpt :: Options -> Bool
hasSemanticOpt Options{ semantic = s } = s


cgrepDispatch :: Options -> CgrepFunction

cgrepDispatch opt 
    | not (hasRegexOpt opt) && not (hasContextOpt opt) && not (hasTokenizerOpt opt) && not (hasSemanticOpt opt) = cgrepSimple
    | not (hasRegexOpt opt) && not (hasTokenizerOpt opt) && not (hasSemanticOpt opt) = cgrepContext
    | not (hasRegexOpt opt) && hasSemanticOpt opt = cgrepCppSemantic
    | not (hasRegexOpt opt) = cgrepCppTokenizer
    | hasRegexOpt opt       = cgrepRegex
    | otherwise             = undefined

     
