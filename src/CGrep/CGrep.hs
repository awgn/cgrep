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

module CGrep.CGrep (sanitizeOptions, cgrepDispatch) where

import qualified CGrep.Strategy.BoyerMoore       as BoyerMoore
import qualified CGrep.Strategy.Levenshtein      as Levenshtein
import qualified CGrep.Strategy.Regex            as Regex
import qualified CGrep.Strategy.Cpp.Tokenizer    as CppTokenizer
import qualified CGrep.Strategy.Cpp.Semantic     as CppSemantic

import CGrep.Lang
import CGrep.Common

import Data.List
import Data.Maybe
import Options


hasLanguage :: FilePath -> Options -> [Lang] -> Bool
hasLanguage path opt xs = isJust $ getLang opt path >>= (`elemIndex` xs)


sanitizeOptions  :: FilePath -> Options -> Options
sanitizeOptions path opt = if hasLanguage path opt [C, Cpp]
                               then opt
                               else opt {
                                            identifier = False,
                                            keyword    = False,
                                            directive  = False,
                                            header     = False,
                                            string     = False,
                                            char       = False,
                                            oper       = False
                                         }

hasEditDistOpt :: Options -> Bool
hasEditDistOpt Options { edit_dist = x } = x


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


cgrepDispatch :: Options -> FilePath -> CgrepFunction

cgrepDispatch opt f
    | not (hasRegexOpt opt) && not (hasTokenizerOpt opt) && not (hasSemanticOpt opt) && hasEditDistOpt opt = Levenshtein.search
    | not (hasRegexOpt opt) && not (hasTokenizerOpt opt) && not (hasSemanticOpt opt) = BoyerMoore.search
    | not (hasRegexOpt opt) && hasSemanticOpt opt && hasLanguage f opt [C,Cpp] = CppSemantic.search
    | not (hasRegexOpt opt) = CppTokenizer.search
    | hasRegexOpt opt       = Regex.search
    | otherwise             = undefined


