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

module CGrep.CGrep (sanitizeOptions, runCgrep, isRegexp) where

import qualified CGrep.Strategy.BoyerMoore       as BoyerMoore
import qualified CGrep.Strategy.Levenshtein      as Levenshtein
import qualified CGrep.Strategy.Regex            as Regex
import qualified CGrep.Strategy.Cpp.Tokenizer    as CppTokenizer
import qualified CGrep.Strategy.Cpp.Semantic     as CppSemantic
import qualified CGrep.Strategy.Generic.Semantic as Semantic

import Control.Monad.Trans.Reader

import CGrep.Lang
import CGrep.Common
import CGrep.Output

import Data.List
import Data.Maybe
import Options
import Reader

hasLanguage :: FilePath -> Options -> [Lang] -> Bool
hasLanguage path opt xs = isJust $ getFileLang opt path >>= (`elemIndex` xs)


sanitizeOptions  :: FilePath -> Options -> Options
sanitizeOptions path opt =
    if hasLanguage path opt [C, Cpp]
        then opt
        else opt { identifier = False
                 , keyword    = False
                 , directive  = False
                 , header     = False
                 , string     = False
                 , char       = False
                 , oper       = False
                 }


hasTokenizerOpt :: Options -> Bool
hasTokenizerOpt Options
  { identifier = i
  , keyword    = k
  , directive  = d
  , header     = h
  , number     = n
  , string     = s
  , char       = c
  , oper       = o
  } = i || k || d || h || n || s || c || o


isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt || regex_pcre opt

runCgrep :: FilePath -> [Text8] -> OptionT IO [Output]
runCgrep filename patterns = do
    opt <- reader snd
    case () of
        _ | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> Levenshtein.search filename patterns
          | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt)                  -> BoyerMoore.search filename patterns
          | (not . isRegexp) opt && semantic opt && hasLanguage filename opt [C,Cpp]                 -> CppSemantic.search filename patterns
          | (not . isRegexp) opt && semantic opt                                                     -> Semantic.search filename patterns
          | (not . isRegexp) opt                                                                     -> CppTokenizer.search filename patterns
          | isRegexp opt                                                                             -> Regex.search filename patterns
          | otherwise                                                                                -> undefined


