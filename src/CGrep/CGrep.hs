--
-- Copyright (c) 2013-2019 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE RecordWildCards #-}

module CGrep.CGrep ( sanitizeOptions
                   , runCgrep
                   , isRegexp) where

import qualified CGrep.Strategy.BoyerMoore       as BoyerMoore
import qualified CGrep.Strategy.Levenshtein      as Levenshtein
import qualified CGrep.Strategy.Regex            as Regex
import qualified CGrep.Strategy.Cpp.Tokenizer    as CppTokenizer
import qualified CGrep.Strategy.Cpp.Semantic     as CppSemantic
import qualified CGrep.Strategy.Generic.Semantic as Semantic

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.Catch ( SomeException, MonadCatch(catch) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import System.IO ( stderr, hPutStrLn )

import CGrep.Lang ( Lang(Cpp, C), getFileLang )
import CGrep.Common ( Text8, takeN )
import CGrep.Output ( Output, showFileName )

import Data.List ( elemIndex )
import Data.Maybe ( isJust )
import Options ( Options(..) )
import Reader ( OptionT )
import Config ( Config )


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
hasTokenizerOpt Options{..} =
  identifier ||
  keyword    ||
  directive  ||
  header     ||
  number     ||
  string     ||
  char       ||
  oper


isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt || regex_pcre opt

runCgrep :: Config -> Options -> FilePath -> [Text8] -> OptionT IO [Output]
runCgrep conf opts filename patterns =
    catch ( do
        opt <- reader snd
        case () of
            _ | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> Levenshtein.search filename patterns
              | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt)                  -> BoyerMoore.search filename patterns
              | (not . isRegexp) opt && semantic opt && hasLanguage filename opt [C,Cpp]                 -> CppSemantic.search filename patterns
              | (not . isRegexp) opt && semantic opt                                                     -> Semantic.search filename patterns
              | (not . isRegexp) opt                                                                     -> CppTokenizer.search filename patterns
              | isRegexp opt                                                                             -> Regex.search filename patterns
              | otherwise                                                                                -> undefined
    )
    (\e -> let msg = show (e :: SomeException) in
        liftIO $ do
            hPutStrLn stderr $ showFileName conf opts filename ++ ": exception: " ++ takeN 80 msg
            return [ ]
    )
