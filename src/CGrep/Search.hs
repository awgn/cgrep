--
-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
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
{-# LANGUAGE MultiWayIf #-}

module CGrep.Search ( runSearch , isRegexp) where

import qualified CGrep.Strategy.BoyerMoore       as BoyerMoore
import qualified CGrep.Strategy.Levenshtein      as Levenshtein
import qualified CGrep.Strategy.Regex            as Regex
import qualified CGrep.Strategy.Tokenizer        as Tokenizer
import qualified CGrep.Strategy.Semantic         as Semantic

import Control.Monad.Trans.Reader ( reader, ask, local )
import Control.Monad.Catch ( SomeException, MonadCatch(catch) )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import System.IO ( stderr, hPutStrLn )

import CGrep.Language ( Language(Cpp, C))
import CGrep.LanguagesMap ( languageLookup, languageInfoLookup )
import CGrep.Common ( Text8, takeN )
import CGrep.Output ( Output, showFileName )

import Data.List ( elemIndex )
import Data.Maybe ( isJust )
import Options ( Options(..) )
import Reader ( OptionIO, Env (..) )
import Data.Functor (($>))
import qualified Data.ByteString.Char8 as C

hasLanguage :: FilePath -> Options -> [Language] -> Bool
hasLanguage path opt xs = isJust $ languageLookup opt path >>= (`elemIndex` xs)
{-# INLINE hasLanguage #-}


hasTokenizerOpt :: Options -> Bool
hasTokenizerOpt Options{..} =
  identifier ||
  keyword    ||
  number     ||
  string     ||
  operator


isRegexp :: Options -> Bool
isRegexp opt = regex_posix opt || regex_pcre opt
{-# INLINE isRegexp #-}


runSearch :: FilePath -> [Text8] -> OptionIO [Output]
runSearch filename patterns = do
  Env{..} <- ask
  let info = languageInfoLookup opt filename
  catch ( do
      local (\env -> env{langType = fst <$> info, langInfo = snd <$> info}) $
        if | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt) && edit_dist opt -> Levenshtein.search filename patterns
           | (not . isRegexp) opt && not (hasTokenizerOpt opt) && not (semantic opt)                  -> BoyerMoore.search filename patterns
           | (not . isRegexp) opt && semantic opt                                                     -> Semantic.search filename patterns
           | (not . isRegexp) opt                                                                     -> Tokenizer.search filename patterns
           | isRegexp opt                                                                             -> Regex.search filename patterns
           | otherwise                                                                                -> undefined
   )
   (\e -> let msg = show (e :: SomeException) in
       liftIO $ C.hPutStrLn stderr (C.pack (showFileName conf opt filename <> ": exception: " <> takeN 80 msg)) $> [ ])
