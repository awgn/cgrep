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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module CGrep.Strategy.Regex (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Text.Regex.Base
    ( AllTextMatches(getAllTextMatches), MatchText )
import Text.Regex.Posix ( (=~) )
import Text.Regex.PCRE ( (=~) )

import Data.Array ( Array, elems )

import CGrep.Common
    ( Text8,
      getTargetName,
      getTargetContents,
      expandMultiline,
      ignoreCase )
import CGrep.Output ( Output, mkOutputElements )
import CGrep.ContextFilter ( mkContextFilter)
import CGrep.LanguagesMap ( languageLookup, contextFilter )

import Reader ( OptionIO, Env (..) )
import Options ( Options(regex_pcre) )
import Verbose ( putStrLnVerbose )

import CGrep.Chunk ( Chunk(..) )
import System.Posix.FilePath (RawFilePath)

search :: RawFilePath -> [Text8] -> OptionIO [Output]
search f patterns = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let ctxFilter = mkContextFilter opt

    let [text''', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                              , contextFilter (languageLookup opt filename) ctxFilter False
                                              , ignoreCase opt
                                              ]

    -- search for matching tokens

        (=~~~) = if regex_pcre opt then (Text.Regex.PCRE.=~) else (Text.Regex.Posix.=~)

        tokens = map (\(str, (off,_)) -> Chunk (fromIntegral off) str) $
                    concatMap elems $ patterns >>= (\p -> elems (getAllTextMatches $ text''' =~~~ p :: (Array Int) (MatchText Text8)))

    putStrLnVerbose 2 $ "strategy  : running regex " <> (if regex_pcre opt then "(pcre)" else "(posix)") <> " search on " <> C.unpack filename <> "..."
    putStrLnVerbose 3 $ "---\n" <> C.unpack text''' <> "\n---"
    putStrLnVerbose 2 $ "tokens    : " <> show tokens

    mkOutputElements filename text text''' tokens
