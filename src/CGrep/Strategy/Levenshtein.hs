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
{-# LANGUAGE OverloadedStrings #-}

module CGrep.Strategy.Levenshtein (search) where
import CGrep.Parser.Line

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.ContextFilter ( mkContextFilter )
import CGrep.Common
    ( Text8,
      getTargetName,
      getTargetContents,
      expandMultiline,
      ignoreCase )
import CGrep.Output ( Output, mkOutputElements )
import CGrep.Distance ( (~==) )
import CGrep.Chunk ( Chunk(..), cToken )
import CGrep.Parser.Chunk ( parseChunks )
import CGrep.Language ( Language )
import CGrep.LanguagesMap
    ( languageLookup, LanguageInfo, contextFilter )

import Reader ( ReaderIO, Env (..) )
import Verbose ( putMsgLnVerbose )
import System.Posix.FilePath (RawFilePath)
import System.IO (stderr)
import Data.Foldable

search :: Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search info f patterns = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let ctxFilter = mkContextFilter opt

    let [text''', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                              , contextFilter (languageLookup opt filename) ctxFilter False
                                              , ignoreCase opt
                                              ]

    -- parse source code, get the Cpp.Token list...

        tokens' = parseChunks (snd <$> info) text'''

    -- filter tokens...

        patterns' = map C.unpack patterns
        matches  = filter (\t -> any (\p -> p ~== C.unpack (cToken t)) patterns') (toList tokens')

    putMsgLnVerbose 2 stderr $ "strategy  : running edit-distance (Levenshtein) search on " <> filename <> "..."
    putMsgLnVerbose 3 stderr $ "---\n" <> text''' <> "\n---"

    putMsgLnVerbose 2 stderr $ "tokens    : " <> show tokens'
    putMsgLnVerbose 2 stderr $ "matches   : " <> show matches

    let lineOffsets = getAllLineOffsets text

    mkOutputElements lineOffsets filename text text''' matches
