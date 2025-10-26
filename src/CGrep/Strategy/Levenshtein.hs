--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.Strategy.Levenshtein (search) where

import CGrep.Line (buildIndex)

import Control.Monad.Trans.Reader (ask)

import CGrep.Common (
    ignoreCase,
 )
import CGrep.ContextFilter (mkContextFilter)
import CGrep.Distance ((~==))
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (
    FileTypeInfo,
 )
import CGrep.FileTypeMapTH (
    mkContextFilterFn,
 )

import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Chunk (cToken, parseChunks)

import Control.Concurrent (MVar)
import Data.Foldable (Foldable (toList))
import qualified Data.Text as T
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath)

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info filename text patterns _strict = do
    Env{..} <- ask
    let lindex = buildIndex text

    -- transform text
    let !contextFilter = mkContextFilterFn (fst <$> info) (mkContextFilter opt) False

    let text' = ignoreCase opt text
    let text'' = contextFilter $ text'

    -- parse source code, get the Cpp.Token list...

    let tokens' = parseChunks (snd <$> info) text''

    -- filter tokens...

    let patterns' = map T.unpack patterns
    let matches = filter (\t -> any (\p -> p ~== T.unpack (cToken t)) patterns') (toList tokens')

    putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"
    putMessageLnVerb 1 lock stderr $ "strategy  : running edit-distance (Levenshtein) search on " <> show filename
    putMessageLnVerb 2 lock stderr $ "tokens    : " <> show tokens'
    putMessageLnVerb 2 lock stderr $ "matches   : " <> show matches

    mkMatches lindex filename text'' matches
