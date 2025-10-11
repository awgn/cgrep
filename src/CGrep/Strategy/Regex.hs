--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.Strategy.Regex (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask, reader)

import Text.Regex.Base (
    AllTextMatches (getAllTextMatches),
    MatchText,
 )
import Text.Regex.PCRE ((=~))
import Text.Regex.Posix ((=~))

import Data.Array (Array, elems)

import CGrep.Common (
    Text8,
    expandMultiline,
    getTargetContents,
    getTargetName,
    ignoreCase,
 )
import CGrep.ContextFilter (mkContextFilter)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo (..), contextFilter, fileTypeLookup)
import CGrep.Output (Output, mkOutputElements)

import Options (Options (regex_pcre))
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)

import CGrep.Parser.Chunk
import CGrep.Parser.Line (getAllLineOffsets)

import System.IO (stderr)
import System.OsPath (OsPath)

search :: Maybe (FileType, FileTypeInfo) -> OsPath -> [Text8] -> ReaderIO [Output]
search info f patterns = do
    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let ctxFilter = mkContextFilter opt

    let [text''', _, _, _] =
            scanr
                ($)
                text
                [ expandMultiline opt
                , contextFilter (fst <$> fileTypeLookup opt filename) ctxFilter False
                , ignoreCase opt
                ]

        -- search for matching tokens

        (=~~~) = if regex_pcre opt then (Text.Regex.PCRE.=~) else (Text.Regex.Posix.=~)

        tokens =
            map (\(str, (off, _)) -> Chunk ChunkUnspec str (fromIntegral off)) $
                concatMap elems $
                    patterns >>= (\p -> elems (getAllTextMatches $ text''' =~~~ p :: (Array Int) (MatchText Text8)))

    putMessageLnVerb 3 stderr $ "---\n" <> text''' <> "\n---"
    putMessageLnVerb 1 stderr $ "strategy  : running regex " <> (if regex_pcre opt then "(pcre)" else "(posix)") <> " search on " <> show filename
    putMessageLnVerb 2 stderr $ "tokens    : " <> show tokens

    let lineOffsets = getAllLineOffsets text

    mkOutputElements lineOffsets filename text text''' tokens
