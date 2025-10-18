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
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask, reader)
import qualified Data.Vector.Unboxed as UV

import Text.Regex.Base (
    AllTextMatches (getAllTextMatches),
    MatchText,
 )

#ifdef ENABLE_PCRE
import qualified Text.Regex.PCRE ((=~))
#endif
import qualified Text.Regex.TDFA ((=~))

import Data.Array (Array, elems)

import CGrep.Common (
    expandMultiline,
    getTargetContents,
    getTargetName,
    ignoreCase,
 )
import CGrep.ContextFilter (mkContextFilter)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo (..))
import CGrep.FileTypeMapTH (contextFilter, fileTypeLookup)
import CGrep.Output (OutputMatch, mkOutputMatches)

#ifdef ENABLE_PCRE
import Options (Options (regex_pcre))
#endif

import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)

import CGrep.Parser.Chunk
import CGrep.Parser.Line (getLineOffsets, getAllLineOffsets)

import System.IO (stderr)
import System.OsPath (OsPath)
import qualified Data.Text as T

search :: Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [OutputMatch]
search info f patterns strict = do
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
#ifdef ENABLE_PCRE
        (=~~~) = if regex_pcre opt then (Text.Regex.PCRE.=~) else (Text.Regex.TDFA.=~)
#else
        (=~~~) = (Text.Regex.TDFA.=~)
#endif
        tokens =
            map (\(str, (off, _)) -> Chunk ChunkUnspec str (fromIntegral off)) $
                concatMap elems $
                    patterns >>= (\p -> elems (getAllTextMatches $ text''' =~~~ p :: (Array Int) (MatchText T.Text)))

    putMessageLnVerb 3 stderr $ "---\n" <> text''' <> "\n---"
    #ifdef ENABLE_PCRE
    putMessageLnVerb 1 stderr $ "strategy  : running regex " <> (if regex_pcre opt then "(pcre)" else "(posix)") <> " search on " <> show filename
    #else
    putMessageLnVerb 1 stderr $ "strategy  : running regex (posix) search on " <> show filename
    #endif
    putMessageLnVerb 2 stderr $ "tokens    : " <> show tokens

    let lineOffsets = getAllLineOffsets text

    mkOutputMatches lineOffsets filename text text''' tokens
