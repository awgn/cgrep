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

module CGrep.Strategy.BoyerMoore (search) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)

import CGrep.Common (
    -- expandMultiline,
    getTargetContents,
    getTargetName,
    ignoreCase,
 )
import CGrep.ContextFilter (mkContextFilter, mustRunContextFilter)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo)
import CGrep.FileTypeMapTH (contextFilter)
import CGrep.Match (Match, mkMatches)
import CGrep.Common(runSearch)

import CGrep.Parser.Chunk
import Options (Options (..))
import PutMessage (putMessageLnVerb, putMessageLn)
import Reader (Env (..), ReaderIO)

import System.IO (stderr)
import System.OsPath (OsPath)

import CGrep.Line (getLineByOffset, getLineOffsets, buildIndex)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Text as T
import CGrep.Text (textIndices, textSlice, textContainsOneOf)
import qualified Data.Text.Unsafe as TU
import qualified Data.Text.Internal.Fusion as TIF

search :: Maybe (FileType, FileTypeInfo) -> OsPath -> [T.Text] -> Bool -> ReaderIO [Match]
search info f patterns _strict = do
    Env{..} <- ask
    text <- liftIO $ getTargetContents f
    let filename = getTargetName f
    let !lindex = buildIndex text

    -- transform text...
    let ctxFilter = mkContextFilter opt
    let mustRunStream = ignore_case opt || mustRunContextFilter ctxFilter;

    let text' = if mustRunStream
                then (TIF.unstream .
                     contextFilter (fst <$> info) ctxFilter False .
                     ignoreCase opt .
                     TIF.stream) text
                else text;

    -- make shallow search
    let !eligibleForSearch = textContainsOneOf patterns text

    -- search for matching tokens
    let lineOffsets = getLineOffsets text'

    putMessageLnVerb 3 stderr $ "---\n" <> text' <> "\n---"
    putMessageLnVerb 1 stderr $ "strategy  : running Boyer-Moore search on " <> show filename

    runSearch opt lindex filename eligibleForSearch $ do

        let indices' = textIndices patterns text'

        let chunks = concat $ zipWith (\p offsets ->
                let blen = TU.lengthWord8 p
                 in map (\offset -> Chunk ChunkUnspec (textSlice text' offset blen)) offsets) patterns indices'

        let chunks' =
                if word_match opt || prefix_match opt || suffix_match opt
                    then filter (filterChunk opt lineOffsets (snd <$> info) text') chunks
                    else chunks

        putMessageLnVerb 2 stderr $ "matches   : " <> show chunks'
        putMessageLnVerb 2 stderr $ "lindex    : " <> show lindex
        mkMatches lindex filename text' chunks'


filterChunk :: Options -> UV.Vector Int -> Maybe FileTypeInfo -> T.Text -> Chunk -> Bool
filterChunk opts loff info text chunk
    | word_match opts = let !off = cOffset chunk - off' in any (\chunk' -> cOffset chunk' == off && cToken chunk' == cToken chunk) cs
    | prefix_match opts = any (\chunk' -> cToken chunk `T.isPrefixOf` cToken chunk' && cOffset chunk' + off' == cOffset chunk) cs
    | suffix_match opts = any (\chunk' -> cToken chunk `T.isSuffixOf` cToken chunk' && cOffset chunk' + off' + (T.length (cToken chunk') - T.length (cToken chunk)) == cOffset chunk) cs
    | otherwise = undefined
  where
    (# line', off' #) = getLineByOffset (cOffset chunk) text loff
    cs = parseChunks info line'
