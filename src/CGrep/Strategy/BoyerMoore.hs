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

module CGrep.Strategy.BoyerMoore (search) where

import Control.Monad.Trans.Reader (ask)

-- expandMultiline,

import CGrep.Common (ignoreCase)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo)
import CGrep.FileTypeMapTH (mkContextFilterFn)
import CGrep.Line (buildIndex, lineOffsetsOf, sliceLineAt)
import CGrep.Match (Match, mkMatches)
import CGrep.Parser.Chunk
import CGrep.Semantic.ContextFilter (isContextFilterAll, mkContextFilter)
import CGrep.Text (textContainsOneOf, textIndices, textOffsetWord8, textSlice)
import Control.Concurrent (MVar)
import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU
import qualified Data.Vector.Unboxed as UV
import Options (Options (..))
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)
import System.IO (stderr)
import System.OsPath (OsPath)

search :: MVar () -> Maybe (FileType, FileTypeInfo) -> OsPath -> T.Text -> [T.Text] -> Bool -> ReaderIO [Match]
search lock info filename text patterns _strict = do
    Env{..} <- ask
    let lindex = buildIndex text
        filt = mkContextFilter opt
        !hasContextFilter = not (isContextFilterAll filt)
        !contextFilter = mkContextFilterFn (fst <$> info) filt False
        text' = ignoreCase opt text
        text''
            | hasContextFilter = ignoreCase opt (contextFilter text)
            | otherwise = text'

    putMessageLnVerb 3 lock stderr $ "---\n" <> text'' <> "\n---"
    putMessageLnVerb 1 lock stderr $ "strategy  : running Boyer-Moore search on " <> show filename

    if hasContextFilter && not (textContainsOneOf patterns text')
        then mkMatches lindex filename T.empty ([] :: [Chunk])
        else do
            let indices = textIndices patterns text''
                !chunks =
                    concat $
                        zipWith
                            ( \p offsets ->
                                let !blen = TU.lengthWord8 p
                                 in map (\offset -> Chunk ChunkUnspec (textSlice text'' offset blen)) offsets
                            )
                            patterns
                            indices

            if null chunks
                then mkMatches lindex filename T.empty ([] :: [Chunk])
                else do
                    let !chunks' =
                            if word_match opt || prefix_match opt || suffix_match opt
                                then
                                    let !loff = lineOffsetsOf lindex
                                        !baseOff = textOffsetWord8 text''
                                     in filter (filterChunk opt loff baseOff (snd <$> info) text'') chunks
                                else chunks

                    putMessageLnVerb 2 lock stderr $ "matches   : " <> show chunks'
                    putMessageLnVerb 2 lock stderr $ "lindex    : " <> show lindex
                    mkMatches lindex filename text'' chunks'

filterChunk :: Options -> UV.Vector Int -> Int -> Maybe FileTypeInfo -> T.Text -> Chunk -> Bool
filterChunk opts loff !baseOff info text chunk
    | word_match opts =
        let !off = cOffset chunk
            !line' = sliceLineAt loff text (off - baseOff)
            !cs = parseChunks info line'
         in any (\chunk' -> cOffset chunk' == off && cToken chunk' == cToken chunk) cs
    | prefix_match opts =
        let !off = cOffset chunk
            !line' = sliceLineAt loff text (off - baseOff)
            !cs = parseChunks info line'
         in any
                ( \chunk' ->
                    cOffset chunk' == off
                        && cToken chunk `T.isPrefixOf` cToken chunk'
                )
                cs
    | suffix_match opts =
        let !off = cOffset chunk
            !line' = sliceLineAt loff text (off - baseOff)
            !cs = parseChunks info line'
            !tokLen = TU.lengthWord8 (cToken chunk)
         in any
                ( \chunk' ->
                    let !tokLen' = TU.lengthWord8 (cToken chunk')
                     in cOffset chunk' + (tokLen' - tokLen) == off
                            && cToken chunk `T.isSuffixOf` cToken chunk'
                )
                cs
    | otherwise = False
