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

module CGrep.Strategy.BoyerMoore (search) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask, reader)
import Data.List (genericLength, isPrefixOf, isSuffixOf)

import CGrep.Common (
    Text8,
    expandMultiline,
    getTargetContents,
    getTargetName,
    ignoreCase,
 )
import CGrep.ContextFilter (mkContextFilter)
import CGrep.FileType (FileType)
import CGrep.FileTypeMap (FileTypeInfo, contextFilter, fileTypeLookup)
import CGrep.Output (Output, mkOutputElements, runSearch)
import CGrep.Search
import CGrep.Types (Offset)

import CGrep.Parser.Chunk
import Data.Int (Int64)
import Options (Options (prefix_match, suffix_match, word_match))
import PutMessage (putMessageLnVerb)
import Reader (Env (..), ReaderIO)

import System.IO (stderr)
import System.OsPath (OsPath)

import CGrep.Parser.Line (getLineByOffset, getLineOffsets)
import Data.Array (indices)
import qualified Data.Vector.Unboxed as UV

search :: Maybe (FileType, FileTypeInfo) -> OsPath -> [Text8] -> ReaderIO [Output]
search info f patterns = do
    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let ctxFilter = mkContextFilter opt

    let [text''', _, text', _] =
            scanr
                ($)
                text
                [ expandMultiline opt
                , contextFilter (fst <$> info) ctxFilter False
                , ignoreCase opt
                ]

    -- make shallow search

    let indices' = searchStringIndices patterns text'
    let indices''' = searchStringIndices patterns text'''

    -- search for matching tokens

    let ctor = Chunk ChunkUnspec

    let chunks = concat $ zipWith (\p xs -> (p `ctor`) <$> xs) patterns indices'''

    -- filter exact/partial matching tokens

    let lineOffsets = getLineOffsets (fromIntegral $ C.length text) text

    let chunks' =
            if word_match opt || prefix_match opt || suffix_match opt
                then filter (checkChunk opt lineOffsets (snd <$> info) text''') chunks
                else chunks

    putMessageLnVerb 3 stderr $ "---\n" <> text''' <> "\n---"
    putMessageLnVerb 1 stderr $ "strategy  : running Boyer-Moore search on " <> show filename

    runSearch opt filename (eligibleForSearch patterns indices') $ do
        putMessageLnVerb 2 stderr $ "chunks'   : " <> show chunks'
        mkOutputElements lineOffsets filename text text''' chunks'

checkChunk :: Options -> UV.Vector Int64 -> Maybe FileTypeInfo -> Text8 -> Chunk -> Bool
checkChunk opt vec info text chunk
    | word_match opt = let !off = cOffset chunk - off' in any (\chunk' -> cOffset chunk' == off && cToken chunk' == cToken chunk) cs
    | prefix_match opt = any (\chunk' -> cToken chunk `C.isPrefixOf` cToken chunk' && cOffset chunk' + off' == cOffset chunk) cs
    | suffix_match opt = any (\chunk' -> cToken chunk `C.isSuffixOf` cToken chunk' && cOffset chunk' + off' + fromIntegral (C.length (cToken chunk') - C.length (cToken chunk)) == cOffset chunk) cs
    | otherwise = undefined
  where
    (# line', off' #) = getLineByOffset (cOffset chunk) text vec
    cs = parseChunks info line'
