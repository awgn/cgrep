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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module CGrep.Strategy.BoyerMoore (search) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( isSuffixOf, isPrefixOf, genericLength )

import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase)
import CGrep.Output ( Output, mkOutputElements, runSearch )
import CGrep.ContextFilter ( mkContextFilter)
import CGrep.Language ( Language )
import CGrep.LanguagesMap ( languageLookup, contextFilter, LanguageInfo )
import CGrep.Types ( Offset )
import CGrep.Search

import Reader ( ReaderIO, Env(..) )
import Options ( Options(word_match, prefix_match, suffix_match) )
import Verbose ( putMsgLnVerbose )
import CGrep.Parser.Chunk
import Data.Int ( Int64 )

import System.Posix.FilePath ( RawFilePath )
import System.IO ( stderr )

import CGrep.Parser.Line ( getLineOffsets, getLineByOffset )
import qualified Data.Vector.Unboxed as UV
import Data.Array (indices)


search :: Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search info f patterns = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let ctxFilter = mkContextFilter opt

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (fst <$> info) ctxFilter False
                                                 , ignoreCase opt
                                                 ]

    -- make shallow search

    let indices' = searchStringIndices patterns text'
    let indices''' = searchStringIndices patterns text'''

    -- search for matching tokens

    let ctor = Chunk ChunkUnspec

    let chunks = concat $ zipWith (\p xs -> (p `ctor` ) <$> xs ) patterns indices'''

    -- filter exact/partial matching tokens

    let lineOffsets = getLineOffsets (fromIntegral $ C.length text) text

    let chunks' = if word_match opt || prefix_match opt || suffix_match opt
                    then filter (checkChunk opt lineOffsets (snd <$> info) text''') chunks
                    else chunks

    putMsgLnVerbose 2 stderr $ "strategy  : running Boyer-Moore search on " <> filename
    putMsgLnVerbose 3 stderr $ "---\n" <> text''' <> "\n---"

    runSearch opt filename (eligibleForSearch patterns indices') $ do

        putMsgLnVerbose 2 stderr $ "chunks'   : " <> show chunks'
        mkOutputElements lineOffsets filename text text''' chunks'


checkChunk :: Options -> UV.Vector Int64 -> Maybe LanguageInfo -> Text8 -> Chunk -> Bool
checkChunk opt vec info text chunk
     | word_match    opt = let !off = cOffset chunk - off' in any (\chunk' -> cOffset chunk' == off && cToken chunk' == cToken chunk) cs
     | prefix_match  opt = any (\chunk' -> cToken chunk `C.isPrefixOf` cToken chunk' && cOffset chunk' + off' == cOffset chunk) cs
     | suffix_match  opt = any (\chunk' -> cToken chunk `C.isSuffixOf` cToken chunk' && cOffset chunk' + off' + fromIntegral (C.length (cToken chunk') - C.length (cToken chunk)) == cOffset chunk) cs
     | otherwise         = undefined
     where (# line',off' #) = getLineByOffset (cOffset chunk) text vec
           cs               = parseChunks info line'
