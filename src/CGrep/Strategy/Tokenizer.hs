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
{-# LANGUAGE OverloadedStrings #-}

module CGrep.Strategy.Tokenizer (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.ContextFilter
    ( contextBitComment, (~!), mkContextFilter )
import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase,
      subText
      )
import CGrep.Output ( Output, mkOutputElements, runSearch )
import CGrep.Distance ( (~==) )

import CGrep.Parser.Line
import CGrep.Parser.Token

import CGrep.FileType ( FileType )
import CGrep.FileTypeMap
    ( fileTypeLookup, FileTypeInfo, contextFilter )

import CGrep.Search
import Data.List ( isSuffixOf, isInfixOf, isPrefixOf )

import Reader ( ReaderIO, Env (..) )
import Options
    ( Options(identifier, keyword, string, number, operator, edit_dist,
              word_match, prefix_match, suffix_match, nativeType) )
import Verbose ( putMsgLnVerbose )

import CGrep.Parser.Chunk (Chunk(..))
import System.Posix.FilePath (RawFilePath)
import System.IO (stderr)

import Data.Foldable ( Foldable(toList) )
import CGrep.Types (Offset)
import Data.Coerce ( coerce )

import qualified Data.Sequence as S
import Util ( mapMaybe' )

search :: Maybe (FileType, FileTypeInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search info f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let filt = mkContextFilter opt ~! contextBitComment

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (fst <$> fileTypeLookup opt filename) filt True
                                                 , ignoreCase opt
                                                 ]


    putMsgLnVerbose 2 stderr $ "strategy: running token search on " <> filename <> "..."
    putMsgLnVerbose 3 stderr $ "---\n" <> text''' <> "\n---"

    let indices' = searchStringIndices ps text'

    runSearch opt filename (eligibleForSearch ps indices') $ do

        -- parse source code, get the token list...

        let tfilter = TokenFilter {
                tfIdentifier = identifier opt,
                tfKeyword    = keyword opt,
                tfNativeType = nativeType opt,
                tfString     = string opt,
                tfNumber     = number opt,
                tfOperator   = operator opt,
                tfBracket    = False }

        let tokens = {-# SCC tok_0 #-} parseTokens tfilter (snd <$> info) (subText indices' text''')

        -- filter tokens and make chunks

            matches = {-# SCC tok_3 #-} mapMaybe' (tokenizerFilter opt ps) tokens

        putMsgLnVerbose 2 stderr $ "tokens    : " <> show tokens
        putMsgLnVerbose 2 stderr $ "matches   : " <> show matches

        let lineOffsets = getAllLineOffsets text

        mkOutputElements lineOffsets filename text text''' matches


tokenizerFilter :: Options -> [C.ByteString] -> Token -> Maybe Chunk
tokenizerFilter opt patterns token
    | isTokenUnspecified token = Nothing
    | tokenPredicate opt patterns token = Just $ coerce token
    | otherwise = Nothing
{-# INLINE tokenizerFilter #-}


tokenPredicate :: Options -> [C.ByteString] -> Token -> Bool
tokenPredicate opt patterns tokens
    | edit_dist    opt = (\t -> any (\p -> C.unpack p ~==  (C.unpack . tToken) t) patterns) tokens
    | word_match   opt = ((`elem` patterns) . tToken) tokens
    | prefix_match opt = ((\t -> any (`C.isPrefixOf`t) patterns) . tToken) tokens
    | suffix_match opt = ((\t -> any (`C.isSuffixOf`t) patterns) . tToken) tokens
    | otherwise        = ((\t -> any (`C.isInfixOf` t) patterns) . tToken) tokens
