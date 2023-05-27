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

import CGrep.Parser.Token
    ( Token(..), TokenFilter(..), parseTokens, filterToken )

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
import CGrep.Language ( Language )
import CGrep.LanguagesMap
    ( languageLookup, LanguageInfo, contextFilter )

import CGrep.Search
import Data.List ( isSuffixOf, isInfixOf, isPrefixOf )

import Reader ( ReaderIO, Env (..) )
import Options
    ( Options(identifier, keyword, string, number, operator, edit_dist,
              word_match, prefix_match, suffix_match) )
import Verbose ( putMsgLnVerbose )
import CGrep.Chunk (Chunk (..))
import System.Posix.FilePath (RawFilePath)
import System.IO (stderr)

import Data.Foldable ( Foldable(toList) )
import CGrep.Types (Offset)
import Debug.Trace

search :: Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search linfo f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let filt = mkContextFilter opt ~! contextBitComment

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (languageLookup opt filename) filt True
                                                 , ignoreCase opt
                                                 ]


    putMsgLnVerbose 2 stderr $ "strategy: running token search on " <> filename <> "..."
    putMsgLnVerbose 3 stderr $ "---\n" <> text''' <> "\n---"

    let indices' = searchStringIndices ps text'

    runSearch opt filename (eligibleForSearch ps indices') $ do

        -- parse source code, get the token list...

        let tokens = {-# SCC tok_0 #-} toList $ parseTokens (snd <$> linfo) (subText indices' ps text''')

        -- filter the tokens...

            tokens'= {-# SCC tok_1 #-} filter (filterToken TokenFilter { filtIdentifier = identifier opt,
                                                       filtKeyword    = keyword opt,
                                                       filtString     = string opt,
                                                       filtNumber     = number opt,
                                                       filtOperator   = operator opt}) tokens


            tokens'' = {-# SCC tok_2 #-} genericTokenFilter opt ps tokens'

        -- convert matching tokens to chunks

            matches = {-# SCC tok_3 #-} map (\t -> let off = fromIntegral (toOffset t) in Chunk off (toString t)) tokens'' :: [Chunk]

        putMsgLnVerbose 2 stderr $ "tokens    : " <> show tokens
        putMsgLnVerbose 2 stderr $ "tokens'   : " <> show tokens'
        putMsgLnVerbose 2 stderr $ "tokens''  : " <> show tokens''
        putMsgLnVerbose 2 stderr $ "matches   : " <> show matches

        let lineOffsets = getAllLineOffsets text

        mkOutputElements lineOffsets filename text text''' matches


genericTokenFilter :: Options -> [C.ByteString] -> [Token] -> [Token]
genericTokenFilter opt patterns tokens
    | edit_dist    opt = filter (\t -> any (\p -> C.unpack p ~==  (C.unpack .toString) t) patterns) tokens
    | word_match   opt = filter ((`elem` patterns) . toString) tokens
    | prefix_match opt = filter ((\t -> any (`C.isPrefixOf`t) patterns) . toString) tokens
    | suffix_match opt = filter ((\t -> any (`C.isSuffixOf`t) patterns) . toString) tokens
    | otherwise        = filter ((\t -> any (`C.isInfixOf` t) patterns) . toString) tokens
