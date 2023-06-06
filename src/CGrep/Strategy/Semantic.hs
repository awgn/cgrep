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
{-# LANGUAGE MultiWayIf #-}

module CGrep.Strategy.Semantic (search) where

import qualified Data.ByteString.Char8 as C

import CGrep.Parser.Token

import CGrep.ContextFilter
import CGrep.Common
    ( Text8,
      trim,
      getTargetName,
      getTargetContents,
      expandMultiline,
      ignoreCase, trim8, subText )

import CGrep.Search
import CGrep.Output ( Output, mkOutputElements, runSearch )
import CGrep.Parser.Line

import CGrep.Parser.Atom
    ( Atom(..),
      mkAtomFromToken,
      combineAtoms,
      filterTokensWithAtoms)

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Data.List ( sortBy, nub )
import Data.Function ( on )
import Data.Maybe ( mapMaybe )

import Reader ( ReaderIO, Env (..) )
import Verbose ( putMsgLnVerbose )
import Util ( rmQuote8 )
import CGrep.Parser.Chunk

import System.Posix.FilePath ( RawFilePath, takeBaseName )

import CGrep.Language ( Language )
import CGrep.LanguagesMap
    ( languageLookup, LanguageInfo, contextFilter )
import System.IO ( stderr )

import qualified Data.Sequence as S
import Data.Foldable ( Foldable(toList) )
import Debug.Trace
import Data.Coerce

search :: Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search info f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    let [text''', _, text', _ ] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (languageLookup opt filename) filt True
                                                 , ignoreCase opt
                                                 ]

        filt = mkContextFilter opt ~! contextBitComment

    -- pre-process patterns

        patterns   = map (parseTokens (snd <$> info) . contextFilter (languageLookup opt filename) filt True) ps
        patterns'  = map (mkAtomFromToken <$>) patterns
        patterns'' = map (combineAtoms . map (:[])) (toList <$> patterns')

        identifiers = mapMaybe
          (\case
             Raw (Token (Chunk ChunkString xs _)) -> Just (rmQuote8 $ trim8 xs)
             Raw (Token (Chunk ChunkIdentifier "OR" _)) -> Nothing
             Raw t -> Just (tToken t)
             _ -> Nothing)
          (concatMap toList patterns')

    -- put banners...

    putMsgLnVerbose 2 stderr $ "strategy  : running generic semantic search on " <> filename <> "..."
    putMsgLnVerbose 2 stderr $ "atoms     : " <> show patterns'' <> " -> identifiers: " <> show identifiers
    putMsgLnVerbose 3 stderr $ "---\n" <> text''' <> "\n---"

    let indices' = searchStringIndices identifiers text'

    runSearch opt filename (eligibleForSearch identifiers indices') $ do

        -- parse source code, get the Generic.Chunk list...

        let tokens = toList $ parseTokens (snd <$> info) (subText indices' text''')

        -- get matching tokens ...

        let tokens' = sortBy (compare `on` tOffset) $ nub $ concatMap (\ms -> filterTokensWithAtoms opt ms tokens) patterns''

        -- convert Tokens to Chunks

        let matches = coerce tokens' :: [Chunk]

        putMsgLnVerbose 2 stderr $ "tokens    : " <> show tokens
        putMsgLnVerbose 2 stderr $ "matches   : " <> show matches

        let lineOffsets = getAllLineOffsets text

        mkOutputElements lineOffsets filename text text''' matches
