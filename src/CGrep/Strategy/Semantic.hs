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

module CGrep.Strategy.Semantic (search) where

import qualified Data.ByteString.Char8 as C

import CGrep.Parser.Token
    ( Token(TokenIdentifier, TokenString, toOffset, toString),
      parseTokens )

import CGrep.ContextFilter
import CGrep.Common
    ( Text8,
      trim,
      getTargetName,
      getTargetContents,
      stringSearch,
      quickMatch,
      runSearch,
      expandMultiline,
      ignoreCase, trim8 )
import CGrep.Output ( Output, mkOutputElements )

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
import Verbose ( putStrLnVerbose )
import Util ( notNull, rmQuote8 )
import CGrep.Chunk (Chunk (..))

import System.Posix.FilePath ( RawFilePath, takeBaseName )

import CGrep.Language ( Language )
import CGrep.LanguagesMap
    ( languageLookup, LanguageInfo, contextFilter )

search :: Maybe (Language, LanguageInfo) -> RawFilePath -> [Text8] -> ReaderIO [Output]
search linfo f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text


    let [text''', text'', text', _ ] = scanr ($) text [ expandMultiline opt
                                                      , contextFilter (languageLookup opt filename) filt True
                                                      , ignoreCase opt
                                                      ]

        filt = mkContextFilter opt ~! contextBitComment


    -- pre-process patterns

        patterns   = map (parseTokens (snd <$> linfo) . contextFilter (languageLookup opt filename) filt True) ps  -- [ [t1,t2,..], [t1,t2...] ]
        patterns'  = map (map mkAtomFromToken) patterns                                                     -- [ [w1,w2,..], [w1,w2,..] ]
        patterns'' = map (combineAtoms . map (:[])) patterns'                                               -- [ [m1,m2,..], [m1,m2,..] ] == [[[w1], [w2],..], [[w1],[w2],..]]


        identif = mapMaybe (\case
                            Token (TokenString xs _)       -> Just (rmQuote8 $ trim8 xs)
                            Token (TokenIdentifier "OR" _) -> Nothing
                            Token t                        -> Just (toString t)
                            _                              -> Nothing
                            ) . concat $ patterns'

    -- put banners...

    putStrLnVerbose 2 $ "strategy  : running generic semantic search on " <> C.unpack filename <> "..."
    putStrLnVerbose 2 $ "atoms     : " <> show patterns'' <> " -> identifiers: " <> show identif
    putStrLnVerbose 3 $ "---\n" <> C.unpack text''' <> "\n---"

    let quick = quickMatch ps $ stringSearch identif text'

    runSearch opt filename quick $ do

        -- parse source code, get the Generic.Chunk list...

        let tokens = parseTokens (snd <$> linfo) text'''

        -- get matching tokens ...

        let tokens' = sortBy (compare `on` toOffset) $ nub $ concatMap (\ms -> filterTokensWithAtoms opt ms tokens) patterns''

        -- convert Tokens to Chunks

        let matches = map (\t -> let n = fromIntegral (toOffset t) in Chunk n (toString t)) tokens' :: [Chunk]

        putStrLnVerbose 2 $ "tokens    : " <> show tokens
        putStrLnVerbose 2 $ "matches   : " <> show matches

        mkOutputElements filename text text''' matches
