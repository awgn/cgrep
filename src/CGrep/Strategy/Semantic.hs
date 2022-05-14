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
    ( ContextFilter(ctxComment, ctxLiteral), mkContextFilter)
import CGrep.LanguagesMap ( languageLookup, contextFilter )
import CGrep.Common
    ( Text8,
      trim,
      getTargetName,
      getTargetContents,
      shallowSearch,
      quickMatch,
      runSearch,
      expandMultiline,
      ignoreCase, trim8 )
import CGrep.Output ( Output, mkOutput )

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

import Reader ( OptionIO, Env (..) )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )
import Util ( notNull, rmQuote, rmQuote8 )
import CGrep.Chunk (Chunk (..))


search :: FilePath -> [Text8] -> OptionIO [Output]
search f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text


    let [text''', text'', text', _ ] = scanr ($) text [ expandMultiline opt
                                                      , contextFilter (languageLookup opt filename) filt
                                                      , ignoreCase opt
                                                      ]

        filt = (mkContextFilter opt) { ctxComment = False, ctxLiteral = False }


    -- pre-process patterns

        patterns   = map (parseTokens langInfo . contextFilter (languageLookup opt filename) filt) ps  -- [ [t1,t2,..], [t1,t2...] ]
        patterns'  = map (map mkAtomFromToken) patterns                                            -- [ [w1,w2,..], [w1,w2,..] ]
        patterns'' = map (combineAtoms . map (:[])) patterns'                                      -- [ [m1,m2,..], [m1,m2,..] ] == [[[w1], [w2],..], [[w1],[w2],..]]

        identif = mapMaybe (\case
                            Token (TokenString xs _)       -> Just (rmQuote8 $ trim8 xs)
                            Token (TokenIdentifier "OR" _) -> Nothing
                            Token t                        -> Just (toString t)
                            _                              -> Nothing
                            ) . concat $ patterns'

    -- put banners...

    putStrLn1 $ "strategy  : running generic semantic search on " <> filename <> "..."
    putStrLn2 $ "atoms     : " <> show patterns''
    putStrLn2 $ "identif   : " <> show identif

    let quick = quickMatch ps $ shallowSearch identif text'

    runSearch opt filename quick $ do

        -- parse source code, get the Generic.Chunk list...

        let tokens = parseTokens langInfo text'''

        -- get matching tokens ...

        let tokens' = sortBy (compare `on` toOffset) $ nub $ concatMap (\ms -> filterTokensWithAtoms opt ms tokens) patterns''

        -- convert Tokens to Chunks

        let matches = map (\t -> let n = fromIntegral (toOffset t) in Chunk n (toString t)) tokens' :: [Chunk]

        putStrLn2 $ "tokens    : " <> show tokens'
        putStrLn2 $ "matches   : " <> show matches
        putStrLn3 $ "---\n" <> C.unpack text''' <> "\n---"

        mkOutput filename text text''' matches
