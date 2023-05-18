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

module CGrep.Strategy.Tokenizer (search) where

import qualified Data.ByteString.Char8 as C

import CGrep.Parser.Token
    ( Token(..), TokenFilter(..), parseTokens, filterToken )

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.ContextFilter
    ( ContextFilter(ctxComment), mkContextFilter)
import CGrep.LanguagesMap
    ( contextFilter, languageLookup, languageLookup )
import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase,
      runSearch,
      shallowSearch,
      quickMatch )
import CGrep.Output ( Output, mkOutputElements )
import CGrep.Distance ( (~==) )

import Data.List ( isSuffixOf, isInfixOf, isPrefixOf )

import Reader ( OptionIO, Env (..) )
import Options
    ( Options(identifier, keyword, string, number, operator, edit_dist,
              word_match, prefix_match, suffix_match) )
import Verbose ( putStrLnVerbose )
import Util ( notNull )
import CGrep.Chunk (Chunk (..))


search :: FilePath -> [Text8] -> OptionIO [Output]
search f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let filt = (mkContextFilter opt) { ctxComment = False }

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (languageLookup opt filename) filt True
                                                 , ignoreCase opt
                                                 ]


    putStrLnVerbose 2 $ "strategy: running token search on " ++ filename ++ "..."
    putStrLnVerbose 3 $ "---\n" ++ C.unpack text''' ++ "\n---"

    let quick = quickMatch ps $ shallowSearch ps text'

    runSearch opt filename quick $ do

        -- parse source code, get the Cpp.Chunk list...

        let tokens = parseTokens langInfo text'''

        -- token-filterting...

            tokens'= filter (filterToken TokenFilter { filtIdentifier = identifier opt,
                                                       filtKeyword    = keyword opt,
                                                       filtString     = string opt,
                                                       filtNumber     = number opt,
                                                       filtOperator   = operator opt}) tokens


        -- filter tokens...

            tokens'' = genericTokenFilter opt ps tokens'

        -- convert Tokens to Chunks

            matches = map (\t -> let off = fromIntegral (toOffset t) in Chunk off (toString t)) tokens'' :: [Chunk]

        putStrLnVerbose 2 $ "tokens    : " ++ show tokens
        putStrLnVerbose 2 $ "tokens'   : " ++ show tokens'
        putStrLnVerbose 2 $ "tokens''  : " ++ show tokens''
        putStrLnVerbose 2 $ "matches   : " ++ show matches

        mkOutputElements filename text text''' matches


genericTokenFilter :: Options -> [C.ByteString] -> [Token] -> [Token]
genericTokenFilter opt patterns tokens
    | edit_dist    opt = filter (\t -> any (\p -> C.unpack p ~==  (C.unpack .toString) t) patterns) tokens
    | word_match   opt = filter ((`elem` patterns) . toString) tokens
    | prefix_match opt = filter ((\t -> any (`C.isPrefixOf`t) patterns) . toString) tokens
    | suffix_match opt = filter ((\t -> any (`C.isSuffixOf`t) patterns) . toString) tokens
    | otherwise        = filter ((\t -> any (`C.isInfixOf` t) patterns) . toString) tokens
