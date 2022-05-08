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

import qualified CGrep.Parser.Generic.Token as Generic

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.ContextFilter
    ( ContextFilter(getFilterComment), mkContextFilter)
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
import CGrep.Output ( Output, mkOutput )
import CGrep.Distance ( (~==) )

import Data.List ( isSuffixOf, isInfixOf, isPrefixOf )

import Reader ( OptionIO, Env (..) )
import Options
    ( Options(identifier, directive, keyword, header, string, number,
              char, oper, edit_dist, word_match, prefix_match, suffix_match) )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )
import Util ( notNull )
import CGrep.Token (Token (Token))


search :: FilePath -> [Text8] -> OptionIO [Output]
search f ps = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let filt = (mkContextFilter opt) { getFilterComment = False }

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (languageLookup opt filename) filt
                                                 , ignoreCase opt
                                                 ]


    putStrLn1 $ "strategy: running token search on " ++ filename ++ "..."

    let quick = quickMatch ps $ shallowSearch ps text'

    runSearch opt filename quick $ do

        -- parse source code, get the Cpp.Token list...

        let tokens = Generic.tokenizer text'''

        -- token-filterting...

            --tokens'= filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt,
            --                                                   Cpp.filtDirective  = directive opt,
            --                                                   Cpp.filtKeyword    = keyword opt,
            --                                                   Cpp.filtHeader     = header opt,
            --                                                   Cpp.filtString     = string opt,
            --                                                   Cpp.filtNumber     = number opt,
            --                                                   Cpp.filtChar       = char opt,
            --                                                   Cpp.filtOper       = oper opt}) tokens

            tokens' = tokens

        -- filter tokens...

            tokens'' = genericTokenFilter opt (map C.unpack ps) tokens'

        -- convert Cpp.Tokens to CGrep.Tokens

            matches = map (\t -> let off = fromIntegral (Generic.toOffset t) in Token off (C.pack (Generic.toString t))) tokens'' :: [Token]

        putStrLn2 $ "tokens    : " ++ show tokens
        putStrLn2 $ "tokens'   : " ++ show tokens'
        putStrLn2 $ "tokens''  : " ++ show tokens''
        putStrLn2 $ "matches   : " ++ show matches
        putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' matches


genericTokenFilter :: Options -> [String] -> [Generic.Token] -> [Generic.Token]
genericTokenFilter opt patterns tokens
    | edit_dist    opt = filter (\t -> any (\p -> p ~== Generic.toString t) patterns) tokens
    | word_match   opt = filter ((`elem` patterns) . Generic.toString) tokens
    | prefix_match opt = filter ((\t -> any (`isPrefixOf`t) patterns) . Generic.toString) tokens
    | suffix_match opt = filter ((\t -> any (`isSuffixOf`t) patterns) . Generic.toString) tokens
    | otherwise        = filter ((\t -> any (`isInfixOf` t) patterns) . Generic.toString) tokens
