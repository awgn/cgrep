--
-- Copyright (c) 2013-2019 Nicola Bonelli <nicola@pfq.io>
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

module CGrep.Strategy.Cpp.Tokenizer (search) where

import qualified Data.ByteString.Char8 as C

import qualified CGrep.Parser.Cpp.Token as Cpp

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.Filter
    ( ContextFilter(getFilterComment), mkContextFilter, contextFilter )
import CGrep.Lang ( getFileLang )
import CGrep.Common
    ( Text8,
      expandMultiline,
      getTargetContents,
      getTargetName,
      ignoreCase,
      runSearch,
      shallowSearch )
import CGrep.Output ( Output, mkOutput )
import CGrep.Distance ( (~==) )

import Data.List ( isSuffixOf, isInfixOf, isPrefixOf )

import Reader ( OptionIO )
import Options
    ( Options(identifier, directive, keyword, header, string, number,
              char, oper, edit_dist, word_match, prefix_match, suffix_match) )
import Verbose
import Util ( notNull )


search :: FilePath -> [Text8] -> OptionIO [Output]
search f ps = do

    opt  <- reader snd
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let filt = (mkContextFilter opt) { getFilterComment = False }

    let [text''', _ , text', _] = scanr ($) text [ expandMultiline opt
                                                 , contextFilter (getFileLang opt filename) filt
                                                 , ignoreCase opt
                                                 ]


    putStrLn1 $ "strategy  : running C/C++ token search on " ++ filename ++ "..."

    let quick = all notNull $ shallowSearch ps text'

    runSearch opt filename quick $ do

        -- parse source code, get the Cpp.Token list...

        let tokens = Cpp.tokenizer text'''

        -- context-filterting...

            tokens'= filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt,
                                                               Cpp.filtDirective  = directive opt,
                                                               Cpp.filtKeyword    = keyword opt,
                                                               Cpp.filtHeader     = header opt,
                                                               Cpp.filtString     = string opt,
                                                               Cpp.filtNumber     = number opt,
                                                               Cpp.filtChar       = char opt,
                                                               Cpp.filtOper       = oper opt}) tokens

        -- filter tokens...

            tokens'' = cppTokenFilter opt (map C.unpack ps) tokens'

        -- convert Cpp.Tokens to CGrep.Tokens

            matches = map (\t -> let off = fromIntegral (Cpp.toOffset t) in (off, Cpp.toString t)) tokens'' :: [(Int, String)]

        putStrLn2 $ "tokens    : " ++ show tokens
        putStrLn2 $ "tokens'   : " ++ show tokens'
        putStrLn2 $ "tokens''  : " ++ show tokens''
        putStrLn2 $ "matches   : " ++ show matches
        putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' matches


cppTokenFilter :: Options -> [String] -> [Cpp.Token] -> [Cpp.Token]
cppTokenFilter opt patterns tokens
    | edit_dist    opt = filter (\t -> any (\p -> p ~== Cpp.toString t) patterns) tokens
    | word_match   opt = filter ((`elem` patterns) . Cpp.toString) tokens
    | prefix_match opt = filter ((\t -> any (`isPrefixOf`t) patterns) . Cpp.toString) tokens
    | suffix_match opt = filter ((\t -> any (`isSuffixOf`t) patterns) . Cpp.toString) tokens
    | otherwise        = filter ((\t -> any (`isInfixOf` t) patterns) . Cpp.toString) tokens
