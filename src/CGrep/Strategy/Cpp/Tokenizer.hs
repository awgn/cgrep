--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import CGrep.Filter
import CGrep.Lang
import CGrep.Common
import CGrep.Output
import CGrep.Distance

import Data.List

import Reader
import Options
import Debug
import Util


search :: FilePath -> [Text8] -> OptionT IO [Output]
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


    putStrLevel1 $ "strategy  : running C/C++ token search on " ++ filename ++ "..."

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

        putStrLevel2 $ "tokens    : " ++ show tokens
        putStrLevel2 $ "tokens'   : " ++ show tokens'
        putStrLevel2 $ "tokens''  : " ++ show tokens''
        putStrLevel2 $ "matches   : " ++ show matches
        putStrLevel3 $ "---\n" ++ C.unpack text''' ++ "\n---"

        mkOutput filename text text''' matches


cppTokenFilter :: Options -> [String] -> [Cpp.Token] -> [Cpp.Token]
cppTokenFilter opt patterns tokens
    | edit_dist    opt = filter (\t -> any (\p -> p ~== Cpp.toString t) patterns) tokens
    | word_match   opt = filter ((`elem` patterns) . Cpp.toString) tokens
    | prefix_match opt = filter ((\t -> any (`isPrefixOf`t) patterns) . Cpp.toString) tokens
    | suffix_match opt = filter ((\t -> any (`isSuffixOf`t) patterns) . Cpp.toString) tokens
    | otherwise        = filter ((\t -> any (`isInfixOf` t) patterns) . Cpp.toString) tokens


