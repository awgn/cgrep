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
import qualified Data.ByteString.Search as SC

import qualified CGrep.Semantic.Cpp.Token as Cpp

import CGrep.Filter
import CGrep.Lang
import CGrep.Common
import CGrep.Output
import CGrep.Distance

import Options
import Debug

import Data.List

search :: CgrepFunction
search opt ps f = do

    let filename = getFileName f

    text <- getText f

    -- transform text

    let text' = expandMultiline opt . ignoreCase opt $ text

    -- get indices...

    let ids = concatMap (`SC.nonOverlappingIndices` text') ps

    putStrLevel1 (debug opt) $ "strategy  : running C/C++ token search on " ++ filename ++ "..."

    if null ids
        then do

            putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"
            return $ mkOutput opt filename text []

        else do

            let text'' = contextFilter (getLang opt filename) ((mkContextFilter opt) {getComment = False}) $ text'

            -- parse source code, get the Cpp.Token list...

            let tokens = Cpp.tokenizer text''

            -- context-filterting...

            let tokens'= filter (Cpp.tokenFilter Cpp.TokenFilter { Cpp.filtIdentifier = identifier opt,
                                                                   Cpp.filtDirective  = directive opt,
                                                                   Cpp.filtKeyword    = keyword opt,
                                                                   Cpp.filtHeader     = header opt,
                                                                   Cpp.filtString     = string opt,
                                                                   Cpp.filtNumber     = number opt,
                                                                   Cpp.filtChar       = char opt,
                                                                   Cpp.filtOper       = oper opt}) tokens

            -- filter tokens...

            let tokens'' = cppTokenFilter opt (map C.unpack ps) tokens'

            -- convert Cpp.Tokens to CGrep.Tokens

            let matches = map (\t -> let off = fromIntegral (Cpp.offset t) in (off, Cpp.toString t)) tokens'' :: [(Int, String)]

            putStrLevel2 (debug opt) $ "tokens    : " ++ show tokens
            putStrLevel2 (debug opt) $ "tokens'   : " ++ show tokens'
            putStrLevel2 (debug opt) $ "tokens''  : " ++ show tokens''
            putStrLevel2 (debug opt) $ "matches   : " ++ show matches

            putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text'' ++ "\n---"

            return $ mkOutput opt filename text matches


cppTokenFilter :: Options -> [String] -> [Cpp.Token] -> [Cpp.Token]
cppTokenFilter opt patterns tokens
    | edit_dist    opt = filter (\t -> any (\p -> p ~== Cpp.toString t) patterns) tokens
    | word_match   opt = filter ((`elem` patterns) . Cpp.toString) tokens
    | prefix_match opt = filter ((\t -> any (`isPrefixOf`t) patterns) . Cpp.toString) tokens
    | suffix_match opt = filter ((\t -> any (`isSuffixOf`t) patterns) . Cpp.toString) tokens
    | otherwise        = filter ((\t -> any (`isInfixOf` t) patterns) . Cpp.toString) tokens


