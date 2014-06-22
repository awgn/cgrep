--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the StopNerds Public License as published by
-- the StopNerds Foundation; either version 1 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- StopNerds Public License for more details.
--
-- You should have received a copy of the StopNerds Public License
-- along with this program; if not, see <http://stopnerds.org/license/>.
--

module CGrep.Strategy.Levenshtein (search) where

import qualified Data.ByteString.Char8 as C

import CGrep.Filter
import CGrep.Lang
import CGrep.Common
import CGrep.Output
import CGrep.Distance
import CGrep.Token

import Options
import Debug

search :: CgrepFunction
search opt ps f = do

    let filename = getFileName f

    text <- getText f

    -- transform text

    let text' = ignoreCase opt . expandMultiline opt . contextFilter (getLang opt filename) (mkContextFilter opt) $ text

    -- parse source code, get the Cpp.Token list...

    let tokens' = tokenizer text'

    -- filter tokens...

    let patterns = map C.unpack ps

    let matches  = filter (\t -> any (\p -> p ~== snd t) patterns) tokens'

    putStrLevel1 (debug opt) $ "strategy  : running edit-distance (Levenshtein) search on " ++ filename ++ "..."
    putStrLevel2 (debug opt) $ "tokens'   : " ++ show tokens'
    putStrLevel2 (debug opt) $ "matches   : " ++ show matches

    putStrLevel3 (debug opt) $ "---\n" ++ C.unpack text' ++ "\n---"

    return $ mkOutput opt filename text matches

