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

module CGrep.Strategy.Levenshtein (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader, ask )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import CGrep.ContextFilter ( mkContextFilter )
import CGrep.LanguagesMap ( languageLookup, contextFilter )
import CGrep.Common
    ( Text8,
      getTargetName,
      getTargetContents,
      expandMultiline,
      ignoreCase )
import CGrep.Output ( Output, mkOutput )
import CGrep.Distance ( (~==) )
import CGrep.Chunk ( Chunk(..) )
import CGrep.Parser.Token ( tokenizer )

import Reader ( OptionIO, Env (..) )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )

search :: FilePath -> [Text8] -> OptionIO [Output]
search f patterns = do

    Env{..} <- ask

    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let [text''', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                              , contextFilter (languageLookup opt filename) (mkContextFilter opt)
                                              , ignoreCase opt
                                              ]

    -- parse source code, get the Cpp.Token list...

        tokens' = tokenizer langInfo text'''

    -- filter tokens...

        patterns' = map C.unpack patterns
        matches  = filter (\t -> any (\p -> p ~== C.unpack (tStr t)) patterns') tokens'

    putStrLn1 $ "strategy  : running edit-distance (Levenshtein) search on " ++ filename ++ "..."
    putStrLn2 $ "tokens    : " ++ show tokens'
    putStrLn2 $ "matches   : " ++ show matches
    putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

    mkOutput filename text text''' matches
