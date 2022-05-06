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

{-# LANGUAGE FlexibleContexts #-}

module CGrep.Strategy.Regex (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader ( reader )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

import Text.Regex.Base
    ( AllTextMatches(getAllTextMatches), MatchText )
import Text.Regex.Posix ( (=~) )
import Text.Regex.PCRE ( (=~) )

import Data.Array ( Array, elems )

import CGrep.Common
    ( Text8,
      getTargetName,
      getTargetContents,
      expandMultiline,
      ignoreCase )
import CGrep.Output ( Output, mkOutput )
import CGrep.ContextFilter ( mkContextFilter, contextFilter )
import CGrep.Lang ( getFileLang )

import Reader ( OptionIO )
import Options ( Options(regex_pcre) )
import Verbose ( putStrLn1, putStrLn2, putStrLn3 )

import CGrep.Token ( Token(Token) )

search :: FilePath -> [Text8] -> OptionIO [Output]
search f patterns = do

    opt  <- reader snd
    text <- liftIO $ getTargetContents f

    let filename = getTargetName f

    -- transform text

    let [text''', _ , _ , _] = scanr ($) text [ expandMultiline opt
                                              , contextFilter (getFileLang opt filename) (mkContextFilter opt)
                                              , ignoreCase opt
                                              ]

    -- search for matching tokens

        (=~~~) = if regex_pcre opt then (Text.Regex.PCRE.=~) else (Text.Regex.Posix.=~)

        tokens = map (\(str, (off,_)) -> Token (fromIntegral off) str) $
                    concatMap elems $ patterns >>= (\p -> elems (getAllTextMatches $ text''' =~~~ p :: (Array Int) (MatchText Text8)))

    putStrLn1 $ "strategy  : running regex " ++ (if regex_pcre opt then "(pcre)" else "(posix)") ++ " search on " ++ filename ++ "..."
    putStrLn2 $ "tokens    : " ++ show tokens
    putStrLn3 $ "---\n" ++ C.unpack text''' ++ "\n---"

    mkOutput filename text text''' tokens
