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

{-# LANGUAGE FlexibleContexts #-}

module CGrep.Strategy.Regex (search) where

import qualified Data.ByteString.Char8 as C

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import Text.Regex.Base
import Text.Regex.Posix
import Text.Regex.PCRE

import Data.Array

import CGrep.Common
import CGrep.Output
import CGrep.Filter
import CGrep.Lang

import Reader
import Options
import Debug



search :: FilePath -> [Text8] -> OptionT IO [Output]
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

        tokens = map (\(str, (off,_)) -> (off, C.unpack str) ) $
                    concatMap elems $ patterns >>= (\p -> elems (getAllTextMatches $ text''' =~~~ p :: (Array Int) (MatchText Text8)))

    putStrLevel1 $ "strategy  : running regex " ++ (if regex_pcre opt then "(pcre)" else "(posix)") ++ " search on " ++ filename ++ "..."
    putStrLevel2 $ "tokens    : " ++ show tokens
    putStrLevel3 $ "---\n" ++ C.unpack text''' ++ "\n---"

    mkOutput filename text text''' tokens

