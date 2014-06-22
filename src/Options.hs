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

{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import Data.Data

data Options = Options
               {
                -- Pattern:
                file        :: String,
                word_match  :: Bool,
                prefix_match:: Bool,
                suffix_match:: Bool,
                edit_dist   :: Bool,
                ignore_case :: Bool,
                regex       :: Bool,

                -- Context:
                code        :: Bool,
                comment     :: Bool,
                literal     :: Bool,

                -- Semantic:

                semantic    :: Bool,

                -- C/C++ Token:
                identifier  :: Bool,
                keyword     :: Bool,
                directive   :: Bool,
                header      :: Bool,
                number      :: Bool,
                string      :: Bool,
                char        :: Bool,
                oper        :: Bool,

                -- Output:
                no_filename    :: Bool,
                no_linenumber  :: Bool,
                lang           :: [String],
                lang_maps      :: Bool,
                force_language :: Maybe String,

                -- General:
                jobs         :: Int,
                multiline    :: Int,
                recursive    :: Bool,
                deference_recursive :: Bool,
                invert_match :: Bool,
                max_count    :: Int,
                count        :: Bool,
                show_match   :: Bool,
                color        :: Bool,
#ifdef ENABLE_HINT
                hint         :: Maybe String,
#endif
                format       :: Maybe String,
                json         :: Bool,
                xml          :: Bool,

                debug        :: Int,
                no_turbo     :: Bool,
                others       :: [String]

               } deriving (Data, Typeable, Show)

