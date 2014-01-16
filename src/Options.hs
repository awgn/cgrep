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

{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import Data.Data

data Options = Options 
               {
                -- Pattern:
                file        :: String,
                word_match  :: Bool,
                regex       :: Bool,
                ignore_case :: Bool,
                edit_dist   :: Bool,                

                -- Context:
                code        :: Bool,
                comment     :: Bool,
                literal     :: Bool,
                
                -- C/C++ Token:
                identifier  :: Bool,
                keyword     :: Bool,
                directive   :: Bool,
                header      :: Bool,
                number      :: Bool,
                string      :: Bool,
                char        :: Bool,
                oper        :: Bool,
                semantic    :: Bool,

                -- Output:
                no_filename    :: Bool,
                no_linenumber  :: Bool,
                lang           :: [String],
                lang_map       :: Bool,
                force_language :: Maybe String,

                -- General:
                jobs         :: Int,
                multiline    :: Int,
                recursive    :: Bool,
                invert_match :: Bool,
                max_count    :: Int,
                count        :: Bool,
                show_match   :: Bool,
                color        :: Bool,
                hint         :: String,

                debug        :: Int,
                others       :: [String]

               } deriving (Data, Typeable, Show)

