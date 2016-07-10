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
    -- Pattern:
    {   file                :: String
    ,   word_match          :: Bool
    ,   prefix_match        :: Bool
    ,   suffix_match        :: Bool
    ,   edit_dist           :: Bool
    ,   regex_posix         :: Bool
    ,   regex_pcre          :: Bool
    ,   ignore_case         :: Bool
    -- Context:
    ,   code                :: Bool
    ,   comment             :: Bool
    ,   literal             :: Bool
    -- Semantic:
    ,   semantic            :: Bool
    -- C/C++ Token:
    ,   identifier          :: Bool
    ,   keyword             :: Bool
    ,   directive           :: Bool
    ,   header              :: Bool
    ,   number              :: Bool
    ,   string              :: Bool
    ,   char                :: Bool
    ,   oper                :: Bool
    -- Output control:
    ,   max_count           :: Int
    ,   language_filter     :: [String]
    ,   language_force      :: Maybe String
    ,   language_map        :: Bool
    ,   magic_filter        :: [String]
    ,   invert_match        :: Bool
    ,   multiline           :: Int
    ,   recursive           :: Bool
    ,   prune_dir           :: [FilePath]
    ,   deference_recursive :: Bool
    -- Output format:
    ,   show_match          :: Bool
    ,   color               :: Bool
    ,   no_color            :: Bool
    ,   no_filename         :: Bool
    ,   no_numbers          :: Bool
    ,   no_column           :: Bool
    ,   count               :: Bool
    ,   filename_only       :: Bool
#ifdef ENABLE_HINT
    ,   hint                :: Maybe String
#endif
    ,   format              :: Maybe String
    ,   json                :: Bool
    ,   xml                 :: Bool
    ,   editor              :: Bool
    ,   vim                 :: Bool
    -- Parallel:
    ,   jobs                :: Int
    ,   cores               :: Int
    ,   chunk               :: Int
    ,   asynch              :: Bool
    -- Misc:
    ,   debug               :: Int
    ,   no_shallow          :: Bool
    ,   others              :: [String]
    } deriving (Data, Typeable, Show)

