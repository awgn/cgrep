--
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

{-# LANGUAGE DeriveDataTypeable #-}

module Options where

import Data.Data ( Data, Typeable )

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
    -- Tokenizer:
    ,   identifier          :: Bool
    ,   keyword             :: Bool
    ,   number              :: Bool
    ,   string              :: Bool
    ,   operator            :: Bool
    -- Semantic:
    ,   semantic            :: Bool
    -- Output control:
    ,   max_count           :: Int
    ,   language_filter     :: [String]
    ,   language_force      :: Maybe String
    ,   language_map        :: Bool
    ,   invert_match        :: Bool
    ,   multiline           :: Int
    ,   recursive           :: Bool
    ,   skip_test           :: Bool
    ,   prune_dir           :: [FilePath]
    ,   follow              :: Bool
    -- Output format:
    ,   show_match          :: Bool
    ,   color               :: Bool
    ,   no_color            :: Bool
    ,   no_filename         :: Bool
    ,   no_numbers          :: Bool
    ,   no_column           :: Bool
    ,   count               :: Bool
    ,   filename_only       :: Bool
    ,   json                :: Bool
    ,   vim                 :: Bool
    ,   editor              :: Bool
    ,   fileline            :: Bool
    -- Parallel:
    ,   jobs                :: Int
    -- Misc:
    ,   verbose             :: Int
    ,   no_shallow          :: Bool
    ,   show_palette        :: Bool
    ,   others              :: [String]
    } deriving (Data, Typeable, Show)
