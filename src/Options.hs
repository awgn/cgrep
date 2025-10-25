--
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

module Options (
    Options (..),
) where

data Options = Options
    { file :: Maybe String
    , word_match :: Bool
    , prefix_match :: Bool
    , suffix_match :: Bool
    , edit_dist :: Bool
    , regex_posix :: Bool
#ifdef ENABLE_PCRE
    , regex_pcre :: Bool
#endif
    , ignore_case :: Bool
    , -- Context:
      code :: Bool
    , comment :: Bool
    , literal :: Bool
    , -- Token filters:
      identifier :: Bool
    , nativeType :: Bool
    , keyword :: Bool
    , number :: Bool
    , string :: Bool
    , operator :: Bool
    , -- File filters:
      type_filter :: [String]
    , kind_filter :: [String]
    , code_only :: Bool
    , hdr_only :: Bool
    , skip_test :: Bool
    , prune_dir :: [FilePath]
    , recursive :: Bool
    , follow :: Bool
    , -- Semantic:
      semantic :: Bool
    , strict :: Bool
    , -- Control:
      max_count :: Int
    , force_type :: Maybe String
    , type_map :: Bool
    , invert_match :: Bool
    , multiline :: Int
    , jobs :: Maybe Int
    , -- Output format:
      show_match :: Bool
    , color :: Bool
    , no_color :: Bool
    , no_filename :: Bool
    , no_numbers :: Bool
    , no_column :: Bool
    , count :: Bool
    , filename_only :: Bool
    , json :: Bool
    , vim :: Bool
    , editor :: Bool
    , fileline :: Bool
    , -- Misc:
      verbose :: Bool
    , debug :: Int
    , null_output :: Bool
    , no_shallow :: Bool
    , show_palette :: Bool
    , others :: [String]
    }
    deriving stock (Show)
