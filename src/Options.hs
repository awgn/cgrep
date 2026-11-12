--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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
    defaultOptions,
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
    , tests :: Maybe Bool
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
    , jobs :: Maybe Int
    , -- Output format:
      show_match :: Bool
    , only_matching :: Bool
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
    , stats :: Bool
    , debug :: Int
    , null_output :: Bool
    , show_palette :: Bool
    , others :: [String]
    }
    deriving stock (Show)

defaultOptions :: Options
defaultOptions =
    Options
        { file = Nothing
        , word_match = False
        , prefix_match = False
        , suffix_match = False
        , edit_dist = False
        , regex_posix = False
#ifdef ENABLE_PCRE
        , regex_pcre = False
#endif
        , ignore_case = False
        , code = False
        , comment = False
        , literal = False
        , identifier = False
        , nativeType = False
        , keyword = False
        , number = False
        , string = False
        , operator = False
        , type_filter = []
        , kind_filter = []
        , code_only = False
        , hdr_only = False
        , tests = Nothing
        , prune_dir = []
        , recursive = False
        , follow = False
        , semantic = False
        , strict = False
        , max_count = maxBound
        , force_type = Nothing
        , type_map = False
        , invert_match = False
        , jobs = Nothing
        , show_match = False
        , only_matching = False
        , color = False
        , no_color = False
        , no_filename = False
        , no_numbers = False
        , no_column = False
        , count = False
        , filename_only = False
        , json = False
        , vim = False
        , editor = False
        , fileline = False
        , verbose = False
        , stats = False
        , debug = 0
        , null_output = False
        , show_palette = False
        , others = []
        }
