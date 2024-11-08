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

module CmdOptions (
    options,
) where

import Data.Version (showVersion)
import System.Console.CmdArgs (
    CmdArgs,
    Mode,
    args,
    cmdArgsMode,
    explicit,
    groupname,
    help,
    name,
    program,
    summary,
    typ,
    (&=),
 )

import Options (Options (..))
import Paths_cgrep (version)

options :: Mode (CmdArgs Options)
options =
    cmdArgsMode $
        Options
            { file = "" &= typ "FILE" &= groupname "Pattern" &= help "Read PATTERNs from file (one per line)"
            , word_match = False &= help "Force word matching" &= explicit &= name "word" &= name "w"
            , prefix_match = False &= help "Force prefix matching" &= explicit &= name "prefix" &= name "p"
            , suffix_match = False &= help "Force suffix matching" &= explicit &= name "suffix" &= name "s"
            , edit_dist = False &= help "Use edit distance" &= explicit &= name "edit" &= name "e"
            , regex_posix = False &= help "Use regex matching (posix)" &= explicit &= name "G" &= name "regex"
            , regex_pcre = False &= help "Use regex matching (pcre)" &= explicit &= name "P" &= name "pcre"
            , ignore_case = False &= help "Ignore case distinctions"
            , code = False &= groupname "\nContext filters" &= help "Enable search in source code" &= explicit &= name "c" &= name "code"
            , comment = False &= help "Enable search in comments" &= explicit &= name "m" &= name "comment"
            , literal = False &= help "Enable search in string literals" &= explicit &= name "l" &= name "literal"
            , identifier = False &= groupname "\nToken filters" &= help "Identifiers" &= explicit &= name "identifier" &= name "name"
            , nativeType = False &= help "Native Types" &= explicit &= name "native" &= name "type"
            , keyword = False &= help "Keywords" &= explicit &= name "keyword"
            , number = False &= help "Literal numbers" &= explicit &= name "number"
            , string = False &= help "Literal strings" &= explicit &= name "string"
            , operator = False &= help "Operators" &= explicit &= name "op"
            , type_filter = [] &= groupname "\nFile filters" &= help "Specify file types. ie: Cpp, +Haskell, -Makefile"
            , kind_filter = [] &= help "Specify file kinds. Text, Config, Language, Data, Markup or Script"
            , code_only = False &= help "Parse code modules only (skip headers/interfaces)" &= explicit &= name "code-only"
            , hdr_only = False &= help "Parse headers/interfaces only (skip modules)" &= explicit &= name "hdr-only"
            , skip_test = False &= help "Skip files that have 'test' in the name" &= explicit &= name "skip-test" &= name "T"
            , prune_dir = [] &= help "Do not descend into dir" &= explicit &= name "prune-dir"
            , recursive = False &= help "Enable recursive search (don't follow symlinks)" &= explicit &= name "recursive" &= name "r"
            , follow = False &= help "Follow symlinks" &= explicit &= name "follow" &= name "L"
            , semantic = False &= groupname "\nSemantic" &= help "\"code\" pattern: _, _1, _2... (identifiers), $, $1, $2... (optionals), ANY, KEY, STR, LIT, NUM, HEX, OCT, OR" &= explicit &= name "S" &= name "semantic"
            , max_count = maxBound &= groupname "\nControl" &= help "Stop search in files after INT matches" &= explicit &= name "max-count"
            , type_force = Nothing &= help "Force the type of file" &= explicit &= name "force-type"
            , type_map = False &= help "List the supported file types" &= explicit &= name "type-list"
            , invert_match = False &= help "Select non-matching lines" &= explicit &= name "invert-match" &= name "v"
            , multiline = 1 &= help "Enable multi-line matching"
            , jobs = Nothing &= help "Number threads to run in parallel" &= explicit &= name "threads" &= name "j"
            , show_match = False &= groupname "\nOutput format" &= help "Show list of matching tokens" &= explicit &= name "show-match"
            , color = False &= help "Use colors to highlight the match strings" &= explicit &= name "color"
            , no_color = False &= help "Do not use colors (override config file)" &= explicit &= name "no-color"
            , no_filename = False &= help "Suppress the file name prefix on output" &= explicit &= name "h" &= name "no-filename"
            , no_numbers = False &= help "Suppress both line and column numbers on output" &= explicit &= name "no-numbers"
            , no_column = False &= help "Suppress the column number on output" &= explicit &= name "no-column"
            , count = False &= help "Print only a count of matching lines per file" &= explicit &= name "count"
            , filename_only = False &= help "Print only the name of files containing matches" &= explicit &= name "filename-only"
            , vim = False &= help "Run vim editor passing the files that match" &= explicit &= name "vim"
            , editor = False &= help "Run the editor specified by EDITOR var., passing the files that match" &= explicit &= name "editor"
            , fileline = False &= help "When edit option is specified, pass the list of matching files in file:line format (e.g. vim 'file-line' plugin)" &= explicit &= name "fileline"
            , json = False &= help "Format output as json object" &= explicit &= name "json"
            , verbose = 0 &= groupname "\nMiscellaneous" &= help "Verbose level: 1, 2 or 3" &= explicit &= name "verbose"
            , no_shallow = False &= help "Disable shallow-search" &= explicit &= name "no-shallow"
            , show_palette = False &= help "Show color palette" &= explicit &= name "palette"
            , others = [] &= args
            }
            &= summary ("Cgrep " <> showVersion version <> ". Usage: cgrep [OPTION] [PATTERN] files...")
            &= program "cgrep"
