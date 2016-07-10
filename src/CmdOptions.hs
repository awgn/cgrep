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

module CmdOptions where

import Data.Version(showVersion)
import System.Console.CmdArgs

import Paths_cgrep
import Options

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
          {     file  = ""  &= typ "FILE"   &= groupname "Pattern" &= help "Read PATTERNs from file (one per line)"
          ,     word_match  = False         &= help "Force word matching" &=explicit &= name "word" &= name "w"
          ,     prefix_match  = False       &= help "Force prefix matching" &=explicit &= name "prefix" &= name "p"
          ,     suffix_match  = False       &= help "Force suffix matching" &=explicit &= name "suffix" &= name "s"
          ,     edit_dist   = False         &= help "Use edit distance" &=explicit &= name "edit" &= name "e"
          ,     regex_posix = False         &= help "Use regex matching (posix)" &= explicit &= name "G" &=name "regex"
          ,     regex_pcre = False          &= help "Use regex matching (pcre)"  &= explicit &= name "P" &=name "regex-pcre"
          ,     ignore_case = False         &= help "Ignore case distinctions"
          ,     code = False                &= groupname "\nContext filters (generic)" &= help "Enable search in source code"     &= explicit &= name "c" &= name "code"
          ,     comment = False             &= help "Enable search in comments"        &= explicit &= name "m" &= name "comment"
          ,     literal = False             &= help "Enable search in string literals" &= explicit &= name "l" &= name "literal"
          ,     semantic = False            &= groupname "\nSemantic (generic)" &= help "\"code\" pattern: _, _1, _2... (identifiers), $, $1, $2... (optionals), ANY, KEY, STR, CHR, LIT, NUM, HEX, OCT, OR. -> e.g. \"_1(_1 && \\$)\" search for move constructors, \"struct OR class _ { OR : OR <\" search for a class declaration" &= explicit &= name "S" &= name "semantic"
          ,     identifier = False          &= groupname "\nC/C++ language" &= help "Identifiers" &= explicit &= name "identifier"
          ,     keyword = False             &= help "Keywords" &= explicit &= name "keyword"
          ,     directive = False           &= help "Preprocessing directives" &= explicit &= name "directive"
          ,     header = False              &= help "Headers names" &= explicit &= name "header"
          ,     number = False              &= help "Literal numbers" &= explicit &= name "number"
          ,     string = False              &= help "Literal strings" &= explicit &= name "string"
          ,     char = False                &= help "Literal chars" &= explicit &= name "char"
          ,     oper = False                &= help "Operators" &= explicit &= name "oper"
          ,     max_count = maxBound        &= groupname "\nOutput control" &= help "Stop search in files after INT matches" &= explicit &= name "max-count"
          ,     language_filter = []        &= help "Specify languages. ie: Cpp, +Haskell, -Makefile"
          ,     language_force = Nothing    &= help "Force the language" &= explicit &= name "language-force"
          ,     language_map = False        &= help "Lists the language mappings"
          ,     magic_filter = []           &= help "Use unix magic as file-filter"
          ,     invert_match = False        &= help "Select non-matching lines" &= explicit &= name "invert-match" &= name "v"
          ,     multiline = 1               &= help "Enable multi-line matching"
          ,     recursive = False           &= help "Enable recursive search (don't follow symlinks)" &= explicit &= name "recursive" &= name "r"
          ,     prune_dir = []              &= help "Do not descend into dir" &= explicit &= name "prune-dir"
          ,     deference_recursive = False &= help "Recursive, follow symlinks" &= explicit &= name "deference-recursive" &= name "R"
          ,     show_match = False          &= groupname "\nOutput format" &= help "Show list of matching tokens" &= explicit &= name "show-match"
          ,     color = False               &= help "Use colors to highlight the matching strings" &= explicit &= name "color"
          ,     no_color = False            &= help "Do not use colors (override configAutoColor)" &= explicit &= name "no-color"
          ,     no_filename = False         &= help "Suppress the file name prefix on output"  &= explicit &= name "h" &= name "no-filename"
          ,     no_numbers = False          &= help "Suppress both line and column numbers on output" &= explicit &= name "no-numbers"
          ,     no_column = False           &= help "Suppress the column number on output" &= explicit &= name "no-column"
          ,     count = False               &= help "Print only a count of matching lines per file" &= explicit &= name "count"
          ,     filename_only = False       &= help "Print only the name of files containing matches" &= explicit &= name "filename-only"
          ,     editor = False              &= help "Run the editor specified by EDITOR var., passing the files that match" &= explicit &=name "editor"
          ,     vim = False                 &= help "Run vim the editor passing the files that match" &= explicit &=name "vim"
#ifdef ENABLE_HINT
          ,     hint = Nothing  &= typ "STRING" &= help "Haskell interpreter output. Var: file, row, line, tokens.\ne.g. \"file ++ show (tokens)\"" &= explicit &= name "hint"
#endif
          ,     format = Nothing &= typ "STRING" &= help "Format output. Var: #f #n #l #t ## #, #; #0 #1...\ne.g. \"#f:#n #0 #1\"" &= explicit &= name "format"
          ,     json = False                &= help "Format output as json object" &= explicit &= name "json"
          ,     xml = False                 &= help "Format output as xml document" &= explicit &= name "xml"
          ,     jobs   = 1                  &= groupname "\nConcurrency" &= help "Number of jobs"
          ,     cores  = 0                  &= help "Number of physical processors utilized"
          ,     chunk  = 16                 &= help "Specify the length of chunks"
          ,     asynch = False              &= help "Process chunks asynchronously"
          ,     debug = 0                   &= groupname "\nMiscellaneous" &= help "Debug level: 1, 2 or 3"
          ,     no_shallow = False          &= help "Disable shallow-search"  &= explicit &= name "no-shallow"
          ,     others = []                 &= args
          } &= summary ("Cgrep " ++ showVersion version ++ ". Usage: cgrep [OPTION] [PATTERN] files...") &= program "cgrep"

