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

import System.Console.CmdArgs

import Options
import Config

options :: Mode (CmdArgs Options)
options = cmdArgsMode $ Options
          {     file  = ""  &= typ "FILE"   &= help "Read PATTERNs from file (one per line)" &= groupname "Pattern"
          ,     word_match  = False         &= help "Force word matching" &=explicit &= name "word" &= name "w"
          ,     prefix_match  = False       &= help "Force prefix matching" &=explicit &= name "prefix" &= name "p"
          ,     suffix_match  = False       &= help "Force suffix matching" &=explicit &= name "suffix" &= name "s"
          ,     edit_dist   = False         &= help "Use edit distance" &=explicit &= name "edit" &= name "e"
          ,     regex = False               &= help "Use regex matching" &= explicit &= name "G" &=name "regex"
          ,     ignore_case = False         &= help "Ignore case distinctions"
          ,     code = False                &= help "Enable search in source code"     &= explicit &= name "c" &= name "code" &= groupname "\nContext filters (generic)"
          ,     comment = False             &= help "Enable search in comments"        &= explicit &= name "m" &= name "comment"
          ,     literal = False             &= help "Enable search in string literals" &= explicit &= name "l" &= name "literal"
          ,     identifier = False          &= help "Identifiers" &= explicit &= name "identifier" &= groupname "\nC/C++ language"
          ,     keyword = False             &= help "Keywords" &= explicit &= name "keyword"
          ,     directive = False           &= help "Preprocessing directives" &= explicit &= name "directive"
          ,     header = False              &= help "Headers names" &= explicit &= name "header"
          ,     number = False              &= help "Literal numbers" &= explicit &= name "number"
          ,     string = False              &= help "Literal strings" &= explicit &= name "string"
          ,     char = False                &= help "Literal chars" &= explicit &= name "char"
          ,     oper = False                &= help "Operators" &= explicit &= name "oper"
          ,     semantic = False            &= groupname "\nSemantic (generic)" &= help "\"code\" pattern: _, _1, _2... (identifiers), $, $1, $2... (optionals), ANY, KEY, STR, CHR, LIT, NUM, HEX, OCT, OR. -> e.g. \"_1(_1 && \\$)\" search for move constructors, \"struct OR class _ { OR : OR <\" search for a class declaration" &= explicit &= name "S" &= name "semantic"
          ,     no_filename = False         &= help "Suppress the file name prefix on output"  &= explicit &= name "h" &= name "no-filename" &= groupname "\nOutput control"
          ,     no_linenumber= False        &= help "Suppress the line number on output lines" &= explicit &= name "N" &= name "no-line-umber"
          ,     lang = []                   &= help "Specify languages. ie: Cpp, +Haskell, -Makefile"
          ,     lang_maps = False           &= help "Lists the language mappings"
          ,     force_language = Nothing    &= help "Force the language" &= explicit &= name "force-language"
          ,     max_count = maxBound        &= help "Stop search in files after INT matches" &= explicit &= name "max-count"
          ,     count = False               &= help "Print only a count of matching lines per file" &= explicit &= name "count"
          ,     jobs   = 1                  &= help "Number of jobs"
          ,     multiline = 1               &= help "Enable multi-line matching"
          ,     recursive = False           &= help "Enable recursive search (don't follow symlinks)" &= explicit &= name "recursive" &= name "r"
          ,     deference_recursive = False &= help "Recursive, follow symlinks" &= explicit &= name "deference-recursive" &= name "R"
          ,     invert_match = False        &= help "Select non-matching lines" &= explicit &= name "invert-match" &= name "v"
          ,     show_match = False          &= help "Show list of matching tokens" &= explicit &= name "show-match"
          ,     color = False               &= help "Use colors to highlight the matching strings" &= explicit &= name "color"
          ,     format = Nothing &= typ "STRING" &= help "Format output. Var: #f #n #l #t ## #, #; #0 #1...\ne.g. \"#f:#n #0 #1\"" &= explicit &= name "format"
#ifdef ENABLE_HINT
          ,     hint = Nothing  &= typ "STRING" &= help "Haskell interpreter output. Var: file, row, line, tokens.\ne.g. \"file ++ show (tokens)\"" &= explicit &= name "hint"
#endif
          ,     json = False               &= help "Format output as json object" &= explicit &= name "json"
          ,     xml = False                &= help "Format output as xml document" &= explicit &= name "xml"
          ,     debug = 0                  &= help "Debug level: 1, 2 or 3" &= groupname "\nMiscellaneous"
          ,     no_turbo = False           &= help "Disable turbo mode"
          ,     others = []                &= args
          } &= summary ("Cgrep " ++ version ++ ". Usage: cgrep [OPTION] [PATTERN] files...") &= program "cgrep"

