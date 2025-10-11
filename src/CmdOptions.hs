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
    parserInfo,
) where

import Data.Version (showVersion)
import Options.Applicative (
    Parser,
    ParserInfo,
    argument,
    auto,
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    many,
    metavar,
    optional,
    option,
    progDesc,
    short,
    str,
    strOption,
    switch,
    value,
    (<**>),
 )

import Options (Options (..))
import Paths_cgrep (version)

-- Parser per le opzioni
options :: Parser Options
options = Options
    <$> strOption
        ( long "file"
       <> metavar "FILE"
       <> value ""
       <> help "Read PATTERNs from file (one per line)" )
    <*> switch
        ( long "word"
       <> short 'w'
       <> help "Force word matching" )
    <*> switch
        ( long "prefix"
       <> short 'p'
       <> help "Force prefix matching" )
    <*> switch
        ( long "suffix"
       <> short 's'
       <> help "Force suffix matching" )
    <*> switch
        ( long "edit"
       <> short 'e'
       <> help "Use edit distance" )
    <*> switch
        ( long "regex"
       <> short 'G'
       <> help "Use regex matching (posix)" )
#ifdef ENABLE_PCRE
    <*> switch
        ( long "pcre"
       <> short 'P'
       <> help "Use regex matching (pcre)" )
#endif
    <*> switch
        ( long "ignore-case"
       <> short 'i'
       <> help "Ignore case distinctions" )
    -- Context filters
    <*> switch
        ( long "code"
       <> short 'c'
       <> help "Enable search in source code" )
    <*> switch
        ( long "comment"
       <> short 'm'
       <> help "Enable search in comments" )
    <*> switch
        ( long "literal"
       <> short 'l'
       <> help "Enable search in string literals" )
    -- Token filters
    <*> switch
        ( long "identifier"
       <> long "name"
       <> help "Identifiers" )
    <*> switch
        ( long "native"
       <> long "type"
       <> help "Native Types" )
    <*> switch
        ( long "keyword"
       <> help "Keywords" )
    <*> switch
        ( long "number"
       <> help "Literal numbers" )
    <*> switch
        ( long "string"
       <> help "Literal strings" )
    <*> switch
        ( long "op"
       <> help "Operators" )
    -- File filters
    <*> many (strOption
        ( long "type"
       <> metavar "TYPE"
       <> help "Specify file types. ie: Cpp, +Haskell, -Makefile" ))
    <*> many (strOption
        ( long "kind"
       <> metavar "KIND"
       <> help "Specify file kinds. Text, Config, Language, Data, Markup or Script" ))
    <*> switch
        ( long "code-only"
       <> help "Parse code modules only (skip headers/interfaces)" )
    <*> switch
        ( long "hdr-only"
       <> help "Parse headers/interfaces only (skip modules)" )
    <*> switch
        ( long "skip-test"
       <> short 'T'
       <> help "Skip files that have 'test' in the name" )
    <*> many (strOption
        ( long "prune-dir"
       <> metavar "DIR"
       <> help "Do not descend into dir" ))
    <*> switch
        ( long "recursive"
       <> short 'r'
       <> help "Enable recursive search (don't follow symlinks)" )
    <*> switch
        ( long "follow"
       <> short 'L'
       <> help "Follow symlinks" )
    -- Semantic
    <*> switch
        ( long "semantic"
       <> short 'S'
       <> help "\"code\" pattern: _, _1, _2... (identifiers), $, $1, $2... (optionals), ANY, KEY, STR, LIT, NUM, HEX, OCT, OR" )
    <*> switch
        ( long "strict"
           <> help "Enable strict semantic for operators" )
    -- Control
    <*> option auto
        ( long "max-count"
       <> metavar "INT"
       <> value maxBound
       <> help "Stop search in files after INT matches" )
    <*> optional (strOption
        ( long "force-type"
       <> metavar "TYPE"
       <> help "Force the type of file" ))
    <*> switch
        ( long "type-list"
       <> help "List the supported file types" )
    <*> switch
        ( long "invert-match"
       <> short 'v'
       <> help "Select non-matching lines" )
    <*> option auto
        ( long "multiline"
       <> metavar "INT"
       <> value 1
       <> help "Enable multi-line matching" )
    <*> optional (option auto
        ( long "threads"
       <> short 'j'
       <> metavar "INT"
       <> help "Number threads to run in parallel" ))
    -- Output format
    <*> switch
        ( long "show-match"
       <> help "Show list of matching tokens" )
    <*> switch
        ( long "color"
       <> help "Use colors to highlight the match strings" )
    <*> switch
        ( long "no-color"
       <> help "Do not use colors (override config file)" )
    <*> switch
        ( long "no-filename"
       <> short 'h'
       <> help "Suppress the file name prefix on output" )
    <*> switch
        ( long "no-numbers"
       <> help "Suppress both line and column numbers on output" )
    <*> switch
        ( long "no-column"
       <> help "Suppress the column number on output" )
    <*> switch
        ( long "count"
       <> help "Print only a count of matching lines per file" )
    <*> switch
        ( long "filename-only"
       <> help "Print only the name of files containing matches" )
    <*> switch
        ( long "vim"
       <> help "Run vim editor passing the files that match" )
    <*> switch
        ( long "editor"
       <> help "Run the editor specified by EDITOR var., passing the files that match" )
    <*> switch
        ( long "fileline"
       <> help "When edit option is specified, pass the list of matching files in file:line format (e.g. vim 'file-line' plugin)" )
    <*> switch
        ( long "json"
       <> help "Format output as json object" )
    -- Miscellaneous
    <*> option auto
        ( long "verbose"
       <> metavar "INT"
       <> value 0
       <> help "Verbose level: 1, 2, 3 or 4" )
    <*> switch
        ( long "no-shallow"
       <> help "Disable shallow-search" )
    <*> switch
        ( long "palette"
       <> help "Show color palette" )
    <*> many (argument str (metavar "PATTERN|FILES..."))

-- Informazioni del parser principale
parserInfo :: ParserInfo Options
parserInfo = info (options <**> helper)
    ( fullDesc
   <> progDesc "Context-aware grep for source codes"
   <> header ("cgrep " <> showVersion version <> " - Usage: cgrep [OPTION] [PATTERN] files...") )
