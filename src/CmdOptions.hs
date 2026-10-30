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

module CmdOptions (
    parserInfo,
) where

import Options.Applicative (
    Parser,
    ParserInfo,
    argument,
    auto,
    flag',
    fullDesc,
    header,
    help,
    helper,
    info,
    long,
    many,
    metavar,
    optional,
    parserOptionGroup,
    option,
    progDesc,
    short,
    str,
    strOption,
    switch,
    value,
    (<**>),
    (<|>),
 )

import Options (Options (..))
import Paths_cgrep (version)
import Options.Applicative.Builder (infoOption)
import Options.Applicative (hidden)
import Data.Version (showVersion)

--  Parser options
options :: Parser Options
options = Options
    <$> parserOptionGroup "Pattern matching" (optional(strOption
        ( long "file"
       <> metavar "FILE"
       <> help "Read PATTERNs from file (one per line)" )))
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "word"
       <> short 'w'
       <> help "Force word matching" ))
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "prefix"
       <> short 'p'
       <> help "Force prefix matching" ))
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "suffix"
       <> short 's'
       <> help "Force suffix matching" ))
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "edit"
       <> short 'e'
       <> help "Use edit distance" ))
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "regex"
       <> short 'G'
       <> help "Use regex matching (posix)" ))
#ifdef ENABLE_PCRE
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "pcre"
       <> short 'P'
       <> help "Use regex matching (pcre)" ))
#endif
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "ignore-case"
       <> short 'i'
       <> help "Ignore case distinctions" ))
    -- Context filters
    <*> parserOptionGroup "Context filters" (switch
        ( long "code"
       <> short 'c'
       <> help "Enable search in source code" ))
    <*> parserOptionGroup "Context filters" (switch
        ( long "comment"
       <> short 'm'
       <> help "Enable search in comments" ))
    <*> parserOptionGroup "Context filters" (switch
        ( long "literal"
       <> short 'l'
       <> help "Enable search in string literals" ))
    -- Token filters
    <*> parserOptionGroup "Token filters" (switch
        ( long "identifier"
       <> long "name"
       <> help "Identifiers" ))
    <*> parserOptionGroup "Token filters" (switch
        ( long "native"
       <> long "type"
       <> help "Native Types" ))
    <*> parserOptionGroup "Token filters" (switch
        ( long "keyword"
       <> help "Keywords" ))
    <*> parserOptionGroup "Token filters" (switch
        ( long "number"
       <> help "Literal numbers" ))
    <*> parserOptionGroup "Token filters" (switch
        ( long "string"
       <> help "Literal strings" ))
    <*> parserOptionGroup "Token filters" (switch
        ( long "op"
       <> help "Operators" ))
    -- File filters
    <*> parserOptionGroup "File selection" (many (strOption
        ( long "type"
       <> metavar "TYPE"
       <> help "Specify file types. ie: Cpp, +Haskell, -Makefile" )))
    <*> parserOptionGroup "File selection" (many (strOption
        ( long "kind"
       <> metavar "KIND"
       <> help "Specify file kinds. Text, Config, Language, Data, Markup or Script" )))
    <*> parserOptionGroup "File selection" (switch
        ( long "code-only"
       <> help "Parse code modules only (skip headers/interfaces)" ))
    <*> parserOptionGroup "File selection" (switch
        ( long "hdr-only"
       <> help "Parse headers/interfaces only (skip modules)" ))
    <*> parserOptionGroup "File selection" (( flag' (Just True)
        ( long "tests-only"
       <> short 'T'
       <> help "Search only in test files" )
      <|> flag' (Just False)
        ( long "no-tests"
       <> help "Exclude test files from search" )
      <|> pure Nothing ))
    <*> parserOptionGroup "File selection" (many (strOption
        ( long "prune-dir"
       <> metavar "DIR"
       <> help "Do not descend into dir" )))
    <*> parserOptionGroup "File selection" (switch
        ( long "recursive"
       <> short 'r'
       <> help "Enable recursive search (don't follow symlinks)" ))
    <*> parserOptionGroup "File selection" (switch
        ( long "follow"
       <> short 'L'
       <> help "Follow symlinks" ))
    -- Semantic
    <*> parserOptionGroup "Semantic search" (switch
        ( long "semantic"
       <> short 'S'
       <> help "\"code\" pattern: _, _1, _2... (identifiers), ..., ANY, KEY, TYP, OP, STR, LIT, NUM, HEX, OCT, OR, NOT" ))
    <*> parserOptionGroup "Semantic search" (switch
        ( long "strict"
           <> help "Enable strict semantic for operators" ))
    -- Control
    <*> parserOptionGroup "Performance and limits" (option auto
        ( long "max-count"
       <> metavar "INT"
       <> value maxBound
       <> help "Stop search in files after INT matches" ))
    <*> parserOptionGroup "File selection" (optional (strOption
        ( long "force-type"
       <> metavar "TYPE"
       <> help "Force the type of file" )))
    <*> parserOptionGroup "Miscellaneous" (switch
        ( long "type-list"
       <> help "List the supported file types" ))
    <*> parserOptionGroup "Pattern matching" (switch
        ( long "invert-match"
       <> short 'v'
       <> help "Select non-matching lines" ))
    <*> parserOptionGroup "Performance and limits" (optional (option auto
        ( long "threads"
       <> short 'j'
       <> metavar "INT"
       <> help "Approximate number of threads to run search" )))
    -- Output format
    <*> parserOptionGroup "Output format" (switch
        ( long "show-match"
       <> help "Show list of matching tokens" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "only-matching"
       <> short 'o'
       <> help "Print only the matching tokens, not the whole line (requires -S)" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "color"
       <> help "Use colors to highlight the match strings" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "no-color"
       <> help "Do not use colors (override config file)" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "no-filename"
       <> short 'h'
       <> help "Suppress the file name prefix on output" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "no-numbers"
       <> help "Suppress both line and column numbers on output" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "no-column"
       <> help "Suppress the column number on output" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "count"
       <> help "Print only a count of matching lines per file" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "filename-only"
       <> help "Print only the name of files containing matches" ))
    <*> parserOptionGroup "Output format" (switch
        ( long "json"
       <> help "Format output as json object" ))
    <*> parserOptionGroup "Editor integration" (switch
        ( long "vim"
       <> help "Run vim editor passing the files that match" ))
    <*> parserOptionGroup "Editor integration" (switch
        ( long "editor"
       <> help "Run the editor specified by EDITOR var., passing the files that match" ))
    <*> parserOptionGroup "Editor integration" (switch
        ( long "fileline"
       <> help "When edit option is specified, pass the list of matching files in file:line format (e.g. vim 'file-line' plugin)" ))
    -- Miscellaneous
    <*> parserOptionGroup "Miscellaneous" (switch
        ( long "verbose"
       <> help "Enable verbose mode"
       ))
    <*> parserOptionGroup "Miscellaneous" (switch
        ( long "stats"
       <> help "Print statistics about the search"
       ))
    <*> option auto
        ( long "debug"
       <> metavar "INT"
       <> value 0
       -- <> help "debug level: 1, 2, 3 or 4"
       <> hidden)
    <*> parserOptionGroup "Miscellaneous" (switch
        ( long "null-output"
       <> help "Disable output for performance evaluation" ))
    <*> parserOptionGroup "Miscellaneous" (switch
        ( long "palette"
       <> help "Show color palette" ))
    <*> many (argument str (metavar "PATTERN FILES..."))

parseVersion :: Parser (a -> a)
parseVersion = infoOption ("cgrep " <> showVersion version)
  ( long "version"
 <> help "Show version information and exit"
 -- <> hidden -- Uncomment to hide it from the main help message
  )

parserInfo :: ParserInfo Options
parserInfo = info (options <**> helper <**> parseVersion)
    ( fullDesc
   <> progDesc "Context-aware grep for source codes"
   <> header ("cgrep " <> showVersion version <> " - Usage: cgrep [OPTION] [PATTERN] files...") )
