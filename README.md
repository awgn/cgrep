CGrep: a context-aware grep for source codes
============================================

[![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
[![Join the chat at https://gitter.im/awgn/cgrep](https://badges.gitter.im/awgn/cgrep.svg)](https://gitter.im/awgn/cgrep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Usage
-----

```
Cgrep 8.0.0. Usage: cgrep [OPTION] [PATTERN] files...

cgrep [OPTIONS] [ITEM]

Pattern:
  -f --file=FILE          Read PATTERNs from file (one per line)
  -w --word               Force word matching
  -p --prefix             Force prefix matching
  -s --suffix             Force suffix matching
  -e --edit               Use edit distance
  -G --regex              Use regex matching (posix)
  -P --pcre               Use regex matching (pcre)
  -i --ignore-case        Ignore case distinctions

Context filters:
  -c --code               Enable search in source code
  -m --comment            Enable search in comments
  -l --literal            Enable search in string literals

Token filters:
     --name --identifier  Identifiers
     --type --native      Native Types
     --keyword            Keywords
     --number             Literal numbers
     --string             Literal strings
     --op                 Operators

Semantic:
  -S --semantic           "code" pattern: _, _1, _2... (identifiers), $, $1,
                          $2... (optionals), ANY, KEY, STR, LIT, NUM, HEX, OCT,
                          OR

Output control:
     --max-count=INT      Stop search in files after INT matches
  -t --type-filter=ITEM   Specify file types. ie: Cpp, +Haskell, -Makefile
  -k --kind-filter=ITEM   Specify file kinds. Text, Config, Language, Data,
                          Markup or Script
     --force-type=ITEM    Force the type of file
     --type-list          List the supported file types
  -v --invert-match       Select non-matching lines
     --multiline=INT      Enable multi-line matching
  -r --recursive          Enable recursive search (don't follow symlinks)
  -T --skip-test          Skip files that have 'test' in the name
     --prune-dir=ITEM     Do not descend into dir
  -L --follow             Follow symlinks

Output format:
     --show-match         Show list of matching tokens
     --color              Use colors to highlight the match strings
     --no-color           Do not use colors (override config file)
  -h --no-filename        Suppress the file name prefix on output
     --no-numbers         Suppress both line and column numbers on output
     --no-column          Suppress the column number on output
     --count              Print only a count of matching lines per file
     --filename-only      Print only the name of files containing matches
     --json               Format output as json object
     --vim                Run vim editor passing the files that match
     --editor             Run the editor specified by EDITOR var., passing
                          the files that match
     --fileline           When edit option is specified, pass the list of
                          matching files in file:line format (e.g. vim
                          'file-line' plugin)

Concurrency:
  -j --threads=INT        Number threads to run in parallel

Miscellaneous:
     --verbose=INT        Verbose level: 1, 2 or 3
     --no-shallow         Disable shallow-search
     --palette            Show color palette
  -? --help               Display help message
  -V --version            Print version information
     --numeric-version    Print just the version number
```
