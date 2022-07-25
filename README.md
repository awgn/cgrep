CGrep: a context-aware grep for source codes
============================================

[![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
[![Join the chat at https://gitter.im/awgn/cgrep](https://badges.gitter.im/awgn/cgrep.svg)](https://gitter.im/awgn/cgrep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Usage
-----

```
Cgrep 7.0.0. Usage: cgrep [OPTION] [PATTERN] files...

cgrep [OPTIONS] [ITEM]

Pattern:
  -f --file=FILE             Read PATTERNs from file (one per line)
  -w --word                  Force word matching
  -p --prefix                Force prefix matching
  -s --suffix                Force suffix matching
  -e --edit                  Use edit distance
  -G --regex                 Use regex matching (posix)
  -P --regex-pcre            Use regex matching (pcre)
  -i --ignore-case           Ignore case distinctions

Context filters:
  -c --code                  Enable search in source code
  -m --comment               Enable search in comments
  -l --literal               Enable search in string literals

Token filters:
     --identifier            Identifiers
     --keyword               Keywords
     --number                Literal numbers
     --string                Literal strings
     --oper                  Operators

Semantic:
  -S --semantic              "code" pattern: _, _1, _2... (identifiers), $,
                             $1, $2... (optionals), ANY, KEY, STR, LIT, NUM,
                             HEX, OCT, OR. -> e.g. "_1(_1 && \$)" search for
                             move constructors, "struct OR class _ { OR : OR <"
                             search for a class declaration

Output control:
     --max-count=INT         Stop search in files after INT matches
     --language-filter=ITEM  Specify languages. ie: Cpp, +Haskell, -Makefile
     --language-force=ITEM   Force the language
     --language-map          Lists the language mappings
     --magic-filter=ITEM     Use unix magic as file-filter
  -v --invert-match          Select non-matching lines
     --multiline=INT         Enable multi-line matching
  -r --recursive             Enable recursive search (don't follow symlinks)
  -T --skip-test             Skip files that have 'test' in the name
     --prune-dir=ITEM        Do not descend into dir
  -R --deference-recursive   Recursive, follow symlinks

Output format:
     --show-match            Show list of matching tokens
     --color                 Use colors to highlight the matching strings
     --no-color              Do not use colors (override config file)
  -h --no-filename           Suppress the file name prefix on output
     --no-numbers            Suppress both line and column numbers on output
     --no-column             Suppress the column number on output
     --count                 Print only a count of matching lines per file
     --filename-only         Print only the name of files containing matches
     --json                  Format output as json object
     --xml                   Format output as xml document
     --vim                   Run vim editor passing the files that match
     --editor                Run the editor specified by EDITOR var., passing
                             the files that match
     --fileline              When edit option is specified, pass the list of
                             matching files in file:line format (e.g. vim
                             'file-line' plugin)

Concurrency:
  -j --jobs=INT              Number of jobs to run in parallel
  -a --asynch                Process chunks asynchronously

Miscellaneous:
     --verbosity=INT         Verbosity level: 1, 2 or 3
     --no-shallow            Disable shallow-search
     --palette               Show color palette
  -? --help                  Display help message
  -V --version               Print version information
     --numeric-version       Print just the version number
```
