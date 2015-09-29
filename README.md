CGrep: a context-aware grep for source codes [![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
============================================

Usage
-----

Cgrep 6.5.6 Usage: cgrep [OPTION] [PATTERN] files...

cgrep [OPTIONS] [ITEM]

Pattern:

    -f --file=FILE            read PATTERNs from file (one per line)
    -w --word                 force word matching
    -p --prefix               force prefix matching
    -s --suffix               force suffix matching
    -e --edit                 use edit distance
    -i --ignore-case          ignore case distinctions
    -G --regex                regex matching

Context filters (generic):

    -c --code                 enable search in source code
    -m --comment              enable search in comments
    -l --literal              enable search in string literals

Semantic (generic):

    -S --semantic             "code" pattern: _, _1, _2... (identifiers), $,
                              $1, $2... (optionals), ANY, KEY, STR, CHR, NUM,
                              HEX, OCT, OR. -> e.g. "_1(_1 && \$)" search for
                              move constructors, "struct OR class _ { OR : OR <"
                              search for a class declaration

C/C++ language:

       --identifier           identifiers
       --keyword              keywords
       --directive            preprocessing directives
       --header               headers names
       --number               literal numbers
       --string               literal strings
       --char                 literal chars
       --oper                 operators
 
Output control:

    -h --no-filename          suppress the file name prefix on output
    -N --no-line-umber        suppress the line number on output lines
       --lang=ITEM            specify languages. ie: Cpp, +Haskell, -Makefile
       --lang-maps            lists the language mappings
       --force-language=ITEM  force the language
       --multiline=INT        enable multi-line matching
    -r --recursive            Enable recursive search (don't follow symlinks)
       --prune-dir=ITEM       Do not descend into dir
    -R --deference-recursive  Recursive, follow symlinks
    -v --invert-match         select non-matching lines
       --max-count=INT        stop search in files after INT matches
       --count                print only a count of matching lines per file
       --show-match           show list of matching tokens
       --color                use colors to highlight the matching strings
       --format=STRING        format output. Var: #f #n #l #t ## #, #; #0 #1...
                              e.g. "#f:#n #0 #1"
       --json                 format output as json object
       --xml                  format output as xml document
  
Concurrency:

    -j --jobs=INT             Number of jobs
       --cores=INT            Number of physical processors utilized
       --chunk=INT            Specify the length of chunks
    -a --asynch               Process chunks asynchronously

Miscellaneous:

    -d --debug=INT            debug level: 1, 2 or 3
    -n --no-turbo             disable turbo mode
    -? --help                 Display help message
    -V --version              Print version information

