CGrep: a context-aware grep for source codes
============================================

Usage
-----

Cgrep 5.5 Usage: cgrep [OPTION] [PATTERN] files...

cgrep [OPTIONS] [ITEM]

Pattern:

    -f --file=FILE            read PATTERNs from file
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

C/C++ language:

       --identifier           identifiers
       --keyword              keywords
       --directive            preprocessing directives
       --header               headers names
       --number               literal numbers
       --string               literal strings
       --char                 literal chars
       --oper                 operators
    -S --semantic             "code" patterns: _, _1, _2..., $, $1, $2...
                              (optional), ANY, KEY, STR, CHR, NUM, HEX, OCT, OR.
                              -> e.g. "_1(_1 && \$)" search for move constructors
  
Output control:

    -h --no-filename          suppress the file name prefix on output
    -N --no-line-umber        suppress the line number on output lines
       --lang=ITEM            specify languages. ie: Cpp, +Haskell, -Makefile
       --lang-maps            lists the language mappings
       --force-language=ITEM  force the language
    -j --jobs=INT             number of jobs
       --multiline=INT        enable multi-line matching
    -r --recursive            enable recursive search
    -v --invert-match         select non-matching lines
       --max-count=INT        stop search in files after INT matches
       --count                print only a count of matching lines per file
       --show-match           show list of matching tokens
       --color                use colors to highlight the matching strings
       --format=STRING        format output. Var: #f #n #l #t ## #, #; #0 #1...
                              e.g. "#f:#n #0 #1"
       --json                 format output as json object
       --xml                  format output as xml document
  
Miscellaneous:

    -d --debug=INT            debug level: 1, 2 or 3
    -? --help                 Display help message
    -V --version              Print version information

