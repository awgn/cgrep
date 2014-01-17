CGrep: a context-aware grep for source codes
============================================

Usage
-----

Cgrep 4.6 Usage: cgrep [OPTION] [PATTERN] files...

cgrep [OPTIONS] [ITEM]

Pattern:

    -f --file=FILE             read PATTERNs from file
    -w --word                  force word matching
    -G --regex                 regex matching
    -i --ignore-case           ignore case distinctions
    -e --edit                  use edit distance

Context:

    -c --code                  search in source code
    -m --comment               search in comments
    -l --literal               search in string literals

C/C++ language:

       --identifier            identifiers
       --keyword               keywords
       --directive             preproc directives
       --header                headers name
       --number                literal numbers
       --string                literal strings
       --char                  literal chars
       --oper                  operators
       -S --semantic           "code" patterns: _, _1, _2..., $, $1, $2... (optional),
                               ANY, KEY, STR, CHR, NUM, HEX, OCT, OR. 
                               -> e.g. "_1(_1 && \$)" search for move constructors
Output control:

    -h --no-filename           suppress the file name prefix on output
    -N --no-line-umber         suppress the line number on output lines
       --lang=ITEM             specify languages to grep for. ie: Cpp, +Haskell,
                               -Makefile
       --lang-map              print the list of language mapping
       --force-language=ITEM   force the language
    -j --jobs=INT              number of jobs
       --multiline=INT         enable multi-line matching
    -r --recursive             enable recursive search
       --invert-match          select non-matching lines
       --max-count=INT         stop search in files after INT matches
       --count                 print only a count of matching lines per file
       --show-match            show list of matching tokens
       --color                 use colors to highlight the matching strings
       --hint=ITEM             haskell interpreter output. Var: file, row, line,
                               tokens. e.g. "file ++ show (tokens)"
       --format=ITEM           format output. Var: #f #n #l #t ## #, #; #0 #1...
                               e.g. "#f:#n #0 #1"
       --json                  format output as json object
       --xml                   format output as xml document

Miscellaneous:

    -d --debug=INT             debug level: 1, 2 or 3
    -? --help                  Display help message
    -V --version               Print version information

