CGrep: a context-aware grep for source codes
============================================

Usage
-----

Cgrep 3.4. Usage: cgrep [OPTION] [PATTERN] files...

cgrep [OPTIONS] [ITEM]

Pattern:

    -f --file=FILE      read PATTERNs from file
    -w --word           force word matching
    -G --regex          regex matching
    -i --ignore-case    ignore case distinctions

Context:

    -c --code           search in source code
    -m --comment        search in comments
    -l --literal        search in string literals

C/C++ language:

       --identifier     identifiers
       --keyword        keywords
       --directive      preproc directives
       --header         headers name
       --number         literal numbers
       --string         literal strings
       --char           literal chars
       --oper           operators
    -S --snippet        "code" e.g. "_1(_1 && _)" search for move ctors

Output control:

    -h --no-filename    suppress the file name prefix on output
    -N --no-line-umber  suppress the line number on output lines
       --lang=ITEM      specify languages to grep for. ie: Cpp, +Haskell,
                        -Makefile
       --lang-map       print the list of language mapping
    -j --jobs=INT       number of jobs
       --multiline=INT  enable multi-line matching
    -r --recursive      enable recursive search
       --invert-match   select non-matching lines
       --max-count=INT  stop search in files after INT matches
       --count          print only a count of matching lines per file

Miscellaneous:

    -d --debug=INT      debug level: 1, 2 or 3
    -? --help           Display help message
    -V --version        Print version information

