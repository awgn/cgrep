### Welcome to CGrep Home-Page.

> Cgrep is a grep tool suitable for searching in large code repositories. It supports several programming languages and searches that go beyond the simple pattern matching. It enables context-aware filtering and semantic searches through wildcard and combinators.

### Features

* **Fine grained Multi-threading** with configurable number of jobs, tasks, physical cores and chunks.

* **Blazing fast**. Running on a single core it is nearly as fast as GNU grep, up to 6x times faster than [ack](http://beyondgrep.com/).

* **Multi-line** pattern searches.

* **Automatic coloring** of tokens and strings matching when the output device is a terminal.

* **Recursive searches**. Support of language filters and pruned directories, with local or global configurations (cgreprc).

* **Classic searches** with Boyer–Moore algorithm, Posix and PCRE regular expressions.

* **Code Tokenizer**. Searches within tokens, as prefix, infix, or suffix sub-strings and by means of the edit distance (Levenshtein).

* **Context-aware filters**. Filters distinguish among code, comments and literals in different programming languages.

* **Languages support** for Assembly, Awk, C, CMake, Cabal, Chapel, Clojure, Coffee, Conf, Cpp, Csharp, Css, D, Elixir, Erlang, Fortran, Fsharp, Go, Haskell, Html, Idris, Java, Javascript, Latex, Lua, Make, OCaml, ObjectiveC, PHP, Perl, Python, Ruby, Scala, Shell, Tcl, Text, VHDL, Verilog, Vim.

* **Semantic searches** through wildcards and combinators. _, _1, _2..., $, $1, $2..., ANY, KEY, STR, CHR, NUM, HEX, OCT, OR. E.g. "_1(_1 && $)" search for move constructors, "struct OR class _ { OR : OR <" search for class declarations.

* **Customizable** output with format string, XML and JSON back-ends.

### Install

* Cgrep is available from [Hackage](http://hackage.haskell.org/package/cgrep) and can be installed using [cabal](https://www.haskell.org/cabal/) or [stack](https://www.stackage.org/) tool.

`cabal install cgrep`

or

`stack install cgrep`

### Author

[Nicola Bonelli](https://twitter.com/nicolabonelli)