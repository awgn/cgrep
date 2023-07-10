### Welcome to the CGrep Homepage.

> CGrep is a grep tool suitable for searching in large code repositories. It supports advanced searches in several programming languages that go beyond simple pattern matching. It enables context-aware filtering and semantic searches using wildcards and combinators.

### Features

* **Fine-grained Multi-threading** with configurable number of threads.

* **Blazing fast**: Running on a single core, it is nearly as fast as GNU grep, and up to 6 times faster than [ack](http://beyondgrep.com/).

* **Multi-line pattern searches**.

* **Automatic coloring** of tokens and strings that match when the output device is a terminal.

* **Recursive searches**: Supports language filters, directory pruning, local and global configurations (cgreprc).

* **Classic searches** using Boyer-Moore, Posix, and PCRE regular expressions.

* **Code Tokenizer**: Search patterns in tokens as prefix, infix, or suffix sub-strings, and by using the edit distance (Levenshtein).

* **Context-aware distinction between code, comments, and literals**: Supported languages include Agda, Assembly, Awk, Bash, C, CMake, Cabal, Chapel, Clojure, Coffee, Conf, Cpp, Csh, Csharp, Css, Cql, D, Dart, Dhall, Elm, Elixir, Erlang, Fish, Fortran, Fsharp, Go, GoMod, Haskell, Html, Idris, Java,  Javascript, Json, Julia, Kotlin, Ksh, Latex, Lisp, Lua, Make, Nim, Nmap, OCaml, ObjectiveC, PHP, Perl, Python, R, Ruby, Rust, Scala, SmallTalk, Swift, Sql, Tcl, Text, Unison, VHDL, Verilog, Yaml, Toml, Ini, Zig, Zsh.

* **Semantic searches** using wildcards and combinators: _, _1, _2..., $, $1, $2..., ANY, KEY, STR, CHR, NUM, HEX, OCT, OR. For example, "_1(_1 && $)" searches for C++ move constructors, and "struct OR class _ { OR :" searches for class declarations.

* **Customizable** output with format string, XML, or JSON.

### What's new in v8.0.0

- **Performance Enhancements:** in this release, significant improvements have been made to enhance the performance of cgrep. The performance is now more on par with ripgrep, with search speeds ranging from 3 times to 13 times faster than the previous release.

- **Expanded Language and Configuration Support:** We have expanded the language and configuration support in cgrep. Starting from this version, it includes support for searching within Zig, Union, Dhall, Fish shell, Toml, and Ini config files. This means you can now easily search for patterns and text within files of these formats.

- **New Semantic Token Filter:** A new semantic token filter has been introduced in this release. This filter is designed to identify and filter out specific native types present in different file types. This enhances the search results by providing more precise and relevant matches based on the semantic meaning of the tokens.

- **Kind Filter Selector:** We have introduced a new feature called the Kind Filter Selector. With this filter selector, users can now specify the kind of files they want to search within. The available options for the filter include Text, Config, Language, Data, Markup, and Script. This allows for more focused and targeted searches based on the desired file type, especially useful in large codebases.

### Installation

* CGrep is available from [Hackage](http://hackage.haskell.org/package/cgrep) and can be installed using the [cabal](https://www.haskell.org/cabal/) or [stack](https://www.stackage.org/) tool.

```shell
cabal install cgrep
```

or

```shell
stack install cgrep
```

### Author

[Nicola Bonelli](https://twitter.com/nicolabonelli)
