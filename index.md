### Welcome to the CGrep Homepage.

> CGrep is a context-aware grep tool designed specifically for searching in large code repositories. It supports advanced searches across 50+ programming languages that go beyond simple pattern matching, enabling context-aware filtering and semantic searches using wildcards and combinators.

### Features

* **Fine-grained Multi-threading** with configurable number of threads for optimal performance.

* **Blazing fast**: cgrep delivers exceptional performance with significant speed improvements in v9.0.0.

* **Full UTF-8 Support**: Native handling of Unicode and international characters with accurate multi-byte character positioning.

* **Multi-line pattern searches** across code blocks.

* **Automatic coloring** of tokens and strings that match when the output device is a terminal.

* **Recursive searches**: Supports language filters, directory pruning, local and global configurations (cgreprc).

* **Classic searches** using Boyer-Moore, Posix, and PCRE regular expressions.

* **Code Tokenizer**: Search patterns in tokens as prefix, infix, or suffix sub-strings, and by using the edit distance (Levenshtein).

* **Context-aware distinction between code, comments, and literals**: Supported languages include Agda, Assembly, Awk, Bash, C, CMake, Cabal, Chapel, Clojure, Coffee, Conf, Cpp, Csh, Csharp, Css, Cql, D, Dart, Dhall, Elm, Elixir, Erlang, Fish, Fortran, Fsharp, Go, GoMod, Haskell, Html, Idris, Java, Javascript, Json, Julia, Kotlin, Ksh, Latex, Lisp, Lua, Make, Nim, Nmap, OCaml, ObjectiveC, PHP, Perl, Python, R, Ruby, Rust, Scala, SmallTalk, Swift, Sql, Tcl, Text, Unison, VHDL, Verilog, Yaml, Toml, Ini, Zig, Zsh.

* **Semantic searches** using wildcards and combinators: _, _1, _2..., ANY, KEY, STR, LIT, NUM, HEX, OCT. For example, "_ = NUM" searches for variable assignments with numeric literals.

* **Semantic Test Filtering**: Automatically detect and filter test code across 27+ programming languages and their respective testing frameworks (Rust, Go, Java, C/C++, Python, JavaScript/TypeScript, Haskell, and many more).

* **Customizable** output with format options, XML, or JSON.

### What's new in v9.0.0

- **Major Performance Improvements:** Version 9 delivers dramatic performance enhancements with plain token search improved by 75% and semantic search improved by 39% compared to v8.

- **Full UTF-8 Support:** Switched from bytestream to text-based processing, enabling proper handling of UTF-8 character sets and international characters with accurate column positioning in multi-byte environments.

- **Semantic Test Filtering:** New capability to intelligently filter out test code from search results. The `-T` flag now supports 27+ programming languages, automatically detecting test code based on language-specific conventions and testing frameworks (JUnit, pytest, Google Test, Mocha, HSpec, XCTest, and many more).

- **Improved Text Processing:** Native support for Unicode and multi-byte character encodings throughout the codebase.

### Installation

* CGrep is available from [Hackage](http://hackage.haskell.org/package/cgrep) and can be installed using the [cabal](https://www.haskell.org/cabal/) or [stack](https://www.stackage.org/) tool.

```shell
cabal install cgrep
```

or

```shell
stack install cgrep
```

### Quick Examples

Search for a pattern in source code only (excluding comments and strings):

```shell
cgrep -c "malloc" *.c
```

Search using semantic wildcards to find variable assignments:

```shell
cgrep -S "_ = NUM" *.c
```

Search in production code only, excluding all test files:

```shell
cgrep -T False "function" -r src/
```

Search recursively in C++ files:

```shell
cgrep --type=Cpp -r "vector" src/
```

### Documentation

For complete documentation, examples, and the full list of supported features, visit the [GitHub repository](https://github.com/awgn/cgrep).

### Author

[Nicola Bonelli](https://twitter.com/nicolabonelli)