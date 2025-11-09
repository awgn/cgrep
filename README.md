# CGrep: a context-aware grep for source codes

[![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
[![Join the chat at https://gitter.im/awgn/cgrep](https://badges.gitter.im/awgn/cgrep.svg)](https://gitter.im/awgn/cgrep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Version 9.0.0** - A powerful, context-aware search tool designed specifically for source code.

CGrep extends the capabilities of traditional grep by understanding the structure and semantics of source code across multiple programming languages. It allows developers to search within specific contexts like code, comments, or string literals, and provides advanced pattern matching with semantic awareness.

---

## What's New in Version 9.0

### 🚀 Major Performance Improvements

- **75% faster plain token search** - Dramatically improved performance for standard token-based searches
- **39% faster semantic search** - Significant speed boost for advanced semantic pattern matching
- **Full UTF-8 support** - Switched from bytestream to text processing, enabling proper handling of UTF-8 character sets and international characters

### ✨ Enhanced Features

- **Semantic Test Filtering** - New capability to filter out test code from search results across 27+ programming languages and their respective testing frameworks (see [Test Framework Support](#test-framework-support) below)
- **Improved Text Processing** - Native support for Unicode and multi-byte character encodings with accurate column positioning

---

## Installation

### From Hackage

```bash
cabal update
cabal install cgrep
```

### From Source

```bash
git clone https://github.com/awgn/cgrep.git
cd cgrep
cabal install
```

or using stack:

```bash
stack build
stack install
```

---

## Usage

```
cgrep 9.0.0 - Usage: cgrep [OPTION] [PATTERN] files...

Usage: cgrep [--file FILE] [-w|--word] [-p|--prefix] [-s|--suffix] [-e|--edit]
             [-G|--regex] [-i|--ignore-case] [-c|--code] [-m|--comment]
             [-l|--literal] [--identifier|--name] [--native|--type] [--keyword]
             [--number] [--string] [--op] [--type TYPE] [--kind KIND]
             [--code-only] [--hdr-only] [-T|--tests ARG] [--prune-dir DIR]
             [-r|--recursive] [-L|--follow] [-S|--semantic] [--strict]
             [--max-count INT] [--force-type TYPE] [--type-list]
             [-v|--invert-match] [-j|--threads INT] [--show-match] [--color]
             [--no-color] [-h|--no-filename] [--no-numbers] [--no-column]
             [--count] [--filename-only] [--json] [--vim] [--editor]
             [--fileline] [--verbose] [--stats] [--null-output] [--palette]
             [PATTERN FILES...] [--version]

  Context-aware grep for source codes

Available options:
  --file FILE              Read PATTERNs from file (one per line)
  -w,--word                Force word matching
  -p,--prefix              Force prefix matching
  -s,--suffix              Force suffix matching
  -e,--edit                Use edit distance
  -G,--regex               Use regex matching (posix)
  -i,--ignore-case         Ignore case distinctions
  -c,--code                Enable search in source code
  -m,--comment             Enable search in comments
  -l,--literal             Enable search in string literals
  --identifier,--name      Identifiers
  --native,--type          Native Types
  --keyword                Keywords
  --number                 Literal numbers
  --string                 Literal strings
  --op                     Operators
  --type TYPE              Specify file types. ie: Cpp, +Haskell, -Makefile
  --kind KIND              Specify file kinds. Text, Config, Language, Data,
                           Markup or Script
  --code-only              Parse code modules only (skip headers/interfaces)
  --hdr-only               Parse headers/interfaces only (skip modules)
  -T,--tests ARG           Filter tests: 'True' tests only, 'False' code only,
                           omitted (search all)
  --prune-dir DIR          Do not descend into dir
  -r,--recursive           Enable recursive search (don't follow symlinks)
  -L,--follow              Follow symlinks
  -S,--semantic            "code" pattern: _, _1, _2... (identifiers), ANY, KEY,
                           STR, LIT, NUM, HEX, OCT
  --strict                 Enable strict semantic for operators
  --max-count INT          Stop search in files after INT matches
  --force-type TYPE        Force the type of file
  --type-list              List the supported file types
  -v,--invert-match        Select non-matching lines
  -j,--threads INT         Approximate number of threads to run search
  --show-match             Show list of matching tokens
  --color                  Use colors to highlight the match strings
  --no-color               Do not use colors (override config file)
  -h,--no-filename         Suppress the file name prefix on output
  --no-numbers             Suppress both line and column numbers on output
  --no-column              Suppress the column number on output
  --count                  Print only a count of matching lines per file
  --filename-only          Print only the name of files containing matches
  --json                   Format output as json object
  --vim                    Run vim editor passing the files that match
  --editor                 Run the editor specified by EDITOR var., passing the
                           files that match
  --fileline               When edit option is specified, pass the list of
                           matching files in file:line format (e.g. vim
                           'file-line' plugin)
  --verbose                Enable verbose mode
  --stats                  Print statistics about the search
  --null-output            Disable output for performance evaluation
  --palette                Show color palette
  -h,--help                Show this help text
  --version                Show version information and exit
```

---

## Examples

### Basic Searches

Search for a simple pattern in source files:

```bash
cgrep "main" *.c
```

Search recursively in a directory:

```bash
cgrep -r "TODO" src/
```

Case-insensitive search:

```bash
cgrep -i "buffer" *.cpp
```

### Context-Aware Searching

Search only in code (exclude comments and strings):

```bash
cgrep -c "malloc" *.c
```

Search only in comments:

```bash
cgrep -m "TODO" -r src/
```

Search only in string literals:

```bash
cgrep -l "hello" *.cpp
```

Search in both code and comments, but not in strings:

```bash
cgrep -c -m "config" *.js
```

### Token Filters

Search for identifiers only:

```bash
cgrep --identifier "main" *.c
```

Search for native types:

```bash
cgrep --type "int" *.c
```

Search for string literals containing specific text:

```bash
cgrep --string "hello" *.cpp
```

### File Type and Kind Filters

Search only in C++ files (recursively):

```bash
cgrep --type=Cpp -r "char" test/
```

Search in Haskell files, but exclude test code:

```bash
cgrep --type=Haskell -T False "function" -r .
```

Search by file kind (configuration files):

```bash
cgrep --kind=Config "database" -r /etc/
```

### Test Filtering (New in v9)

Search only in production code, excluding all tests:

```bash
cgrep -T False "function" -r src/
```

Search only in test code:

```bash
cgrep -T True "mock" -r tests/
```

This feature automatically detects and filters test code based on language-specific conventions (see [Test Framework Support](#test-framework-support)).

### Semantic Search

Semantic search allows you to match code patterns using wildcards:

- `_`, `_1`, `_2`, ... : Match any identifier
- `ANY` : Match any token
- `KEY` : Match any keyword
- `STR` : Match any string literal
- `LIT` : Match any literal
- `NUM` : Match any number
- `HEX` : Match any hexadecimal number
- `OCT` : Match any octal number

Find variable assignments with numeric literals:

```bash
cgrep -S "_ = NUM" *.c
```

### Advanced Pattern Matching

Use regular expressions (POSIX):

```bash
cgrep -G "main|return" *.c
```

Use word boundaries:

```bash
cgrep -w "read" *.c    # Matches "read" but not "thread" or "reader"
```

Prefix matching:

```bash
cgrep -p "ma" *.c   # Matches "main", etc.
```

Suffix matching:

```bash
cgrep -s "rn" *.c  # Matches "return", etc.
```

Case-insensitive search:

```bash
cgrep -i "SED" test.cpp  # Matches "Sed", "sed", etc.
```

### Output Formatting

Show only filenames of files containing matches:

```bash
cgrep --filename-only "char" *.cpp
```

Count matches per file:

```bash
cgrep --count "char" test.cpp
```

JSON output (useful for scripting):

```bash
cgrep --json "error" *.log
```

Suppress filename prefix:

```bash
cgrep -h "pattern" file.c
```

Show matching tokens:

```bash
cgrep --show-match "std::" *.cpp
```

### Editor Integration

Open matching files in vim:

```bash
cgrep --vim "FIXME" -r src/
```

Open with your default editor (set via EDITOR environment variable):

```bash
cgrep --editor "bug" -r src/
```

Use vim with file:line format (works with vim-file-line plugin):

```bash
cgrep --vim --fileline "error" -r src/
```

### Performance and Control

Limit number of threads:

```bash
cgrep -j 4 "pattern" -r large_codebase/
```

Stop after finding N matches in each file:

```bash
cgrep --max-count=5 "TODO" -r src/
```

Show search statistics:

```bash
cgrep --stats "function" -r src/
```

### UTF-8 and International Characters

Version 9's improved UTF-8 support allows searching in files with international characters:

```bash
cgrep "Hello" test.utf8
```

---

## Test Framework Support

Version 9 introduces intelligent test code filtering across 27+ programming languages. When using the `-T` flag, cgrep can automatically detect and filter test code based on language-specific conventions and testing frameworks.

| Language | Testing Frameworks Detected | Detection Patterns |
|----------|----------------------------|-------------------|
| **Rust** | Built-in, cargo test | `#[test]`, `#[cfg(test)]` modules |
| **Go** | Built-in testing | `func Test*`, `func Benchmark*` |
| **Java** | JUnit | `@Test` annotations |
| **Kotlin** | JUnit | `@Test` annotations |
| **C** | Google Test, Catch2 | `TEST()`, `TEST_F()`, `TEST_CASE()`, `test_*` functions |
| **C++** | Google Test, Catch2, Boost.Test | `TEST()`, `TEST_F()`, `TEST_CASE()`, `BOOST_AUTO_TEST`, `Test*` functions |
| **Python** | pytest, unittest | `test_*` functions, `Test*` classes, `@pytest`, `@unittest` decorators |
| **Zig** | Built-in | `test "..."` blocks |
| **JavaScript** | Mocha, Jasmine, Jest | `describe()`, `it()`, `test()`, `context()` |
| **TypeScript** | Mocha, Jasmine, Jest | `describe()`, `it()`, `test()`, `context()` |
| **Scala** | ScalaTest, MUnit | `test()`, `it()`, `describe()`, `scenario()`, `feature()` |
| **Haskell** | HSpec, Tasty, QuickCheck, HUnit | `describe`, `it`, `context`, `testCase`, `testGroup`, `testProperty`, `prop_*` functions |
| **C#** | NUnit, xUnit, MSTest | `[Test]`, `[Fact]`, `[Theory]`, `[TestMethod]`, `[TestFixture]`, `[TestClass]` |
| **F#** | NUnit, xUnit, Expecto | `[<Test>]`, `[<Fact>]`, `[<Theory>]`, `testCase`, `testList`, `test` |
| **Dart** | Built-in test package, Flutter | `test()`, `group()`, `testWidgets()` |
| **Elixir** | ExUnit | `test "..."`, `describe "..."`, `defmodule *Test` |
| **Ruby** | RSpec, Minitest | `describe`, `context`, `it`, `test_*`, `def test_*` |
| **PHP** | PHPUnit | `@test` annotations, `test*` methods, `*Test` classes |
| **Swift** | XCTest | `XCTestCase` classes, `func test*()` |
| **Objective-C** | XCTest | `XCTestCase` classes, test methods |
| **R** | testthat | `test_that()`, `describe()`, `context()` |
| **Julia** | Built-in Test | `@testset`, `@test` |
| **Perl** | Test::More, Test::Simple | `subtest`, `*.t` files |
| **OCaml** | OUnit, Alcotest | `let test_*`, `test_case` |
| **Erlang** | EUnit | `*_test()`, `*_test_()` functions |
| **Nim** | unittest | `unittest` module, `test "..."` |
| **Clojure** | clojure.test | `(deftest ...)`, `(testing ...)` |
| **D** | Built-in | `unittest { ... }` blocks |

### Example: Finding Production Code Only

```bash
# Search for "config" only in production code, skipping all tests
cgrep -T False "config" -r src/

# Search for mock usage only in test files
cgrep -T True "mock" -r .
```

---

## Supported Languages

CGrep supports a wide range of programming languages and file formats:

**Programming Languages:** C, C++, C#, Java, Kotlin, Scala, JavaScript, TypeScript, CoffeeScript, Python, Ruby, Perl, PHP, Go, Rust, Haskell, OCaml, F#, Erlang, Elixir, Clojure, Lisp, Scheme, Lua, R, Julia, Dart, Nim, Zig, D, Swift, Objective-C, Chapel, Awk, Shell scripts (Bash, Fish)

**Markup & Config:** HTML, XML, LaTeX, Markdown, YAML, JSON, TOML, INI, Dhall, CMake, Makefile, Cabal

To see the complete list of supported file types:

```bash
cgrep --type-list
```

---

## Configuration

CGrep can be configured using a configuration file located at `~/.cgreprc` (or `$XDG_CONFIG_HOME/cgrep/cgreprc`).

Example configuration:

```yaml
colors: true
file-types:
  - +Cpp
  - +Haskell
  - -Test
jobs: 8
```

---

## Performance Tips

1. **Use file type filters** to reduce the number of files processed:
   ```bash
   cgrep --type=Cpp "pattern" -r .
   ```

2. **Limit the search scope** with `--prune-dir` to exclude directories:
   ```bash
   cgrep --prune-dir=node_modules --prune-dir=.git "pattern" -r .
   ```

3. **Use context filters** when you know where to search:
   ```bash
   cgrep -c "pattern" -r .  # Search only in code
   ```

4. **Use test filtering** to focus on production code:
   ```bash
   cgrep -T False "pattern" -r .  # Exclude all test code
   ```

5. **Adjust thread count** for optimal performance on your system:
   ```bash
   cgrep -j 16 "pattern" -r .
   ```

---

## Benchmarks (v9 vs v8)

| Search Type | v8 | v9 | Improvement |
|-------------|----|----|-------------|
| Plain token search | 1.0x | **1.75x** | **+75%** |
| Semantic search | 1.0x | **1.39x** | **+39%** |

*Benchmarks performed on a typical codebase with ~100k lines of code.*

---

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests on [GitHub](https://github.com/awgn/cgrep).

---

## License

CGrep is released under the GPL-2.0-or-later license. See the LICENSE file for details.

---

## Author

Nicola Bonelli <nicola@larthia.com>

---

## Links

- **Homepage:** [http://awgn.github.io/cgrep/](http://awgn.github.io/cgrep/)
- **Hackage:** [https://hackage.haskell.org/package/cgrep](https://hackage.haskell.org/package/cgrep)
- **GitHub:** [https://github.com/awgn/cgrep](https://github.com/awgn/cgrep)
- **Gitter Chat:** [https://gitter.im/awgn/cgrep](https://gitter.im/awgn/cgrep)