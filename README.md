# CGrep: a context-aware grep for source codes

[![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
[![Join the chat at https://gitter.im/awgn/cgrep](https://badges.gitter.im/awgn/cgrep.svg)](https://gitter.im/awgn/cgrep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Version 9.3.1** - A powerful, context-aware search tool designed specifically for source code.

CGrep extends the capabilities of traditional grep by understanding the structure and semantics of source code across multiple programming languages. It allows developers to search within specific contexts like code, comments, or string literals, and provides advanced pattern matching with semantic awareness.

---

## What's New in Version 9.3.1 (powered by Google Argon 4)

- **New Semantic Wildcards (`TYP`, `OP`, `LIT`)** - Match native types (`TYP`), operators (`OP`), and any string or numeric literal (`LIT`, distinct from `STR` which matches only string literals).
- **Structural Ellipsis Operator (`...`)** - Match zero or more tokens across expressions, argument lists, or statements while automatically respecting balanced brackets (`()`, `[]`, `{}`) and statement boundaries (`;`).
- **Semantic Combinators (`NOT`, `OR`)** - Combine or negate semantic atoms and wildcards directly in `-S` patterns (e.g., `TYP OR KEY _ ( ... )`, `_ = NOT STR`, `STR OR NUM`).
- **Only-Matching Output (`-o`)** - With `-S`, print only the matched tokens instead of the whole line (e.g. `cgrep -S -o -h --no-numbers "_ = NOT STR" *.c`).
- **Improved Placeholder Unification (`_`, `_0`, `_1`, ...)** - Language-agnostic unification for numbered identifier placeholders and preserved leading underscores on normal identifiers (`_foo`, `__init__`).

## What's New in Version 9.3.0 (powered by Google Argon 4)

- **Modular Semantic Test Filtering Across 29 Languages** - Refactored the semantic test filter (`--tests-only` / `--no-tests`) into a type-safe per-language architecture and added support for **Lua** (Busted, Lust, LuaUnit), **Elm** (`elm-test`), and **Bash** (BATS, shUnit2, bashunit).
- **Expanded Testing Framework Coverage** - Added detection for Kotest (Kotlin), ZIO Test and Specs2 (Scala), Pest and PHP 8 `#[Test]` attributes (PHP), Swift Testing `@Test`/`@Suite` macros and Quick/Nimble (Swift), `Deno.test` and `.each`/`.concurrent` chains (JS/TS), Doctest (`TEST_SUITE`, `SUBCASE`), `RUnit` (R), `Test::Class` and `Test::Spec` (Perl), Common Test (Erlang), and Midje (Clojure).
- **Comprehensive False-Positive Hardening** - Eliminated false positives on domain identifiers (`testify`, `@Testimonial`, `[Testable]`), forward/abstract declarations, record/method field accesses (`regex.test()`, `#state.value`, `[1:end]`), and nested block boundaries across all 29 languages in both default and `--strict` modes.

## What's New in Version 9.2.1

- Replaced the somewhat awkward `-T True`/`-T False` options with the more standard `--tests-only` (or `-T`) and `--no-tests` flags for test filtering.

## What's New in Version 9.2

### 🚀 Major Performance Improvements

- **75% faster plain token search** - Dramatically improved performance for standard token-based searches
- **39% faster semantic search** - Significant speed boost for advanced semantic pattern matching
- **Full UTF-8 support** - Switched from bytestream to text processing, enabling proper handling of UTF-8 character sets and international characters

### ✨ Enhanced Features

- **Semantic Test Filtering** - Improved capability to filter out test code from search results across 29 programming languages and their respective testing frameworks (see [Test Framework Support](#test-framework-support) below)
- **Improved Text Processing** - Native support for utf-8 character encodings with accurate column positioning

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
cgrep 9.3.1 - Usage: cgrep [OPTION] [PATTERN] files...

Usage: cgrep [--file FILE] [-w|--word] [-p|--prefix] [-s|--suffix] [-e|--edit]
             [-G|--regex] [-i|--ignore-case] [-c|--code] [-m|--comment]
             [-l|--literal] [--identifier|--name] [--native|--type] [--keyword]
             [--number] [--string] [--op] [--type TYPE] [--kind KIND]
             [--code-only] [--hdr-only] [-T|--tests-only] [--no-tests]
             [--prune-dir DIR] [-r|--recursive] [-L|--follow] [-S|--semantic]
             [--strict]
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
  -T,--tests-only          Search only in test files
  --no-tests               Exclude test files from search
  --prune-dir DIR          Do not descend into dir
  -r,--recursive           Enable recursive search (don't follow symlinks)
  -L,--follow              Follow symlinks
  -S,--semantic            "code" pattern: _, _1, _2... (identifiers), ..., ANY,
                           KEY, TYP, OP, STR, LIT, NUM, HEX, OCT, OR, NOT
  --strict                 Enable strict semantic for operators
  --max-count INT          Stop search in files after INT matches
  --force-type TYPE        Force the type of file
  --type-list              List the supported file types
  -v,--invert-match        Select non-matching lines
  -j,--threads INT         Approximate number of threads to run search
  --show-match             Show list of matching tokens
  -o,--only-matching       Print only the matching tokens, not the whole line
                           (requires -S)
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
cgrep --type=Haskell --no-tests "function" -r .
```

Search by file kind (configuration files):

```bash
cgrep --kind=Config "database" -r /etc/
```

### Test Filtering (New in v9)

Search only in production code, excluding all tests:

```bash
cgrep --no-tests "function" -r src/
```

Search only in test code:

```bash
cgrep --tests-only "mock" -r tests/
```

This feature automatically detects and filters test code based on language-specific conventions (see [Test Framework Support](#test-framework-support)).

### Semantic Search

Semantic search (`-S` / `--semantic`) allows you to match structural code patterns using wildcards and combinators:

- `_` : Match any identifier (anonymous wildcard)
- `_0`, `_1`, `_2`, ... : Match any identifier with unification (same placeholder in a pattern must bind to the same identifier token)
- `...` : Match zero or more tokens (bracket-balanced across `()`, `[]`, `{}` and stopping at `;` at top depth)
- `ANY` : Match any single token
- `KEY` : Match any keyword
- `TYP` : Match any native type
- `OP` : Match any operator
- `STR` : Match any string literal
- `LIT` : Match any literal (string or number)
- `NUM` : Match any number
- `HEX` : Match any hexadecimal number
- `OCT` : Match any octal number
- `NOT <atom>` : Match any token that does not match `<atom>`
- `<atom1> OR <atom2>` : Match either `<atom1>` or `<atom2>`

*(Prefix any wildcard or combinator keyword with `_`, e.g. `_ANY`, `_NOT`, `_OR`, to search for it as a literal identifier.)*

Find variable assignments with numeric literals:

```bash
cgrep -S "_ = NUM" *.c
```

Find self-assignments (`x = x`) using unified placeholders:

```bash
cgrep -S "_1 = _1" *.c
```

Find function calls or definitions with arbitrary arguments using `...` and combinators:

```bash
cgrep -S "TYP OR KEY _ ( ... )" *.c
cgrep -S "_ = NOT STR" *.py
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

Version 9.3 provides intelligent test code filtering across 29 programming languages. When using `--tests-only` (`-T`) or `--no-tests`, cgrep automatically detects and filters test code based on language-specific conventions and testing frameworks.

| Language | Testing Frameworks Detected | Detection Patterns |
|----------|----------------------------|-------------------|
| **Rust** | Built-in, cargo test, tokio, rstest | `#[test]`, `#[tokio::test]`, `#[rstest]`, `#[test_case]`, `#[cfg(test)]` modules/items |
| **Go** | Built-in testing | `func Test*`, `func Benchmark*`, `func Example*`, `func Fuzz*`, `t.Run()`, `b.Run()`, `*_test.go` |
| **Java** | JUnit 4/5, TestNG | `@Test`, `@ParameterizedTest`, `@RepeatedTest`, `@TestFactory`, `@TestTemplate`, `@Before*`, `@After*`, `@Nested`, `@DataProvider` |
| **Kotlin** | JUnit 4/5, TestNG, Kotest | `@Test`, `@ParameterizedTest`, `@Before*`, `@After*`, Kotest `FunSpec`, `StringSpec`, `BehaviorSpec`, `DescribeSpec`, `ShouldSpec`, `WordSpec`, `FreeSpec`, `FeatureSpec`, `ExpectSpec` |
| **C** | Google Test, Catch2, Doctest, Unity/Check | `TEST()`, `TEST_F()`, `TEST_CASE()`, `SCENARIO()`, `SECTION()`, `SUBCASE()`, `START_TEST()`, `test_*` functions |
| **C++** | Google Test, Catch2, Doctest, Boost.Test | `TEST()`, `TEST_F()`, `TEST_P()`, `TYPED_TEST()`, `TEST_CASE()`, `SCENARIO()`, `TEST_SUITE()`, `SUBCASE()`, `BOOST_AUTO_TEST_CASE()`, `BOOST_FIXTURE_TEST_CASE()` |
| **Python** | pytest, unittest | `def test_*`, `async def test_*`, `class Test*`, `unittest.TestCase` subclasses, `@pytest.*`, `@unittest.*` stacked decorators |
| **Zig** | Built-in | `test "..." { ... }`, `test { ... }`, `test Identifier { ... }` blocks |
| **JavaScript** | Jest, Mocha, Jasmine, Vitest, Deno | `describe()`, `it()`, `test()`, `suite()`, `.only`/`.skip`/`.each`/`.concurrent` chains, `Deno.test()`, lifecycle hooks |
| **TypeScript** | Jest, Mocha, Jasmine, Vitest, Deno | `describe()`, `it()`, `test()`, `suite()`, `.only`/`.skip`/`.each`/`.concurrent` chains, `Deno.test()`, lifecycle hooks |
| **Scala** | ScalaTest, MUnit, Specs2, ZIO Test | `AnyFunSuite`, `AnyFlatSpec`, `AnyWordSpec`, `AnyFreeSpec`, `AnyFunSpec`, `AnyPropSpec`, `FunSuite`, `Specification`, `ZIOSpecDefault`, `test()`, `describe()`, `suite()` |
| **Haskell** | HSpec, Tasty, QuickCheck, Hedgehog, HUnit | `describe`, `it`, `specify`, `context`, `testCase`, `testGroup`, `testProperty`, `prop_*` signatures & bindings |
| **C#** | NUnit, xUnit, MSTest | `[Test]`, `[Fact]`, `[Theory]`, `[TestCase]`, `[TestCaseSource]`, `[InlineData]`, `[MemberData]`, `[ClassData]`, `[TestClass]`, `[TestMethod]`, `[DataTestMethod]`, `[TestFixture]`, `[SetUp]`, `[TearDown]` |
| **F#** | NUnit, xUnit, MSTest, Expecto | `[<Test>]`, `[<Fact>]`, `[<Theory>]`, `[<TestCase>]`, `[<Property>]`, `test`, `testCase`, `testCaseAsync`, `testList`, `testTheory`, `testProperty`, `ftest*`, `ptest*` |
| **Dart** | Built-in test package, Flutter | `test()`, `group()`, `testWidgets()`, `setUp()`, `tearDown()`, `setUpAll()`, `tearDownAll()` with callbacks |
| **Elixir** | ExUnit | `test`, `describe`, `setup`, `setup_all`, `doctest` (`do ... end` and `, do:` forms) |
| **Ruby** | RSpec, Minitest, Test::Unit | `RSpec.describe`, `describe`, `context`, `it`, `specify`, `example`, `before`, `after`, `let`, `let!`, `subject`, `def test_*`, `class ... < Minitest::Test` |
| **PHP** | PHPUnit, Pest, Codeception | `#[Test]`, `#[DataProvider]`, `@test`, `extends TestCase`, `function test*()`, `setUp()`, `tearDown()`, Pest `test()`, `it()`, `describe()` |
| **Swift** | XCTest, Swift Testing, Quick/Nimble | `@Test`, `@Suite`, `XCTestCase` subclasses, `func test*()`, `setUp*()`, `tearDown*()`, `describe()`, `context()`, `it()` |
| **Objective-C** | XCTest | `XCTestCase` subclasses, `- (void)test*`, `setUp`, `tearDown` |
| **R** | testthat, RUnit | `test_that()`, `describe()`, `it()`, `test.* <- function()` |
| **Julia** | Built-in Test | `@testset`, `@test`, `@test_throws`, `@test_broken`, `@test_skip`, `@test_warn`, `@test_nowarn`, `@inferred` |
| **Perl** | Test::More, Test::Class, Test::Spec | `subtest`, `sub test_*`, `: Test(...)` / `: Tests(...)` attributes, `describe`, `context`, `it`, `they` |
| **OCaml** | OUnit2, Alcotest, Jane Street PPX | `let%test`, `let%test_unit`, `let%test_module`, `let%expect_test`, `test_case`, `>::`, `>:::` |
| **Erlang** | EUnit, Common Test | `*_test()`, `*_test_()`, `all()`, `groups()`, `init_per_suite()`, `end_per_suite()`, `init_per_testcase()`, `end_per_testcase()` |
| **Nim** | unittest | `suite`, `test`, `setup:`, `teardown:`, `check()` |
| **Clojure** | clojure.test, Expectations, Midje | `(deftest ...)`, `(deftest- ...)`, `(testing ...)`, `(defexpect ...)`, `(facts ...)`, `(fact ...)` |
| **D** | Built-in | `unittest { ... }` (including attributed and `version(unittest)` blocks) |
| **Lua** | Busted, Lust, LuaUnit | `describe()`, `insulate()`, `expose()`, `context()`, `it()`, `spec()`, `test()`, `pending()`, `setup()`, `teardown()`, `before_each()`, `after_each()`, `function test*()`, `Test*:test*()` |
| **Elm** | elm-test | `suite : Test`, `describe`, `test`, `fuzz`, `fuzz2`, `fuzz3`, `fuzzWith`, `todo` |
| **Bash** | BATS, shUnit2, bashunit | `@test "..." { ... }`, `test_*() { ... }`, `test*() { ... }`, `function test_*`, `oneTimeSetUp()`, `setUp()`, `tearDown()`, `set_up()`, `tear_down()` |

### Example: Finding Production Code Only

```bash
# Search for "config" only in production code, skipping all tests
cgrep --no-tests "config" -r src/

# Search for mock usage only in test files
cgrep --tests-only "mock" -r .
```

---

## Supported Languages

CGrep supports a wide range of programming languages and file formats:

**Programming Languages:** C, C++, C#, Java, Kotlin, Scala, JavaScript, TypeScript, CoffeeScript, Python, Ruby, Perl, PHP, Go, Rust, Haskell, OCaml, F#, Erlang, Elixir, Elm, Clojure, Lisp, Scheme, Lua, R, Julia, Dart, Nim, Zig, D, Swift, Objective-C, Chapel, Awk, Shell scripts (Bash, Fish, Ksh, Zsh)

**Markup & Config:** HTML, XML, LaTeX, Markdown, YAML, JSON, TOML, INI, Dhall, CMake, Makefile, Cabal, ProtoBuf

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
   cgrep --no-tests "pattern" -r .  # Exclude all test code
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
