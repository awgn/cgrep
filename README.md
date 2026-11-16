# CGrep: a context-aware grep for source codes

[![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
[![Join the chat at https://gitter.im/awgn/cgrep](https://badges.gitter.im/awgn/cgrep.svg)](https://gitter.im/awgn/cgrep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Version 10.0.0** - A powerful, context-aware search tool designed specifically for source code.

CGrep extends the capabilities of traditional grep by understanding the structure and semantics of source code across multiple programming languages. It allows developers to search within specific contexts like code, comments, or string literals, and provides advanced pattern matching with semantic awareness.

---

## What's New in Version 10.0.0

Version 10.0 is a major architectural release combining a ground-up performance overhaul across every stage of the engine, adaptive `mmap` I/O with pre-decode raw-byte filtering, resilient non-UTF-8 source handling, structural lexer improvements, and a deep accuracy upgrade of `--tests-only` / `--no-tests` across 30 languages.

### 🚀 Where the Performance Improvements Went (2x–39x Faster, up to 89x Less Memory)

Every stage of CGrep was profiled and re-engineered against a new reproducible `criterion` benchmark suite (`bench/`, backed by 31 real-world open-source files totaling **2.74 MB / 80,849 lines**) and verified with differential QuickCheck property suites:

- **Context Filter (`7.0x–7.7x` faster, `79x` less heap — `369 MiB -> 4.7 MiB`):** Replaced character-by-character `Text.unfoldr` with an in-place `ST` `MArray` byte engine that preserves exact UTF-8 byte offsets, a 128-bit ASCII delimiter bitmask, vectorized C `memchr` scanning inside comments and raw strings, and CAF-memoized `ParConfig` tables per language.
- **Unboxed Token & Chunk Lexers (`2.9x–3.4x` faster stage, `5.3x–5.4x` faster end-to-end Tokenizer, up to `58x` less heap):** Replaced per-character `STRef` boxes and `Data.Sequence` finger-trees with unboxed mutually recursive loops directly over `Data.Text.Array`, `memchr` string-literal scanning, early `TokenFilter` pruning before token allocation, zero-cost `coerce` conversions, and $O(1)$ branch-table character predicates.
- **LineIndex, Boyer-Moore & Case-Insensitive Search (`7.0x` faster `-i`, `5.5x` faster `-c`, `3.2x` faster `-w`, `3.1x` faster `LineIndex`):** Built `LineIndex` directly into an unboxed `Vector Int` via SIMD `memchr`, replaced per-call skip-table heap allocations with register-resident 64-bit bloom-mask + `memchr` substring search, added a zero-copy / ASCII byte-level fast path for `ignoreCase`, and replaced forward index lists with backwards `sliceToLastMatch`.
- **Fuzzy / Edit-Distance Search (`39.0x` faster — `938 ms -> 24.1 ms`, `89x` less heap — `4.74 GiB -> 54.5 MiB`):** Replaced `[Char]` unpacking and lazy mutually-recursive diagonal DP with a precompiled `EditPattern`, $O(1)$ exact length-window pruning, and zero-allocation **64-bit Myers bit-parallel DP** in CPU registers (with early row termination) for ASCII tokens plus an unboxed single-row `ST` DP fallback for Unicode and long patterns.
- **Semantic, Combinator & POSIX Regex Engines (`4.5x–5.4x` faster `-S` with `11x–15.5x` less heap; `5.8x` faster `-G` with `7.4x` less heap):** Added `CompiledAtom` / `SliceMatcher` compilation with specialized allocation-free loops, zero-allocation ASCII case-insensitive/infix comparators, high-selectivity mandatory anchor extraction (`extractAtomAnchors` — avoiding false wakeups on punctuation like `(` or `*` in patterns such as `fn(...)`), and a sound POSIX ERE DNF required-literal extractor (`extractPosixLinePrefilter`) that runs `TDFA` only on candidate lines.
- **Adaptive `mmap` I/O & Pre-Decode Raw-Byte Filtering (`1.6x–21x` faster disk searches, up to `43x` less heap):** Uses `BS.hGet` for files `< 64 KiB` (avoiding multi-core kernel VM-lock contention and TLB shootdowns on small headers) and deterministic `mmapWithFilePtr` for files `>= 64 KiB`, running SIMD `c_memchr` / 64-bit bloom-mask `RawPrefilter` directly on raw bytes *before* UTF-8 decoding so non-matching files allocate zero `Text` buffers and matching large files copy only once (`mmap -> Text`).

### 🛡️ Language Lexer, Encoding Resilience & Test-Filtering Upgrades

- **Resilient Non-UTF-8 Source & Path Decoding:** File loading (`getTargetContents`, `--file`, `--json`) now uses `decodeFS` + `decodeUtf8Lenient`, smoothly handling C/C++/kernel/driver trees containing legacy Latin-1 (`©`, `µ`), Windows-1252, or GBK/Shift-JIS bytes without aborting files or misaligning UTF-8 byte offsets.
- **Deep Overhaul of `--tests-only` (`-T`) / `--no-tests` Across 30 Languages:** Audited and upgraded all 28 semantic test parser modules with path-level test-file recognition (`isTestFile` for Cargo `tests/*.rs`, Maven/Gradle/SBT `src/test/...` & `src/it/...`, `conftest.py`, `__tests__/`, `*.test.*`/`*.spec.*`, `*_test.*`, `runtests.jl`, `.bats`, SwiftPM `Tests/`), expanded framework support, strict unboxed `LineState` tracking (`1.4x–3.4x` faster `filterTests`), and hardened false-positive guards.
- **Structural Lexer & `FileTypeMap` Fixes:** Added JS/TS single-quoted strings `'...'` and `.jsx`/`.mjs`/`.cjs`/`.mts`/`.cts` extensions, PHP 8 `#[...]` attribute vs `#` comment disambiguation, Scala `'symbol` vs char literal disambiguation and odd-quote `"""` closing quotes, Python backslash-guarded raw strings, OCaml `{|...|}` raw strings, Zig `\\` multiline strings, Elixir `"""`/`'''` heredocs, R `r"(...)"` raw strings, and D `/+ ... +/` block comments.

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
cgrep 10.0.0 - Usage: cgrep [OPTION] [PATTERN] files...

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

Version 10.0 provides intelligent test code filtering across 30 programming languages. When using `--tests-only` (`-T`) or `--no-tests`, cgrep combines file-layout conventions (`isTestFile`) with token-level AST/block recognition to isolate or exclude test code accurately:

| Language | Testing Frameworks Detected | Detection Patterns |
|----------|----------------------------|-------------------|
| **Rust** | Built-in, cargo test, tokio, rstest, proptest, quickcheck, criterion | `#[test]`, `#[tokio::test]`, `#[rstest]`, `#[test_case]`, `#[bench]`, `#[wasm_bindgen_test]`, `#[cfg(test)]`, `proptest!`, `quickcheck!`, `tests/*.rs` |
| **Go** | Built-in testing | `func Test*`, `func Benchmark*`, `func Example*`, `func Fuzz*`, `func TestMain`, `t.Run()`, `b.Run()`, `*testing.T`/`B`/`F`/`M` helpers, `*_test.go` |
| **Java** | JUnit 4/5, TestNG | `@Test`, `@ParameterizedTest`, `@RepeatedTest`, `@TestFactory`, `@TestTemplate`, `@Before*`, `@After*`, `@Nested`, `@DataProvider`, `src/test/java/...` |
| **Kotlin** | JUnit 4/5, TestNG, Kotest, Spek | `@Test`, `@ParameterizedTest`, `@Before*`, `@After*`, `@Rule`, Kotest `FunSpec`/`StringSpec`/`BehaviorSpec`/`DescribeSpec`/`ShouldSpec`/`WordSpec`/`FreeSpec`/`FeatureSpec`/`ExpectSpec`, `src/*Test/...` |
| **C** | Google Test, Catch2, Doctest, Criterion, Unity/Check, CMocka | `TEST()`, `TEST_F()`, `TEST_CASE()`, `SCENARIO()`, `BENCHMARK()`, `SUBCASE()`, `START_TEST()`, `Test()`, `cmocka_unit_test*`, `test_*` functions |
| **C++** | Google Test, Catch2, Doctest, Boost.Test | `TEST()`, `TEST_F()`, `TEST_P()`, `TYPED_TEST*()`, `INSTANTIATE_TEST_SUITE_P()`, `TEST_CASE*()`, `SCENARIO*()`, `TEST_SUITE*()`, `BENCHMARK*()`, `BOOST_*_TEST_CASE*()` |
| **Python** | pytest, unittest, hypothesis | `def test_*`, `async def test_*`, `class Test*`, `*TestCase` subclasses, `@pytest.*`, `@fixture`, `@parametrize`, `@given`, `@unittest.*`, `test_*.py`, `*_test.py`, `conftest.py` |
| **Zig** | Built-in | `test "..." { ... }`, `test { ... }`, `test path.Ident { ... }`, `test @"..." { ... }`, `if (builtin.is_test) { ... }`, `*_test.zig` |
| **JavaScript** | Jest, Mocha, Jasmine, Vitest, Playwright, Deno, QUnit | `describe()`, `it()`, `test()`, `suite()`, `.only`/`.skip`/`.each`/`.concurrent`/`.step` chains, `Deno.test()`, `QUnit.test()`, `*.test.*`, `*.spec.*`, `__tests__/` |
| **TypeScript** | Jest, Mocha, Jasmine, Vitest, Playwright, Deno, QUnit | `describe()`, `it()`, `test()`, `suite()`, `.only`/`.skip`/`.each`/`.concurrent`/`.step` chains, `Deno.test()`, `QUnit.test()`, `*.test.ts*`, `*.spec.ts*`, `__tests__/` |
| **Scala** | ScalaTest, MUnit, Specs2, ZIO Test, ScalaCheck, uTest | `*Suite`, `*Spec`, `ZIOSpecDefault`, `Specification`, `Properties`, `@Test`, `Tests { ... }`, `test()`, `describe()`, `suite()`, `property() =`, `src/test/scala/...`, `src/it/scala/...` |
| **Haskell** | Hspec, Tasty, QuickCheck, Hedgehog, HUnit | `Spec` / `TestTree` bindings, `describe`, `it`, `specify`, `context`, `testCase*`, `testGroup`, `testProperty`, `prop_*` signatures & bindings, `test/` & `*Spec.hs` |
| **C#** | NUnit, xUnit, MSTest | `[Test]`, `[Fact]`, `[Theory]`, `[TestCase*]` (including generic `[TestCase<T>]`), `[InlineData]`, `[MemberData]`, `[ClassData]`, `[TestClass]`, `[TestMethod]`, `[DataTestMethod]`, `[TestFixture]`, `[SetUp]`, `[TearDown]`, `[TestInitialize]`, `[TestCleanup]` |
| **F#** | NUnit, xUnit, MSTest, Expecto, FsCheck | `[<Test>]`, `[<Fact>]`, `[<Theory>]`, `[<TestCase>]`, `[<Property>]`, `[<Tests>]`, Expecto `test`, `testCase*`, `testList`, `testAsync`, `testTask`, `testTheory*`, `testProperty*`, `ftest*`, `ptest*` |
| **Dart** | Built-in `test`, `flutter_test` | `test()`, `group()`, `testWidgets()`, `setUp()`, `tearDown()`, `setUpAll()`, `tearDownAll()`, `*_test.dart` |
| **Elixir** | ExUnit, StreamData | `use ExUnit.Case`, `*Test` modules, `test`, `describe`, `property`, `setup`, `setup_all`, `doctest`, `@tag`/`@describetag`/`@moduletag`, `*_test.exs` |
| **Ruby** | RSpec, Minitest, Test::Unit, ActiveSupport | `RSpec.describe`, `describe`, `context`, `it`, `specify`, `example`, `before`, `after`, `around`, `let`, `subject`, `shared_examples`, `def test_*`, `Minitest::Test` / `TestCase` subclasses, `*_spec.rb`, `*_test.rb` |
| **PHP** | PHPUnit, Pest, Codeception | `#[Test]`, `#[DataProvider]`, `#[Before]`, `#[After]`, `@test`, `extends TestCase`, `function test*()`, `setUp()`, `tearDown()`, Pest `test()`, `it()`, `describe()`, `*Test.php` |
| **Swift** | XCTest, Swift Testing, Quick/Nimble | `@Test`, `@Suite` (`struct`/`class`/`actor`), `XCTestCase` subclasses, `func test*()`, `setUp*()`, `tearDown*()`, `describe()`, `context()`, `it()`, `Tests/...` |
| **Objective-C** | XCTest, Cedar/Kiwi | `@interface ... : XCTestCase` + `@implementation` tracking, `- (void)test*`, `setUp*`, `tearDown*`, `*Tests.m` |
| **R** | testthat, tinytest, RUnit | `test_that()`, `describe()`, `it()`, `test.* <- function()`, `tests/testthat/test-*.R` |
| **Julia** | Built-in `Test`, TestItems | `@testset`, `@test`, `@test_*`, `@inferred`, `@testitem`, `@testmodule`, `@testsnippet`, `runtests.jl` |
| **Perl** | Test::More, Test::Class, Test::Spec, Test2 | `subtest`, `sub test_*`, `: Test(...)` / `: Tests(...)` attributes, `describe`, `context`, `it`, `they`, `*.t` |
| **OCaml** | OUnit2, Alcotest, Jane Street PPX, QCheck | `let%test`, `let%test_unit`, `let%test_module`, `let%expect_test`, `test_case`, `>::`, `>:::`, `*_test.ml` |
| **Erlang** | EUnit, Common Test | `*_test()`, `*_test_()`, `all()`, `groups()`, `init_per_suite()`, `end_per_suite()`, `init_per_group()`, `end_per_group()`, `init_per_testcase()`, `end_per_testcase()`, `*_SUITE.erl`, `*_tests.erl` |
| **Nim** | `std/unittest`, Testament | `suite`, `test`, `setup:`, `teardown:`, `when defined(test\|testing\|unittest):`, `tests/*.nim`, `test_*.nim` |
| **Clojure** | `clojure.test`, Expectations, Midje | `(deftest ...)`, `(deftest- ...)`, `(testing ...)`, `(defexpect ...)`, `(facts ...)`, `(fact ...)`, `^:test` / `{:test ...}` metadata, `*_test.clj*` |
| **D** | Built-in `unittest`, `d-unit`, `unit-threaded` | `unittest { ... }`, `version(unittest)` (`{...}`, `:`, single-decl), `@Test` / `@UnitTest` UDAs, `mixin UnitTest;` |
| **Lua** | Busted, Lust, LuaUnit | `describe()`, `insulate()`, `expose()`, `context()`, `it()`, `spec()`, `test()`, `pending()`, `setup()`, `teardown()`, `before_each()`, `after_each()`, `function test*()`, `*_spec.lua` |
| **Elm** | `elm-test` | `Test` bindings, `describe`, `test`, `fuzz*`, `todo`, `tests/*.elm` |
| **Bash** | BATS, shUnit2, bashunit | `@test "..." { ... }`, `test_*()`, `test-*()`, `pkg::test_*()`, `function test_*`, `oneTimeSetUp()`, `setUp()`, `tearDown()`, `set_up()`, `tear_down()`, `setup_file()`, `*.bats` |

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

## Benchmarks (v10 vs v9.3)

Measured with `criterion` (`stack bench`, source in `bench/`) on a 31-language open-source corpus (**2.74 MB / 80,849 lines** across C, C++, Rust, Go, Python, JS/TS, Java, Kotlin, Scala, Haskell, OCaml, F#, C#, Swift, Objective-C, PHP, Ruby, Perl, Elixir, Erlang, Clojure, Lua, Dart, Julia, R, Nim, D, Zig, and Bash):

### Core Engine & Search Strategies (In-Memory Corpus Pass)

| Stage / Search Strategy | v9.3 (Time) | v10.0 (Time) | Speedup | v9.3 (Heap Alloc) | v10.0 (Heap Alloc) | Alloc Reduction |
|---|---:|---:|---:|---:|---:|---:|
| **Edit-distance / Fuzzy (`-e`)** | 937.76 ms | **24.06 ms** | **39.0x** | 4.74 GiB | **54.50 MiB** | **89.0x** |
| **ContextFilter (`-c` / `-m` / `-S`)** | 54.17–58.00 ms | **7.31–8.00 ms** | **7.2x–7.9x** | 369.35 MiB | **4.57 MiB** | **80.8x** |
| **Literal case-insensitive (`-i`)** | 47.75 ms | **6.83 ms** | **7.0x** | 22.98 MiB | **5.95 MiB** | **3.9x** |
| **POSIX Regex (`-G`)** | 106.08 ms | **17.72 ms** | **6.0x** | 555.58 MiB | **75.45 MiB** | **7.4x** |
| **Literal code-only (`-c`)** | 66.72 ms | **12.20 ms** | **5.5x** | 383.67 MiB | **6.79 MiB** | **56.5x** |
| **Semantic case-insensitive (`-S -i`)** | 153.40 ms | **28.40 ms** | **5.4x** | 594.57 MiB | **50.16 MiB** | **11.9x** |
| **Tokenizer (`--identifier` / `--keyword`)** | 101.22–107.43 ms | **18.85–19.25 ms** | **5.4x–5.6x** | 510–521 MiB | **8.86–14.26 MiB** | **35.8x–58.8x** |
| **Semantic (`-S`, `--strict`, `OR`/`NOT`/`...`)** | 119.54–143.71 ms | **26.27–29.10 ms** | **4.5x–4.9x** | 557–597 MiB | **35.9–50.0 MiB** | **11.2x–15.5x** |
| **Semantic test-filtered (`--no-tests` / `-T`)** | 121.03–157.70 ms | **34.43–40.40 ms** | **3.5x–3.9x** | 563–614 MiB | **59.8–91.5 MiB** | **6.7x–9.4x** |
| **Token & Chunk Lexers (`parseTokens` / `parseChunks`)** | 39.07–45.02 ms | **12.76–13.55 ms** | **2.9x–3.5x** | 125–146 MiB | **29.3–37.8 MiB** | **3.3x–4.8x** |
| **Literal word-match (`-w`)** | 13.22 ms | **4.10 ms** | **3.2x** | 37.60 MiB | **4.81 MiB** | **7.8x** |
| **LineIndex (`buildIndex`)** | 4.49 ms | **1.42 ms** | **3.2x** | 15.46 MiB | **1.15 MiB** | **13.5x** |
| **Literal exact** | 6.45 ms | **3.27 ms** | **2.0x** | 17.28 MiB | **2.89 MiB** | **6.0x** |

### End-to-End Disk I/O Pipeline (Adaptive `mmap` + Pre-Decode `RawPrefilter`)

| Disk-to-Match Workload (31 files / 2.74 MB) | `readFile` Baseline | v10.0 (`mmap` + `RawPrefilter`) | Speedup | Heap Alloc Reduction |
|---|---:|---:|---:|---:|
| **Semantic miss (`-S "absent(...)"`)** | 23.53 ms | **1.12 ms** | **21.0x** | 41.56 MiB -> **0.96 MiB** (**43.2x**) |
| **Semantic selective hit (`-S "ZSTD_DCtx * ANY"`)** | 23.86 ms | **1.75 ms** | **13.6x** | 41.66 MiB -> **2.52 MiB** (**16.5x**) |
| **Semantic `-i` selective hit** | 26.37 ms | **2.89 ms** | **9.1x** | 41.69 MiB -> **2.53 MiB** (**16.5x**) |
| **Boyer-Moore `-i` selective hit** | 4.88 ms | **2.06 ms** | **2.4x** | 8.66 MiB -> **1.17 MiB** (**7.4x**) |
| **POSIX Regex `-G` selective hit** | 2.83 ms | **1.24 ms** | **2.3x** | 8.16 MiB -> **3.01 MiB** (**2.7x**) |
| **Boyer-Moore selective hit** | 1.36 ms | **0.85 ms** | **1.6x** | 6.05 MiB -> **1.07 MiB** (**5.7x**) |

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
