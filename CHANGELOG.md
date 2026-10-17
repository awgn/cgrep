## Changelog

# cgrep v9.3.0

- **Modular Semantic Test Filter Architecture:** Refactored the semantic test filter (`--tests-only` / `--no-tests`) from a single monolithic file into a typeclass-based (`LanguageTestFilter (lang :: FileType)`) modular architecture with dedicated per-language modules under `CGrep.Semantic.Tests.*`.
- **New Test Filter Languages (Lua, Elm, Bash):** Added semantic test filtering for **Lua** (Busted, Lust, LuaUnit), **Elm** (`elm-test`), and **Bash** (BATS, shUnit2, bashunit), bringing total test-filtering coverage to 29 languages.
- **Expanded Framework Support & False-Positive Hardening:** Hardened test block boundaries, nested block balancing, and false-positive guards across all 29 supported languages:
  - **Rust:** Support qualified attributes (`#[tokio::test]`, `#[rstest::rstest]`), `#[cfg(test)]` modules/items (excluding `#[cfg(not(test))]`), and intervening outer attributes.
  - **Go:** Support `*testing.T`, `*testing.B`, `*testing.F`, `*testing.M`, `Example*` functions, `t.Run`/`b.Run` subtests, and `*_test.go` file-level filtering.
  - **Java & Kotlin:** Support JUnit 4/5, TestNG, and Kotest DSL (`FunSpec`, `StringSpec`, `BehaviorSpec`, `DescribeSpec`, `ShouldSpec`, `WordSpec`, `FreeSpec`, `FeatureSpec`, `ExpectSpec`, `AnnotationSpec`), while preventing false positives on domain annotations (`@Testimonial`) and abstract/interface declarations.
  - **C / C++:** Support GoogleTest, Catch2, Doctest (`TEST_SUITE`, `SUBCASE`), and Boost.Test, while ignoring forward declarations.
  - **Python:** Support stacked decorators above `def`/`async def`/`class` and `unittest.TestCase` subclasses, avoiding false positives on functions like `testify`.
  - **Zig:** Support named `test "..."`, unnamed `test { ... }`, and decltest `test Identifier { ... }` blocks.
  - **JavaScript / TypeScript:** Support `.only`/`.skip`/`.each(...)`/`.concurrent` chains and `Deno.test(...)`, while guarding against `regex.test(...)` calls.
  - **Scala:** Support ScalaTest, MUnit, Specs2, and ZIO Test (`suite`, `test`), avoiding false positives on `.in(...)` method calls.
  - **Haskell:** Track bracket depth for inline type annotations `(1 :: Int)` and prevent RHS `prop_*` references from triggering test blocks.
  - **C# & F#:** Support multi-attribute lists `[Test, Category("...")]`, generic attributes, and Expecto/NUnit/xUnit/MSTest patterns with indentation and bracket tracking.
  - **Dart, Elixir, Ruby, PHP:** Require callback arguments in Dart test builders; support Elixir `, do:` keyword tests and `fn ... end` balancing; handle Ruby endless methods (`def foo = bar`) and modifier conditionals; support PHP 8 `#[Test]`/`#[DataProvider]` attributes and Pest (`test`, `it`, `describe`).
  - **Swift & Objective-C:** Support Swift Testing `@Test`/`@Suite` macros (in both default and `--strict` modes), Quick/Nimble, and Objective-C XCTest methods (excluding parameterized methods).
  - **R, Julia, Perl, OCaml, Erlang, Nim, Clojure, D:** Add support for `RUnit`, Julia `let`/`quote` block balancing and `[1:end]` indexing guards, Perl `Test::Class` and `Test::Spec`, OCaml nested `let ... in` and `struct`/`sig` blocks, Erlang Common Test and `#record.field` dot guards, Nim anonymous `proc` blocks, Clojure namespaced `deftest`/`testing` forms and Midje `facts`, and D `version(unittest)` / attributed `unittest` blocks.

# cgrep v9.2.3

- **Column Calculation Fix:** Fixed incorrect column numbers reported in search matches (PR #53).

# cgrep v9.2.2

- **CLI Options Validation:** Added validation for mutually exclusive `--tests-only` and `--no-tests` options.

# cgrep v9.2.1

- **Test Filter CLI Flags:** Replaced `-T True` / `-T False` with standard `-T` / `--tests-only` and `--no-tests` flags.

# cgrep v9.2.0

- **Extended Semantic Test Filtering:** Improved test detection across C, C++, Go, Rust, Java, Kotlin, Python, Zig, JavaScript, TypeScript (`.ts`/`.tsx`), Scala, Haskell, C#, F#, Dart, Elixir, Ruby, PHP, Swift, Objective-C, R, Julia, Perl (`.t`), OCaml, Erlang, Nim, Clojure, and D.
- **Protocol Buffers Support:** Added `ProtoBuf` file type support.

# cgrep v9.0.0

- **Full UTF-8 Text Architecture:** Migrated core search and tokenization pipeline from `ByteString` to `Text` with UTF-8 byte offset indexing, achieving +75% faster plain token search and +39% faster semantic search.
- **Initial Semantic Test Filtering:** Introduced language-aware test code filtering across 26 programming languages.
- **Compile-Time Language Map & CLI Improvements:** Populated `FileTypeInfoMap` at compile time via Template Haskell, switched CLI parsing to `optparse-applicative`, and added `--strict` semantic operator mode and `--stats` search timing.

# cgrep v8.2.0

- **Header and Code Module Filters:** Added `--hdr-only` and `--code-only` flags to filter header/interface files vs implementation modules.
- **Parallel Search Improvements:** Improved worker job assignment and parallelism.

# cgrep v8.0.0

- **Performance Enhancements:** In this release, significant improvements have been made to enhance the performance of cgrep. The performance is now more on par with ripgrep, with search speeds ranging from 3 times to 13 times faster than the previous release.

- **Expanded Language and Configuration Support:** We have expanded the language and configuration support in cgrep. Starting from this version, it includes support for searching within Zig, Union, Dhall, Fish shell, Toml, and Ini config files. This means you can now easily search for patterns and text within files of these formats.

- **New Semantic Token Filter:** A new semantic token filter has been introduced in this release. This filter is designed to identify and filter out specific native types present in different file types. This enhances the search results by providing more precise and relevant matches based on the semantic meaning of the tokens.

- **Kind Filter Selector:** We have introduced a new feature called the Kind Filter Selector. With this filter selector, users can now specify the kind of files they want to search within. The available options for the filter include Text, Config, Language, Data, Markup, and Script. This allows for more focused and targeted searches based on the desired file type, especially useful in large codebases.
