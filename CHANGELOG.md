## Changelog

# cgrep v10.0.0

- **Major Multi-Stage Performance Overhaul (2x–39x Faster, up to 89x Lower Memory Allocation):** Re-engineered every stage of the search and I/O pipeline, verified by differential QuickCheck properties and measured with a new `criterion` benchmark suite (`bench/`, 31 real-world open-source files, 2.74 MB / 80,849 lines):
  - **ContextFilter (`7.0x–7.7x` faster, `79x` less heap):** Replaced character-by-character `Text.unfoldr` (`369 MiB` allocated per corpus pass) with an in-place `ST` `MArray` byte engine preserving UTF-8 byte lengths, 128-bit ASCII delimiter bitmasks, SIMD `memchr` comment/raw-string scanning, and CAF-memoized `ParConfig` tables.
  - **Token & Chunk Lexers (`2.9x–3.4x` faster stage, `5.3x–5.4x` faster end-to-end Tokenizer, up to `58x` less heap):** Replaced `STRef` + `Data.Sequence` lexers with unboxed mutually recursive byte-offset loops over `Data.Text.Array`, SIMD `memchr` string literal scanning, early `TokenFilter` pruning, zero-cost `coerce` conversions, and $O(1)$ character predicates.
  - **LineIndex, Boyer-Moore & Case-Insensitive Search (`7.0x` faster `-i`, `5.5x` faster `-c`, `3.2x` faster `-w`, `3.1x` faster `LineIndex`):** Built `LineIndex` directly into unboxed `Vector Int` via SIMD `memchr`, replaced `Data.Text.Internal.Search.indices` skip-table allocations with register-resident 64-bit bloom-mask + `memchr` scanning, added ASCII byte-level + zero-copy lowercase `ignoreCase`, and backwards `sliceToLastMatch`.
  - **Edit-Distance / Fuzzy Search (`39.0x` faster: `938 ms -> 24.1 ms`, `89x` less heap: `4.74 GiB -> 54.5 MiB`):** Added precompiled `EditPattern` with $O(1)$ exact length-window pruning and zero-allocation 64-bit Myers bit-parallel DP in CPU registers (with early row exit) for ASCII tokens, plus an unboxed single-row `ST` DP fallback for Unicode/long patterns.
  - **Semantic, Combinator & POSIX Regex Engines (`4.5x–5.4x` faster `-S` with `11x–15.5x` less heap; `5.8x` faster `-G` with `7.4x` less heap):** Added `CompiledAtom` / `SliceMatcher` compilation with specialized pure loops and zero-allocation ASCII CI/infix comparators, high-selectivity mandatory semantic anchor selection (`extractAtomAnchors`), and a sound POSIX ERE DNF required-literal line pre-filter (`extractPosixLinePrefilter`) running `TDFA` only on candidate lines.
  - **Zero-Stat C Directory Scanner, Work-Stealing & Pre-Decode SIMD I/O (`1.6x–21x` faster disk searches, `~10x` lower RSS):** Added POSIX C `readdir` `d_type` directory scanning (`cbits/cgrep_io.c`, toggleable at compile time via the `enable_cbits` Cabal flag / `-f-enable_cbits` for a 100% pure-Haskell build) with parallel work-stealing across capabilities, single-lookup `open` + `fstat` into reusable per-worker `64 KiB` scratch buffers (`mmap` `>= 64 KiB`), NEON/SIMD C raw prefiltering (`0` GHC heap bytes on non-matching files), `LineIndex`-free forward match resolution (`mkMatchesDirect`), zero-copy ASCII case-insensitive scanning (`indicesFastAsciiCI`), once-per-search `TDFA.Regex` compilation, single-pass `ByteString.Builder` output, and L2-cache-tuned GHC RTS (`-A8m -n2m -qg`).
- **Resilient Non-UTF-8 File & Path Decoding:** Switched file reading (`getTargetContents`, `--file`, `--json`) to `decodeFS` + `decodeUtf8Lenient`, cleanly handling legacy Latin-1, Windows-1252, and GBK/Shift-JIS bytes in comments and strings without aborting files.
- **Deep Audit & Upgrade of Semantic Test Filtering (`--tests-only` / `--no-tests`) Across 30 Languages:** Overhauled all 28 `CGrep.Semantic.Tests.*` modules with test-file path detection (`isTestFile`), broader framework coverage (Rust `proptest!`/`quickcheck!`/`#[bench]`/`#[wasm_bindgen_test]`, Catch2 `BENCHMARK`, Criterion, Playwright, QUnit, Expecto offside blocks, Clojure `:test` metadata, Julia `@testitem`, D `@Test` UDAs, Nim `when defined(test):`, etc.), deduplicated strict unboxed `LineState` (`1.4x–3.4x` faster `filterTests`), and comprehensive false-positive guards.
- **Structural Lexer, Concurrency & `FileTypeMap` Improvements:** Added JS/TS single-quoted strings and `.jsx`/`.mjs`/`.cjs`/`.mts`/`.cts` extensions, PHP 8 `#[...]` attribute vs `#` comment disambiguation, Scala `'symbol` vs char literal disambiguation and odd-quote `"""` closing support, Python backslash-guarded raw strings, OCaml `{|...|}` raw strings, Zig `\\` line strings, Elixir `"""`/`'''` heredocs, R `r"(...)"` raw strings, D `/+ ... +/` comments, Unicode word-boundary chunking (`-w`/`-p`/`-s`/`-e`) on plain-text/config formats (`Text`, `Yaml`, `Json`, `Conf`, `Ini`, `Assembly`, `Latex`), and lock-serialized strict UTF-8 `stdout` batch writes eliminating multi-threaded line tearing.

# cgrep v9.3.3
- **Only-Matching Output (`-o` / `--only-matching`):** New option that prints just the matched tokens (separated by a space) instead of the whole line. Combine with `-h` to get the bare match.

# cgrep v9.3.1

- **New Semantic Wildcards (`TYP`, `OP`, `LIT`):** Extended semantic search (`-S` / `--semantic`) with `TYP` (native types), `OP` (operators), and `LIT` (any string or numeric literal, distinct from `STR` which matches only string literals).
- **Structural Ellipsis Operator (`...`):** Added the `...` operator to semantic patterns to match zero or more tokens across expressions or argument lists while respecting balanced brackets (`()`, `[]`, `{}`) and statement terminators (`;`).
- **Semantic Combinators (`NOT`, `OR`):** Added prefix negation (`NOT <atom>`) and infix alternation (`<atom1> OR <atom2>`) to semantic search patterns (with `_NOT` and `_OR` escaping for literal identifiers).
- **Placeholder & Identifier Unescaping Fixes:** Fixed numbered placeholder recognition (`_`, `_0`, `_1`, ...) across all languages without relying on `$` in identifier character sets, and preserved leading underscores on regular identifiers (e.g. `_foo`, `__init__`) unless escaping a reserved semantic wildcard or placeholder.

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
