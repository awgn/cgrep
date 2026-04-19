---
layout: default
---
# CGrep: Context-aware grep for source codes

[![Hackage](https://img.shields.io/hackage/v/cgrep.svg?style=flat)](https://hackage.haskell.org/package/cgrep)
[![Join the chat at https://gitter.im/awgn/cgrep](https://badges.gitter.im/awgn/cgrep.svg)](https://gitter.im/awgn/cgrep?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

**Version 9.2.1** - A powerful, context-aware search tool designed specifically for source code.

> CGrep extends the capabilities of traditional grep by understanding the structure and semantics of source code across multiple programming languages. It allows developers to search within specific contexts like code, comments, or string literals, and provides advanced pattern matching with semantic awareness.

---

## 🚀 What's New in Version 9.2

- **Major Performance Improvements:** 75% faster plain token search and 39% faster semantic search!
- **Semantic Test Filtering:** The `--tests-only` (`-T`) and `--no-tests` flags allow you to intelligently filter out test code from search results across 28+ programming languages and their respective testing frameworks.
- **Full UTF-8 Support:** Native support for UTF-8 character encodings with accurate column positioning.

---

## ✨ Features

* **Context-aware distinction between code, comments, and literals**: Search in the parts of the code you actually care about.
* **Semantic Test Filtering**: Automatically detect and filter test code across 28+ programming languages (Rust, Go, Java, C/C++, Python, JavaScript/TypeScript, Haskell, and many more).
* **Semantic searches** using wildcards and combinators: `_`, `_1`, `_2...`, `ANY`, `KEY`, `STR`, `LIT`, `NUM`, `HEX`, `OCT`.
* **Fine-grained Multi-threading** with configurable number of threads for optimal performance.
* **Code Tokenizer**: Search patterns in tokens as prefix, infix, or suffix sub-strings, and by using the edit distance (Levenshtein).
* **Multi-line pattern searches** across code blocks.
* **Recursive searches**: Supports language filters, directory pruning, local and global configurations (`cgreprc`).

---

## 📦 Installation

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
# or using stack:
# stack build && stack install
```

---

## 💡 Quick Examples

### Context-Aware Searching
Search for a pattern in source code only (excluding comments and strings):
```bash
cgrep -c "malloc" *.c
```

Search only in comments:
```bash
cgrep -m "TODO" -r src/
```

### Semantic Search
Search using semantic wildcards to find variable assignments with numeric literals:
```bash
cgrep -S "_ = NUM" *.c
```

### Test Filtering (New!)
Search in production code only, excluding all test files:
```bash
cgrep --no-tests "function" -r src/
```

Search only in test files:
```bash
cgrep --tests-only "mock" -r tests/
```

### File Type Filters
Search recursively in C++ files:
```bash
cgrep --type=Cpp -r "vector" src/
```

---

## 🧪 Test Framework Support

Version 9 introduces intelligent test code filtering across 28+ programming languages. When using the `-T` flag, cgrep can automatically detect and filter test code based on language-specific conventions and testing frameworks.

| Language | Testing Frameworks Detected | Detection Patterns |
|----------|----------------------------|-------------------|
| **Rust** | Built-in, cargo test | `#[test]`, `#[rstest]`, `#[test_case]` modules |
| **Go** | Built-in testing | `func Test*`, `func Benchmark*`, `func Example*`, `func Fuzz*` |
| **Java / Kotlin** | JUnit, TestNG | `@Test`, `@ParameterizedTest`, `@BeforeEach`, `@AfterEach` annotations |
| **C / C++** | Google Test, Catch2 | `TEST()`, `TEST_F()`, `TEST_CASE()`, `SECTION()` |
| **Python** | pytest, unittest | `test_*` functions, `Test*` classes, `@pytest` |
| **JS / TS** | Mocha, Jasmine, Jest | `describe()`, `it()`, `test()` |
| **Haskell** | HSpec, Tasty, QuickCheck | `describe`, `it`, `prop_*` functions |

*(And many more, including C#, F#, Dart, Elixir, Ruby, PHP, Swift, R, Julia, OCaml...)*

---

## 🌐 Supported Languages

CGrep supports a wide range of programming languages and file formats:

**Programming Languages:** C, C++, C#, Java, Kotlin, Scala, JavaScript, TypeScript, CoffeeScript, Python, Ruby, Perl, PHP, Go, Rust, Haskell, OCaml, F#, Erlang, Elixir, Clojure, Lisp, Scheme, Lua, R, Julia, Dart, Nim, Zig, D, Swift, Objective-C, Chapel, Awk, Shell scripts (Bash, Fish)

**Markup & Config:** HTML, XML, LaTeX, Markdown, YAML, JSON, TOML, INI, Dhall, CMake, Makefile, Cabal

---

## ⚙️ Configuration

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

## 🔗 Links

- **GitHub Repository:** [https://github.com/awgn/cgrep](https://github.com/awgn/cgrep)
- **Hackage:** [https://hackage.haskell.org/package/cgrep](https://hackage.haskell.org/package/cgrep)
- **Author:** [Nicola Bonelli](https://twitter.com/nicolabonelli)