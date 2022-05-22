--
-- Copyright (c) 2013-2022 Nicola Bonelli <nicola@pfq.io>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module CGrep.LanguagesMap where

import CGrep.Language ( Language(..), FileType(..) )
import CGrep.ContextFilter
    ( ParConfig(ParConfig),
      ContextFilter(ContextFilter),
      FilterFunction,
      runContextFilter, mkParConfig)

import CGrep.Types ( Text8 )
import qualified Data.Map as Map
import System.FilePath ( takeExtension, takeFileName )
import Control.Monad ( forM_ )
import Data.Maybe ( fromJust, isJust )
import Control.Applicative ( Alternative((<|>)) )
import Options ( Options(Options, language_force) )
import qualified Data.ByteString.Char8 as C

import qualified Data.Array.BitArray as BA

import qualified Data.Set as S
import Data.List (findIndex)

import CGrep.Boundary ( Boundary(Boundary) )
import CGrep.Parser.Char

type LanguagesMapType = Map.Map Language LanguageInfo
type FileMapType = Map.Map FileType Language


type CharIdentf = (Char -> Bool)


data LanguageInfo = LanguageInfo {
    langExtensions                 :: [FileType]
,   langChar                       :: [Boundary]
,   langString                     :: [Boundary]
,   langRawString                  :: [Boundary]
,   langComment                    :: [Boundary]
,   langValidIdentifierChars       :: (CharIdentf, CharIdentf)
,   langResKeywords                :: S.Set C.ByteString
}


languagesMap :: LanguagesMapType
languagesMap = Map.fromList
    [
        (Agda,      LanguageInfo {
        langExtensions = [Ext "agda", Ext "lagda"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString= ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langComment  = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "abstract", "codata", "constructor", "data", "eta-equality", "field",
            "forall", "hiding", "import", "in", "inductive", "infix", "infixl",
            "infixr", "instance", "let", "module", "mutual", "no-eta-equality", "open",
            "pattern", "postulate", "primitive", "private", "public", "quoteContext", "quoteGoal",
            "record", "renaming", "rewrite", "Set", "syntax", "tactic", "using", "where", "with"
        ]
    })
   ,  (Assembly,  LanguageInfo {
       langExtensions = [Ext "s", Ext "S"]
   ,   langChar = []
   ,   langString= ["\"" ~~ "\""]
   ,   langRawString = []
   ,   langComment = ["#" ~~ "\n", ";" ~~ "\n", "|" ~~ "\n", "!" ~~ "\n", "/*" ~~ "*/"]
   ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
   ,   langResKeywords = keywords []
   })
    ,  (Awk,       LanguageInfo {
        langExtensions = [Ext "awk", Ext "mawk", Ext "gawk"]
    ,   langChar = []
    ,   langString= ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langComment = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "BEGIN", "END", "if", "else", "while", "do", "for", "in", "break", "continue",
                "delete", "next", "nextfile", "function", "func", "exit"]
    })
    ,    (C,         LanguageInfo {
        langExtensions = [Ext "c", Ext "C", Ext "inc"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["R\"" ~~ "\""]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern",
            "float", "for", "goto", "if", "inline", "int", "long", "register", "restrict", "return", "short",
            "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while",
            "_Alignas", "_Alignof", "_Atomic", "_Bool", "_Complex", "_Decimal128", "_Decimal32", "_Decimal64", "_Generic",
            "_Imaginary", "_Noreturn", "_Static_assert", "_Thread_local", "if", "elif", "else", "endif", "ifdef", "ifndef",
            "define", "undef", "include", "line", "error", "pragma", "defined", "__has_c_attribute", "_Pragma", "asm", "fortran"
        ]
    })
--    ,  (CMake,     LanguageInfo {
--        langExtensions = [Name "CMakeLists.txt", Ext "cmake"]
--    ,   langChar = ["'"]
--    ,   langString= ["\"" ~~ "\""]
--    ,   langRawString = []
--    ,   langComment = ["#" ~~ "\n"]
--    ,   langAdditonalValidIdentifChars = ("", "-_")
--    ,   langResKeywords = keywords []
--    })
--    ,  (Cabal,     LanguageInfo {
--        langExtensions = [Ext "cabal"]
--    ,   langChar = []
--    ,   langString= ["\"" ~~ "\""]
--    ,   langRawString = []
--    ,   langComment = mkFilter ["--" ~~ "\n"]
--    ,   langAdditonalValidIdentifChars = ("", "-")
--    ,   langResKeywords = keywords []
--    })
--    ,  (Chapel,    LanguageInfo {
--        langExtensions = [Ext "chpl"]
--    ,   langChar = []
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "atomic", "begin", "bool", "break", "by", "class", "cobegin", "coforall", "complex", "config",
--            "const", "continue", "def", "distributed", "do", "domain", "else", "enum", "false", "for",
--            "forall", "goto", "if", "imag", "in", "int", "inout", "let", "locale", "module",
--            "nil", "of", "on", "ordered", "otherwise", "out", "param", "pragma", "range", "real",
--            "record", "reduce", "return", "scan", "select", "serial", "single", "sync", "then", "true",
--            "type", "uint", "union", "use", "var", "when", "where", "while", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_$")
--    })
--    ,  (Clojure,   LanguageInfo {
--        langExtensions = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"]
--    ,   langFilter = mkFilter [";" ~~ "\n"] ["\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "and", "let", "def", "defn", "if", "else", "do", "quote", "var", "fn", "loop", "recur", "throw", "try",
--            "monitor-enter", "monitor-exit"
--        ]
--    ,  langAdditonalValidIdentifChars = ("", "*+!-?<>=_")
--    })
--    ,  (Coffee,    LanguageInfo {
--        langExtensions = [Ext "coffee"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "case", "default", "function", "var", "void", "with", "const", "let", "enum", "export", "import", "native",
--            "__hasProp", "__extends", "__slice", "__bind", "__indexOf", "implements", "interface", "package", "private",
--            "protected", "public", "static", "yield", "true", "false", "null", "this", "new", "delete", "typeof", "in",
--            "arguments", "eval", "instanceof", "return", "throw", "break", "continue", "debugger", "if", "else", "switch",
--            "for", "while", "do", "try", "catch", "finally", "class", "extends", "super", "undefined", "then", "unless",
--            "until", "loop", "of", "by", "when", "and", "or", "is", "isnt", "not", "yes", "no", "on", "off"
--        ]
--    ,   langAdditonalValidIdentifChars = ("$_", "$")
--    })
--    ,  (Conf,      LanguageInfo {
--        langExtensions = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "_")
--    })
    ,  (Cpp,    LanguageInfo {
        langExtensions = [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc", Ext "cp", Ext "c++", Ext "tcc",
                          Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP", Ext "hxx",
                          Ext "hh", Ext "hp", Ext "h++", Ext "cu", Ext "cuh"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "alignas", "alignof", "and", "and_eq", "asm", "atomic_cancel", "atomic_commit", "atomic_noexcept",
            "auto", "bitand", "bitor", "bool", "break", "case", "catch", "char", "char8_t", "char16_t", "char32_t",
            "class", "compl", "concept", "const", "consteval", "constexpr", "constinit", "const_cast", "continue",
            "co_await", "co_return", "co_yield", "decltype", "default", "delete", "do", "double", "dynamic_cast",
            "else", "enum", "explicit", "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline",
            "int", "long", "mutable", "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator",
            "or", "or_eq", "private", "protected", "public", "reflexpr", "register", "reinterpret_cast", "requires",
            "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct", "switch",
            "synchronized", "template", "this", "thread_local", "throw", "true", "try", "typedef", "typeid",
            "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq",
            "final", "override", "transaction_safe", "transaction_safe_dynamic", "import", "module",
            "elif", "endif", "ifdef", "ifndef", "define", "undef", "include", "line", "error", "pragma", "defined",
            "__has_include", "__has_cpp_attribute", "export", "import", "module",
            "_Pragma"
        ]
    })
--    ,  (Csharp,    LanguageInfo {
--        langExtensions = [Ext "cs", Ext "CS"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const",
--            "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern",
--            "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface",
--            "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params",
--            "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc",
--            "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked",
--            "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Css,       LanguageInfo {
--        langExtensions = [Ext "css"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/"] ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("-_", "-_")
--    })
--    ,  (D,         LanguageInfo {
--        langExtensions = [Ext "d", Ext "D"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "abstract", "alias", "align", "asm", "assert", "auto", "body", "bool", "break", "byte", "case", "cast", "catch",
--            "cdouble", "cent", "cfloat", "char", "class", "const", "continue", "creal", "dchar", "debug", "default", "delegate",
--            "delete", "deprecated", "do", "double", "else", "enum", "export", "extern", "false", "final", "finally", "float",
--            "for", "foreach", "foreach_reverse", "function", "goto", "idouble", "if", "ifloat", "immutable", "import", "in",
--            "inout", "int", "interface", "invariant", "ireal", "is", "lazy", "long", "macro", "mixin", "module", "new", "nothrow",
--            "null", "out", "override", "package", "pragma", "private", "protected", "public", "pure", "real", "ref", "return",
--            "scope", "shared", "short", "static", "struct", "super", "switch", "synchronized", "template", "this", "throw", "true",
--            "try", "typeid", "typeof", "ubyte", "ucent", "uint", "ulong", "union", "unittest", "ushort", "version", "void", "wchar",
--            "while", "with", "__FILE__", "__FILE_FULL_PATH__", "__MODULE__", "__LINE__", "__FUNCTION__", "__PRETTY_FUNCTION__",
--            "__gshared", "__traits", "__vector", "__parameters"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Dart,      LanguageInfo {
--        langExtensions = [Ext "dart"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--         "assert", "break", "case", "catch", "class", "const", "continue", "default", "do", "else", "enum", "extends", "false",
--         "final", "finally", "for", "if", "in", "is", "new", "null", "rethrow", "return", "super", "switch", "this", "throw",
--         "true", "try", "var", "void", "while", "with", "async", "hide", "on", "show", "sync", "abstract", "as", "covariant",
--         "deferred", "dynamic", "export", "extension", "external", "factory", "function", "get", "implements", "import", "interface",
--         "library", "mixin", "operator", "part", "set", "static", "typedef",  "await", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Elixir,    LanguageInfo {
--        langExtensions = [Ext "ex", Ext "exs"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--        "true", "false", "nil", "when", "and", "or", "not", "in", "fn", "do", "end", "catch", "rescue", "after", "else"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Elm,       LanguageInfo {
--        langExtensions = [Ext "elm"]
--    ,   langFilter =  mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "type", "alias", "port", "if", "then", "else", "case", "of", "let", "in", "infix", "left", "right", "non",
--            "module", "import", "exposing", "as", "where", "effect", "command", "subscription", "true", "false", "null"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Erlang,    LanguageInfo {
--        langExtensions = [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"]
--    ,   langFilter = mkFilter ["%" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond",
--            "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "receive", "rem", "try", "when", "xor"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Eta,       LanguageInfo {
--        langExtensions = [Ext "eta"]
--    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (Fortran,   LanguageInfo {
--        langExtensions = [Ext "f", Ext "for", Ext "ftn",
--                    Ext "F", Ext "FOR", Ext "FTN", Ext "fpp", Ext "FPP",
--                    Ext "f90", Ext "f95", Ext "f03", Ext "f08",
--                    Ext "F90", Ext "F95", Ext "F03", Ext "F08"]
--    ,   langFilter = mkFilter []  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            -- fortran77
--            "assign", "backspace", "block", "data", "call", "close", "common", "continue", "data", "dimension", "do",
--            "else", "else", "if", "end", "endfile", "endif", "entry", "equivalence", "external", "format", "function",
--            "goto", "if", "implicit", "inquire", "intrinsic", "open", "parameter", "pause", "print", "program", "read",
--            "return", "rewind", "rewrite", "save", "stop", "subroutine", "then", "write",
--            -- fortran 90
--            "allocatable", "allocate", "case", "contains", "cycle", "deallocate", "elsewhere", "exit?", "include",
--            "interface", "intent", "module", "namelist", "nullify", "only", "operator", "optional", "pointer", "private",
--            "procedure", "public", "recursive", "result", "select", "sequence", "target", "use", "while", "where",
--            -- fortran 95
--            "elemental", "forall", "pure",
--            -- fortran 03
--            "abstract", "associate", "asynchronous", "bind", "class", "deferred", "enum", "enumerator", "extends", "final",
--            "flush", "generic", "import", "non_overridable", "nopass", "pass", "protected", "value", "volatile", "wait",
--            -- fortran 08
--            "block", "codimension", "do", "concurrent", "contiguous", "critical", "error", "stop", "submodule", "sync",
--            "all", "sync", "images", "sync",  "memory", "lock", "unlock"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Fsharp,    LanguageInfo {
--        langExtensions = [Ext "fs", Ext "fsx", Ext "fsi"]
--    ,   langFilter =  mkFilter ["(*" ~~ "*)", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "abstract", "and", "as", "assert", "base", "begin", "class", "default", "delegate", "do", "done", "downcast",
--            "downto", "elif", "else", "end", "exception", "extern", "FALSE", "finally", "fixed", "for", "fun", "function",
--            "global", "if", "in", "inherit", "inline", "interface", "internal", "lazy", "let", "let!",  "match", "match!",
--            "member", "module", "mutable", "namespace", "new", "not", "null", "of", "open", "or", "override", "private",
--            "public", "rec", "return", "return!",  "select", "static", "struct", "then", "to", "TRUE", "try", "type",
--            "upcast", "use", "use!",  "val", "void", "when", "while", "with", "yield", "yield!",  "const", "asr", "land",
--            "lor", "lsl", "lsr", "lxor", "mod", "sig", "break", "checked", "component", "const", "constraint", "continue",
--            "event", "external", "include", "mixin", "parallel", "process", "protected", "pure", "sealed", "tailcall", "trait", "virtual"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
    ,  (Go,        LanguageInfo {
        langExtensions = [Ext "go"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["`" ~~ "`"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "break", "default", "func", "interface", "select", "case", "defer", "go", "map",
            "struct", "chan", "else", "goto", "package", "switch", "const", "fallthrough", "if",
            "range", "type", "continue", "for", "import", "return", "var", "append", "bool", "byte",
            "cap", "close", "complex", "complex64", "complex128", "uint16", "copy", "false",
            "float32", "float64", "imag", "int", "int8", "int16", "uint32", "int32", "int64",
            "iota", "len", "make", "new", "nil", "panic", "uint64", "print", "println", "real",
            "recover", "string", "true"," uint", "uint8", "uintptr"
        ]
    })
    ,  (Haskell,   LanguageInfo {
        langExtensions = [Ext "hs", Ext "lhs", Ext "hsc"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = [ "[r|" ~~ "|]", "[q|" ~~ "|]", "[s|" ~~ "|]", "[here|" ~~"|]",  "[i|" ~~ "|]"]
    ,   langComment = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langValidIdentifierChars = (isAlpha_', isAlphaNum_')
    ,   langResKeywords = keywords [
            "as", "case", "class", "data", "default", "deriving", "do", "else", "hiding", "if", "import",
            "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "qualified",
            "then", "type", "where", "forall", "mdo", "family", "role", "pattern", "static", "group",
            "by", "using", "foreign", "export", "label", "dynamic", "safe", "interruptible", "unsafe",
            "stdcall", "ccall", "capi", "prim", "javascript", "rec", "proc"
        ]
    })
--    ,  (Html,      LanguageInfo {
--        langExtensions = [Ext "htm", Ext "html"]
--    ,   langFilter = mkFilter ["<!--" ~~ "-->"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "-_:.")
--    })
--    ,  (Idris,     LanguageInfo {
--        langExtensions = [Ext "idr", Ext "lidr"]
--    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n", "|||" ~~ "\n"] ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("_", "_'")
--    })
--    ,  (Java,      LanguageInfo {
--        langExtensions = [Ext "java"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "continue", "const",
--            "default", "do", "double", "else", "enum", "exports", "extends", "final", "finally", "float", "for", "goto",
--            "if", "implements", "import", "instanceof", "int", "interface", "long", "module", "native", "new", "package",
--            "private", "protected", "public", "requires", "return", "short", "static", "strictfp", "super", "switch",
--            "synchronized", "this", "throw", "throws", "transient", "try", "var", "void", "volatile", "while", "true",
--            "false", "null"
--        ]
--    ,   langAdditonalValidIdentifChars = ("$_", "$_")
--    })
--    ,  (Javascript,LanguageInfo {
--        langExtensions = [Ext "js"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "abstract", "arguments", "await", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
--            "debugger", "default", "delete", "do", "double", "else", "enum", "eval", "export", "extends", "false", "final",
--            "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int", "interface",
--            "let", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static",
--            "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "typeof", "var", "void",
--            "volatile", "while", "with", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("$_", "$_")
--    })
--    ,  (Json,      LanguageInfo {
--        langExtensions = [Ext "json", Ext "ndjson"]
--    ,   langFilter = mkFilter []  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (Julia,      LanguageInfo {
--        langExtensions = [Ext "jl"]
--    ,   langFilter = mkFilter ["#" ~~ "\n", "#-" ~~ "-#"]  ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "baremodule",  "begin",  "break",  "catch",  "const",  "continue",  "do",  "else",  "elseif",  "end",
--            "export",  "false",  "finally",  "for",  "function",  "global",  "if",  "import",  "let",  "local",
--            "macro",  "module",  "quote",  "return",  "struct",  "true",  "try",  "using",  "while"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Kotlin,    LanguageInfo {
--        langExtensions = [Ext "kt", Ext "kts", Ext "ktm"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'", "\"\"\"" ~~ "\"\"\""]
--    ,   langResKeywords = keywords [
--        "as", "break", "class", "continue", "do", "else", "false", "for", "fun", "if", "in", "interface", "is", "null", "object",
--        "package", "return", "super", "this", "throw", "true", "try", "typealias", "typeof", "val", "var", "when", "while",
--        "by", "catch", "constructor", "delegate", "dynamic", "field", "file", "finally", "get", "import", "init",
--        "param", "property", "receiver", "set", "setparam", "value", "where",
--        "abstract", "actual", "annotation", "companion", "const", "crossinline", "data", "enum", "expect", "external",
--        "final", "infix", "inline", "inner", "internal", "lateinit", "noinline", "open", "operator", "out", "override",
--        "private", "protected", "public", "reified", "sealed", "suspend", "tailrec", "vararg",
--        "field", "it"
--        ]
--    ,   langAdditonalValidIdentifChars = ("$_", "$_")
--    })
--    ,  (Latex,     LanguageInfo {
--        langExtensions = [Ext "latex", Ext "tex"]
--    ,   langFilter = mkFilter ["%" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (Lisp,      LanguageInfo {
--        langExtensions = [Ext "lisp", Ext "cl"]
--    ,   langFilter = mkFilter [";" ~~ "\n", "#|" ~~ "|#"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("_", "!$%&*+-./:<=>?@^_~")
--    })
--    ,  (Lua,       LanguageInfo {
--        langExtensions = [Ext "lua"]
--    ,   langFilter = mkFilter ["--[[" ~~ "--]]", "--" ~~ "\n"]
--                              ["'" ~~ "'", "\"" ~~ "\"", "[===[" ~~ "]===]", "[==[" ~~ "]==]", "[=[" ~~ "]=]", "[[" ~~ "]]" ]
--    ,   langResKeywords = keywords [
--            "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if",
--            "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Make,      LanguageInfo {
--        langExtensions = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("-_", "-_")
--    })
--    ,  (Nmap,      LanguageInfo {
--        langExtensions = [Ext "nse"]
--    ,   langFilter = mkFilter ["--" ~~ "\n", "[[" ~~"]]"] ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (Nim,         LanguageInfo {
--        langExtensions = [Ext "nim"]
--    ,   langFilter = mkFilter ["#[" ~~ "#]", "#" ~~ "\n"]  ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "addr", "and", "as", "asm",
--            "bind", "block", "break",
--            "case", "cast", "concept", "const", "continue", "converter",
--            "defer", "discard", "distinct", "div", "do",
--            "elif", "else", "end", "enum", "except", "export",
--            "finally", "for", "from", "func",
--            "if", "import", "in", "include", "interface", "is", "isnot", "iterator",
--            "let", "macro", "method", "mixin", "mod", "nil", "not", "notin",
--            "object", "of", "or", "out", "proc", "ptr",
--            "raise", "ref", "return", "shl", "shr", "static",
--            "template", "try", "tuple", "type",
--            "using", "var", "when", "while", "xor", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (OCaml ,    LanguageInfo {
--        langExtensions = [Ext "ml", Ext "mli"]
--    ,   langFilter = mkFilter ["(*" ~~ "*)"] ["\"" ~~ "\"", "'" ~~ "'", "{id|" ~~ "|id}"]
--    ,   langResKeywords = keywords [
--            "and",         "as",          "assert",      "asr",         "begin",       "class",
--            "constraint",  "do",          "done",        "downto",      "else",        "end",
--            "exception",   "external",    "false",       "for",         "fun",         "function",
--            "functor",     "if",          "in",          "include",     "inherit",     "initializer",
--            "land",        "lazy",        "let",         "lor",         "lsl",         "lsr",
--            "lxor",        "match",       "method",      "mod",         "module",      "mutable",
--            "new",         "nonrec",      "object",      "of",          "open",        "or",
--            "private",     "rec",         "sig",         "struct",      "then",        "to",
--            "true",        "try",         "type",        "val",         "virtual",     "when",
--            "while",       "with"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (ObjectiveC,LanguageInfo {
--        langExtensions = [Ext "m", Ext "mi"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "void", "char", "short", "int", "long", "float", "double", "signed", "unsigned", "id", "const", "volatile", "in",
--            "out", "inout", "bycopy", "byref", "oneway", "self", "super", "interface", "end", "@implementation", "@end",
--            "@interface", "@end", "@implementation", "@end", "@protoco", "@end", "@class"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (PHP,       LanguageInfo {
--        langExtensions = [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n", "#" ~~ "\n" ]  ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class",
--            "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif",
--            "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval",
--            "exit", "extends", "final", "finally", "fn", "for", "foreach", "function", "global", "goto",
--            "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface",
--            "isset", "list", "match", "namespace", "new", "or", "print", "private", "protected", "public",
--            "readonly", "require", "require_once", "return", "static", "switch", "throw", "trait",
--            "try", "unset", "use", "var", "while", "xor", "yield", "yield", "from"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Perl,      LanguageInfo {
--        langExtensions = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]
--    ,   langFilter = mkFilter ["=pod" ~~ "=cut", "#" ~~ "\n"]   ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords [ ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Python,    LanguageInfo {
--        langExtensions = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''", "'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "False", "await", "else", "import", "pass", "None", "break", "except", "in", "raise",
--            "True", "class", "finally", "is", "return", "and", "continue", "for", "lambda", "try",
--            "as", "def", "from", "nonlocal", "while", "assert", "del", "global", "not", "with",
--            "async", "elif", "if", "or", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (R,         LanguageInfo {
--        langExtensions = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "if", "else", "repeat", "while", "function", "for", "in", "next", "break", "TRUE", "FALSE", "NULL",
--            "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_","…"
--        ]
--    ,   langAdditonalValidIdentifChars = ("._", "._")
--    })
--    ,  (Ruby,      LanguageInfo {
--        langExtensions = [Ext "rb", Ext "ruby"]
--    ,   langFilter = mkFilter ["=begin" ~~ "=end", "#" ~~ "\n"] ["'" ~~ "'", "\"" ~~ "\"", "%|" ~~ "|", "%q(" ~~ ")", "%Q(" ~~ ")" ]
--    ,   langResKeywords = keywords [
--            "BEGIN", "END", "alias", "and", "begin", "break", "case", "class", "def",
--            "module", "next", "nil", "not", "or", "redo", "rescue", "retry", "return",
--            "elsif", "end", "false", "ensure", "for", "if", "true", "undef", "unless",
--            "do", "else", "super", "then", "until", "when", "while", "defined?", "self"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Rust,      LanguageInfo {
--        langExtensions = [Ext "rs", Ext "rlib"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "as", "use", "extern crate", "break", "const", "continue", "crate", "else", "if", "if let",
--            "enum", "extern", "false", "fn", "for", "if", "impl", "in", "for", "let", "loop", "match",
--            "mod", "move", "mut", "pub", "impl", "ref", "return", "Self", "self", "static", "struct",
--            "super", "trait", "true", "type", "unsafe", "use", "where", "while", "abstract", "alignof",
--            "become", "box", "do", "final", "macro", "offsetof", "override", "priv", "proc", "pure",
--            "sizeof", "typeof", "unsized", "virtual", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_#")
--    })
--    ,  (Scala,     LanguageInfo {
--        langExtensions = [Ext "scala"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
--            "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null",
--            "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw",
--            "trait", "true", "try", "type", "val", "var", "while", "with", "yield"
--        ]
--    ,   langAdditonalValidIdentifChars = ("$_", "$_")
--    })
--    ,  (SmallTalk, LanguageInfo {
--        langExtensions = [Ext "st", Ext "gst"]
--    ,   langFilter = mkFilter ["\"" ~~ "\""] ["'" ~~ "'"]
--    ,   langResKeywords = keywords [
--            "true", "false", "nil", "self", "super", "thisContext"
--        ]
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (Shell,     LanguageInfo {
--        langExtensions = [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "if", "then", "elif", "else", "fi", "time", "for", "in", "until", "while", "do", "done",
--            "case", "esac", "coproc", "select", "function"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Swift,     LanguageInfo {
--        langExtensions = [Ext "swift"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "associatedtype",  "class",  "deinit",  "enum",  "extension",  "fileprivate",  "func",  "import",  "init",  "inout",
--            "internal",  "let",  "open", "operator",  "private",  "precedencegroup",  "protocol",  "public",  "rethrows",  "static",
--            "struct",  "subscript",  "typealias",  "var", "break",  "case",  "catch",  "continue",  "default",  "defer",  "do",  "else",
--            "fallthrough",  "for",  "guard",  "if",  "in",  "repeat", "return",  "throw",  "switch",  "where",  "while", "Any",  "as",
--            "catch",  "false",  "is",  "nil",  "rethrows",  "self",  "Self",  "super", "throw",  "throws",  "true",  "try",
--            "#available",  "#colorLiteral",  "#column", "#dsohandle",  "#elseif",  "#else",  "#endif",  "#error",  "#fileID", "#fileLiteral",
--            "#filePath", "#file",  "#function",  "#if",  "#imageLiteral",  "#keyPath",  "#line",  "#selector",  "#sourceLocation",  "#warning",
--            "associativity",  "convenience",  "didSet",  "dynamic",  "final",  "get",  "indirect",  "infix",  "lazy",  "left",  "mutating",  "none",
--            "nonmutating",  "optional",  "override",  "postfix",  "precedence",  "prefix",  "Protocol",  "required",  "right",  "set",  "some",
--            "Type",  "unowned",  "weak",  "willSet"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Tcl,       LanguageInfo {
--        langExtensions = [Ext "tcl", Ext "tk"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (Text,  LanguageInfo {
--        langExtensions = [
--            Ext "txt", Ext "md", Ext "markdown", Ext "mdown", Ext "mkdn", Ext "mkd", Ext "mdwn",
--            Ext "mdtxt", Ext "mdtext", Ext "text", Name "README", Name "INSTALL", Name "VERSION",
--            Name "LICENSE", Name "AUTHORS", Name "CHANGELOG"
--        ]
--    ,   langFilter = Nothing
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
--    ,  (VHDL,      LanguageInfo {
--        langExtensions = [Ext "vhd", Ext "vhdl"]
--    ,   langFilter = mkFilter ["--" ~~ "\n"] ["\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert", "attribute", "begin", "block", "body",
--            "buffer", "bus", "case", "component", "configuration", "constant", "disconnect", "downto", "else", "elsif", "end", "entity",
--            "exit", "file", "for", "function", "generate", "generic", "group", "guarded", "if", "impure", "in", "inertial", "inout", "is",
--            "label", "library", "linkage", "literal", "loop", "map", "mod", "nand", "new", "next", "nor", "not", "null", "of", "on", "open",
--            "or", "others", "out", "package", "port", "postponed", "procedure", "process", "pure", "range", "record", "register", "reject",
--            "return", "rol", "ror", "select", "severity", "signal", "shared", "sla", "sli", "sra", "srl", "subtype", "then", "to", "transport",
--            "type", "unaffected", "units", "until", "use", "variable", "wait", "when", "while", "with", "xnor", "xor"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Verilog,   LanguageInfo {
--        langExtensions = [Ext "v", Ext "vh", Ext "sv"]
--    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords [
--            "always", "end", "ifnone", "or", "rpmos", "tranif1", "and", "endcase", "initial", "output", "rtran", "tri", "assign", "endmodule",
--            "inout", "parameter", "rtranif0", "tri0", "begin", "endfunction", "input", "pmos", "rtranif1", "tri1", "buf", "endprimitive", "integer",
--            "posedge", "scalared", "triand", "bufif0", "endspecify", "join", "primitive", "small", "trior", "bufif1", "endtable", "large", "pull0",
--            "specify", "trireg", "case", "endtask", "macromodule", "pull1", "specparam", "vectored", "casex", "event", "medium", "pullup", "strong0",
--            "wait", "casez", "for", "module", "pulldown", "strong1", "wand", "cmos", "force", "nand", "rcmos", "supply0", "weak0", "deassign", "forever",
--            "negedge", "real", "supply1", "weak1", "default", "for", "nmos", "realtime", "table", "while", "defparam", "function", "nor", "reg", "task",
--            "wire", "disable", "highz0", "not", "release", "time", "wor", "edge", "highz1", "notif0", "repeat", "tran", "xnor", "else", "if", "notif1",
--            "rnmos", "tranif0", "xor"
--        ]
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Vim,       LanguageInfo {
--        langExtensions = [Ext "vim"]
--    ,   langFilter = mkFilter ["\"" ~~ "\n"] ["'" ~~ "'", "\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("_", "_")
--    })
--    ,  (Yaml,      LanguageInfo {
--        langExtensions = [Ext "yaml", Ext "yml"]
--    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
--    ,   langResKeywords = keywords []
--    ,   langAdditonalValidIdentifChars = ("", "")
--    })
    ]


-- mkFilter :: [Boundary] -> [Boundary] -> Maybe FilterFunction
-- mkFilter cs ls = Just $ runContextFilter (mkParConfig cs ls)
-- {-# INLINE mkFilter #-}


mkLangFilter :: Bool -> LanguageInfo -> Maybe FilterFunction
mkLangFilter  alterBoundary LanguageInfo {..} = Just $
    runContextFilter (mkParConfig langComment langString langRawString langChar alterBoundary)
{-# INLINE mkLangFilter #-}


contextFilter :: Maybe Language -> ContextFilter -> Bool -> Text8 -> Text8
contextFilter _ (ContextFilter True True True) False txt = txt
contextFilter Nothing _ _ txt = txt
contextFilter (Just language) filt alterBoundary txt
   | Just fun <- parFunc = fun filt txt
   | otherwise = txt
        where parFunc = mkLangFilter alterBoundary =<< Map.lookup language languagesMap
{-# INLINE contextFilter #-}


languageLookup :: Options -> FilePath -> Maybe Language
languageLookup opts f = forcedLang opts <|> lookupFileLang f
    where lookupFileLang :: FilePath -> Maybe Language
          lookupFileLang f = Map.lookup (Name $ takeFileName f) languagesFileMap <|> Map.lookup (Ext (let name = takeExtension f in case name of ('.':xs) -> xs; _ -> name )) languagesFileMap
{-# INLINE languageLookup #-}


languageInfoLookup :: Options -> FilePath -> Maybe (Language, LanguageInfo)
languageInfoLookup opts f = languageLookup opts f >>= \l -> (l,) <$> Map.lookup l languagesMap
{-# INLINE languageInfoLookup #-}


languagesFileMap :: FileMapType
languagesFileMap = Map.fromList $ concatMap (\(l, LanguageInfo{..}) -> map (,l) langExtensions ) $ Map.toList languagesMap
{-# INLINE languagesFileMap #-}


dumpLanguagesMap :: LanguagesMapType -> IO ()
dumpLanguagesMap m = forM_ (Map.toList m) $ \(l, ex) ->
                putStrLn $ show l <> [ ' ' | _ <- [length (show l)..12]] <> "-> " <> show (langExtensions ex)


dumpLanguagesFileMap :: FileMapType -> IO ()
dumpLanguagesFileMap m = forM_ (Map.toList m) $ \(ext, l) ->
                    putStrLn $ show ext <> [ ' ' | _ <- [length (show ext)..12 ]] <> "-> " <> show l



forcedLang :: Options -> Maybe Language
forcedLang Options{ language_force = l }
    | Nothing <- l = Nothing
    | otherwise    = Map.lookup (Ext $ fromJust l) languagesFileMap <|> Map.lookup (Name $ fromJust l) languagesFileMap


(~~) :: C.ByteString -> C.ByteString -> Boundary
(~~) = Boundary
{-# INLINE (~~) #-}


keywords :: (Ord a) => [a] -> S.Set a
keywords = S.fromList