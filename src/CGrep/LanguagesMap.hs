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
import Options ( Options(Options, language_force, keyword) )
import qualified Data.ByteString.Char8 as C

import qualified Data.Array.BitArray as BA

import qualified Data.Set as S
import Data.List (findIndex)

import CGrep.Boundary ( Boundary(Boundary) )
import CGrep.Parser.Char
    ( isAlphaNum_,
      isAlphaNum_',
      isAlpha_,
      isAlpha_',
      isAlphaNum_and,
      isAlpha_and )
import Data.Char ( isAlpha, isAlphaNum )

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
    ,   langComment  = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString= ["\"" ~~ "\""]
    ,   langRawString = []
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
   ,   langComment = ["#" ~~ "\n", ";" ~~ "\n", "|" ~~ "\n", "!" ~~ "\n", "/*" ~~ "*/"]
   ,   langChar = []
   ,   langString= ["\"" ~~ "\""]
   ,   langRawString = []
   ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
   ,   langResKeywords = keywords []
   })
    ,  (Awk,       LanguageInfo {
        langExtensions = [Ext "awk", Ext "mawk", Ext "gawk"]
    ,   langComment = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langChar = []
    ,   langString= ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "BEGIN", "END", "if", "else", "while", "do", "for", "in", "break", "continue",
                "delete", "next", "nextfile", "function", "func", "exit"]
    })
    ,  (C,         LanguageInfo {
        langExtensions = [Ext "c", Ext "C", Ext "inc"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["R\"" ~~ "\""]
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
    ,  (CMake,     LanguageInfo {
        langExtensions = [Name "CMakeLists.txt", Ext "cmake"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString= ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords []
    })
    ,  (Cabal,     LanguageInfo {
        langExtensions = [Ext "cabal"]
    ,   langComment = ["--" ~~ "\n"]
    ,   langChar = []
    ,   langString= ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (const False, const False)
    ,   langResKeywords = keywords []
    })
    ,  (Chapel,    LanguageInfo {
        langExtensions = [Ext "chpl"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langRawString = ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_and "$")
    ,   langResKeywords = keywords [
        "align", "as", "atomic", "begin", "bool", "borrowed", "break", "by", "bytes", "catch",
        "class", "cobegin", "coforall", "complex", "config", "const", "continue", "defer",
        "delete", "dmapped", "do", "domain", "else", "enum", "except", "export", "extern", "false",
        "for", "forall", "forwarding", "if", "imag", "in", "index", "inline", "inout", "int", "iter",
        "label", "let", "lifetime", "local", "locale", "module", "new", "nil", "noinit", "on", "only",
        "otherwise", "out", "override", "owned", "param", "private", "prototype", "proc", "public",
        "real", "record", "reduce", "ref", "require", "return", "scan", "select", "serial", "shared",
        "single", "sparse", "string", "subdomain", "sync", "then", "this", "throw", "throws", "true",
        "try", "type", "uint", "union", "unmanaged", "use", "var", "when", "where", "while", "with", "yield", "zip"
        ]
    })
    ,  (Clojure,   LanguageInfo {
        langExtensions = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"]
    ,   langComment = [";" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha, isAlphaNum_and "*+!-_?")
    ,   langResKeywords = keywords [
           "and", "let", "def", "defn", "if", "else", "do", "quote", "var", "fn", "loop", "recur", "throw", "try",
           "monitor-enter", "monitor-exit"
       ]
   })
    ,  (Coffee,    LanguageInfo {
        langExtensions = [Ext "coffee"]
    ,   langComment = ["#" ~~ "\n", "###" ~~ "###"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = ["'''" ~~ "'''", "\"\"\"" ~~ "\"\"\"" ]
    ,   langValidIdentifierChars = (isAlpha_and "$", isAlphaNum_and "$")
    ,   langResKeywords = keywords [
            "case", "default", "function", "var", "void", "with", "const", "let", "enum", "export", "import", "native",
            "__hasProp", "__extends", "__slice", "__bind", "__indexOf", "implements", "interface", "package", "private",
            "protected", "public", "static", "yield", "true", "false", "null", "this", "new", "delete", "typeof", "in",
            "arguments", "eval", "instanceof", "return", "throw", "break", "continue", "debugger", "if", "else", "switch",
            "for", "while", "do", "try", "catch", "finally", "class", "extends", "super", "undefined", "then", "unless",
            "until", "loop", "of", "by", "when", "and", "or", "is", "isnt", "not", "yes", "no", "on", "off"
        ]
    })
    ,  (Conf,      LanguageInfo {
        langExtensions = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords []
    })
    ,  (Cpp,    LanguageInfo {
        langExtensions = [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc", Ext "cp", Ext "c++", Ext "tcc",
                          Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP", Ext "hxx",
                          Ext "hh", Ext "hp", Ext "h++", Ext "cu", Ext "cuh"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["R\"(" ~~ ")\"", "R\"-(" ~~ ")-\"", "R\"--(" ~~ ")--\""]
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
    ,  (Csharp,    LanguageInfo {
        langExtensions = [Ext "cs", Ext "CS"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "abstract", "as", "base", "bool", "break", "byte", "case", "catch", "char", "checked", "class", "const",
            "continue", "decimal", "default", "delegate", "do", "double", "else", "enum", "event", "explicit", "extern",
            "false", "finally", "fixed", "float", "for", "foreach", "goto", "if", "implicit", "in", "int", "interface",
            "internal", "is", "lock", "long", "namespace", "new", "null", "object", "operator", "out", "override", "params",
            "private", "protected", "public", "readonly", "ref", "return", "sbyte", "sealed", "short", "sizeof", "stackalloc",
            "static", "string", "struct", "switch", "this", "throw", "true", "try", "typeof", "uint", "ulong", "unchecked",
            "unsafe", "ushort", "using", "virtual", "void", "volatile", "while"
        ]
    })
    ,  (Css,       LanguageInfo {
        langExtensions = [Ext "css"]
    ,   langComment = ["/*" ~~ "*/"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_and "-", isAlphaNum_and "-")
    ,   langResKeywords = keywords []
    })
    ,  (Cql,       LanguageInfo {
        langExtensions = [Ext "cql"]
    ,   langComment = ["--" ~~ "\n"]
    ,   langChar = []
    ,   langString= ["'" ~~ "'"]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "ADD", "AGGREGATE", "ALL",  "ALLOW",  "ALTER",  "AND",  "ANY",  "APPLY",  "AS",
            "ASC",  "ASCII",  "AUTHORIZE",  "BATCH",  "BEGIN",  "BIGINT",  "BLOB",  "BOOLEAN",
            "BY",  "CLUSTERING",  "COLUMNFAMILY",  "COMPACT",  "CONSISTENCY",  "COUNT",  "COUNTER",
            "CREATE",  "CUSTOM",  "DECIMAL",  "DELETE",  "DESC",  "DISTINCT",  "DOUBLE",  "DROP",
            "EACH_QUORUM",  "ENTRIES",  "EXISTS",  "FILTERING",  "FLOAT",  "FROM",  "FROZEN",  "FULL",
            "GRANT",  "IF",  "IN",  "INDEX",  "INET",  "INFINITY",  "INSERT",  "INT",
            "INTO",  "KEY",  "KEYSPACE",  "KEYSPACES",  "LEVEL",  "LIMIT",  "LIST",  "LOCAL_ONE",
            "LOCAL_QUORUM",  "MAP",  "MATERIALIZED",  "MODIFY",  "NAN",  "RECURSIVE",  "SUPERUSER",  "T",
            "OF",  "ON",  "ONE",  "ORDER",  "PARTITION",  "PASSWORD",  "PER",  "PERMISSION",  "PERMISSIONS",
            "PRIMARY",  "QUORUM",  "RENAME",  "REVOKE",  "SCHEMA",  "SELECT",  "SET",  "STATIC",  "STORAGE",
            "SUPERUSER",  "TABLE",  "TEXT",  "TIME",  "TIMESTAMP",  "TIMEUUID",  "THREE",  "TO",  "TOKEN",
            "TRUNCATE",  "TTL",  "TUPLE",  "TWO",  "TYPE",  "UNLOGGED",  "UPDATE",  "USE",  "USER",
            "USERS",  "USING",  "UUID",  "VALUES",  "VARCHAR",  "VARINT",  "VIEW",  "WHERE",  "WITH","WRITETIME"
        ]
    })
    ,  (D,         LanguageInfo {
        langExtensions = [Ext "d", Ext "D"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["r\"" ~~ "\"", "`" ~~ "`"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "abstract", "alias", "align", "asm", "assert", "auto", "body", "bool", "break", "byte", "case", "cast", "catch",
            "cdouble", "cent", "cfloat", "char", "class", "const", "continue", "creal", "dchar", "debug", "default", "delegate",
            "delete", "deprecated", "do", "double", "else", "enum", "export", "extern", "false", "final", "finally", "float",
            "for", "foreach", "foreach_reverse", "function", "goto", "idouble", "if", "ifloat", "immutable", "import", "in",
            "inout", "int", "interface", "invariant", "ireal", "is", "lazy", "long", "macro", "mixin", "module", "new", "nothrow",
            "null", "out", "override", "package", "pragma", "private", "protected", "public", "pure", "real", "ref", "return",
            "scope", "shared", "short", "static", "struct", "super", "switch", "synchronized", "template", "this", "throw", "true",
            "try", "typeid", "typeof", "ubyte", "ucent", "uint", "ulong", "union", "unittest", "ushort", "version", "void", "wchar",
            "while", "with", "__FILE__", "__FILE_FULL_PATH__", "__MODULE__", "__LINE__", "__FUNCTION__", "__PRETTY_FUNCTION__",
            "__gshared", "__traits", "__vector", "__parameters"
        ]
    })
    ,  (Dart,      LanguageInfo {
        langExtensions = [Ext "dart"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langRawString = ["'''" ~~ "'''", "\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "assert", "break", "case", "catch", "class", "const", "continue", "default", "do", "else", "enum", "extends", "false",
            "final", "finally", "for", "if", "in", "is", "new", "null", "rethrow", "return", "super", "switch", "this", "throw",
            "true", "try", "var", "void", "while", "with", "async", "hide", "on", "show", "sync", "abstract", "as", "covariant",
            "deferred", "dynamic", "export", "extension", "external", "factory", "function", "get", "implements", "import", "interface",
            "library", "mixin", "operator", "part", "set", "static", "typedef",  "await", "yield"
        ]
    })
    ,  (Elixir,    LanguageInfo {
        langExtensions = [Ext "ex", Ext "exs"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "true", "false", "nil", "when", "and", "or", "not", "in", "fn", "do", "end", "catch", "rescue", "after", "else"
        ]
    })
    ,  (Elm,       LanguageInfo {
        langExtensions = [Ext "elm"]
    ,   langComment = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString =  ["\"" ~~ "\""]
    ,   langRawString =  ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha,  isAlphaNum_)
    ,   langResKeywords = keywords [
            "type", "alias", "port", "if", "then", "else", "case", "of", "let", "in", "infix", "left", "right", "non",
            "module", "import", "exposing", "as", "where", "effect", "command", "subscription", "true", "false", "null"
        ]
    })
   ,  (Erlang,    LanguageInfo {
       langExtensions = [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"]
   ,   langComment = ["%" ~~ "\n"]
   ,   langChar = []
   ,   langString = ["\"" ~~ "\""]
   ,   langRawString = []
   ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
   ,   langResKeywords = keywords [
           "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case", "catch", "cond",
           "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "receive", "rem", "try", "when", "xor"
       ]
   })
    ,  (Fortran,   LanguageInfo {
        langExtensions = [Ext "f", Ext "for", Ext "ftn",
                    Ext "F", Ext "FOR", Ext "FTN", Ext "fpp", Ext "FPP",
                    Ext "f90", Ext "f95", Ext "f03", Ext "f08",
                    Ext "F90", Ext "F95", Ext "F03", Ext "F08"]
    ,   langComment = ["!" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langRawString =[]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            -- fortran77
            "assign", "backspace", "block", "data", "call", "close", "common", "continue", "data", "dimension", "do",
            "else", "else", "if", "end", "endfile", "endif", "entry", "equivalence", "external", "format", "function",
            "goto", "if", "implicit", "inquire", "intrinsic", "open", "parameter", "pause", "print", "program", "read",
            "return", "rewind", "rewrite", "save", "stop", "subroutine", "then", "write",
            -- fortran 90
            "allocatable", "allocate", "case", "contains", "cycle", "deallocate", "elsewhere", "exit?", "include",
            "interface", "intent", "module", "namelist", "nullify", "only", "operator", "optional", "pointer", "private",
            "procedure", "public", "recursive", "result", "select", "sequence", "target", "use", "while", "where",
            -- fortran 95
            "elemental", "forall", "pure",
            -- fortran 03
            "abstract", "associate", "asynchronous", "bind", "class", "deferred", "enum", "enumerator", "extends", "final",
            "flush", "generic", "import", "non_overridable", "nopass", "pass", "protected", "value", "volatile", "wait",
            -- fortran 08
            "block", "codimension", "do", "concurrent", "contiguous", "critical", "error", "stop", "submodule", "sync",
            "all", "sync", "images", "sync",  "memory", "lock", "unlock"
        ]
    })
    ,  (Fsharp,    LanguageInfo {
        langExtensions = [Ext "fs", Ext "fsx", Ext "fsi"]
    ,   langComment =  ["(*" ~~ "*)", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString =  ["\"" ~~ "\""]
    ,   langRawString =  ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_and "$@`?", isAlphaNum_and "$@`?")
    ,   langResKeywords = keywords [
            "abstract", "and", "as", "assert", "base", "begin", "class", "default", "delegate", "do", "done", "downcast",
            "downto", "elif", "else", "end", "exception", "extern", "FALSE", "finally", "fixed", "for", "fun", "function",
            "global", "if", "in", "inherit", "inline", "interface", "internal", "lazy", "let", "let!",  "match", "match!",
            "member", "module", "mutable", "namespace", "new", "not", "null", "of", "open", "or", "override", "private",
            "public", "rec", "return", "return!",  "select", "static", "struct", "then", "to", "TRUE", "try", "type",
            "upcast", "use", "use!",  "val", "void", "when", "while", "with", "yield", "yield!",  "const", "asr", "land",
            "lor", "lsl", "lsr", "lxor", "mod", "sig", "break", "checked", "component", "const", "constraint", "continue",
            "event", "external", "include", "mixin", "parallel", "process", "protected", "pure", "sealed", "tailcall", "trait", "virtual"
        ]
    })
    ,  (Go,        LanguageInfo {
        langExtensions = [Ext "go"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["`" ~~ "`"]
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
    ,  (GoMod,        LanguageInfo {
        langExtensions = [Name "go.mod"]
    ,   langComment = ["//" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (const False, const False)
    ,   langResKeywords = keywords [
            "module", "go", "require", "exclude", "replace", "retract"
        ]
    })
    ,  (Haskell,   LanguageInfo {
        langExtensions = [Ext "hs", Ext "lhs", Ext "hsc"]
    ,   langComment = ["{-" ~~ "-}", "--" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = [ "[r|" ~~ "|]", "[q|" ~~ "|]", "[s|" ~~ "|]", "[here|" ~~"|]",  "[i|" ~~ "|]"]
    ,   langValidIdentifierChars = (isAlpha_', isAlphaNum_')
    ,   langResKeywords = keywords [
            "as", "case", "class", "data", "default", "deriving", "do", "else", "hiding", "if", "import",
            "in", "infix", "infixl", "infixr", "instance", "let", "module", "newtype", "of", "qualified",
            "then", "type", "where", "forall", "mdo", "family", "role", "pattern", "static", "group",
            "by", "using", "foreign", "export", "label", "dynamic", "safe", "interruptible", "unsafe",
            "stdcall", "ccall", "capi", "prim", "javascript", "rec", "proc"
        ]
    })
    ,  (Html,      LanguageInfo {
        langExtensions = [Ext "htm", Ext "html"]
    ,   langComment = ["<!--" ~~ "-->"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlpha_and "-:.")
    ,   langResKeywords = keywords []
    })
    ,  (Idris,     LanguageInfo {
        langExtensions = [Ext "idr", Ext "lidr"]
    ,   langComment = ["{-" ~~ "-}", "--" ~~ "\n", "|||" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_', isAlphaNum_')
    ,   langResKeywords = keywords []
    })
    ,  (Java,      LanguageInfo {
        langExtensions = [Ext "java"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_and "$", isAlphaNum_and "$")
    ,   langResKeywords = keywords [
            "abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "continue", "const",
            "default", "do", "double", "else", "enum", "exports", "extends", "final", "finally", "float", "for", "goto",
            "if", "implements", "import", "instanceof", "int", "interface", "long", "module", "native", "new", "package",
            "private", "protected", "public", "requires", "return", "short", "static", "strictfp", "super", "switch",
            "synchronized", "this", "throw", "throws", "transient", "try", "var", "void", "volatile", "while", "true",
            "false", "null"
        ]
    })
    ,  (Javascript,LanguageInfo {
        langExtensions = [Ext "js"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_and "$", isAlphaNum_and "$")
    ,   langResKeywords = keywords [
            "abstract", "arguments", "await", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue",
            "debugger", "default", "delete", "do", "double", "else", "enum", "eval", "export", "extends", "false", "final",
            "finally", "float", "for", "function", "goto", "if", "implements", "import", "in", "instanceof", "int", "interface",
            "let", "long", "native", "new", "null", "package", "private", "protected", "public", "return", "short", "static",
            "super", "switch", "synchronized", "this", "throw", "throws", "transient", "true", "try", "typeof", "var", "void",
            "volatile", "while", "with", "yield"
        ]
    })
    ,  (Json,      LanguageInfo {
        langExtensions = [Ext "json", Ext "ndjson"]
    ,   langComment = []
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (const False, const False)
    ,   langResKeywords = keywords []
    })
    ,  (Julia,      LanguageInfo {
        langExtensions = [Ext "jl"]
    ,   langComment = ["#" ~~ "\n", "#-" ~~ "-#"]
    ,   langChar= ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "baremodule",  "begin",  "break",  "catch",  "const",  "continue",  "do",  "else",  "elseif",  "end",
            "export",  "false",  "finally",  "for",  "function",  "global",  "if",  "import",  "let",  "local",
            "macro",  "module",  "quote",  "return",  "struct",  "true",  "try",  "using",  "while"
        ]
    })
    ,  (Kotlin,    LanguageInfo {
        langExtensions = [Ext "kt", Ext "kts", Ext "ktm"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
        "as", "break", "class", "continue", "do", "else", "false", "for", "fun", "if", "in", "interface", "is", "null", "object",
        "package", "return", "super", "this", "throw", "true", "try", "typealias", "typeof", "val", "var", "when", "while",
        "by", "catch", "constructor", "delegate", "dynamic", "field", "file", "finally", "get", "import", "init",
        "param", "property", "receiver", "set", "setparam", "value", "where",
        "abstract", "actual", "annotation", "companion", "const", "crossinline", "data", "enum", "expect", "external",
        "final", "infix", "inline", "inner", "internal", "lateinit", "noinline", "open", "operator", "out", "override",
        "private", "protected", "public", "reified", "sealed", "suspend", "tailrec", "vararg",
        "field", "it"
        ]
    })
    ,  (Latex,     LanguageInfo {
        langExtensions = [Ext "latex", Ext "tex"]
    ,   langComment = ["%" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (const False, const False)
    ,   langResKeywords = keywords []
    })
    ,  (Lisp,      LanguageInfo {
        langExtensions = [Ext "lisp", Ext "cl"]
    ,   langComment = [";" ~~ "\n", "#|" ~~ "|#"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_and "!$%&*+-./:<=>?@^~")
    ,   langResKeywords = keywords []
    })
    ,  (Lua,       LanguageInfo {
        langExtensions = [Ext "lua"]
    ,   langComment = ["--[[" ~~ "--]]", "--" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = ["[===[" ~~ "]===]", "[==[" ~~ "]==]", "[=[" ~~ "]=]", "[[" ~~ "]]"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "and", "break", "do", "else", "elseif", "end", "false", "for", "function", "if",
            "in", "local", "nil", "not", "or", "repeat", "return", "then", "true", "until", "while"
        ]
    })
    ,  (Make,      LanguageInfo {
        langExtensions = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak", Ext "make"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_and "-", isAlpha_and "-")
    ,   langResKeywords = keywords []
    })
    ,  (Nmap,      LanguageInfo {
        langExtensions = [Ext "nse"]
    ,   langComment = ["--" ~~ "\n", "[[" ~~"]]"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (const False, const False)
    ,   langResKeywords = keywords []
    })
    ,  (Nim,         LanguageInfo {
        langExtensions = [Ext "nim"]
    ,   langComment = ["#[" ~~ "#]", "#" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString  = ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "addr", "and", "as", "asm",
            "bind", "block", "break",
            "case", "cast", "concept", "const", "continue", "converter",
            "defer", "discard", "distinct", "div", "do",
            "elif", "else", "end", "enum", "except", "export",
            "finally", "for", "from", "func",
            "if", "import", "in", "include", "interface", "is", "isnot", "iterator",
            "let", "macro", "method", "mixin", "mod", "nil", "not", "notin",
            "object", "of", "or", "out", "proc", "ptr",
            "raise", "ref", "return", "shl", "shr", "static",
            "template", "try", "tuple", "type",
            "using", "var", "when", "while", "xor", "yield"
        ]
    })
    ,  (OCaml ,    LanguageInfo {
        langExtensions = [Ext "ml", Ext "mli"]
    ,   langComment = ["(*" ~~ "*)"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["{id|" ~~ "|id}"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_')
    ,   langResKeywords = keywords [
            "and",         "as",          "assert",      "asr",         "begin",       "class",
            "constraint",  "do",          "done",        "downto",      "else",        "end",
            "exception",   "external",    "false",       "for",         "fun",         "function",
            "functor",     "if",          "in",          "include",     "inherit",     "initializer",
            "land",        "lazy",        "let",         "lor",         "lsl",         "lsr",
            "lxor",        "match",       "method",      "mod",         "module",      "mutable",
            "new",         "nonrec",      "object",      "of",          "open",        "or",
            "private",     "rec",         "sig",         "struct",      "then",        "to",
            "true",        "try",         "type",        "val",         "virtual",     "when",
            "while",       "with"
        ]
    })
    ,  (ObjectiveC,LanguageInfo {
        langExtensions = [Ext "m", Ext "mi"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "void", "char", "short", "int", "long", "float", "double", "signed", "unsigned", "id", "const", "volatile", "in",
            "out", "inout", "bycopy", "byref", "oneway", "self", "super", "interface", "end", "@implementation", "@end",
            "@interface", "@end", "@implementation", "@end", "@protoco", "@end", "@class"
        ]
    })
    ,  (PHP,       LanguageInfo {
        langExtensions = [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n", "#" ~~ "\n" ]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = ["<<END" ~~ "END;", "<<'END'" ~~ "END;"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "__halt_compiler", "abstract", "and", "array", "as", "break", "callable", "case", "catch", "class",
            "clone", "const", "continue", "declare", "default", "die", "do", "echo", "else", "elseif",
            "empty", "enddeclare", "endfor", "endforeach", "endif", "endswitch", "endwhile", "eval",
            "exit", "extends", "final", "finally", "fn", "for", "foreach", "function", "global", "goto",
            "if", "implements", "include", "include_once", "instanceof", "insteadof", "interface",
            "isset", "list", "match", "namespace", "new", "or", "print", "private", "protected", "public",
            "readonly", "require", "require_once", "return", "static", "switch", "throw", "trait",
            "try", "unset", "use", "var", "while", "xor", "yield", "yield", "from"
        ]
    })
    ,  (Perl,      LanguageInfo {
        langExtensions = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]
    ,   langComment = ["=pod" ~~ "=cut", "#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = ["<<\"END\";" ~~ "END", "<<'END'" ~~ "END", "<<'EOT';" ~~ "EOT", "<<\"EOT\";" ~~ "EOT"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [ ]
    })
    ,  (Python,      LanguageInfo {
        langExtensions = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''", "r'" ~~ "'"]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "False", "await", "else", "import", "pass", "None", "break", "except", "in", "raise",
            "True", "class", "finally", "is", "return", "and", "continue", "for", "lambda", "try",
            "as", "def", "from", "nonlocal", "while", "assert", "del", "global", "not", "with",
            "async", "elif", "if", "or", "yield"
        ]
    })
    ,  (R,         LanguageInfo {
        langExtensions = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langRawString =[]
    ,   langValidIdentifierChars = (isAlpha_and ".", isAlphaNum_and ".")
    ,   langResKeywords = keywords [
            "if", "else", "repeat", "while", "function", "for", "in", "next", "break", "TRUE", "FALSE", "NULL",
            "Inf", "NaN", "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_","…"
        ]
    })
    ,  (Ruby,      LanguageInfo {
        langExtensions = [Ext "rb", Ext "ruby"]
    ,   langComment = ["=begin" ~~ "=end", "#" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["'" ~~ "'", "\"" ~~ "\"", "%|" ~~ "|", "%q(" ~~ ")", "%Q(" ~~ ")" ]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "BEGIN", "END", "alias", "and", "begin", "break", "case", "class", "def",
            "module", "next", "nil", "not", "or", "redo", "rescue", "retry", "return",
            "elsif", "end", "false", "ensure", "for", "if", "true", "undef", "unless",
            "do", "else", "super", "then", "until", "when", "while", "defined?", "self"
        ]
    })
    ,  (Rust,      LanguageInfo {
        langExtensions = [Ext "rs", Ext "rlib"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["r##\"" ~~ "\"##",  "r#\"" ~~ "\"#", "r\"" ~~ "\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_and "#")
    ,   langResKeywords = keywords [
            "as", "use", "extern crate", "break", "const", "continue", "crate", "else", "if", "if let",
            "enum", "extern", "false", "fn", "for", "if", "impl", "in", "for", "let", "loop", "match",
            "mod", "move", "mut", "pub", "impl", "ref", "return", "Self", "self", "static", "struct",
            "super", "trait", "true", "type", "unsafe", "use", "where", "while", "abstract", "alignof",
            "become", "box", "do", "final", "macro", "offsetof", "override", "priv", "proc", "pure",
            "sizeof", "typeof", "unsized", "virtual", "yield"
        ]
    })
   ,  (Scala,     LanguageInfo {
       langExtensions = [Ext "scala"]
   ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
   ,   langChar = ["'" ~~ "'"]
   ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
   ,   langRawString = []
   ,   langValidIdentifierChars = (isAlpha_and "$", isAlphaNum_and "$")
   ,   langResKeywords = keywords [
           "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
           "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null",
           "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw",
           "trait", "true", "try", "type", "val", "var", "while", "with", "yield"
       ]
   })
    ,  (SmallTalk, LanguageInfo {
        langExtensions = [Ext "st", Ext "gst"]
    ,   langComment = ["\"" ~~ "\""]
    ,   langChar = ["$" ~~ ""]
    ,   langString = ["'" ~~ "'"]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha, isAlphaNum)
    ,   langResKeywords = keywords [
            "true", "false", "nil", "self", "super", "thisContext"
        ]
    })
    ,  (Shell,     LanguageInfo {
        langExtensions = [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["'" ~~ "'", "\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "if", "then", "elif", "else", "fi", "time", "for", "in", "until", "while", "do", "done",
            "case", "esac", "coproc", "select", "function"
        ]
    })
    ,  (Swift,     LanguageInfo {
        langExtensions = [Ext "swift"]
    ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = ["\"\"\"" ~~ "\"\"\""]
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "associatedtype",  "class",  "deinit",  "enum",  "extension",  "fileprivate",  "func",  "import",  "init",  "inout",
            "internal",  "let",  "open", "operator",  "private",  "precedencegroup",  "protocol",  "public",  "rethrows",  "static",
            "struct",  "subscript",  "typealias",  "var", "break",  "case",  "catch",  "continue",  "default",  "defer",  "do",  "else",
            "fallthrough",  "for",  "guard",  "if",  "in",  "repeat", "return",  "throw",  "switch",  "where",  "while", "Any",  "as",
            "catch",  "false",  "is",  "nil",  "rethrows",  "self",  "Self",  "super", "throw",  "throws",  "true",  "try",
            "#available",  "#colorLiteral",  "#column", "#dsohandle",  "#elseif",  "#else",  "#endif",  "#error",  "#fileID", "#fileLiteral",
            "#filePath", "#file",  "#function",  "#if",  "#imageLiteral",  "#keyPath",  "#line",  "#selector",  "#sourceLocation",  "#warning",
            "associativity",  "convenience",  "didSet",  "dynamic",  "final",  "get",  "indirect",  "infix",  "lazy",  "left",  "mutating",  "none",
            "nonmutating",  "optional",  "override",  "postfix",  "precedence",  "prefix",  "Protocol",  "required",  "right",  "set",  "some",
            "Type",  "unowned",  "weak",  "willSet"
        ]
    })
    ,  (Sql,       LanguageInfo {
        langExtensions = [Ext "sql"]
    ,   langComment = ["--" ~~ "\n"]
    ,   langChar = []
    ,   langString= ["'" ~~ "'"]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "ABORT", "ABORTSESSION", "ABS", "ABSOLUTE", "ACCESS", "ACCESSIBLE", "ACCESS_LOCK", "ACCOUNT", "ACOS", "ACOSH", "ACTION", "ADD", "ADD_MONTHS", "ADMIN", "AFTER", "AGGREGATE", "ALIAS",
            "ALL", "ALLOCATE", "ALLOW", "ALTER", "ALTERAND", "AMP", "ANALYSE", "ANALYZE", "AND", "ANSIDATE", "ANY", "ARE", "ARRAY", "ARRAY_AGG", "ARRAY_EXISTS", "ARRAY_MAX_CARDINALITY", "AS",
            "ASC", "ASENSITIVE", "ASIN", "ASINH", "ASSERTION", "ASSOCIATE", "ASUTIME", "ASYMMETRIC", "AT", "ATAN", "ATAN2", "ATANH", "ATOMIC", "AUDIT", "AUTHORIZATION", "AUX", "AUXILIARY", "AVE",
            "AVERAGE", "AVG", "BACKUP", "BEFORE", "BEGIN", "BEGIN_FRAME", "BEGIN_PARTITION", "BETWEEN", "BIGINT", "BINARY", "BIT", "BLOB", "BOOLEAN", "BOTH", "BREADTH", "BREAK", "BROWSE", "BT",
            "BUFFERPOOL", "BULK", "BUT", "BY", "BYTE", "BYTEINT", "BYTES", "CALL", "CALLED", "CAPTURE", "CARDINALITY", "CASCADE", "CASCADED", "CASE", "CASESPECIFIC", "CASE_N", "CAST", "CATALOG", "CCSID",
            "CD", "CEIL", "CEILING", "CHANGE", "CHAR", "CHAR2HEXINT", "CHARACTER", "CHARACTERS", "CHARACTER_LENGTH", "CHARS", "CHAR_LENGTH", "CHECK", "CHECKPOINT", "CLASS", "CLASSIFIER", "CLOB", "CLONE",
            "CLOSE", "CLUSTER", "CLUSTERED", "CM", "COALESCE", "COLLATE", "COLLATION", "COLLECT", "COLLECTION", "COLLID", "COLUMN", "COLUMN_VALUE", "COMMENT", "COMMIT", "COMPLETION", "COMPRESS",
            "COMPUTE", "CONCAT", "CONCURRENTLY", "CONDITION", "CONNECT", "CONNECTION", "CONSTRAINT", "CONSTRAINTS", "CONSTRUCTOR", "CONTAINS", "CONTAINSTABLE", "CONTENT", "CONTINUE", "CONVERT",
            "CONVERT_TABLE_HEADER", "COPY", "CORR", "CORRESPONDING", "COS", "COSH", "COUNT", "COVAR_POP", "COVAR_SAMP", "CREATE", "CROSS", "CS", "CSUM", "CT", "CUBE", "CUME_DIST", "CURRENT",
            "CURRENT_CATALOG", "CURRENT_DATE", "CURRENT_DEFAULT_TRANSFORM_GROUP", "CURRENT_LC_CTYPE", "CURRENT_PATH", "CURRENT_ROLE", "CURRENT_ROW", "CURRENT_SCHEMA",
            "CURRENT_SERVER", "CURRENT_TIME", "CURRENT_TIMESTAMP", "CURRENT_TIMEZONE", "CURRENT_TRANSFORM_GROUP_FOR_TYPE", "CURRENT_USER", "CURRVAL", "CURSOR", "CV", "CYCLE", "DATA", "DATABASE",
            "DATABASES", "DATABLOCKSIZE", "DATE", "DATEFORM", "DAY", "DAYS", "DAY_HOUR", "DAY_MICROSECOND", "DAY_MINUTE", "DAY_SECOND", "DBCC", "DBINFO", "DEALLOCATE", "DEC",
            "DECFLOAT", "DECIMAL", "DECLARE", "DEFAULT", "DEFERRABLE", "DEFERRED", "DEFINE", "DEGREES", "DEL", "DELAYED", "DELETE", "DENSE_RANK", "DENY", "DEPTH", "DEREF", "DESC",
            "DESCRIBE", "DESCRIPTOR", "DESTROY", "DESTRUCTOR", "DETERMINISTIC", "DIAGNOSTIC", "DIAGNOSTICS", "DICTIONARY", "DISABLE", "DISABLED", "DISALLOW", "DISCONNECT", "DISK", "DISTINCT", "DISTINCTROW",
            "DISTRIBUTED", "DIV", "DO", "DOCUMENT", "DOMAIN", "DOUBLE", "DROP", "DSSIZE", "DUAL", "DUMP", "DYNAMIC", "EACH", "ECHO", "EDITPROC", "ELEMENT", "ELSE", "ELSEIF", "EMPTY", "ENABLED", "ENCLOSED",
            "ENCODING", "ENCRYPTION", "END", "END", "EXEC", "ENDING", "END_FRAME", "END_PARTITION", "EQ", "EQUALS", "ERASE", "ERRLVL", "ERROR", "ERRORFILES", "ERRORTABLES", "ESCAPE", "ESCAPED", "ET", "EVERY",
            "EXCEPT", "EXCEPTION", "EXCLUSIVE", "EXEC", "EXECUTE", "EXISTS", "EXIT", "EXP", "EXPLAIN", "EXTERNAL", "EXTRACT", "FALLBACK", "FALSE", "FASTEXPORT", "FENCED", "FETCH", "FIELDPROC", "FILE",
            "FILLFACTOR", "FILTER", "FINAL", "FIRST", "FIRST_VALUE", "FLOAT", "FLOAT4", "FLOAT8", "FLOOR", "FOR", "FORCE", "FOREIGN", "FORMAT", "FOUND", "FRAME_ROW", "FREE", "FREESPACE", "FREETEXT",
            "FREETEXTTABLE", "FREEZE", "FROM", "FULL", "FULLTEXT", "FUNCTION", "FUSION", "GE", "GENERAL", "GENERATED", "GET", "GIVE", "GLOBAL", "GO", "GOTO", "GRANT", "GRAPHIC", "GROUP",
            "GROUPING", "GROUPS", "GT", "HANDLER", "HASH", "HASHAMP", "HASHBAKAMP", "HASHBUCKET", "HASHROW", "HAVING", "HELP", "HIGH_PRIORITY", "HOLD", "HOLDLOCK", "HOST", "HOUR", "HOURS", "HOUR_MICROSECOND",
            "HOUR_MINUTE", "HOUR_SECOND", "IDENTIFIED", "IDENTITY", "IDENTITYCOL", "IDENTITY_INSERT", "IF", "IGNORE", "ILIKE", "IMMEDIATE", "IN", "INCLUSIVE", "INCONSISTENT", "INCREMENT", "INDEX", "INDICATOR", "INFILE", "INHERIT",
            "INITIAL", "INITIALIZE", "INITIALLY", "INITIATE", "INNER", "INOUT", "INPUT", "INS", "INSENSITIVE", "INSERT", "INSTEAD", "INT", "INT1", "INT2", "INT3", "INT4", "INT8", "INTEGER",
            "INTEGERDATE", "INTERSECT", "INTERSECTION", "INTERVAL", "INTO", "IO_AFTER_GTIDS", "IO_BEFORE_GTIDS", "IS", "ISNULL", "ISOBID", "ISOLATION", "ITERATE", "JAR", "JOIN", "JOURNAL", "JSON_ARRAY", "JSON_ARRAYAGG", "JSON_EXISTS",
            "JSON_OBJECT", "JSON_OBJECTAGG", "JSON_QUERY", "JSON_TABLE", "JSON_TABLE_PRIMITIVE", "JSON_VALUE", "KEEP", "KEY", "KEYS", "KILL", "KURTOSIS", "LABEL", "LAG", "LANGUAGE", "LARGE", "LAST", "LAST_VALUE", "LATERAL",
            "LC_CTYPE", "LE", "LEAD", "LEADING", "LEAVE", "LEFT", "LESS", "LEVEL", "LIKE", "LIKE_REGEX", "LIMIT", "LINEAR", "LINENO", "LINES", "LISTAGG", "LN", "LOAD", "LOADING",
            "LOCAL", "LOCALE", "LOCALTIME", "LOCALTIMESTAMP", "LOCATOR", "LOCATORS", "LOCK", "LOCKING", "LOCKMAX", "LOCKSIZE", "LOG", "LOG10", "LOGGING", "LOGON", "LONG", "LONGBLOB", "LONGTEXT", "LOOP",
            "LOWER", "LOW_PRIORITY", "LT", "MACRO", "MAINTAINED", "MAP", "MASTER_BIND", "MASTER_SSL_VERIFY_SERVER_CERT", "MATCH", "MATCHES", "MATCH_NUMBER", "MATCH_RECOGNIZE", "MATERIALIZED", "MAVG", "MAX", "MAXEXTENTS", "MAXIMUM", "MAXVALUE",
            "MCHARACTERS", "MDIFF", "MEDIUMBLOB", "MEDIUMINT", "MEDIUMTEXT", "MEMBER", "MERGE", "METHOD", "MICROSECOND", "MICROSECONDS", "MIDDLEINT", "MIN", "MINDEX", "MINIMUM", "MINUS", "MINUTE", "MINUTES", "MINUTE_MICROSECOND",
            "MINUTE_SECOND", "MLINREG", "MLOAD", "MLSLABEL", "MOD", "MODE", "MODIFIES", "MODIFY", "MODULE", "MONITOR", "MONRESOURCE", "MONSESSION", "MONTH", "MONTHS", "MSUBSTR", "MSUM", "MULTISET", "NAMED",
            "NAMES", "NATIONAL", "NATURAL", "NCHAR", "NCLOB", "NE", "NESTED_TABLE_ID", "NEW", "NEW_TABLE", "NEXT", "NEXTVAL", "NO", "NOAUDIT", "NOCHECK", "NOCOMPRESS", "NONCLUSTERED", "NONE", "NORMALIZE",
            "NOT", "NOTNULL", "NOWAIT", "NO_WRITE_TO_BINLOG", "NTH_VALUE", "NTILE", "NULL", "NULLIF", "NULLIFZERO", "NULLS", "NUMBER", "NUMERIC", "NUMPARTS", "OBID", "OBJECT", "OBJECTS", "OCCURRENCES_REGEX", "OCTET_LENGTH",
            "OF", "OFF", "OFFLINE", "OFFSET", "OFFSETS", "OLD", "OLD_TABLE", "OMIT", "ON", "ONE", "ONLINE", "ONLY", "OPEN", "OPENDATASOURCE", "OPENQUERY", "OPENROWSET", "OPENXML", "OPERATION",
            "OPTIMIZATION", "OPTIMIZE", "OPTIMIZER_COSTS", "OPTION", "OPTIONALLY", "OR", "ORDER", "ORDINALITY", "ORGANIZATION", "OUT", "OUTER", "OUTFILE", "OUTPUT", "OVER", "OVERLAPS", "OVERLAY", "OVERRIDE", "PACKAGE",
            "PAD", "PADDED", "PARAMETER", "PARAMETERS", "PART", "PARTIAL", "PARTITION", "PARTITIONED", "PARTITIONING", "PASSWORD", "PATH", "PATTERN", "PCTFREE", "PER", "PERCENT", "PERCENTILE_CONT", "PERCENTILE_DISC", "PERCENT_RANK",
            "PERIOD", "PERM", "PERMANENT", "PIECESIZE", "PIVOT", "PLACING", "PLAN", "PORTION", "POSITION", "POSITION_REGEX", "POSTFIX", "POWER", "PRECEDES", "PRECISION", "PREFIX", "PREORDER", "PREPARE", "PRESERVE",
            "PREVVAL", "PRIMARY", "PRINT", "PRIOR", "PRIQTY", "PRIVATE", "PRIVILEGES", "PROC", "PROCEDURE", "PROFILE", "PROGRAM", "PROPORTIONAL", "PROTECTION", "PSID", "PTF", "PUBLIC", "PURGE", "QUALIFIED",
            "QUALIFY", "QUANTILE", "QUERY", "QUERYNO", "RADIANS", "RAISERROR", "RANDOM", "RANGE", "RANGE_N", "RANK", "RAW", "READ", "READS", "READTEXT", "READ_WRITE", "REAL", "RECONFIGURE", "RECURSIVE",
            "REF", "REFERENCES", "REFERENCING", "REFRESH", "REGEXP", "REGR_AVGX", "REGR_AVGY", "REGR_COUNT", "REGR_INTERCEPT", "REGR_R2", "REGR_SLOPE", "REGR_SXX", "REGR_SXY", "REGR_SYY", "RELATIVE", "RELEASE", "RENAME", "REPEAT",
            "REPLACE", "REPLICATION", "REPOVERRIDE", "REQUEST", "REQUIRE", "RESIGNAL", "RESOURCE", "RESTART", "RESTORE", "RESTRICT", "RESULT", "RESULT_SET_LOCATOR", "RESUME", "RET", "RETRIEVE", "RETURN", "RETURNING", "RETURNS",
            "REVALIDATE", "REVERT", "REVOKE", "RIGHT", "RIGHTS", "RLIKE", "ROLE", "ROLLBACK", "ROLLFORWARD", "ROLLUP", "ROUND_CEILING", "ROUND_DOWN", "ROUND_FLOOR", "ROUND_HALF_DOWN", "ROUND_HALF_EVEN", "ROUND_HALF_UP", "ROUND_UP", "ROUTINE",
            "ROW", "ROWCOUNT", "ROWGUIDCOL", "ROWID", "ROWNUM", "ROWS", "ROWSET", "ROW_NUMBER", "RULE", "RUN", "RUNNING", "SAMPLE", "SAMPLEID", "SAVE", "SAVEPOINT", "SCHEMA", "SCHEMAS", "SCOPE",
            "SCRATCHPAD", "SCROLL", "SEARCH", "SECOND", "SECONDS", "SECOND_MICROSECOND", "SECQTY", "SECTION", "SECURITY", "SECURITYAUDIT", "SEEK", "SEL", "SELECT", "SEMANTICKEYPHRASETABLE", "SEMANTICSIMILARITYDETAILSTABLE",
            "SEMANTICSIMILARITYTABLE", "SENSITIVE", "SEPARATOR", "SEQUENCE", "SESSION", "SESSION_USER", "SET", "SETRESRATE", "SETS", "SETSESSRATE", "SETUSER", "SHARE", "SHOW", "SHUTDOWN", "SIGNAL", "SIMILAR", "SIMPLE", "SIN", "SINH", "SIZE",
            "SKEW", "SKIP", "SMALLINT", "SOME", "SOUNDEX", "SOURCE", "SPACE", "SPATIAL", "SPECIFIC", "SPECIFICTYPE", "SPOOL", "SQL", "SQLEXCEPTION", "SQLSTATE", "SQLTEXT", "SQLWARNING", "SQL_BIG_RESULT", "SQL_CALC_FOUND_ROWS", "SQL_SMALL_RESULT",
            "SQRT", "SS", "SSL", "STANDARD", "START", "STARTING", "STARTUP", "STATE", "STATEMENT", "STATIC", "STATISTICS", "STAY", "STDDEV_POP", "STDDEV_SAMP", "STEPINFO", "STOGROUP", "STORED", "STORES",
            "STRAIGHT_JOIN", "STRING_CS", "STRUCTURE", "STYLE", "SUBMULTISET", "SUBSCRIBER", "SUBSET", "SUBSTR", "SUBSTRING", "SUBSTRING_REGEX", "SUCCEEDS", "SUCCESSFUL", "SUM", "SUMMARY", "SUSPEND", "SYMMETRIC", "SYNONYM", "SYSDATE",
            "SYSTEM", "SYSTEM_TIME", "SYSTEM_USER", "SYSTIMESTAMP", "TABLE", "TABLESAMPLE", "TABLESPACE", "TAN", "TANH", "TBL_CS", "TEMPORARY", "TERMINATE", "TERMINATED", "TEXTSIZE", "THAN", "THEN", "THRESHOLD", "TIME",
            "TIMESTAMP", "TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TINYBLOB", "TINYINT", "TINYTEXT", "TITLE", "TO", "TOP", "TRACE", "TRAILING", "TRAN", "TRANSACTION", "TRANSLATE", "TRANSLATE_CHK", "TRANSLATE_REGEX", "TRANSLATION", "TREAT",
            "TRIGGER", "TRIM", "TRIM_ARRAY", "TRUE", "TRUNCATE", "TRY_CONVERT", "TSEQUAL", "TYPE", "UC", "UESCAPE", "UID", "UNDEFINED", "UNDER", "UNDO", "UNION", "UNIQUE", "UNKNOWN", "UNLOCK",
            "UNNEST", "UNPIVOT", "UNSIGNED", "UNTIL", "UPD", "UPDATE", "UPDATETEXT", "UPPER", "UPPERCASE", "USAGE", "USE", "USER", "USING", "UTC_DATE", "UTC_TIME", "UTC_TIMESTAMP", "VALIDATE", "VALIDPROC",
            "VALUE", "VALUES", "VALUE_OF", "VARBINARY", "VARBYTE", "VARCHAR", "VARCHAR2", "VARCHARACTER", "VARGRAPHIC", "VARIABLE", "VARIADIC", "VARIANT", "VARYING", "VAR_POP", "VAR_SAMP", "VCAT", "VERBOSE", "VERSIONING",
            "VIEW", "VIRTUAL", "VOLATILE", "VOLUMES", "WAIT", "WAITFOR", "WHEN", "WHENEVER", "WHERE", "WHILE", "WIDTH_BUCKET", "WINDOW", "WITH", "WITHIN", "WITHIN_GROUP", "WITHOUT", "WLM", "WORK", "WRITE", "WRITETEXT", "XMLCAST",
            "XMLEXISTS", "XMLNAMESPACES", "XOR", "YEAR", "YEARS", "YEAR_MONTH", "ZEROFILL", "ZEROIFNULL", "ZONE"
        ]
    })
    ,  (Tcl,       LanguageInfo {
        langExtensions = [Ext "tcl", Ext "tk"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langResKeywords = keywords []
    ,   langValidIdentifierChars = (isAlpha, isAlphaNum)
    })
   ,  (Text,  LanguageInfo {
       langExtensions = [
           Ext "txt", Ext "md", Ext "markdown", Ext "mdown", Ext "mkdn", Ext "mkd", Ext "mdwn",
           Ext "mdtxt", Ext "mdtext", Ext "text", Name "README", Name "INSTALL", Name "VERSION",
           Name "LICENSE", Name "AUTHORS", Name "CHANGELOG", Name "go.sum"
       ]
   ,   langComment = []
   ,   langChar = []
   ,   langString = []
   ,   langRawString = []
   ,   langValidIdentifierChars = (const False, const False)
   ,   langResKeywords = keywords []
   })
    ,  (VHDL,      LanguageInfo {
        langExtensions = [Ext "vhd", Ext "vhdl"]
    ,   langComment = ["--" ~~ "\n"]
    ,   langChar = ["'" ~~ "'"]
    ,   langString = ["\"" ~~ "\""]
    ,   langRawString = []
    ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
    ,   langResKeywords = keywords [
            "abs", "access", "after", "alias", "all", "and", "architecture", "array", "assert", "attribute", "begin", "block", "body",
            "buffer", "bus", "case", "component", "configuration", "constant", "disconnect", "downto", "else", "elsif", "end", "entity",
            "exit", "file", "for", "function", "generate", "generic", "group", "guarded", "if", "impure", "in", "inertial", "inout", "is",
            "label", "library", "linkage", "literal", "loop", "map", "mod", "nand", "new", "next", "nor", "not", "null", "of", "on", "open",
            "or", "others", "out", "package", "port", "postponed", "procedure", "process", "pure", "range", "record", "register", "reject",
            "return", "rol", "ror", "select", "severity", "signal", "shared", "sla", "sli", "sra", "srl", "subtype", "then", "to", "transport",
            "type", "unaffected", "units", "until", "use", "variable", "wait", "when", "while", "with", "xnor", "xor"
        ]
    })
   ,  (Verilog,   LanguageInfo {
       langExtensions = [Ext "v", Ext "vh", Ext "sv"]
   ,   langComment = ["/*" ~~ "*/", "//" ~~ "\n"]
   ,   langChar = []
   ,   langString = ["\"" ~~ "\""]
   ,   langRawString = []
   ,   langValidIdentifierChars = (isAlpha_, isAlphaNum_)
   ,   langResKeywords = keywords [
           "always", "end", "ifnone", "or", "rpmos", "tranif1", "and", "endcase", "initial", "output", "rtran", "tri", "assign", "endmodule",
           "inout", "parameter", "rtranif0", "tri0", "begin", "endfunction", "input", "pmos", "rtranif1", "tri1", "buf", "endprimitive", "integer",
           "posedge", "scalared", "triand", "bufif0", "endspecify", "join", "primitive", "small", "trior", "bufif1", "endtable", "large", "pull0",
           "specify", "trireg", "case", "endtask", "macromodule", "pull1", "specparam", "vectored", "casex", "event", "medium", "pullup", "strong0",
           "wait", "casez", "for", "module", "pulldown", "strong1", "wand", "cmos", "force", "nand", "rcmos", "supply0", "weak0", "deassign", "forever",
           "negedge", "real", "supply1", "weak1", "default", "for", "nmos", "realtime", "table", "while", "defparam", "function", "nor", "reg", "task",
           "wire", "disable", "highz0", "not", "release", "time", "wor", "edge", "highz1", "notif0", "repeat", "tran", "xnor", "else", "if", "notif1",
           "rnmos", "tranif0", "xor"
       ]
   })
    ,  (Yaml,      LanguageInfo {
        langExtensions = [Ext "yaml", Ext "yml"]
    ,   langComment = ["#" ~~ "\n"]
    ,   langChar = []
    ,   langString = ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langRawString = []
    ,   langValidIdentifierChars = (const False, const False)
    ,   langResKeywords = keywords []
    })
    ]


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