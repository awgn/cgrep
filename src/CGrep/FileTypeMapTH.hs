{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
--
-- Copyright (c) 2013-2025 Nicola Bonelli <nicola@larthia.com>
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module CGrep.FileTypeMapTH (
    fileTypeInfoMap,
    mkContextFilterFn,
    fileTypeLookup,
    fileTypeInfoLookup,
    dumpFileTypeInfoMap,
) where

import CGrep.ContextFilter (
    ContextFilter,
    FilterFunction,
    isContextFilterAll,
    mkParConfig,
    runContextFilter,
 )

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_)
import Language.Haskell.TH.Syntax (lift)
import Options (Options (Options, code_only, force_type, hdr_only, keyword))
import qualified System.OsPath as OS

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map

import CGrep.FileKind
import CGrep.FileType (FileSelector (..), FileType (..), ext, hdr, name)
import CGrep.FileTypeMap

import qualified Data.Text as T

fileTypeInfoMap :: FileTypeInfoMap
fileTypeInfoMap =
    $( lift $
        FileTypeInfoMap $
            Map.fromList
                [
                    ( Agda
                    , FileTypeInfo
                        { ftSelector = [ext ".agda", ext ".lagda"]
                        , ftKind = KindLanguage
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (AgdaIdent, AgdaIdent)
                        , ftKeywords =
                            reserved
                                [ "abstract"
                                , "codata"
                                , "constructor"
                                , "data"
                                , "eta-equality"
                                , "field"
                                , "forall"
                                , "hiding"
                                , "import"
                                , "in"
                                , "inductive"
                                , "infix"
                                , "infixl"
                                , "infixr"
                                , "instance"
                                , "let"
                                , "module"
                                , "mutual"
                                , "no-eta-equality"
                                , "open"
                                , "pattern"
                                , "postulate"
                                , "primitive"
                                , "private"
                                , "public"
                                , "quoteContext"
                                , "quoteGoal"
                                , "record"
                                , "renaming"
                                , "rewrite"
                                , "Set"
                                , "syntax"
                                , "tactic"
                                , "using"
                                , "where"
                                , "with"
                                ]
                        }
                    )
                ,
                    ( Assembly
                    , FileTypeInfo
                        { ftSelector = [ext ".s", ext ".S", ext ".asm", ext ".ASM"]
                        , ftKind = KindLanguage
                        , ftComment = ["#" ~~ "\n", ";" ~~ "\n", "|" ~~ "\n", "!" ~~ "\n", "/*" ~~ "*/"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Awk
                    , FileTypeInfo
                        { ftSelector = [ext ".awk", ext ".mawk", ext ".gawk"]
                        , ftKind = KindScript
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n", "#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "BEGIN"
                                , "END"
                                , "if"
                                , "else"
                                , "while"
                                , "do"
                                , "for"
                                , "in"
                                , "break"
                                , "continue"
                                , "delete"
                                , "next"
                                , "nextfile"
                                , "function"
                                , "func"
                                , "exit"
                                ]
                        }
                    )
                ,
                    ( Bash
                    , FileTypeInfo
                        { ftSelector = [ext ".sh", ext ".bash"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "if"
                                , "then"
                                , "elif"
                                , "else"
                                , "fi"
                                , "time"
                                , "for"
                                , "in"
                                , "until"
                                , "while"
                                , "do"
                                , "done"
                                , "case"
                                , "esac"
                                , "coproc"
                                , "select"
                                , "function"
                                ]
                        }
                    )
                ,
                    ( C
                    , FileTypeInfo
                        { ftSelector = [ext ".c", ext ".C", hdr ".inc"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["R\"" ~~ "\""]
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            types
                                [ "int"
                                , "short"
                                , "long"
                                , "float"
                                , "double"
                                , "char"
                                , "bool"
                                , "int8_t"
                                , "int16_t"
                                , "int32_t"
                                , "int64_t"
                                , "int_fast8_t"
                                , "int_fast16_t"
                                , "int_fast32_t"
                                , "int_fast64_t"
                                , "int_least8_t"
                                , "int_least16_t"
                                , "int_least32_t"
                                , "int_least64_t"
                                , "intmax_t"
                                , "intptr_t"
                                , "uint8_t"
                                , "uint16_t"
                                , "uint32_t"
                                , "uint64_t"
                                , "uint_fast8_t"
                                , "uint_fast16_t"
                                , "uint_fast32_t"
                                , "uint_fast64_t"
                                , "uint_least8_t"
                                , "uint_least16_t"
                                , "uint_least32_t"
                                , "uint_least64_t"
                                , "uintmax_t"
                                , "uintptr_t"
                                ]
                                <> reserved
                                    [ "auto"
                                    , "break"
                                    , "case"
                                    , "const"
                                    , "continue"
                                    , "default"
                                    , "do"
                                    , "else"
                                    , "enum"
                                    , "extern"
                                    , "for"
                                    , "goto"
                                    , "if"
                                    , "inline"
                                    , "register"
                                    , "restrict"
                                    , "return"
                                    , "signed"
                                    , "sizeof"
                                    , "static"
                                    , "struct"
                                    , "switch"
                                    , "typedef"
                                    , "union"
                                    , "unsigned"
                                    , "void"
                                    , "volatile"
                                    , "while"
                                    , "_Alignas"
                                    , "_Alignof"
                                    , "_Atomic"
                                    , "_Bool"
                                    , "_Complex"
                                    , "_Decimal128"
                                    , "_Decimal32"
                                    , "_Decimal64"
                                    , "_Generic"
                                    , "_Imaginary"
                                    , "_Noreturn"
                                    , "_Static_assert"
                                    , "_Thread_local"
                                    , "if"
                                    , "elif"
                                    , "else"
                                    , "endif"
                                    , "ifdef"
                                    , "ifndef"
                                    , "define"
                                    , "undef"
                                    , "include"
                                    , "line"
                                    , "error"
                                    , "pragma"
                                    , "defined"
                                    , "__has_c_attribute"
                                    , "_Pragma"
                                    , "asm"
                                    , "fortran"
                                    ]
                        }
                    )
                ,
                    ( Cpp
                    , FileTypeInfo
                        { ftSelector =
                            [ ext ".cpp"
                            , ext ".CPP"
                            , ext ".cxx"
                            , ext ".cc"
                            , ext ".cp"
                            , ext ".c++"
                            , ext ".cu"
                            , ext ".cuh"
                            , hdr ".tcc"
                            , hdr ".h"
                            , hdr ".H"
                            , hdr ".hpp"
                            , hdr ".ipp"
                            , hdr ".HPP"
                            , hdr ".hxx"
                            , hdr ".hh"
                            , hdr ".hp"
                            , hdr ".h++"
                            ]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["R\"(" ~~ ")\"", "R\"-(" ~~ ")-\"", "R\"--(" ~~ ")--\""]
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            types
                                [ "int"
                                , "short"
                                , "long"
                                , "float"
                                , "double"
                                , "char"
                                , "bool"
                                , "int8_t"
                                , "int16_t"
                                , "int32_t"
                                , "int64_t"
                                , "int_fast8_t"
                                , "int_fast16_t"
                                , "int_fast32_t"
                                , "int_fast64_t"
                                , "int_least8_t"
                                , "int_least16_t"
                                , "int_least32_t"
                                , "int_least64_t"
                                , "intmax_t"
                                , "intptr_t"
                                , "uint8_t"
                                , "uint16_t"
                                , "uint32_t"
                                , "uint64_t"
                                , "uint_fast8_t"
                                , "uint_fast16_t"
                                , "uint_fast32_t"
                                , "uint_fast64_t"
                                , "uint_least8_t"
                                , "uint_least16_t"
                                , "uint_least32_t"
                                , "uint_least64_t"
                                , "uintmax_t"
                                , "uintptr_t"
                                , "nullptr_t"
                                ]
                                <> reserved
                                    [ "alignas"
                                    , "alignof"
                                    , "and"
                                    , "and_eq"
                                    , "asm"
                                    , "atomic_cancel"
                                    , "atomic_commit"
                                    , "atomic_noexcept"
                                    , "auto"
                                    , "bitand"
                                    , "bitor"
                                    , "break"
                                    , "case"
                                    , "catch"
                                    , "class"
                                    , "compl"
                                    , "concept"
                                    , "const"
                                    , "consteval"
                                    , "constexpr"
                                    , "constinit"
                                    , "const_cast"
                                    , "continue"
                                    , "co_await"
                                    , "co_return"
                                    , "co_yield"
                                    , "decltype"
                                    , "default"
                                    , "delete"
                                    , "do"
                                    , "dynamic_cast"
                                    , "else"
                                    , "enum"
                                    , "explicit"
                                    , "export"
                                    , "extern"
                                    , "false"
                                    , "for"
                                    , "friend"
                                    , "goto"
                                    , "if"
                                    , "inline"
                                    , "mutable"
                                    , "namespace"
                                    , "new"
                                    , "noexcept"
                                    , "not"
                                    , "not_eq"
                                    , "nullptr"
                                    , "operator"
                                    , "or"
                                    , "or_eq"
                                    , "private"
                                    , "protected"
                                    , "public"
                                    , "reflexpr"
                                    , "register"
                                    , "reinterpret_cast"
                                    , "requires"
                                    , "return"
                                    , "signed"
                                    , "sizeof"
                                    , "static"
                                    , "static_assert"
                                    , "static_cast"
                                    , "struct"
                                    , "switch"
                                    , "synchronized"
                                    , "template"
                                    , "this"
                                    , "thread_local"
                                    , "throw"
                                    , "true"
                                    , "try"
                                    , "typedef"
                                    , "typeid"
                                    , "typename"
                                    , "union"
                                    , "unsigned"
                                    , "using"
                                    , "virtual"
                                    , "void"
                                    , "volatile"
                                    , "wchar_t"
                                    , "while"
                                    , "xor"
                                    , "xor_eq"
                                    , "final"
                                    , "override"
                                    , "transaction_safe"
                                    , "transaction_safe_dynamic"
                                    , "import"
                                    , "module"
                                    , "elif"
                                    , "endif"
                                    , "ifdef"
                                    , "ifndef"
                                    , "define"
                                    , "undef"
                                    , "include"
                                    , "line"
                                    , "error"
                                    , "pragma"
                                    , "defined"
                                    , "__has_include"
                                    , "__has_cpp_attribute"
                                    , "export"
                                    , "import"
                                    , "module"
                                    ]
                        }
                    )
                ,
                    ( CMake
                    , FileTypeInfo
                        { ftSelector = [name "CMakeLists.txt", ext ".cmake"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNumDash_)
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Cabal
                    , FileTypeInfo
                        { ftSelector = [ext ".cabal"]
                        , ftKind = KindConfig
                        , ftComment = ["--" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_')
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Chapel
                    , FileTypeInfo
                        { ftSelector = [ext ".chpl"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''"]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNumDollar_)
                        , ftKeywords =
                            types
                                [ "void"
                                , "nothing"
                                , "bool"
                                , "int"
                                , "uint"
                                , "real"
                                , "imag"
                                , "complex"
                                , "string"
                                , "bytes"
                                ]
                                <> reserved
                                    [ "align"
                                    , "as"
                                    , "atomic"
                                    , "begin"
                                    , "bool"
                                    , "borrowed"
                                    , "break"
                                    , "by"
                                    , "catch"
                                    , "class"
                                    , "cobegin"
                                    , "coforall"
                                    , "config"
                                    , "const"
                                    , "continue"
                                    , "defer"
                                    , "delete"
                                    , "dmapped"
                                    , "do"
                                    , "domain"
                                    , "else"
                                    , "enum"
                                    , "except"
                                    , "export"
                                    , "extern"
                                    , "false"
                                    , "for"
                                    , "forall"
                                    , "forwarding"
                                    , "if"
                                    , "in"
                                    , "index"
                                    , "inline"
                                    , "inout"
                                    , "iter"
                                    , "label"
                                    , "let"
                                    , "lifetime"
                                    , "local"
                                    , "locale"
                                    , "module"
                                    , "new"
                                    , "nil"
                                    , "noinit"
                                    , "on"
                                    , "only"
                                    , "otherwise"
                                    , "out"
                                    , "override"
                                    , "owned"
                                    , "param"
                                    , "private"
                                    , "prototype"
                                    , "proc"
                                    , "public"
                                    , "record"
                                    , "reduce"
                                    , "ref"
                                    , "require"
                                    , "return"
                                    , "scan"
                                    , "select"
                                    , "serial"
                                    , "shared"
                                    , "single"
                                    , "sparse"
                                    , "subdomain"
                                    , "sync"
                                    , "then"
                                    , "this"
                                    , "throw"
                                    , "throws"
                                    , "true"
                                    , "try"
                                    , "type"
                                    , "union"
                                    , "unmanaged"
                                    , "use"
                                    , "var"
                                    , "when"
                                    , "where"
                                    , "while"
                                    , "with"
                                    , "yield"
                                    , "zip"
                                    ]
                        }
                    )
                ,
                    ( Clojure
                    , FileTypeInfo
                        { ftSelector = [ext ".clj", ext ".cljs", ext ".cljc", ext ".edn"]
                        , ftKind = KindLanguage
                        , ftComment = [";" ~~ "\n", ";;" ~~ "\n", ";;;" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (ClojureIdentStart, ClojureIdentCont)
                        , ftKeywords =
                            types
                                [ "nil"
                                , "boolean"
                                , "char"
                                , "byte"
                                , "short"
                                , "int"
                                , "long"
                                , "float"
                                , "double"
                                ]
                                <> reserved
                                    [ "and"
                                    , "let"
                                    , "def"
                                    , "defn"
                                    , "if"
                                    , "else"
                                    , "do"
                                    , "quote"
                                    , "var"
                                    , "fn"
                                    , "loop"
                                    , "recur"
                                    , "throw"
                                    , "try"
                                    , "monitor-enter"
                                    , "monitor-exit"
                                    ]
                        }
                    )
                ,
                    ( Coffee
                    , FileTypeInfo
                        { ftSelector = [ext ".coffee"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n", "###" ~~ "###"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = ["'''" ~~ "'''", "\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (UnicodeDollar_, UnicodeNumDollar_)
                        , ftKeywords =
                            reserved
                                [ "case"
                                , "default"
                                , "function"
                                , "var"
                                , "void"
                                , "with"
                                , "const"
                                , "let"
                                , "enum"
                                , "export"
                                , "import"
                                , "native"
                                , "__hasProp"
                                , "__extends"
                                , "__slice"
                                , "__bind"
                                , "__indexOf"
                                , "implements"
                                , "interface"
                                , "package"
                                , "private"
                                , "protected"
                                , "public"
                                , "static"
                                , "yield"
                                , "true"
                                , "false"
                                , "null"
                                , "this"
                                , "new"
                                , "delete"
                                , "typeof"
                                , "in"
                                , "arguments"
                                , "eval"
                                , "instanceof"
                                , "return"
                                , "throw"
                                , "break"
                                , "continue"
                                , "debugger"
                                , "if"
                                , "else"
                                , "switch"
                                , "for"
                                , "while"
                                , "do"
                                , "try"
                                , "catch"
                                , "finally"
                                , "class"
                                , "extends"
                                , "super"
                                , "undefined"
                                , "then"
                                , "unless"
                                , "until"
                                , "loop"
                                , "of"
                                , "by"
                                , "when"
                                , "and"
                                , "or"
                                , "is"
                                , "isnt"
                                , "not"
                                , "yes"
                                , "no"
                                , "on"
                                , "off"
                                ]
                        }
                    )
                ,
                    ( Conf
                    , FileTypeInfo
                        { ftSelector = [ext ".config", ext ".conf", ext ".cfg", ext ".doxy"]
                        , ftKind = KindConfig
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Csharp
                    , FileTypeInfo
                        { ftSelector = [ext ".cs", ext ".CS"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (CSharpIdentStart, CSharpIdentCont)
                        , ftKeywords =
                            types
                                [ "bool"
                                , "byte"
                                , "sbyte"
                                , "char"
                                , "decimal"
                                , "double"
                                , "float"
                                , "int"
                                , "uint"
                                , "nint"
                                , "nuint"
                                , "long"
                                , "ulong"
                                , "short"
                                , "ushort"
                                ]
                                <> reserved
                                    [ "abstract"
                                    , "as"
                                    , "base"
                                    , "break"
                                    , "case"
                                    , "catch"
                                    , "checked"
                                    , "class"
                                    , "const"
                                    , "continue"
                                    , "default"
                                    , "delegate"
                                    , "do"
                                    , "else"
                                    , "enum"
                                    , "event"
                                    , "explicit"
                                    , "extern"
                                    , "false"
                                    , "finally"
                                    , "fixed"
                                    , "for"
                                    , "foreach"
                                    , "goto"
                                    , "if"
                                    , "implicit"
                                    , "in"
                                    , "interface"
                                    , "internal"
                                    , "is"
                                    , "lock"
                                    , "namespace"
                                    , "new"
                                    , "null"
                                    , "object"
                                    , "operator"
                                    , "out"
                                    , "override"
                                    , "params"
                                    , "private"
                                    , "protected"
                                    , "public"
                                    , "readonly"
                                    , "ref"
                                    , "return"
                                    , "sealed"
                                    , "sizeof"
                                    , "stackalloc"
                                    , "static"
                                    , "string"
                                    , "struct"
                                    , "switch"
                                    , "this"
                                    , "throw"
                                    , "true"
                                    , "try"
                                    , "typeof"
                                    , "unchecked"
                                    , "unsafe"
                                    , "using"
                                    , "virtual"
                                    , "void"
                                    , "volatile"
                                    , "while"
                                    ]
                        }
                    )
                ,
                    ( Csh
                    , FileTypeInfo
                        { ftSelector = [ext ".csh", ext ".tcsh"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "alias"
                                , "alloc"
                                , "bg"
                                , "bindkey"
                                , "break"
                                , "breaksw"
                                , "built-ins"
                                , "bye"
                                , "case"
                                , "cd"
                                , "chdir"
                                , "complete"
                                , "continue"
                                , "default"
                                , "dirs"
                                , "echo"
                                , "echotc"
                                , "else"
                                , "end"
                                , "endif"
                                , "endsw"
                                , "eval"
                                , "exec"
                                , "exit"
                                , "fg"
                                , "filetest"
                                , "foreach"
                                , "glob"
                                , "goto"
                                , "hashstat"
                                , "history"
                                , "hup"
                                , "if"
                                , "jobs"
                                , "kill"
                                , "limit"
                                , "log"
                                , "login"
                                , "logout"
                                , "ls-F"
                                , "newgrp"
                                , "nice"
                                , "nohup"
                                , "notify"
                                , "onintr"
                                , "popd"
                                , "printenv"
                                , "pushd"
                                , "rehash"
                                , "repeat"
                                , "sched"
                                , "set"
                                , "setenv"
                                , "settc"
                                , "setty"
                                , "shift"
                                , "source"
                                , "stop"
                                , "suspend"
                                , "switch"
                                , "telltc"
                                , "time"
                                , "umask"
                                , "unalias"
                                , "uncomplete"
                                , "unhash"
                                , "unlimit"
                                , "unset"
                                , "unsetenv"
                                , "wait"
                                , "watchlog"
                                , "where"
                                , "which"
                                , "while"
                                ]
                        }
                    )
                ,
                    ( Css
                    , FileTypeInfo
                        { ftSelector = [ext ".css"]
                        , ftKind = KindMarkup
                        , ftComment = ["/*" ~~ "*/"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (AlphaDash_, AlphaNumDash_)
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Cql
                    , FileTypeInfo
                        { ftSelector = [ext ".cql"]
                        , ftKind = KindLanguage
                        , ftComment = ["--" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "ASCII"
                                , "ADD"
                                , "AGGREGATE"
                                , "ALL"
                                , "ALLOW"
                                , "ALTER"
                                , "AND"
                                , "ANY"
                                , "APPLY"
                                , "AS"
                                , "ASC"
                                , "AUTHORIZE"
                                , "BATCH"
                                , "BEGIN"
                                , "BIGINT"
                                , "BY"
                                , "CLUSTERING"
                                , "COLUMNFAMILY"
                                , "COMPACT"
                                , "CONSISTENCY"
                                , "COUNT"
                                , "COUNTER"
                                , "CREATE"
                                , "CUSTOM"
                                , "DATE"
                                , "DECIMAL"
                                , "DELETE"
                                , "DESC"
                                , "DISTINCT"
                                , "DOUBLE"
                                , "DROP"
                                , "EACH_QUORUM"
                                , "ENTRIES"
                                , "EXISTS"
                                , "FILTERING"
                                , "FLOAT"
                                , "FROM"
                                , "FROZEN"
                                , "FULL"
                                , "GRANT"
                                , "IF"
                                , "IN"
                                , "INDEX"
                                , "INET"
                                , "INFINITY"
                                , "INSERT"
                                , "INT"
                                , "INTO"
                                , "KEY"
                                , "KEYSPACE"
                                , "KEYSPACES"
                                , "LEVEL"
                                , "LIMIT"
                                , "LIST"
                                , "LOCAL_ONE"
                                , "LOCAL_QUORUM"
                                , "MAP"
                                , "MATERIALIZED"
                                , "MODIFY"
                                , "NAN"
                                , "OF"
                                , "ON"
                                , "ONE"
                                , "ORDER"
                                , "PARTITION"
                                , "PASSWORD"
                                , "PER"
                                , "PERMISSION"
                                , "PERMISSIONS"
                                , "PRIMARY"
                                , "QUORUM"
                                , "RECURSIVE"
                                , "RENAME"
                                , "REVOKE"
                                , "SCHEMA"
                                , "SELECT"
                                , "SET"
                                , "SMALLINT"
                                , "STATIC"
                                , "STORAGE"
                                , "SUPERUSER"
                                , "T"
                                , "TABLE"
                                , "TEXT"
                                , "THREE"
                                , "TIME"
                                , "TIMESTAMP"
                                , "TIMEUUID"
                                , "TINYINT"
                                , "TO"
                                , "TOKEN"
                                , "TRUNCATE"
                                , "TTL"
                                , "TUPLE"
                                , "TWO"
                                , "TYPE"
                                , "UNLOGGED"
                                , "UPDATE"
                                , "USE"
                                , "USER"
                                , "USERS"
                                , "USING"
                                , "UUID"
                                , "VALUES"
                                , "VARCHAR"
                                , "VARINT"
                                , "VIEW"
                                , "WHERE"
                                , "WITH"
                                , "WRITETIME"
                                ]
                        }
                    )
                ,
                    ( D
                    , FileTypeInfo
                        { ftSelector = [ext ".d", ext ".D", hdr ".di"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["r\"" ~~ "\"", "`" ~~ "`"]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "bool"
                                , "byte"
                                , "ubyte"
                                , "short"
                                , "ushort"
                                , "int"
                                , "uint"
                                , "long"
                                , "ulong"
                                , "cent"
                                , "ucent"
                                , "char"
                                , "wchar"
                                , "dchar"
                                , "float"
                                , "double"
                                , "real"
                                , "ifloat"
                                , "idouble"
                                , "ireal"
                                , "cfloat"
                                , "cdouble"
                                , "creal"
                                , "void"
                                ]
                                <> reserved
                                    [ "abstract"
                                    , "alias"
                                    , "align"
                                    , "asm"
                                    , "assert"
                                    , "auto"
                                    , "body"
                                    , "break"
                                    , "case"
                                    , "cast"
                                    , "catch"
                                    , "class"
                                    , "const"
                                    , "continue"
                                    , "debug"
                                    , "default"
                                    , "delegate"
                                    , "delete"
                                    , "deprecated"
                                    , "do"
                                    , "else"
                                    , "enum"
                                    , "export"
                                    , "extern"
                                    , "false"
                                    , "final"
                                    , "finally"
                                    , "for"
                                    , "foreach"
                                    , "foreach_reverse"
                                    , "function"
                                    , "goto"
                                    , "if"
                                    , "immutable"
                                    , "import"
                                    , "in"
                                    , "inout"
                                    , "interface"
                                    , "invariant"
                                    , "is"
                                    , "lazy"
                                    , "macro"
                                    , "mixin"
                                    , "module"
                                    , "new"
                                    , "nothrow"
                                    , "null"
                                    , "out"
                                    , "override"
                                    , "package"
                                    , "pragma"
                                    , "private"
                                    , "protected"
                                    , "public"
                                    , "pure"
                                    , "ref"
                                    , "return"
                                    , "scope"
                                    , "shared"
                                    , "static"
                                    , "struct"
                                    , "super"
                                    , "switch"
                                    , "synchronized"
                                    , "template"
                                    , "this"
                                    , "throw"
                                    , "true"
                                    , "try"
                                    , "typeid"
                                    , "typeof"
                                    , "union"
                                    , "unittest"
                                    , "version"
                                    , "while"
                                    , "with"
                                    , "__FILE__"
                                    , "__FILE_FULL_PATH__"
                                    , "__MODULE__"
                                    , "__LINE__"
                                    , "__FUNCTION__"
                                    , "__PRETTY_FUNCTION__"
                                    , "__gshared"
                                    , "__traits"
                                    , "__vector"
                                    , "__parameters"
                                    ]
                        }
                    )
                ,
                    ( Dart
                    , FileTypeInfo
                        { ftSelector = [ext ".dart"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = ["'''" ~~ "'''", "\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types ["int", "double", "num", "bool", "null"]
                                <> reserved
                                    [ "assert"
                                    , "break"
                                    , "case"
                                    , "catch"
                                    , "class"
                                    , "const"
                                    , "continue"
                                    , "default"
                                    , "do"
                                    , "else"
                                    , "enum"
                                    , "extends"
                                    , "false"
                                    , "final"
                                    , "finally"
                                    , "for"
                                    , "if"
                                    , "in"
                                    , "is"
                                    , "new"
                                    , "rethrow"
                                    , "return"
                                    , "super"
                                    , "switch"
                                    , "this"
                                    , "throw"
                                    , "true"
                                    , "try"
                                    , "var"
                                    , "void"
                                    , "while"
                                    , "with"
                                    , "async"
                                    , "hide"
                                    , "on"
                                    , "show"
                                    , "sync"
                                    , "abstract"
                                    , "as"
                                    , "covariant"
                                    , "deferred"
                                    , "dynamic"
                                    , "export"
                                    , "extension"
                                    , "external"
                                    , "factory"
                                    , "function"
                                    , "get"
                                    , "implements"
                                    , "import"
                                    , "interface"
                                    , "library"
                                    , "mixin"
                                    , "operator"
                                    , "part"
                                    , "set"
                                    , "static"
                                    , "typedef"
                                    , "await"
                                    , "yield"
                                    ]
                        }
                    )
                ,
                    ( Dhall
                    , FileTypeInfo
                        { ftSelector = [ext ".dhall"]
                        , ftKind = KindConfig
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_')
                        , ftKeywords =
                            types ["Bool", "Natural", "Integer", "Double", "Text", "Date", "Time", "List", "Optional"]
                                <> reserved
                                    [ "if"
                                    , "then"
                                    , "else"
                                    , "toMap"
                                    , "with"
                                    , "merge"
                                    , "showConstructor"
                                    , "missing"
                                    , "as"
                                    , "using"
                                    , "let"
                                    , "assert"
                                    ]
                        }
                    )
                ,
                    ( Elixir
                    , FileTypeInfo
                        { ftSelector = [ext ".ex", ext ".exs"]
                        , ftKind = KindLanguage
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "integer"
                                , "float"
                                , "boolean"
                                , "atom"
                                , "binary"
                                , "string"
                                , "list"
                                , "tuple"
                                , "map"
                                ]
                                <> reserved
                                    [ "true"
                                    , "false"
                                    , "nil"
                                    , "when"
                                    , "and"
                                    , "or"
                                    , "not"
                                    , "in"
                                    , "fn"
                                    , "do"
                                    , "end"
                                    , "catch"
                                    , "rescue"
                                    , "after"
                                    , "else"
                                    ]
                        }
                    )
                ,
                    ( Elm
                    , FileTypeInfo
                        { ftSelector = [ext ".elm"]
                        , ftKind = KindLanguage
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_')
                        , ftKeywords =
                            types
                                [ "Bool"
                                , "Int"
                                , "Float"
                                , "String"
                                , "Char"
                                , "List"
                                , "Maybe"
                                , "Result"
                                , "Order"
                                , "Never"
                                , "Html"
                                , "msg"
                                , "Cmd"
                                , "Sub"
                                ]
                                <> reserved
                                    [ "type"
                                    , "alias"
                                    , "port"
                                    , "if"
                                    , "then"
                                    , "else"
                                    , "case"
                                    , "of"
                                    , "let"
                                    , "in"
                                    , "infix"
                                    , "left"
                                    , "right"
                                    , "non"
                                    , "module"
                                    , "import"
                                    , "exposing"
                                    , "as"
                                    , "where"
                                    , "effect"
                                    , "command"
                                    , "subscription"
                                    , "true"
                                    , "false"
                                    , "null"
                                    ]
                        }
                    )
                ,
                    ( Erlang
                    , FileTypeInfo
                        { ftSelector = [ext ".erl", ext ".ERL", ext ".hrl", ext ".HRL"]
                        , ftKind = KindLanguage
                        , ftComment = ["%" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            types
                                [ "Atom"
                                , "Integer"
                                , "Float"
                                , "Boolean"
                                , "String"
                                , "Tuple"
                                , "List"
                                , "Function"
                                , "Binary"
                                , "PID"
                                , "Port"
                                , "Reference"
                                , "Map"
                                ]
                                <> reserved
                                    [ "after"
                                    , "and"
                                    , "andalso"
                                    , "band"
                                    , "begin"
                                    , "bnot"
                                    , "bor"
                                    , "bsl"
                                    , "bsr"
                                    , "bxor"
                                    , "case"
                                    , "catch"
                                    , "cond"
                                    , "div"
                                    , "end"
                                    , "fun"
                                    , "if"
                                    , "let"
                                    , "not"
                                    , "of"
                                    , "or"
                                    , "orelse"
                                    , "receive"
                                    , "rem"
                                    , "try"
                                    , "when"
                                    , "xor"
                                    ]
                        }
                    )
                ,
                    ( Fish
                    , FileTypeInfo
                        { ftSelector = [ext ".fish"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "and"
                                , "argparse"
                                , "begin"
                                , "break"
                                , "builtin"
                                , "case"
                                , "command"
                                , "continue"
                                , "else"
                                , "end"
                                , "eval"
                                , "exec"
                                , "for"
                                , "function"
                                , "if"
                                , "not"
                                , "or"
                                , "read"
                                , "return"
                                , "set"
                                , "status"
                                , "string"
                                , "switch"
                                , "test"
                                , "time"
                                , "while"
                                ]
                        }
                    )
                ,
                    ( Fortran
                    , FileTypeInfo
                        { ftSelector =
                            [ ext ".f"
                            , ext ".for"
                            , ext ".ftn"
                            , ext ".F"
                            , ext ".FOR"
                            , ext ".FTN"
                            , ext ".fpp"
                            , ext ".FPP"
                            , ext ".f90"
                            , ext ".f95"
                            , ext ".f03"
                            , ext ".f08"
                            , ext ".F90"
                            , ext ".F95"
                            , ext ".F03"
                            , ext ".F08"
                            ]
                        , ftKind = KindLanguage
                        , ftComment = ["!" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha, AlphaNum_)
                        , ftKeywords =
                            types ["integer", "real", "complex", "logical", "character"]
                                <> reserved
                                    [ -- fortran77
                                      "assign"
                                    , "backspace"
                                    , "block"
                                    , "data"
                                    , "call"
                                    , "close"
                                    , "common"
                                    , "continue"
                                    , "data"
                                    , "dimension"
                                    , "do"
                                    , "else"
                                    , "else"
                                    , "if"
                                    , "end"
                                    , "endfile"
                                    , "endif"
                                    , "entry"
                                    , "equivalence"
                                    , "external"
                                    , "format"
                                    , "function"
                                    , "goto"
                                    , "if"
                                    , "implicit"
                                    , "inquire"
                                    , "intrinsic"
                                    , "open"
                                    , "parameter"
                                    , "pause"
                                    , "print"
                                    , "program"
                                    , "read"
                                    , "return"
                                    , "rewind"
                                    , "rewrite"
                                    , "save"
                                    , "stop"
                                    , "subroutine"
                                    , "then"
                                    , "write"
                                    , -- fortran 90
                                      "allocatable"
                                    , "allocate"
                                    , "case"
                                    , "contains"
                                    , "cycle"
                                    , "deallocate"
                                    , "elsewhere"
                                    , "exit?"
                                    , "include"
                                    , "interface"
                                    , "intent"
                                    , "module"
                                    , "namelist"
                                    , "nullify"
                                    , "only"
                                    , "operator"
                                    , "optional"
                                    , "pointer"
                                    , "private"
                                    , "procedure"
                                    , "public"
                                    , "recursive"
                                    , "result"
                                    , "select"
                                    , "sequence"
                                    , "target"
                                    , "use"
                                    , "while"
                                    , "where"
                                    , -- fortran 95
                                      "elemental"
                                    , "forall"
                                    , "pure"
                                    , -- fortran 03
                                      "abstract"
                                    , "associate"
                                    , "asynchronous"
                                    , "bind"
                                    , "class"
                                    , "deferred"
                                    , "enum"
                                    , "enumerator"
                                    , "extends"
                                    , "final"
                                    , "flush"
                                    , "generic"
                                    , "import"
                                    , "non_overridable"
                                    , "nopass"
                                    , "pass"
                                    , "protected"
                                    , "value"
                                    , "volatile"
                                    , "wait"
                                    , -- fortran 08
                                      "block"
                                    , "codimension"
                                    , "do"
                                    , "concurrent"
                                    , "contiguous"
                                    , "critical"
                                    , "error"
                                    , "stop"
                                    , "submodule"
                                    , "sync"
                                    , "all"
                                    , "sync"
                                    , "images"
                                    , "sync"
                                    , "memory"
                                    , "lock"
                                    , "unlock"
                                    ]
                        }
                    )
                ,
                    ( Fsharp
                    , FileTypeInfo
                        { ftSelector = [ext ".fs", ext ".fsx", ext ".fsi"]
                        , ftKind = KindLanguage
                        , ftComment = ["(*" ~~ "*)", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_')
                        , ftKeywords =
                            types
                                [ "bool"
                                , "byte"
                                , "sbyte"
                                , "int16"
                                , "uint16"
                                , "int"
                                , "uint"
                                , "int64"
                                , "uint64"
                                , "nativeint"
                                , "unativeint"
                                , "decimal"
                                , "float"
                                , "double"
                                , "float32"
                                , "single"
                                , "char"
                                , "string"
                                ]
                                <> reserved
                                    [ "abstract"
                                    , "and"
                                    , "as"
                                    , "assert"
                                    , "base"
                                    , "begin"
                                    , "class"
                                    , "default"
                                    , "delegate"
                                    , "do"
                                    , "done"
                                    , "downcast"
                                    , "downto"
                                    , "elif"
                                    , "else"
                                    , "end"
                                    , "exception"
                                    , "extern"
                                    , "FALSE"
                                    , "finally"
                                    , "fixed"
                                    , "for"
                                    , "fun"
                                    , "function"
                                    , "global"
                                    , "if"
                                    , "in"
                                    , "inherit"
                                    , "inline"
                                    , "interface"
                                    , "internal"
                                    , "lazy"
                                    , "let"
                                    , "let!"
                                    , "match"
                                    , "match!"
                                    , "member"
                                    , "module"
                                    , "mutable"
                                    , "namespace"
                                    , "new"
                                    , "not"
                                    , "null"
                                    , "of"
                                    , "open"
                                    , "or"
                                    , "override"
                                    , "private"
                                    , "public"
                                    , "rec"
                                    , "return"
                                    , "return!"
                                    , "select"
                                    , "static"
                                    , "struct"
                                    , "then"
                                    , "to"
                                    , "TRUE"
                                    , "try"
                                    , "type"
                                    , "upcast"
                                    , "use"
                                    , "use!"
                                    , "val"
                                    , "void"
                                    , "when"
                                    , "while"
                                    , "with"
                                    , "yield"
                                    , "yield!"
                                    , "const"
                                    , "asr"
                                    , "land"
                                    , "lor"
                                    , "lsl"
                                    , "lsr"
                                    , "lxor"
                                    , "mod"
                                    , "sig"
                                    , "break"
                                    , "checked"
                                    , "component"
                                    , "const"
                                    , "constraint"
                                    , "continue"
                                    , "event"
                                    , "external"
                                    , "include"
                                    , "mixin"
                                    , "parallel"
                                    , "process"
                                    , "protected"
                                    , "pure"
                                    , "sealed"
                                    , "tailcall"
                                    , "trait"
                                    , "virtual"
                                    ]
                        }
                    )
                ,
                    ( Go
                    , FileTypeInfo
                        { ftSelector = [ext ".go"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["`" ~~ "`"]
                        , ftIdentCharSet = Just (UnicodeXIDStart_, UnicodeNumXIDCont_)
                        , ftKeywords =
                            types
                                [ "int8"
                                , "int16"
                                , "int32"
                                , "int64"
                                , "uint8"
                                , "uint16"
                                , "uint32"
                                , "uint64"
                                , "int"
                                , "uint"
                                , "rune"
                                , "byte"
                                , "uintptr"
                                , "float32"
                                , "float64"
                                , "complex64"
                                , "complex128"
                                , "string"
                                , "error"
                                , "bool"
                                ]
                                <> reserved
                                    [ "break"
                                    , "default"
                                    , "func"
                                    , "interface"
                                    , "select"
                                    , "case"
                                    , "defer"
                                    , "go"
                                    , "map"
                                    , "struct"
                                    , "chan"
                                    , "else"
                                    , "goto"
                                    , "package"
                                    , "switch"
                                    , "const"
                                    , "fallthrough"
                                    , "if"
                                    , "range"
                                    , "type"
                                    , "continue"
                                    , "for"
                                    , "import"
                                    , "return"
                                    , "var"
                                    , "append"
                                    , "cap"
                                    , "close"
                                    , "copy"
                                    , "false"
                                    , "float32"
                                    , "float64"
                                    , "imag"
                                    , "iota"
                                    , "len"
                                    , "make"
                                    , "new"
                                    , "nil"
                                    , "panic"
                                    , "print"
                                    , "println"
                                    , "real"
                                    , "recover"
                                    , "true"
                                    ]
                        }
                    )
                ,
                    ( GoMod
                    , FileTypeInfo
                        { ftSelector = [name "go.mod"]
                        , ftKind = KindConfig
                        , ftComment = ["//" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords =
                            reserved
                                [ "module"
                                , "go"
                                , "require"
                                , "exclude"
                                , "replace"
                                , "retract"
                                ]
                        }
                    )
                ,
                    ( Haskell
                    , FileTypeInfo
                        { ftSelector = [ext ".hs", ext ".lhs", ext ".hsc"]
                        , ftKind = KindLanguage
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["[r|" ~~ "|]", "[q|" ~~ "|]", "[s|" ~~ "|]", "[here|" ~~ "|]", "[i|" ~~ "|]"]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_')
                        , ftKeywords =
                            types
                                [ "Bool"
                                , "Char"
                                , "String"
                                , "ByteString"
                                , "Text"
                                , "Int"
                                , "Int8"
                                , "Int16"
                                , "Int32"
                                , "Int64"
                                , "Word8"
                                , "Word16"
                                , "Word32"
                                , "Word64"
                                , "Integer"
                                , "Float"
                                , "Double"
                                , "Complex"
                                ]
                                <> reserved
                                    [ "as"
                                    , "case"
                                    , "class"
                                    , "data"
                                    , "default"
                                    , "deriving"
                                    , "do"
                                    , "else"
                                    , "hiding"
                                    , "if"
                                    , "import"
                                    , "in"
                                    , "infix"
                                    , "infixl"
                                    , "infixr"
                                    , "instance"
                                    , "let"
                                    , "module"
                                    , "newtype"
                                    , "of"
                                    , "qualified"
                                    , "then"
                                    , "type"
                                    , "where"
                                    , "forall"
                                    , "mdo"
                                    , "family"
                                    , "role"
                                    , "pattern"
                                    , "static"
                                    , "group"
                                    , "by"
                                    , "using"
                                    , "foreign"
                                    , "export"
                                    , "label"
                                    , "dynamic"
                                    , "safe"
                                    , "interruptible"
                                    , "unsafe"
                                    , "stdcall"
                                    , "ccall"
                                    , "capi"
                                    , "prim"
                                    , "javascript"
                                    , "rec"
                                    , "proc"
                                    ]
                        }
                    )
                ,
                    ( Html
                    , FileTypeInfo
                        { ftSelector = [ext ".htm", ext ".html"]
                        , ftKind = KindMarkup
                        , ftComment = ["<!--" ~~ "-->"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (HtmlIdentStart, HtmlIdentCont)
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Idris
                    , FileTypeInfo
                        { ftSelector = [ext ".idr", ext ".lidr"]
                        , ftKind = KindLanguage
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n", "|||" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_')
                        , ftKeywords =
                            types ["Int", "Integer", "Double", "Char", "String", "Ptr", "Bool"]
                                <> reserved
                                    [ "abstract"
                                    , "auto"
                                    , "codata"
                                    , "data"
                                    , "if"
                                    , "parameters"
                                    , "public"
                                    , "then"
                                    , "total"
                                    , "using"
                                    , "case"
                                    , "class"
                                    , "concrete"
                                    , "covering"
                                    , "default"
                                    , "do"
                                    , "else"
                                    , "export"
                                    , "failing"
                                    , "forall"
                                    , "implementation"
                                    , "implicit"
                                    , "import"
                                    , "impossible"
                                    , "in"
                                    , "incomplete"
                                    , "instance"
                                    , "interface"
                                    , "let"
                                    , "module"
                                    , "mutual"
                                    , "namespace"
                                    , "of"
                                    , "open"
                                    , "params"
                                    , "partial"
                                    , "postulate"
                                    , "private"
                                    , "proof"
                                    , "public"
                                    , "record"
                                    , "return"
                                    , "rewrite"
                                    , "syntax"
                                    , "then"
                                    , "total"
                                    , "unreachable"
                                    , "where"
                                    , "with"
                                    , "using"
                                    ]
                        }
                    )
                ,
                    ( Java
                    , FileTypeInfo
                        { ftSelector = [ext ".java"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (JavaIdentStart, JavaIdentCont)
                        , ftKeywords =
                            types
                                [ "bytes"
                                , "char"
                                , "short"
                                , "int"
                                , "long"
                                , "float"
                                , "double"
                                , "boolean"
                                , "void"
                                ]
                                <> reserved
                                    [ "abstract"
                                    , "assert"
                                    , "break"
                                    , "case"
                                    , "catch"
                                    , "class"
                                    , "continue"
                                    , "const"
                                    , "default"
                                    , "do"
                                    , "else"
                                    , "enum"
                                    , "exports"
                                    , "extends"
                                    , "final"
                                    , "finally"
                                    , "for"
                                    , "goto"
                                    , "if"
                                    , "implements"
                                    , "import"
                                    , "instanceof"
                                    , "int"
                                    , "interface"
                                    , "module"
                                    , "native"
                                    , "new"
                                    , "package"
                                    , "private"
                                    , "protected"
                                    , "public"
                                    , "requires"
                                    , "return"
                                    , "static"
                                    , "strictfp"
                                    , "super"
                                    , "switch"
                                    , "synchronized"
                                    , "this"
                                    , "throw"
                                    , "throws"
                                    , "transient"
                                    , "try"
                                    , "var"
                                    , "volatile"
                                    , "while"
                                    , "true"
                                    , "false"
                                    , "null"
                                    ]
                        }
                    )
                ,
                    ( Javascript
                    , FileTypeInfo
                        { ftSelector = [ext ".js"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (JavaIdentStart, JavaIdentCont)
                        , ftKeywords =
                            reserved
                                [ "abstract"
                                , "arguments"
                                , "await"
                                , "break"
                                , "byte"
                                , "case"
                                , "catch"
                                , "char"
                                , "class"
                                , "const"
                                , "continue"
                                , "debugger"
                                , "default"
                                , "delete"
                                , "do"
                                , "double"
                                , "else"
                                , "enum"
                                , "eval"
                                , "export"
                                , "extends"
                                , "false"
                                , "final"
                                , "finally"
                                , "float"
                                , "for"
                                , "function"
                                , "goto"
                                , "if"
                                , "implements"
                                , "import"
                                , "in"
                                , "instanceof"
                                , "int"
                                , "interface"
                                , "let"
                                , "long"
                                , "native"
                                , "new"
                                , "null"
                                , "package"
                                , "private"
                                , "protected"
                                , "public"
                                , "return"
                                , "short"
                                , "static"
                                , "super"
                                , "switch"
                                , "synchronized"
                                , "this"
                                , "throw"
                                , "throws"
                                , "transient"
                                , "true"
                                , "try"
                                , "typeof"
                                , "var"
                                , "volatile"
                                , "while"
                                , "with"
                                , "yield"
                                ]
                        }
                    )
                ,
                    ( Json
                    , FileTypeInfo
                        { ftSelector = [ext ".json", ext ".ndjson"]
                        , ftKind = KindData
                        , ftComment = []
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Julia
                    , FileTypeInfo
                        { ftSelector = [ext ".jl"]
                        , ftKind = KindLanguage
                        , ftComment = ["#" ~~ "\n", "#-" ~~ "-#"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (JuliaIdentStart, JuliaIdentCont)
                        , ftKeywords =
                            types
                                [ "Float16"
                                , "Float32"
                                , "Float64"
                                , "Bool"
                                , "Char"
                                , "Int8"
                                , "UInt8"
                                , "Int16"
                                , "UInt16"
                                , "Int32"
                                , "UInt32"
                                , "Int64"
                                , "UInt64"
                                , "Int128"
                                , "UInt128"
                                , -- super types
                                  "Integer"
                                , "AbstractChar"
                                , "AbstractFloat"
                                , "Signed"
                                , "Unsigned"
                                ]
                                <> reserved
                                    [ "baremodule"
                                    , "begin"
                                    , "break"
                                    , "catch"
                                    , "const"
                                    , "continue"
                                    , "do"
                                    , "else"
                                    , "elseif"
                                    , "end"
                                    , "export"
                                    , "false"
                                    , "finally"
                                    , "for"
                                    , "function"
                                    , "global"
                                    , "if"
                                    , "import"
                                    , "let"
                                    , "local"
                                    , "macro"
                                    , "module"
                                    , "quote"
                                    , "return"
                                    , "struct"
                                    , "true"
                                    , "try"
                                    , "using"
                                    , "while"
                                    ]
                        }
                    )
                ,
                    ( Kotlin
                    , FileTypeInfo
                        { ftSelector = [ext ".kt", ext ".kts", ext ".ktm"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "Byte"
                                , "Short"
                                , "Int"
                                , "Long"
                                , "Float"
                                , "Double"
                                , "UByte"
                                , "UShort"
                                , "UInt"
                                , "ULong"
                                , "Boolean"
                                , "Char"
                                ]
                                <> reserved
                                    [ "as"
                                    , "break"
                                    , "class"
                                    , "continue"
                                    , "do"
                                    , "else"
                                    , "false"
                                    , "for"
                                    , "fun"
                                    , "if"
                                    , "in"
                                    , "interface"
                                    , "is"
                                    , "null"
                                    , "object"
                                    , "package"
                                    , "return"
                                    , "super"
                                    , "this"
                                    , "throw"
                                    , "true"
                                    , "try"
                                    , "typealias"
                                    , "typeof"
                                    , "val"
                                    , "var"
                                    , "when"
                                    , "while"
                                    , "by"
                                    , "catch"
                                    , "constructor"
                                    , "delegate"
                                    , "dynamic"
                                    , "field"
                                    , "file"
                                    , "finally"
                                    , "get"
                                    , "import"
                                    , "init"
                                    , "param"
                                    , "property"
                                    , "receiver"
                                    , "set"
                                    , "setparam"
                                    , "value"
                                    , "where"
                                    , "abstract"
                                    , "actual"
                                    , "annotation"
                                    , "companion"
                                    , "const"
                                    , "crossinline"
                                    , "data"
                                    , "enum"
                                    , "expect"
                                    , "external"
                                    , "final"
                                    , "infix"
                                    , "inline"
                                    , "inner"
                                    , "internal"
                                    , "lateinit"
                                    , "noinline"
                                    , "open"
                                    , "operator"
                                    , "out"
                                    , "override"
                                    , "private"
                                    , "protected"
                                    , "public"
                                    , "reified"
                                    , "sealed"
                                    , "suspend"
                                    , "tailrec"
                                    , "vararg"
                                    , "field"
                                    , "it"
                                    ]
                        }
                    )
                ,
                    ( Ksh
                    , FileTypeInfo
                        { ftSelector = [ext ".ksh"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "case"
                                , "do"
                                , "done"
                                , "elif"
                                , "else"
                                , "esac"
                                , "fi"
                                , "for"
                                , "function"
                                , "if"
                                , "in"
                                , "select"
                                , "then"
                                , "time"
                                , "until"
                                , "while"
                                ]
                        }
                    )
                ,
                    ( Latex
                    , FileTypeInfo
                        { ftSelector = [ext ".latex", ext ".tex"]
                        , ftKind = KindMarkup
                        , ftComment = ["%" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Lisp
                    , FileTypeInfo
                        { ftSelector = [ext ".lisp", ext ".cl"]
                        , ftKind = KindLanguage
                        , ftComment = [";" ~~ "\n", "#|" ~~ "|#"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (ListIdent, ListIdent)
                        , ftKeywords =
                            types
                                [ "array"
                                , "atom"
                                , "bignum"
                                , "bit"
                                , "bit-vector"
                                , "character"
                                , "compiled-function"
                                , "complex"
                                , "cons"
                                , "double-float"
                                , "fixnum"
                                , "float"
                                , "function"
                                , "hash-table"
                                , "integer"
                                , "keyword"
                                , "list"
                                , "long-float"
                                , "nil"
                                , "null"
                                , "number"
                                , "package"
                                , "pathname"
                                , "random-state"
                                , "ratio"
                                , "rational"
                                , "readtable"
                                , "sequence"
                                , "short-float"
                                , "signed-byte"
                                , "simple-array"
                                , "simple-bit-vector"
                                , "simple-string"
                                , "simple-vector"
                                , "single-float"
                                , "standard-char"
                                , "stream"
                                , "string"
                                , "symbol"
                                , "t"
                                , "unsigned-byte"
                                , "vector"
                                ]
                        }
                    )
                ,
                    ( Lua
                    , FileTypeInfo
                        { ftSelector = [ext ".lua"]
                        , ftKind = KindLanguage
                        , ftComment = ["--[[" ~~ "--]]", "--" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = ["[===[" ~~ "]===]", "[==[" ~~ "]==]", "[=[" ~~ "]=]", "[[" ~~ "]]"]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            reserved
                                [ "and"
                                , "break"
                                , "do"
                                , "else"
                                , "elseif"
                                , "end"
                                , "false"
                                , "for"
                                , "function"
                                , "if"
                                , "in"
                                , "local"
                                , "nil"
                                , "not"
                                , "or"
                                , "repeat"
                                , "return"
                                , "then"
                                , "true"
                                , "until"
                                , "while"
                                ]
                        }
                    )
                ,
                    ( Make
                    , FileTypeInfo
                        { ftSelector = [name "Makefile", name "makefile", name "GNUmakefile", ext ".mk", ext ".mak", ext ".make"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (AlphaDash_, AlphaNumDash_)
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Nmap
                    , FileTypeInfo
                        { ftSelector = [ext ".nse"]
                        , ftKind = KindScript
                        , ftComment = ["--" ~~ "\n", "[[" ~~ "]]"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Nim
                    , FileTypeInfo
                        { ftSelector = [ext ".nim"]
                        , ftKind = KindLanguage
                        , ftComment = ["#[" ~~ "#]", "#" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "int"
                                , "int8"
                                , "int16"
                                , "int32"
                                , "int64"
                                , "uint"
                                , "uint8"
                                , "uint16"
                                , "uint32"
                                , "uint64"
                                , "float"
                                , "float32"
                                , "float64"
                                , "bool"
                                , "string"
                                , "cstring"
                                ]
                                <> reserved
                                    [ "addr"
                                    , "and"
                                    , "as"
                                    , "asm"
                                    , "bind"
                                    , "block"
                                    , "break"
                                    , "case"
                                    , "cast"
                                    , "concept"
                                    , "const"
                                    , "continue"
                                    , "converter"
                                    , "defer"
                                    , "discard"
                                    , "distinct"
                                    , "div"
                                    , "do"
                                    , "elif"
                                    , "else"
                                    , "end"
                                    , "enum"
                                    , "except"
                                    , "export"
                                    , "finally"
                                    , "for"
                                    , "from"
                                    , "func"
                                    , "if"
                                    , "import"
                                    , "in"
                                    , "include"
                                    , "interface"
                                    , "is"
                                    , "isnot"
                                    , "iterator"
                                    , "let"
                                    , "macro"
                                    , "method"
                                    , "mixin"
                                    , "mod"
                                    , "nil"
                                    , "not"
                                    , "notin"
                                    , "object"
                                    , "of"
                                    , "or"
                                    , "out"
                                    , "proc"
                                    , "ptr"
                                    , "raise"
                                    , "ref"
                                    , "return"
                                    , "shl"
                                    , "shr"
                                    , "static"
                                    , "template"
                                    , "try"
                                    , "tuple"
                                    , "type"
                                    , "using"
                                    , "var"
                                    , "when"
                                    , "while"
                                    , "xor"
                                    , "yield"
                                    ]
                        }
                    )
                ,
                    ( OCaml
                    , FileTypeInfo
                        { ftSelector = [ext ".ml", hdr ".mli"]
                        , ftKind = KindLanguage
                        , ftComment = ["(*" ~~ "*)"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["{id|" ~~ "|id}"]
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_')
                        , ftKeywords =
                            types
                                [ "int"
                                , "float"
                                , "char"
                                , "string"
                                , "bool"
                                , "unit"
                                , "list"
                                , "array"
                                , "exn"
                                , "option"
                                , "ref"
                                ]
                                <> reserved
                                    [ "and"
                                    , "as"
                                    , "assert"
                                    , "asr"
                                    , "begin"
                                    , "class"
                                    , "constraint"
                                    , "do"
                                    , "done"
                                    , "downto"
                                    , "else"
                                    , "end"
                                    , "exception"
                                    , "external"
                                    , "false"
                                    , "for"
                                    , "fun"
                                    , "function"
                                    , "functor"
                                    , "if"
                                    , "in"
                                    , "include"
                                    , "inherit"
                                    , "initializer"
                                    , "land"
                                    , "lazy"
                                    , "let"
                                    , "lor"
                                    , "lsl"
                                    , "lsr"
                                    , "lxor"
                                    , "match"
                                    , "method"
                                    , "mod"
                                    , "module"
                                    , "mutable"
                                    , "new"
                                    , "nonrec"
                                    , "object"
                                    , "of"
                                    , "open"
                                    , "or"
                                    , "private"
                                    , "rec"
                                    , "sig"
                                    , "struct"
                                    , "then"
                                    , "to"
                                    , "true"
                                    , "try"
                                    , "type"
                                    , "val"
                                    , "virtual"
                                    , "when"
                                    , "while"
                                    , "with"
                                    ]
                        }
                    )
                ,
                    ( ObjectiveC
                    , FileTypeInfo
                        { ftSelector = [ext ".m", ext ".mi"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "char"
                                , "int"
                                , "short"
                                , "long"
                                , "float"
                                , "double"
                                , "signed"
                                , "unsigned"
                                ]
                                <> reserved
                                    [ "void"
                                    , "id"
                                    , "const"
                                    , "volatile"
                                    , "in"
                                    , "out"
                                    , "inout"
                                    , "bycopy"
                                    , "byref"
                                    , "oneway"
                                    , "self"
                                    , "super"
                                    , "interface"
                                    , "end"
                                    , "@implementation"
                                    , "@end"
                                    , "@interface"
                                    , "@end"
                                    , "@implementation"
                                    , "@end"
                                    , "@protoco"
                                    , "@end"
                                    , "@class"
                                    ]
                        }
                    )
                ,
                    ( PHP
                    , FileTypeInfo
                        { ftSelector = [ext ".php", ext ".php3", ext ".php4", ext ".php5", ext ".phtml"]
                        , ftKind = KindScript
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n", "#" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = ["<<END" ~~ "END;", "<<'END'" ~~ "END;"]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            reserved
                                [ "__halt_compiler"
                                , "abstract"
                                , "and"
                                , "array"
                                , "as"
                                , "break"
                                , "callable"
                                , "case"
                                , "catch"
                                , "class"
                                , "clone"
                                , "const"
                                , "continue"
                                , "declare"
                                , "default"
                                , "die"
                                , "do"
                                , "echo"
                                , "else"
                                , "elseif"
                                , "empty"
                                , "enddeclare"
                                , "endfor"
                                , "endforeach"
                                , "endif"
                                , "endswitch"
                                , "endwhile"
                                , "eval"
                                , "exit"
                                , "extends"
                                , "final"
                                , "finally"
                                , "fn"
                                , "for"
                                , "foreach"
                                , "function"
                                , "global"
                                , "goto"
                                , "if"
                                , "implements"
                                , "include"
                                , "include_once"
                                , "instanceof"
                                , "insteadof"
                                , "interface"
                                , "isset"
                                , "list"
                                , "match"
                                , "namespace"
                                , "new"
                                , "or"
                                , "print"
                                , "private"
                                , "protected"
                                , "public"
                                , "readonly"
                                , "require"
                                , "require_once"
                                , "return"
                                , "static"
                                , "switch"
                                , "throw"
                                , "trait"
                                , "try"
                                , "unset"
                                , "use"
                                , "var"
                                , "while"
                                , "xor"
                                , "yield"
                                , "yield"
                                , "from"
                                ]
                        }
                    )
                ,
                    ( Perl
                    , FileTypeInfo
                        { ftSelector = [ext ".pl", ext ".pm", ext ".pm6", ext ".plx", ext ".perl"]
                        , ftKind = KindScript
                        , ftComment = ["=pod" ~~ "=cut", "#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = ["<<\"END\";" ~~ "END", "<<'END'" ~~ "END", "<<'EOT';" ~~ "EOT", "<<\"EOT\";" ~~ "EOT"]
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords = reserved []
                        }
                    )
                ,
                    ( Python
                    , FileTypeInfo
                        { ftSelector = [ext ".py", ext ".pyx", ext ".pxd", ext ".pxi", ext ".scons"]
                        , ftKind = KindLanguage
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''", "r'" ~~ "'"]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            reserved
                                [ "False"
                                , "await"
                                , "else"
                                , "import"
                                , "pass"
                                , "None"
                                , "break"
                                , "except"
                                , "in"
                                , "raise"
                                , "True"
                                , "class"
                                , "finally"
                                , "is"
                                , "return"
                                , "and"
                                , "continue"
                                , "for"
                                , "lambda"
                                , "try"
                                , "as"
                                , "def"
                                , "from"
                                , "nonlocal"
                                , "while"
                                , "assert"
                                , "del"
                                , "global"
                                , "not"
                                , "with"
                                , "async"
                                , "elif"
                                , "if"
                                , "or"
                                , "yield"
                                ]
                        }
                    )
                ,
                    ( R
                    , FileTypeInfo
                        { ftSelector = [ext ".r", ext ".rdata", ext ".rds", ext ".rda"]
                        , ftKind = KindLanguage
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            reserved
                                [ "if"
                                , "else"
                                , "repeat"
                                , "while"
                                , "function"
                                , "for"
                                , "in"
                                , "next"
                                , "break"
                                , "TRUE"
                                , "FALSE"
                                , "NULL"
                                , "Inf"
                                , "NaN"
                                , "NA"
                                , "NA_integer_"
                                , "NA_real_"
                                , "NA_complex_"
                                , "NA_character_"
                                , "…"
                                ]
                        }
                    )
                ,
                    ( Ruby
                    , FileTypeInfo
                        { ftSelector = [ext ".rb", ext ".ruby"]
                        , ftKind = KindLanguage
                        , ftComment = ["=begin" ~~ "=end", "#" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["'" ~~ "'", "\"" ~~ "\"", "%|" ~~ "|", "%q(" ~~ ")", "%Q(" ~~ ")"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            reserved
                                [ "BEGIN"
                                , "END"
                                , "alias"
                                , "and"
                                , "begin"
                                , "break"
                                , "case"
                                , "class"
                                , "def"
                                , "module"
                                , "next"
                                , "nil"
                                , "not"
                                , "or"
                                , "redo"
                                , "rescue"
                                , "retry"
                                , "return"
                                , "elsif"
                                , "end"
                                , "false"
                                , "ensure"
                                , "for"
                                , "if"
                                , "true"
                                , "undef"
                                , "unless"
                                , "do"
                                , "else"
                                , "super"
                                , "then"
                                , "until"
                                , "when"
                                , "while"
                                , "defined?"
                                , "self"
                                ]
                        }
                    )
                ,
                    ( Rust
                    , FileTypeInfo
                        { ftSelector = [ext ".rs"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["r##\"" ~~ "\"##", "r#\"" ~~ "\"#", "r\"" ~~ "\""]
                        , ftIdentCharSet = Just (UnicodeXIDStart_, UnicodeNumXIDCont_)
                        , ftKeywords =
                            types
                                [ "i8"
                                , "u8"
                                , "i16"
                                , "u16"
                                , "i32"
                                , "u32"
                                , "i64"
                                , "u64"
                                , "i128"
                                , "u128"
                                , "isize"
                                , "usize"
                                , "bool"
                                , "char"
                                , "str"
                                , "String"
                                ]
                                <> reserved
                                    [ "as"
                                    , "use"
                                    , "extern"
                                    , "crate"
                                    , "break"
                                    , "const"
                                    , "continue"
                                    , "crate"
                                    , "else"
                                    , "if"
                                    , "let"
                                    , "enum"
                                    , "extern"
                                    , "false"
                                    , "fn"
                                    , "for"
                                    , "if"
                                    , "impl"
                                    , "in"
                                    , "for"
                                    , "let"
                                    , "loop"
                                    , "match"
                                    , "mod"
                                    , "move"
                                    , "mut"
                                    , "pub"
                                    , "impl"
                                    , "ref"
                                    , "return"
                                    , "Self"
                                    , "self"
                                    , "static"
                                    , "struct"
                                    , "super"
                                    , "trait"
                                    , "true"
                                    , "type"
                                    , "unsafe"
                                    , "use"
                                    , "where"
                                    , "while"
                                    , "abstract"
                                    , "alignof"
                                    , "become"
                                    , "box"
                                    , "do"
                                    , "final"
                                    , "macro"
                                    , "offsetof"
                                    , "override"
                                    , "priv"
                                    , "proc"
                                    , "pure"
                                    , "sizeof"
                                    , "typeof"
                                    , "unsized"
                                    , "virtual"
                                    , "yield"
                                    , "async"
                                    , "await"
                                    , "dyn"
                                    , -- weak keywords
                                      "macro_rules"
                                    , "union"
                                    , "'static"
                                    , -- reserved for future use
                                      "abstract"
                                    , "become"
                                    , "box"
                                    , "do"
                                    , "final"
                                    , "macro"
                                    , "override"
                                    , "priv"
                                    , "typeof"
                                    , "unsized"
                                    , "virtual"
                                    , "yield"
                                    , "try"
                                    ]
                        }
                    )
                ,
                    ( Scala
                    , FileTypeInfo
                        { ftSelector = [ext ".scala"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "Byte"
                                , "Short"
                                , "Int"
                                , "Long"
                                , "Float"
                                , "Double"
                                , "Char"
                                , "String"
                                , "Boolean"
                                , "Unit"
                                , "Null"
                                , "Nothing"
                                , "Any"
                                , "AnyRef"
                                ]
                                <> reserved
                                    [ "abstract"
                                    , "case"
                                    , "catch"
                                    , "class"
                                    , "def"
                                    , "do"
                                    , "else"
                                    , "extends"
                                    , "false"
                                    , "final"
                                    , "finally"
                                    , "for"
                                    , "forSome"
                                    , "if"
                                    , "implicit"
                                    , "import"
                                    , "lazy"
                                    , "match"
                                    , "new"
                                    , "null"
                                    , "object"
                                    , "override"
                                    , "package"
                                    , "private"
                                    , "protected"
                                    , "return"
                                    , "sealed"
                                    , "super"
                                    , "this"
                                    , "throw"
                                    , "trait"
                                    , "true"
                                    , "try"
                                    , "type"
                                    , "val"
                                    , "var"
                                    , "while"
                                    , "with"
                                    , "yield"
                                    ]
                        }
                    )
                ,
                    ( SmallTalk
                    , FileTypeInfo
                        { ftSelector = [ext ".st", ext ".gst"]
                        , ftKind = KindLanguage
                        , ftComment = ["\"" ~~ "\""]
                        , ftChar = ["$" ~~ ""]
                        , ftString = ["'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            reserved
                                [ "true"
                                , "false"
                                , "nil"
                                , "self"
                                , "super"
                                , "thisContext"
                                ]
                        }
                    )
                ,
                    ( Swift
                    , FileTypeInfo
                        { ftSelector = [ext ".swift"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        , ftKeywords =
                            types
                                [ "Int"
                                , "Int8"
                                , "Int16"
                                , "Int32"
                                , "Int64"
                                , "UInt"
                                , "UInt8"
                                , "UInt16"
                                , "UInt32"
                                , "UInt64"
                                , "Float"
                                , "Double"
                                , "Float80"
                                , "Float16"
                                , "Bool"
                                , "String"
                                , "Character"
                                , "Array"
                                , "Dictionary"
                                , "Set"
                                , "Optional"
                                , "Any"
                                , "AnyObject"
                                ]
                                <> reserved
                                    [ "associatedtype"
                                    , "class"
                                    , "deinit"
                                    , "enum"
                                    , "extension"
                                    , "fileprivate"
                                    , "func"
                                    , "import"
                                    , "init"
                                    , "inout"
                                    , "internal"
                                    , "let"
                                    , "open"
                                    , "operator"
                                    , "private"
                                    , "precedencegroup"
                                    , "protocol"
                                    , "public"
                                    , "rethrows"
                                    , "static"
                                    , "struct"
                                    , "subscript"
                                    , "typealias"
                                    , "var"
                                    , "break"
                                    , "case"
                                    , "catch"
                                    , "continue"
                                    , "default"
                                    , "defer"
                                    , "do"
                                    , "else"
                                    , "fallthrough"
                                    , "for"
                                    , "guard"
                                    , "if"
                                    , "in"
                                    , "repeat"
                                    , "return"
                                    , "throw"
                                    , "switch"
                                    , "where"
                                    , "while"
                                    , "Any"
                                    , "as"
                                    , "catch"
                                    , "false"
                                    , "is"
                                    , "nil"
                                    , "rethrows"
                                    , "self"
                                    , "Self"
                                    , "super"
                                    , "throw"
                                    , "throws"
                                    , "true"
                                    , "try"
                                    , "#available"
                                    , "#colorLiteral"
                                    , "#column"
                                    , "#dsohandle"
                                    , "#elseif"
                                    , "#else"
                                    , "#endif"
                                    , "#error"
                                    , "#fileID"
                                    , "#fileLiteral"
                                    , "#filePath"
                                    , "#file"
                                    , "#function"
                                    , "#if"
                                    , "#imageLiteral"
                                    , "#keyPath"
                                    , "#line"
                                    , "#selector"
                                    , "#sourceLocation"
                                    , "#warning"
                                    , "associativity"
                                    , "convenience"
                                    , "didSet"
                                    , "dynamic"
                                    , "final"
                                    , "get"
                                    , "indirect"
                                    , "infix"
                                    , "lazy"
                                    , "left"
                                    , "mutating"
                                    , "none"
                                    , "nonmutating"
                                    , "optional"
                                    , "override"
                                    , "postfix"
                                    , "precedence"
                                    , "prefix"
                                    , "Protocol"
                                    , "required"
                                    , "right"
                                    , "set"
                                    , "some"
                                    , "Type"
                                    , "unowned"
                                    , "weak"
                                    , "willSet"
                                    ]
                        }
                    )
                ,
                    ( Sql
                    , FileTypeInfo
                        { ftSelector = [ext ".sql"]
                        , ftKind = KindLanguage
                        , ftComment = ["--" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "ABORT"
                                , "ABORTSESSION"
                                , "ABS"
                                , "ABSOLUTE"
                                , "ACCESS"
                                , "ACCESSIBLE"
                                , "ACCESS_LOCK"
                                , "ACCOUNT"
                                , "ACOS"
                                , "ACOSH"
                                , "ACTION"
                                , "ADD"
                                , "ADD_MONTHS"
                                , "ADMIN"
                                , "AFTER"
                                , "AGGREGATE"
                                , "ALIAS"
                                , "ALL"
                                , "ALLOCATE"
                                , "ALLOW"
                                , "ALTER"
                                , "ALTERAND"
                                , "AMP"
                                , "ANALYSE"
                                , "ANALYZE"
                                , "AND"
                                , "ANSIDATE"
                                , "ANY"
                                , "ARE"
                                , "ARRAY"
                                , "ARRAY_AGG"
                                , "ARRAY_EXISTS"
                                , "ARRAY_MAX_CARDINALITY"
                                , "AS"
                                , "ASC"
                                , "ASENSITIVE"
                                , "ASIN"
                                , "ASINH"
                                , "ASSERTION"
                                , "ASSOCIATE"
                                , "ASUTIME"
                                , "ASYMMETRIC"
                                , "AT"
                                , "ATAN"
                                , "ATAN2"
                                , "ATANH"
                                , "ATOMIC"
                                , "AUDIT"
                                , "AUTHORIZATION"
                                , "AUX"
                                , "AUXILIARY"
                                , "AVE"
                                , "AVERAGE"
                                , "AVG"
                                , "BACKUP"
                                , "BEFORE"
                                , "BEGIN"
                                , "BEGIN_FRAME"
                                , "BEGIN_PARTITION"
                                , "BETWEEN"
                                , "BIGINT"
                                , "BINARY"
                                , "BIT"
                                , "BLOB"
                                , "BOOLEAN"
                                , "BOTH"
                                , "BREADTH"
                                , "BREAK"
                                , "BROWSE"
                                , "BT"
                                , "BUFFERPOOL"
                                , "BULK"
                                , "BUT"
                                , "BY"
                                , "BYTE"
                                , "BYTEINT"
                                , "BYTES"
                                , "CALL"
                                , "CALLED"
                                , "CAPTURE"
                                , "CARDINALITY"
                                , "CASCADE"
                                , "CASCADED"
                                , "CASE"
                                , "CASESPECIFIC"
                                , "CASE_N"
                                , "CAST"
                                , "CATALOG"
                                , "CCSID"
                                , "CD"
                                , "CEIL"
                                , "CEILING"
                                , "CHANGE"
                                , "CHAR"
                                , "CHAR2HEXINT"
                                , "CHARACTER"
                                , "CHARACTERS"
                                , "CHARACTER_LENGTH"
                                , "CHARS"
                                , "CHAR_LENGTH"
                                , "CHECK"
                                , "CHECKPOINT"
                                , "CLASS"
                                , "CLASSIFIER"
                                , "CLOB"
                                , "CLONE"
                                , "CLOSE"
                                , "CLUSTER"
                                , "CLUSTERED"
                                , "CM"
                                , "COALESCE"
                                , "COLLATE"
                                , "COLLATION"
                                , "COLLECT"
                                , "COLLECTION"
                                , "COLLID"
                                , "COLUMN"
                                , "COLUMN_VALUE"
                                , "COMMENT"
                                , "COMMIT"
                                , "COMPLETION"
                                , "COMPRESS"
                                , "COMPUTE"
                                , "CONCAT"
                                , "CONCURRENTLY"
                                , "CONDITION"
                                , "CONNECT"
                                , "CONNECTION"
                                , "CONSTRAINT"
                                , "CONSTRAINTS"
                                , "CONSTRUCTOR"
                                , "CONTAINS"
                                , "CONTAINSTABLE"
                                , "CONTENT"
                                , "CONTINUE"
                                , "CONVERT"
                                , "CONVERT_TABLE_HEADER"
                                , "COPY"
                                , "CORR"
                                , "CORRESPONDING"
                                , "COS"
                                , "COSH"
                                , "COUNT"
                                , "COVAR_POP"
                                , "COVAR_SAMP"
                                , "CREATE"
                                , "CROSS"
                                , "CS"
                                , "CSUM"
                                , "CT"
                                , "CUBE"
                                , "CUME_DIST"
                                , "CURRENT"
                                , "CURRENT_CATALOG"
                                , "CURRENT_DATE"
                                , "CURRENT_DEFAULT_TRANSFORM_GROUP"
                                , "CURRENT_LC_CTYPE"
                                , "CURRENT_PATH"
                                , "CURRENT_ROLE"
                                , "CURRENT_ROW"
                                , "CURRENT_SCHEMA"
                                , "CURRENT_SERVER"
                                , "CURRENT_TIME"
                                , "CURRENT_TIMESTAMP"
                                , "CURRENT_TIMEZONE"
                                , "CURRENT_TRANSFORM_GROUP_FOR_TYPE"
                                , "CURRENT_USER"
                                , "CURRVAL"
                                , "CURSOR"
                                , "CV"
                                , "CYCLE"
                                , "DATA"
                                , "DATABASE"
                                , "DATABASES"
                                , "DATABLOCKSIZE"
                                , "DATE"
                                , "DATEFORM"
                                , "DAY"
                                , "DAYS"
                                , "DAY_HOUR"
                                , "DAY_MICROSECOND"
                                , "DAY_MINUTE"
                                , "DAY_SECOND"
                                , "DBCC"
                                , "DBINFO"
                                , "DEALLOCATE"
                                , "DEC"
                                , "DECFLOAT"
                                , "DECIMAL"
                                , "DECLARE"
                                , "DEFAULT"
                                , "DEFERRABLE"
                                , "DEFERRED"
                                , "DEFINE"
                                , "DEGREES"
                                , "DEL"
                                , "DELAYED"
                                , "DELETE"
                                , "DENSE_RANK"
                                , "DENY"
                                , "DEPTH"
                                , "DEREF"
                                , "DESC"
                                , "DESCRIBE"
                                , "DESCRIPTOR"
                                , "DESTROY"
                                , "DESTRUCTOR"
                                , "DETERMINISTIC"
                                , "DIAGNOSTIC"
                                , "DIAGNOSTICS"
                                , "DICTIONARY"
                                , "DISABLE"
                                , "DISABLED"
                                , "DISALLOW"
                                , "DISCONNECT"
                                , "DISK"
                                , "DISTINCT"
                                , "DISTINCTROW"
                                , "DISTRIBUTED"
                                , "DIV"
                                , "DO"
                                , "DOCUMENT"
                                , "DOMAIN"
                                , "DOUBLE"
                                , "DROP"
                                , "DSSIZE"
                                , "DUAL"
                                , "DUMP"
                                , "DYNAMIC"
                                , "EACH"
                                , "ECHO"
                                , "EDITPROC"
                                , "ELEMENT"
                                , "ELSE"
                                , "ELSEIF"
                                , "EMPTY"
                                , "ENABLED"
                                , "ENCLOSED"
                                , "ENCODING"
                                , "ENCRYPTION"
                                , "END"
                                , "END"
                                , "EXEC"
                                , "ENDING"
                                , "END_FRAME"
                                , "END_PARTITION"
                                , "EQ"
                                , "EQUALS"
                                , "ERASE"
                                , "ERRLVL"
                                , "ERROR"
                                , "ERRORFILES"
                                , "ERRORTABLES"
                                , "ESCAPE"
                                , "ESCAPED"
                                , "ET"
                                , "EVERY"
                                , "EXCEPT"
                                , "EXCEPTION"
                                , "EXCLUSIVE"
                                , "EXEC"
                                , "EXECUTE"
                                , "EXISTS"
                                , "EXIT"
                                , "EXP"
                                , "EXPLAIN"
                                , "EXTERNAL"
                                , "EXTRACT"
                                , "FALLBACK"
                                , "FALSE"
                                , "FASTEXPORT"
                                , "FENCED"
                                , "FETCH"
                                , "FIELDPROC"
                                , "FILE"
                                , "FILLFACTOR"
                                , "FILTER"
                                , "FINAL"
                                , "FIRST"
                                , "FIRST_VALUE"
                                , "FLOAT"
                                , "FLOAT4"
                                , "FLOAT8"
                                , "FLOOR"
                                , "FOR"
                                , "FORCE"
                                , "FOREIGN"
                                , "FORMAT"
                                , "FOUND"
                                , "FRAME_ROW"
                                , "FREE"
                                , "FREESPACE"
                                , "FREETEXT"
                                , "FREETEXTTABLE"
                                , "FREEZE"
                                , "FROM"
                                , "FULL"
                                , "FULLTEXT"
                                , "FUNCTION"
                                , "FUSION"
                                , "GE"
                                , "GENERAL"
                                , "GENERATED"
                                , "GET"
                                , "GIVE"
                                , "GLOBAL"
                                , "GO"
                                , "GOTO"
                                , "GRANT"
                                , "GRAPHIC"
                                , "GROUP"
                                , "GROUPING"
                                , "GROUPS"
                                , "GT"
                                , "HANDLER"
                                , "HASH"
                                , "HASHAMP"
                                , "HASHBAKAMP"
                                , "HASHBUCKET"
                                , "HASHROW"
                                , "HAVING"
                                , "HELP"
                                , "HIGH_PRIORITY"
                                , "HOLD"
                                , "HOLDLOCK"
                                , "HOST"
                                , "HOUR"
                                , "HOURS"
                                , "HOUR_MICROSECOND"
                                , "HOUR_MINUTE"
                                , "HOUR_SECOND"
                                , "IDENTIFIED"
                                , "IDENTITY"
                                , "IDENTITYCOL"
                                , "IDENTITY_INSERT"
                                , "IF"
                                , "IGNORE"
                                , "ILIKE"
                                , "IMMEDIATE"
                                , "IN"
                                , "INCLUSIVE"
                                , "INCONSISTENT"
                                , "INCREMENT"
                                , "INDEX"
                                , "INDICATOR"
                                , "INFILE"
                                , "INHERIT"
                                , "INITIAL"
                                , "INITIALIZE"
                                , "INITIALLY"
                                , "INITIATE"
                                , "INNER"
                                , "INOUT"
                                , "INPUT"
                                , "INS"
                                , "INSENSITIVE"
                                , "INSERT"
                                , "INSTEAD"
                                , "INT"
                                , "INT1"
                                , "INT2"
                                , "INT3"
                                , "INT4"
                                , "INT8"
                                , "INTEGER"
                                , "INTEGERDATE"
                                , "INTERSECT"
                                , "INTERSECTION"
                                , "INTERVAL"
                                , "INTO"
                                , "IO_AFTER_GTIDS"
                                , "IO_BEFORE_GTIDS"
                                , "IS"
                                , "ISNULL"
                                , "ISOBID"
                                , "ISOLATION"
                                , "ITERATE"
                                , "JAR"
                                , "JOIN"
                                , "JOURNAL"
                                , "JSON_ARRAY"
                                , "JSON_ARRAYAGG"
                                , "JSON_EXISTS"
                                , "JSON_OBJECT"
                                , "JSON_OBJECTAGG"
                                , "JSON_QUERY"
                                , "JSON_TABLE"
                                , "JSON_TABLE_PRIMITIVE"
                                , "JSON_VALUE"
                                , "KEEP"
                                , "KEY"
                                , "KEYS"
                                , "KILL"
                                , "KURTOSIS"
                                , "LABEL"
                                , "LAG"
                                , "FTUAGE"
                                , "LARGE"
                                , "LAST"
                                , "LAST_VALUE"
                                , "LATERAL"
                                , "LC_CTYPE"
                                , "LE"
                                , "LEAD"
                                , "LEADING"
                                , "LEAVE"
                                , "LEFT"
                                , "LESS"
                                , "LEVEL"
                                , "LIKE"
                                , "LIKE_REGEX"
                                , "LIMIT"
                                , "LINEAR"
                                , "LINENO"
                                , "LINES"
                                , "LISTAGG"
                                , "LN"
                                , "LOAD"
                                , "LOADING"
                                , "LOCAL"
                                , "LOCALE"
                                , "LOCALTIME"
                                , "LOCALTIMESTAMP"
                                , "LOCATOR"
                                , "LOCATORS"
                                , "LOCK"
                                , "LOCKING"
                                , "LOCKMAX"
                                , "LOCKSIZE"
                                , "LOG"
                                , "LOG10"
                                , "LOGGING"
                                , "LOGON"
                                , "LONG"
                                , "LONGBLOB"
                                , "LONGTEXT"
                                , "LOOP"
                                , "LOWER"
                                , "LOW_PRIORITY"
                                , "LT"
                                , "MACRO"
                                , "MAINTAINED"
                                , "MAP"
                                , "MASTER_BIND"
                                , "MASTER_SSL_VERIFY_SERVER_CERT"
                                , "MATCH"
                                , "MATCHES"
                                , "MATCH_NUMBER"
                                , "MATCH_RECOGNIZE"
                                , "MATERIALIZED"
                                , "MAVG"
                                , "MAX"
                                , "MAXEXTENTS"
                                , "MAXIMUM"
                                , "MAXVALUE"
                                , "MCHARACTERS"
                                , "MDIFF"
                                , "MEDIUMBLOB"
                                , "MEDIUMINT"
                                , "MEDIUMTEXT"
                                , "MEMBER"
                                , "MERGE"
                                , "METHOD"
                                , "MICROSECOND"
                                , "MICROSECONDS"
                                , "MIDDLEINT"
                                , "MIN"
                                , "MINDEX"
                                , "MINIMUM"
                                , "MINUS"
                                , "MINUTE"
                                , "MINUTES"
                                , "MINUTE_MICROSECOND"
                                , "MINUTE_SECOND"
                                , "MLINREG"
                                , "MLOAD"
                                , "MLSLABEL"
                                , "MOD"
                                , "MODE"
                                , "MODIFIES"
                                , "MODIFY"
                                , "MODULE"
                                , "MONITOR"
                                , "MONRESOURCE"
                                , "MONSESSION"
                                , "MONTH"
                                , "MONTHS"
                                , "MSUBSTR"
                                , "MSUM"
                                , "MULTISET"
                                , "NAMED"
                                , "NAMES"
                                , "NATIONAL"
                                , "NATURAL"
                                , "NCHAR"
                                , "NCLOB"
                                , "NE"
                                , "NESTED_TABLE_ID"
                                , "NEW"
                                , "NEW_TABLE"
                                , "NEXT"
                                , "NEXTVAL"
                                , "NO"
                                , "NOAUDIT"
                                , "NOCHECK"
                                , "NOCOMPRESS"
                                , "NONCLUSTERED"
                                , "NONE"
                                , "NORMALIZE"
                                , "NOT"
                                , "NOTNULL"
                                , "NOWAIT"
                                , "NO_WRITE_TO_BINLOG"
                                , "NTH_VALUE"
                                , "NTILE"
                                , "NULL"
                                , "NULLIF"
                                , "NULLIFZERO"
                                , "NULLS"
                                , "NUMBER"
                                , "NUMERIC"
                                , "NUMPARTS"
                                , "OBID"
                                , "OBJECT"
                                , "OBJECTS"
                                , "OCCURRENCES_REGEX"
                                , "OCTET_LENGTH"
                                , "OF"
                                , "OFF"
                                , "OFFLINE"
                                , "OFFSET"
                                , "OFFSETS"
                                , "OLD"
                                , "OLD_TABLE"
                                , "OMIT"
                                , "ON"
                                , "ONE"
                                , "ONLINE"
                                , "ONLY"
                                , "OPEN"
                                , "OPENDATASOURCE"
                                , "OPENQUERY"
                                , "OPENROWSET"
                                , "OPENXML"
                                , "OPERATION"
                                , "OPTIMIZATION"
                                , "OPTIMIZE"
                                , "OPTIMIZER_COSTS"
                                , "OPTION"
                                , "OPTIONALLY"
                                , "OR"
                                , "ORDER"
                                , "ORDINALITY"
                                , "ORGANIZATION"
                                , "OUT"
                                , "OUTER"
                                , "OUTFILE"
                                , "OUTPUT"
                                , "OVER"
                                , "OVERLAPS"
                                , "OVERLAY"
                                , "OVERRIDE"
                                , "PACKAGE"
                                , "PAD"
                                , "PADDED"
                                , "PARAMETER"
                                , "PARAMETERS"
                                , "PART"
                                , "PARTIAL"
                                , "PARTITION"
                                , "PARTITIONED"
                                , "PARTITIONING"
                                , "PASSWORD"
                                , "PATH"
                                , "PATTERN"
                                , "PCTFREE"
                                , "PER"
                                , "PERCENT"
                                , "PERCENTILE_CONT"
                                , "PERCENTILE_DISC"
                                , "PERCENT_RANK"
                                , "PERIOD"
                                , "PERM"
                                , "PERMANENT"
                                , "PIECESIZE"
                                , "PIVOT"
                                , "PLACING"
                                , "PLAN"
                                , "PORTION"
                                , "POSITION"
                                , "POSITION_REGEX"
                                , "POSTFIX"
                                , "POWER"
                                , "PRECEDES"
                                , "PRECISION"
                                , "PREFIX"
                                , "PREORDER"
                                , "PREPARE"
                                , "PRESERVE"
                                , "PREVVAL"
                                , "PRIMARY"
                                , "PRINT"
                                , "PRIOR"
                                , "PRIQTY"
                                , "PRIVATE"
                                , "PRIVILEGES"
                                , "PROC"
                                , "PROCEDURE"
                                , "PROFILE"
                                , "PROGRAM"
                                , "PROPORTIONAL"
                                , "PROTECTION"
                                , "PSID"
                                , "PTF"
                                , "PUBLIC"
                                , "PURGE"
                                , "QUALIFIED"
                                , "QUALIFY"
                                , "QUANTILE"
                                , "QUERY"
                                , "QUERYNO"
                                , "RADIANS"
                                , "RAISERROR"
                                , "RANDOM"
                                , "RANGE"
                                , "RANGE_N"
                                , "RANK"
                                , "RAW"
                                , "READ"
                                , "READS"
                                , "READTEXT"
                                , "READ_WRITE"
                                , "REAL"
                                , "RECONFIGURE"
                                , "RECURSIVE"
                                , "REF"
                                , "REFERENCES"
                                , "REFERENCING"
                                , "REFRESH"
                                , "REGEXP"
                                , "REGR_AVGX"
                                , "REGR_AVGY"
                                , "REGR_COUNT"
                                , "REGR_INTERCEPT"
                                , "REGR_R2"
                                , "REGR_SLOPE"
                                , "REGR_SXX"
                                , "REGR_SXY"
                                , "REGR_SYY"
                                , "RELATIVE"
                                , "RELEASE"
                                , "RENAME"
                                , "REPEAT"
                                , "REPLACE"
                                , "REPLICATION"
                                , "REPOVERRIDE"
                                , "REQUEST"
                                , "REQUIRE"
                                , "RESIGNAL"
                                , "RESOURCE"
                                , "RESTART"
                                , "RESTORE"
                                , "RESTRICT"
                                , "RESULT"
                                , "RESULT_SET_LOCATOR"
                                , "RESUME"
                                , "RET"
                                , "RETRIEVE"
                                , "RETURN"
                                , "RETURNING"
                                , "RETURNS"
                                , "REVALIDATE"
                                , "REVERT"
                                , "REVOKE"
                                , "RIGHT"
                                , "RIGHTS"
                                , "RLIKE"
                                , "ROLE"
                                , "ROLLBACK"
                                , "ROLLFORWARD"
                                , "ROLLUP"
                                , "ROUND_CEILING"
                                , "ROUND_DOWN"
                                , "ROUND_FLOOR"
                                , "ROUND_HALF_DOWN"
                                , "ROUND_HALF_EVEN"
                                , "ROUND_HALF_UP"
                                , "ROUND_UP"
                                , "ROUTINE"
                                , "ROW"
                                , "ROWCOUNT"
                                , "ROWGUIDCOL"
                                , "ROWID"
                                , "ROWNUM"
                                , "ROWS"
                                , "ROWSET"
                                , "ROW_NUMBER"
                                , "RULE"
                                , "RUN"
                                , "RUNNING"
                                , "SAMPLE"
                                , "SAMPLEID"
                                , "SAVE"
                                , "SAVEPOINT"
                                , "SCHEMA"
                                , "SCHEMAS"
                                , "SCOPE"
                                , "SCRATCHPAD"
                                , "SCROLL"
                                , "SEARCH"
                                , "SECOND"
                                , "SECONDS"
                                , "SECOND_MICROSECOND"
                                , "SECQTY"
                                , "SECTION"
                                , "SECURITY"
                                , "SECURITYAUDIT"
                                , "SEEK"
                                , "SEL"
                                , "SELECT"
                                , "SEMANTICKEYPHRASETABLE"
                                , "SEMANTICSIMILARITYDETAILSTABLE"
                                , "SEMANTICSIMILARITYTABLE"
                                , "SENSITIVE"
                                , "SEPARATOR"
                                , "SEQUENCE"
                                , "SESSION"
                                , "SESSION_USER"
                                , "SET"
                                , "SETRESRATE"
                                , "SETS"
                                , "SETSESSRATE"
                                , "SETUSER"
                                , "SHARE"
                                , "SHOW"
                                , "SHUTDOWN"
                                , "SIGNAL"
                                , "SIMILAR"
                                , "SIMPLE"
                                , "SIN"
                                , "SINH"
                                , "SIZE"
                                , "SKEW"
                                , "SKIP"
                                , "SMALLINT"
                                , "SOME"
                                , "SOUNDEX"
                                , "SOURCE"
                                , "SPACE"
                                , "SPATIAL"
                                , "SPECIFIC"
                                , "SPECIFICTYPE"
                                , "SPOOL"
                                , "SQL"
                                , "SQLEXCEPTION"
                                , "SQLSTATE"
                                , "SQLTEXT"
                                , "SQLWARNING"
                                , "SQL_BIG_RESULT"
                                , "SQL_CALC_FOUND_ROWS"
                                , "SQL_SMALL_RESULT"
                                , "SQRT"
                                , "SS"
                                , "SSL"
                                , "STANDARD"
                                , "START"
                                , "STARTING"
                                , "STARTUP"
                                , "STATE"
                                , "STATEMENT"
                                , "STATIC"
                                , "STATISTICS"
                                , "STAY"
                                , "STDDEV_POP"
                                , "STDDEV_SAMP"
                                , "STEPINFO"
                                , "STOGROUP"
                                , "STORED"
                                , "STORES"
                                , "STRAIGHT_JOIN"
                                , "STRING_CS"
                                , "STRUCTURE"
                                , "STYLE"
                                , "SUBMULTISET"
                                , "SUBSCRIBER"
                                , "SUBSET"
                                , "SUBSTR"
                                , "SUBSTRING"
                                , "SUBSTRING_REGEX"
                                , "SUCCEEDS"
                                , "SUCCESSFUL"
                                , "SUM"
                                , "SUMMARY"
                                , "SUSPEND"
                                , "SYMMETRIC"
                                , "SYNONYM"
                                , "SYSDATE"
                                , "SYSTEM"
                                , "SYSTEM_TIME"
                                , "SYSTEM_USER"
                                , "SYSTIMESTAMP"
                                , "TABLE"
                                , "TABLESAMPLE"
                                , "TABLESPACE"
                                , "TAN"
                                , "TANH"
                                , "TBL_CS"
                                , "TEMPORARY"
                                , "TERMINATE"
                                , "TERMINATED"
                                , "TEXTSIZE"
                                , "THAN"
                                , "THEN"
                                , "THRESHOLD"
                                , "TIME"
                                , "TIMESTAMP"
                                , "TIMEZONE_HOUR"
                                , "TIMEZONE_MINUTE"
                                , "TINYBLOB"
                                , "TINYINT"
                                , "TINYTEXT"
                                , "TITLE"
                                , "TO"
                                , "TOP"
                                , "TRACE"
                                , "TRAILING"
                                , "TRAN"
                                , "TRANSACTION"
                                , "TRANSLATE"
                                , "TRANSLATE_CHK"
                                , "TRANSLATE_REGEX"
                                , "TRANSLATION"
                                , "TREAT"
                                , "TRIGGER"
                                , "TRIM"
                                , "TRIM_ARRAY"
                                , "TRUE"
                                , "TRUNCATE"
                                , "TRY_CONVERT"
                                , "TSEQUAL"
                                , "TYPE"
                                , "UC"
                                , "UESCAPE"
                                , "UID"
                                , "UNDEFINED"
                                , "UNDER"
                                , "UNDO"
                                , "UNION"
                                , "UNIQUE"
                                , "UNKNOWN"
                                , "UNLOCK"
                                , "UNNEST"
                                , "UNPIVOT"
                                , "UNSIGNED"
                                , "UNTIL"
                                , "UPD"
                                , "UPDATE"
                                , "UPDATETEXT"
                                , "UPPER"
                                , "UPPERCASE"
                                , "USAGE"
                                , "USE"
                                , "USER"
                                , "USING"
                                , "UTC_DATE"
                                , "UTC_TIME"
                                , "UTC_TIMESTAMP"
                                , "VALIDATE"
                                , "VALIDPROC"
                                , "VALUE"
                                , "VALUES"
                                , "VALUE_OF"
                                , "VARBINARY"
                                , "VARBYTE"
                                , "VARCHAR"
                                , "VARCHAR2"
                                , "VARCHARACTER"
                                , "VARGRAPHIC"
                                , "VARIABLE"
                                , "VARIADIC"
                                , "VARIANT"
                                , "VARYING"
                                , "VAR_POP"
                                , "VAR_SAMP"
                                , "VCAT"
                                , "VERBOSE"
                                , "VERSIONING"
                                , "VIEW"
                                , "VIRTUAL"
                                , "VOLATILE"
                                , "VOLUMES"
                                , "WAIT"
                                , "WAITFOR"
                                , "WHEN"
                                , "WHENEVER"
                                , "WHERE"
                                , "WHILE"
                                , "WIDTH_BUCKET"
                                , "WINDOW"
                                , "WITH"
                                , "WITHIN"
                                , "WITHIN_GROUP"
                                , "WITHOUT"
                                , "WLM"
                                , "WORK"
                                , "WRITE"
                                , "WRITETEXT"
                                , "XMLCAST"
                                , "XMLEXISTS"
                                , "XMLNAMESPACES"
                                , "XOR"
                                , "YEAR"
                                , "YEARS"
                                , "YEAR_MONTH"
                                , "ZEROFILL"
                                , "ZEROIFNULL"
                                , "ZONE"
                                ]
                        }
                    )
                ,
                    ( Tcl
                    , FileTypeInfo
                        { ftSelector = [ext ".tcl", ext ".tk"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftKeywords = HM.empty
                        , ftIdentCharSet = Just (Unicode_, UnicodeNum_)
                        }
                    )
                ,
                    ( Text
                    , FileTypeInfo
                        { ftSelector =
                            [ ext ".txt"
                            , ext ".md"
                            , ext ".markdown"
                            , ext ".mdown"
                            , ext ".mkdn"
                            , ext ".mkd"
                            , ext ".mdwn"
                            , ext ".mdtxt"
                            , ext ".mdtext"
                            , ext ".text"
                            , name "README"
                            , name "INSTALL"
                            , name "VERSION"
                            , name "LICENSE"
                            , name "AUTHORS"
                            , name "CHANGELOG"
                            , name "go.sum"
                            ]
                        , ftKind = KindText
                        , ftComment = []
                        , ftChar = []
                        , ftString = []
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Unison
                    , FileTypeInfo
                        { ftSelector = [ext ".u"]
                        , ftKind = KindLanguage
                        , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_')
                        , ftKeywords =
                            types
                                [ "Text"
                                , "Number"
                                , "Boolean"
                                , "List"
                                , "Optional"
                                , "Maybe"
                                , "Either"
                                , "Tuple"
                                , "Function"
                                ]
                                <> reserved
                                    [ "type"
                                    , "ability"
                                    , "structural"
                                    , "unique"
                                    , "if"
                                    , "then"
                                    , "else"
                                    , "forall"
                                    , "handle"
                                    , "with"
                                    , "where"
                                    , "use"
                                    , "true"
                                    , "false"
                                    , "alias"
                                    , "typeLink"
                                    , "termLink"
                                    , "let"
                                    , "namespace"
                                    , "match"
                                    , "cases"
                                    ]
                        }
                    )
                ,
                    ( VHDL
                    , FileTypeInfo
                        { ftSelector = [ext ".vhd", ext ".vhdl"]
                        , ftKind = KindLanguage
                        , ftComment = ["--" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha, AlphaNum_)
                        , ftKeywords =
                            types
                                [ "std_logic"
                                , "std_logic_vector"
                                , "integer"
                                , "real"
                                , "boolean"
                                , "character"
                                , "string"
                                , "time"
                                , "bit"
                                , "bit_vector"
                                , "signed"
                                , "unsigned"
                                ]
                                <> reserved
                                    [ "abs"
                                    , "access"
                                    , "after"
                                    , "alias"
                                    , "all"
                                    , "and"
                                    , "architecture"
                                    , "array"
                                    , "assert"
                                    , "attribute"
                                    , "begin"
                                    , "block"
                                    , "body"
                                    , "buffer"
                                    , "bus"
                                    , "case"
                                    , "component"
                                    , "configuration"
                                    , "constant"
                                    , "disconnect"
                                    , "downto"
                                    , "else"
                                    , "elsif"
                                    , "end"
                                    , "entity"
                                    , "exit"
                                    , "file"
                                    , "for"
                                    , "function"
                                    , "generate"
                                    , "generic"
                                    , "group"
                                    , "guarded"
                                    , "if"
                                    , "impure"
                                    , "in"
                                    , "inertial"
                                    , "inout"
                                    , "is"
                                    , "label"
                                    , "library"
                                    , "linkage"
                                    , "literal"
                                    , "loop"
                                    , "map"
                                    , "mod"
                                    , "nand"
                                    , "new"
                                    , "next"
                                    , "nor"
                                    , "not"
                                    , "null"
                                    , "of"
                                    , "on"
                                    , "open"
                                    , "or"
                                    , "others"
                                    , "out"
                                    , "package"
                                    , "port"
                                    , "postponed"
                                    , "procedure"
                                    , "process"
                                    , "pure"
                                    , "range"
                                    , "record"
                                    , "register"
                                    , "reject"
                                    , "return"
                                    , "rol"
                                    , "ror"
                                    , "select"
                                    , "severity"
                                    , "signal"
                                    , "shared"
                                    , "sla"
                                    , "sli"
                                    , "sra"
                                    , "srl"
                                    , "subtype"
                                    , "then"
                                    , "to"
                                    , "transport"
                                    , "type"
                                    , "unaffected"
                                    , "units"
                                    , "until"
                                    , "use"
                                    , "variable"
                                    , "wait"
                                    , "when"
                                    , "while"
                                    , "with"
                                    , "xnor"
                                    , "xor"
                                    ]
                        }
                    )
                ,
                    ( Verilog
                    , FileTypeInfo
                        { ftSelector = [ext ".v", ext ".vh", ext ".sv"]
                        , ftKind = KindLanguage
                        , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            types
                                [ "wire"
                                , "reg"
                                , "integer"
                                , "real"
                                , "time"
                                , "parameter"
                                , "event"
                                , "genvar"
                                , "string"
                                ]
                                <> reserved
                                    [ "always"
                                    , "end"
                                    , "ifnone"
                                    , "or"
                                    , "rpmos"
                                    , "tranif1"
                                    , "and"
                                    , "endcase"
                                    , "initial"
                                    , "output"
                                    , "rtran"
                                    , "tri"
                                    , "assign"
                                    , "endmodule"
                                    , "inout"
                                    , "rtranif0"
                                    , "tri0"
                                    , "begin"
                                    , "endfunction"
                                    , "input"
                                    , "pmos"
                                    , "rtranif1"
                                    , "tri1"
                                    , "buf"
                                    , "endprimitive"
                                    , "posedge"
                                    , "scalared"
                                    , "triand"
                                    , "bufif0"
                                    , "endspecify"
                                    , "join"
                                    , "primitive"
                                    , "small"
                                    , "trior"
                                    , "bufif1"
                                    , "endtable"
                                    , "large"
                                    , "pull0"
                                    , "specify"
                                    , "trireg"
                                    , "case"
                                    , "endtask"
                                    , "macromodule"
                                    , "pull1"
                                    , "specparam"
                                    , "vectored"
                                    , "casex"
                                    , "medium"
                                    , "pullup"
                                    , "strong0"
                                    , "wait"
                                    , "casez"
                                    , "for"
                                    , "module"
                                    , "pulldown"
                                    , "strong1"
                                    , "wand"
                                    , "cmos"
                                    , "force"
                                    , "nand"
                                    , "rcmos"
                                    , "supply0"
                                    , "weak0"
                                    , "deassign"
                                    , "forever"
                                    , "negedge"
                                    , "supply1"
                                    , "weak1"
                                    , "default"
                                    , "for"
                                    , "nmos"
                                    , "realtime"
                                    , "table"
                                    , "while"
                                    , "defparam"
                                    , "function"
                                    , "nor"
                                    , "task"
                                    , "disable"
                                    , "highz0"
                                    , "not"
                                    , "release"
                                    , "wor"
                                    , "edge"
                                    , "highz1"
                                    , "notif0"
                                    , "repeat"
                                    , "tran"
                                    , "xnor"
                                    , "else"
                                    , "if"
                                    , "notif1"
                                    , "rnmos"
                                    , "tranif0"
                                    , "xor"
                                    ]
                        }
                    )
                ,
                    ( Yaml
                    , FileTypeInfo
                        { ftSelector = [ext ".yaml", ext ".yml"]
                        , ftKind = KindConfig
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Toml
                    , FileTypeInfo
                        { ftSelector = [ext ".toml", name "Cargo.lock"]
                        , ftKind = KindConfig
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\""]
                        , ftRawString = ["'" ~~ "'", "'''" ~~ "'''"]
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Ini
                    , FileTypeInfo
                        { ftSelector = [ext ".ini"]
                        , ftKind = KindConfig
                        , ftComment = [";" ~~ "\n", "#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Nothing
                        , ftKeywords = HM.empty
                        }
                    )
                ,
                    ( Zig
                    , FileTypeInfo
                        { ftSelector = [ext ".zig"]
                        , ftKind = KindLanguage
                        , ftComment = ["//" ~~ "\n"]
                        , ftChar = ["'" ~~ "'"]
                        , ftString = ["\"" ~~ "\""]
                        , ftRawString = ["\\" ~~ "\n"]
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            types
                                [ "i8"
                                , "u8"
                                , "i16"
                                , "u16"
                                , "i32"
                                , "u32"
                                , "i64"
                                , "u64"
                                , "i128"
                                , "u128"
                                , "isize"
                                , "usize"
                                , "c_short"
                                , "c_ushort"
                                , "c_int"
                                , "c_uint"
                                , "c_long"
                                , "c_ulong"
                                , "c_longlong"
                                , "c_ulonglong"
                                , "c_longdouble"
                                , "f16"
                                , "f32"
                                , "f64"
                                , "f80"
                                , "f128"
                                , "bool"
                                , "anyopaque"
                                , "void"
                                , "noreturn"
                                , "type"
                                , "anyerror"
                                , "comptime_int"
                                , "comptime_float"
                                ]
                                <> reserved
                                    [ "addrspace"
                                    , "align"
                                    , "allowzero"
                                    , "and"
                                    , "anyframe"
                                    , "anytype"
                                    , "asm"
                                    , "async"
                                    , "await"
                                    , "break"
                                    , "catch"
                                    , "comptime"
                                    , "const"
                                    , "continue"
                                    , "defer"
                                    , "else"
                                    , "enum"
                                    , "errdefer"
                                    , "error"
                                    , "export"
                                    , "extern"
                                    , "fn"
                                    , "for"
                                    , "if"
                                    , "inline"
                                    , "linksection"
                                    , "noalias"
                                    , "noinline"
                                    , "nosuspend"
                                    , "or"
                                    , "orelse"
                                    , "packed"
                                    , "pub"
                                    , "resume"
                                    , "return"
                                    , "struct"
                                    , "suspend"
                                    , "switch"
                                    , "test"
                                    , "threadlocal"
                                    , "try"
                                    , "union"
                                    , "unreachable"
                                    , "usingnamespace"
                                    , "var"
                                    , "volatile"
                                    , "while"
                                    ]
                        }
                    )
                ,
                    ( Zsh
                    , FileTypeInfo
                        { ftSelector = [ext ".zsh"]
                        , ftKind = KindScript
                        , ftComment = ["#" ~~ "\n"]
                        , ftChar = []
                        , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                        , ftRawString = []
                        , ftIdentCharSet = Just (Alpha_, AlphaNum_)
                        , ftKeywords =
                            reserved
                                [ "do"
                                , "done"
                                , "esac"
                                , "then"
                                , "elif"
                                , "else"
                                , "fi"
                                , "for"
                                , "case"
                                , "if"
                                , "while"
                                , "function"
                                , "repeat"
                                , "time"
                                , "until"
                                , "select"
                                , "coproc"
                                , "nocorrect"
                                , "foreach"
                                , "end"
                                ]
                        }
                    )
                ]
     )

mkContextFilterFn :: Maybe FileType -> ContextFilter -> Bool -> (T.Text -> T.Text)
mkContextFilterFn _ (isContextFilterAll -> True) False = id
mkContextFilterFn Nothing _ _ = id
mkContextFilterFn (Just ftype) filt useMarkers
    | Just fun <- parFunc = fun filt
    | otherwise = id
  where
    parFunc = mkFilterFunction useMarkers =<< Map.lookup ftype (unMapInfo fileTypeInfoMap)

fileTypeLookup :: Options -> OS.OsPath -> Maybe (FileType, FileKind)
fileTypeLookup opts f = forcedType opts <|> lookupFileType f (code_only opts) (hdr_only opts)
  where
    lookupFileType :: OS.OsPath -> Bool -> Bool -> Maybe (FileType, FileKind)
    lookupFileType file False False = Map.lookup (Name $ OS.takeFileName file) m <|> Map.lookup (Ext e) m <|> Map.lookup (Hdr e) m
    lookupFileType file True False = Map.lookup (Name $ OS.takeFileName file) m <|> Map.lookup (Ext e) m
    lookupFileType file False True = Map.lookup (Name $ OS.takeFileName file) m <|> Map.lookup (Hdr e) m
    lookupFileType _ True True = errorWithoutStackTrace "CGrep: code-only and hdr-only are mutually exclusive!"
    e = OS.takeExtension f
    m = unMap fileTypeMap
{-# INLINE fileTypeLookup #-}

fileTypeInfoLookup :: Options -> OS.OsPath -> Maybe (FileType, FileTypeInfo)
fileTypeInfoLookup opts f = fileTypeLookup opts f >>= \(typ, _kid) -> (typ,) <$> Map.lookup typ (unMapInfo fileTypeInfoMap)
{-# INLINE fileTypeInfoLookup #-}

fileTypeMap :: FileTypeMap
fileTypeMap = FileTypeMap $ Map.fromList $ concatMap (\(typ, FileTypeInfo{..}) -> map (,(typ, ftKind)) ftSelector) $ Map.toList (unMapInfo fileTypeInfoMap)
{-# NOINLINE fileTypeMap #-}

dumpFileTypeInfoMap :: FileTypeInfoMap -> IO ()
dumpFileTypeInfoMap m = forM_ ((Map.toList . unMapInfo) m) $ \(l, ex) ->
    putStrLn $ show l <> [' ' | _ <- [length (show l) .. 12]] <> "-> " <> show (ftSelector ex)

-- dumpFileTypeMap :: FileTypeMap -> IO ()
-- dumpFileTypeMap m = forM_ (Map.toList (unMap m)) $ \(e, l) ->
--     putStrLn $ show e <> [' ' | _ <- [length (show e) .. 12]] <> "-> " <> show l

forcedType :: Options -> Maybe (FileType, FileKind)
forcedType Options{..}
    | Just typ <- force_type = Map.lookup (ext typ) m <|> Map.lookup (name typ) m
    | otherwise = Nothing
  where
    m = unMap fileTypeMap

mkFilterFunction :: Bool -> FileTypeInfo -> Maybe FilterFunction
mkFilterFunction useMarkers FileTypeInfo{..} =
    Just $
        runContextFilter (mkParConfig ftComment ftString ftRawString ftChar useMarkers)
{-# INLINE mkFilterFunction #-}
