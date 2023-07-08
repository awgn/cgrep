{-# LANGUAGE OverloadedStrings #-}
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

module CGrep.FileTypeMap (
    FileTypeInfo (..),
    FileTypeInfoMap (..),
    WordType (..),
    CharIdentifierF,
    fileTypeLookup,
    fileTypeInfoLookup,
    fileTypeInfoMap,
    dumpFileTypeInfoMap,
    contextFilter,
) where

import CGrep.ContextFilter (
    ContextFilter,
    FilterFunction,
    isContextFilterAll,
    mkParConfig,
    runContextFilter,
 )
import CGrep.FileType (FileSelector (..), FileType (..))

import CGrep.Types (Text8)
import Control.Applicative (Alternative ((<|>)))
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Options (Options (Options, keyword, type_force))

import qualified Data.Array.BitArray as BA

import Data.List (findIndex)
import qualified Data.Set as S

import CGrep.Boundary (Boundary (Boundary))
import CGrep.Parser.Char (
    isAlphaNum_,
    isAlphaNum_',
    isAlphaNum_and,
    isAlpha_,
    isAlpha_',
    isAlpha_and,
 )
import Data.Char (isAlpha, isAlphaNum)

import System.Posix.FilePath (RawFilePath, takeBaseName, takeExtension, takeFileName)

import Data.Aeson (Value (Bool, String))
import Data.Bits (Bits (isSigned))
import qualified Data.HashMap.Strict as HM
import Data.MonoTraversable (WrappedPoly)
import GHC.Conc (BlockReason (BlockedOnBlackHole))
import CGrep.FileKind

newtype FileTypeInfoMap = FileTypeInfoMap {
    unMapInfo :: Map.Map FileType FileTypeInfo
}

newtype FileTypeMap = FileTypeMap {
    unMap ::  Map.Map FileSelector (FileType, FileKind)
}

type CharIdentifierF = (Char -> Bool)

data WordType = Keyword | NativeType
    deriving (Eq, Ord, Show)

data FileTypeInfo = FileTypeInfo
    { ftSelector :: [FileSelector]
    , ftKind :: FileKind
    , ftChar :: [Boundary]
    , ftString :: [Boundary]
    , ftRawString :: [Boundary]
    , ftComment :: [Boundary]
    , ftIdentifierChars :: Maybe (CharIdentifierF, CharIdentifierF)
    , ftKeywords :: HM.HashMap C.ByteString WordType
    }

fileTypeInfoMap :: FileTypeInfoMap
fileTypeInfoMap =  FileTypeInfoMap $
    Map.fromList
        [
            ( Agda
            , FileTypeInfo
                { ftSelector = [Ext "agda", Ext "lagda"]
                , ftKind = KindLanguage
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "s", Ext "S"]
                , ftKind = KindLanguage
                , ftComment = ["#" ~~ "\n", ";" ~~ "\n", "|" ~~ "\n", "!" ~~ "\n", "/*" ~~ "*/"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Awk
            , FileTypeInfo
                { ftSelector = [Ext "awk", Ext "mawk", Ext "gawk"]
                , ftKind = KindScript
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "sh", Ext "bash"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "c", Ext "C", Ext "inc"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["R\"" ~~ "\""]
                , ftIdentifierChars = Nothing
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
            ( CMake
            , FileTypeInfo
                { ftSelector = [Name "CMakeLists.txt", Ext "cmake"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Cabal
            , FileTypeInfo
                { ftSelector = [Ext "cabal"]
                , ftKind = KindConfig
                , ftComment = ["--" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Chapel
            , FileTypeInfo
                { ftSelector = [Ext "chpl"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''"]
                , ftIdentifierChars = Just (isAlpha_, isAlphaNum_and "$")
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
                { ftSelector = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"]
                , ftKind = KindLanguage
                , ftComment = [";" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha, isAlphaNum_and "*+!-_?")
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
                { ftSelector = [Ext "coffee"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n", "###" ~~ "###"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = ["'''" ~~ "'''", "\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Just (isAlpha_and "$", isAlphaNum_and "$")
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
                { ftSelector = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"]
                , ftKind = KindConfig
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Cpp
            , FileTypeInfo
                { ftSelector =
                    [ Ext "cpp"
                    , Ext "CPP"
                    , Ext "cxx"
                    , Ext "cc"
                    , Ext "cp"
                    , Ext "c++"
                    , Ext "tcc"
                    , Ext "h"
                    , Ext "H"
                    , Ext "hpp"
                    , Ext "ipp"
                    , Ext "HPP"
                    , Ext "hxx"
                    , Ext "hh"
                    , Ext "hp"
                    , Ext "h++"
                    , Ext "cu"
                    , Ext "cuh"
                    ]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["R\"(" ~~ ")\"", "R\"-(" ~~ ")-\"", "R\"--(" ~~ ")--\""]
                , ftIdentifierChars = Nothing
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
            ( Csharp
            , FileTypeInfo
                { ftSelector = [Ext "cs", Ext "CS"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "csh", Ext "tcsh"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "css"]
                , ftKind = KindMarkup
                , ftComment = ["/*" ~~ "*/"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_and "-", isAlphaNum_and "-")
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Cql
            , FileTypeInfo
                { ftSelector = [Ext "cql"]
                , ftKind = KindLanguage
                , ftComment = ["--" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "d", Ext "D"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["r\"" ~~ "\"", "`" ~~ "`"]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "dart"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = ["'''" ~~ "'''", "\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "dhall"]
                , ftKind = KindConfig
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_', isAlphaNum_')
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
                { ftSelector = [Ext "ex", Ext "exs"]
                , ftKind = KindLanguage
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "elm"]
                , ftKind = KindLanguage
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Just (isAlpha, isAlphaNum_)
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
                { ftSelector = [Ext "erl", Ext "ERL", Ext "hrl", Ext "HRL"]
                , ftKind = KindLanguage
                , ftComment = ["%" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "fish"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                    [ Ext "f"
                    , Ext "for"
                    , Ext "ftn"
                    , Ext "F"
                    , Ext "FOR"
                    , Ext "FTN"
                    , Ext "fpp"
                    , Ext "FPP"
                    , Ext "f90"
                    , Ext "f95"
                    , Ext "f03"
                    , Ext "f08"
                    , Ext "F90"
                    , Ext "F95"
                    , Ext "F03"
                    , Ext "F08"
                    ]
                , ftKind = KindLanguage
                , ftComment = ["!" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "fs", Ext "fsx", Ext "fsi"]
                , ftKind = KindLanguage
                , ftComment = ["(*" ~~ "*)", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Just (isAlpha_and "$@`?", isAlphaNum_and "$@`?")
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
                { ftSelector = [Ext "go"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["`" ~~ "`"]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Name "go.mod"]
                , ftKind = KindConfig
                , ftComment = ["//" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
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
                { ftSelector = [Ext "hs", Ext "lhs", Ext "hsc"]
                , ftKind = KindLanguage
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["[r|" ~~ "|]", "[q|" ~~ "|]", "[s|" ~~ "|]", "[here|" ~~ "|]", "[i|" ~~ "|]"]
                , ftIdentifierChars = Just (isAlpha_', isAlphaNum_')
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
                { ftSelector = [Ext "htm", Ext "html"]
                , ftKind = KindMarkup
                , ftComment = ["<!--" ~~ "-->"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_, isAlpha_and "-:.")
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Idris
            , FileTypeInfo
                { ftSelector = [Ext "idr", Ext "lidr"]
                , ftKind = KindLanguage
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n", "|||" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_', isAlphaNum_')
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
                { ftSelector = [Ext "java"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_and "$", isAlphaNum_and "$")
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
                { ftSelector = [Ext "js"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_and "$", isAlphaNum_and "$")
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
                { ftSelector = [Ext "json", Ext "ndjson"]
                , ftKind = KindData
                , ftComment = []
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Julia
            , FileTypeInfo
                { ftSelector = [Ext "jl"]
                , ftKind = KindLanguage
                , ftComment = ["#" ~~ "\n", "#-" ~~ "-#"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "kt", Ext "kts", Ext "ktm"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "ksh"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "latex", Ext "tex"]
                , ftKind = KindMarkup
                , ftComment = ["%" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Lisp
            , FileTypeInfo
                { ftSelector = [Ext "lisp", Ext "cl"]
                , ftKind = KindLanguage
                , ftComment = [";" ~~ "\n", "#|" ~~ "|#"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_, isAlphaNum_and "!$%&*+-./:<=>?@^~")
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
                { ftSelector = [Ext "lua"]
                , ftKind = KindLanguage
                , ftComment = ["--[[" ~~ "--]]", "--" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = ["[===[" ~~ "]===]", "[==[" ~~ "]==]", "[=[" ~~ "]=]", "[[" ~~ "]]"]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext "mak", Ext "make"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_and "-", isAlpha_and "-")
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Nmap
            , FileTypeInfo
                { ftSelector = [Ext "nse"]
                , ftKind = KindScript
                , ftComment = ["--" ~~ "\n", "[[" ~~ "]]"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Nim
            , FileTypeInfo
                { ftSelector = [Ext "nim"]
                , ftKind = KindLanguage
                , ftComment = ["#[" ~~ "#]", "#" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "ml", Ext "mli"]
                , ftKind = KindLanguage
                , ftComment = ["(*" ~~ "*)"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["{id|" ~~ "|id}"]
                , ftIdentifierChars = Just (isAlpha_, isAlphaNum_')
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
                { ftSelector = [Ext "m", Ext "mi"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "php", Ext "php3", Ext "php4", Ext "php5", Ext "phtml"]
                , ftKind = KindScript
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n", "#" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = ["<<END" ~~ "END;", "<<'END'" ~~ "END;"]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]
                , ftKind = KindScript
                , ftComment = ["=pod" ~~ "=cut", "#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = ["<<\"END\";" ~~ "END", "<<'END'" ~~ "END", "<<'EOT';" ~~ "EOT", "<<\"EOT\";" ~~ "EOT"]
                , ftIdentifierChars = Nothing
                , ftKeywords = reserved []
                }
            )
        ,
            ( Python
            , FileTypeInfo
                { ftSelector = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]
                , ftKind = KindLanguage
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''", "r'" ~~ "'"]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"]
                , ftKind = KindLanguage
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_and ".", isAlphaNum_and ".")
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
                { ftSelector = [Ext "rb", Ext "ruby"]
                , ftKind = KindLanguage
                , ftComment = ["=begin" ~~ "=end", "#" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["'" ~~ "'", "\"" ~~ "\"", "%|" ~~ "|", "%q(" ~~ ")", "%Q(" ~~ ")"]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "rs", Ext "rlib"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["r##\"" ~~ "\"##", "r#\"" ~~ "\"#", "r\"" ~~ "\""]
                , ftIdentifierChars = Just (isAlpha_, isAlphaNum_and "#")
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
                { ftSelector = [Ext "scala"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_and "$", isAlphaNum_and "$")
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
                { ftSelector = [Ext "st", Ext "gst"]
                , ftKind = KindLanguage
                , ftComment = ["\"" ~~ "\""]
                , ftChar = ["$" ~~ ""]
                , ftString = ["'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha, isAlphaNum)
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
                { ftSelector = [Ext "swift"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\"\"\"" ~~ "\"\"\""]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "sql"]
                , ftKind = KindLanguage
                , ftComment = ["--" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "tcl", Ext "tk"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftKeywords = HM.empty
                , ftIdentifierChars = Just (isAlpha, isAlphaNum)
                }
            )
        ,
            ( Text
            , FileTypeInfo
                { ftSelector =
                    [ Ext "txt"
                    , Ext "md"
                    , Ext "markdown"
                    , Ext "mdown"
                    , Ext "mkdn"
                    , Ext "mkd"
                    , Ext "mdwn"
                    , Ext "mdtxt"
                    , Ext "mdtext"
                    , Ext "text"
                    , Name "README"
                    , Name "INSTALL"
                    , Name "VERSION"
                    , Name "LICENSE"
                    , Name "AUTHORS"
                    , Name "CHANGELOG"
                    , Name "go.sum"
                    ]
                , ftKind = KindText
                , ftComment = []
                , ftChar = []
                , ftString = []
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Unison
            , FileTypeInfo
                { ftSelector = [Ext "u"]
                , ftKind = KindLanguage
                , ftComment = ["{-" ~~ "-}", "--" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\""]
                , ftRawString = []
                , ftIdentifierChars = Just (isAlpha_', isAlphaNum_and ['!', '\''])
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
                { ftSelector = [Ext "vhd", Ext "vhdl"]
                , ftKind = KindLanguage
                , ftComment = ["--" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "v", Ext "vh", Ext "sv"]
                , ftKind = KindLanguage
                , ftComment = ["/*" ~~ "*/", "//" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "yaml", Ext "yml"]
                , ftKind = KindConfig
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "'" ~~ "'"]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Toml
            , FileTypeInfo
                { ftSelector = [Ext "toml", Name "Cargo.lock"]
                , ftKind = KindConfig
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\""]
                , ftRawString = ["'" ~~ "'", "'''" ~~ "'''"]
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Ini
            , FileTypeInfo
                { ftSelector = [Ext "ini"]
                , ftKind = KindConfig
                , ftComment = [";" ~~ "\n", "#" ~~ "\n"]
                , ftChar = []
                , ftString = ["\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Just (const False, const False)
                , ftKeywords = HM.empty
                }
            )
        ,
            ( Zig
            , FileTypeInfo
                { ftSelector = [Ext "zig"]
                , ftKind = KindLanguage
                , ftComment = ["//" ~~ "\n"]
                , ftChar = ["'" ~~ "'"]
                , ftString = ["\"" ~~ "\""]
                , ftRawString = ["\\" ~~ "\n"]
                , ftIdentifierChars = Nothing
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
                { ftSelector = [Ext "zsh"]
                , ftKind = KindScript
                , ftComment = ["#" ~~ "\n"]
                , ftChar = []
                , ftString = ["'" ~~ "'", "\"" ~~ "\""]
                , ftRawString = []
                , ftIdentifierChars = Nothing
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


reserved :: [C.ByteString] -> HM.HashMap C.ByteString WordType
reserved = HM.fromList . map (,Keyword)
{-# INLINE reserved #-}

types :: [C.ByteString] -> HM.HashMap C.ByteString WordType
types = HM.fromList . map (,NativeType)
{-# INLINE types #-}

mkFilterFunction :: Bool -> FileTypeInfo -> Maybe FilterFunction
mkFilterFunction alterBoundary FileTypeInfo{..} =
    Just $
        runContextFilter (mkParConfig ftComment ftString ftRawString ftChar alterBoundary)
{-# INLINE mkFilterFunction #-}

contextFilter :: Maybe FileType -> ContextFilter -> Bool -> Text8 -> Text8
contextFilter _ (isContextFilterAll -> True) False txt = txt
contextFilter Nothing _ _ txt = txt
contextFilter (Just ftype) filt alterBoundary txt
    | Just fun <- parFunc = fun filt txt
    | otherwise = txt
  where
    parFunc = mkFilterFunction alterBoundary =<< Map.lookup ftype (unMapInfo fileTypeInfoMap)
{-# INLINE contextFilter #-}

fileTypeLookup :: Options -> RawFilePath -> Maybe (FileType, FileKind)
fileTypeLookup opts f = forcedType opts <|> lookupFileType f
  where
    lookupFileType :: RawFilePath -> Maybe (FileType, FileKind)
    lookupFileType f = Map.lookup (Name $ takeFileName f) m <|> Map.lookup (Ext (C.dropWhile (== '.') $ takeExtension f)) m
    m = unMap fileTypeMap
{-# INLINE fileTypeLookup #-}


fileTypeInfoLookup :: Options -> RawFilePath -> Maybe (FileType, FileTypeInfo)
fileTypeInfoLookup opts f = fileTypeLookup opts f >>= \(typ, kid) -> (typ,) <$> Map.lookup typ (unMapInfo fileTypeInfoMap)
{-# INLINE fileTypeInfoLookup #-}

fileTypeMap :: FileTypeMap
fileTypeMap = FileTypeMap $ Map.fromList $ concatMap (\(typ, FileTypeInfo{..}) -> map (, (typ, ftKind)) ftSelector) $ Map.toList (unMapInfo fileTypeInfoMap)
{-# NOINLINE fileTypeMap #-}

dumpFileTypeInfoMap :: FileTypeInfoMap -> IO ()
dumpFileTypeInfoMap m = forM_ ((Map.toList . unMapInfo) m) $ \(l, ex) ->
    putStrLn $ show l <> [' ' | _ <- [length (show l) .. 12]] <> "-> " <> show (ftSelector ex)

dumpFileTypeMap :: FileTypeMap -> IO ()
dumpFileTypeMap m = forM_ (Map.toList (unMap m)) $ \(ext, l) ->
    putStrLn $ show ext <> [' ' | _ <- [length (show ext) .. 12]] <> "-> " <> show l

forcedType :: Options -> Maybe (FileType, FileKind)
forcedType Options{type_force = l}
    | Just typ <- l = Map.lookup (Ext $ C.pack typ) m <|> Map.lookup (Name $ C.pack typ) m
    | otherwise = Nothing
        where m = unMap fileTypeMap

(~~) :: C.ByteString -> C.ByteString -> Boundary
(~~) = Boundary
{-# INLINE (~~) #-}
