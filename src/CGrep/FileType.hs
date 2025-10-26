---
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
{-# LANGUAGE DeriveLift #-}

module CGrep.FileType (
    FileType (..),
    FileSelector (..),
    readTypeList,
    readKindList,
    ext,
    hdr,
    name,
)
where

import CGrep.FileKind (FileKind)
import Language.Haskell.TH.Syntax (Lift)
import System.OsPath (OsPath)
import qualified System.OsPath as OS
import Util (prettyRead)

data FileType
    = Agda
    | Assembly
    | Awk
    | Bash
    | C
    | CMake
    | Cabal
    | Chapel
    | Clojure
    | Coffee
    | Conf
    | Cpp
    | Csh
    | Csharp
    | Css
    | Cql
    | D
    | Dart
    | Dhall
    | Elm
    | Elixir
    | Erlang
    | Fish
    | Fortran
    | Fsharp
    | Go
    | GoMod
    | Haskell
    | Html
    | Idris
    | Java
    | Javascript
    | Json
    | Julia
    | Kotlin
    | Ksh
    | Latex
    | Lisp
    | Lua
    | Make
    | Nim
    | Nmap
    | OCaml
    | ObjectiveC
    | PHP
    | Perl
    | Python
    | R
    | Ruby
    | Rust
    | Scala
    | SmallTalk
    | Swift
    | Sql
    | Tcl
    | Text
    | Unison
    | VHDL
    | Verilog
    | Yaml
    | Toml
    | Ini
    | Zig
    | Zsh
    deriving stock (Read, Show, Eq, Ord, Bounded, Lift)

data FileSelector = Name OsPath | Ext OsPath | Hdr OsPath
    deriving stock (Eq, Ord, Lift)

ext :: String -> FileSelector
ext = Ext . OS.unsafeEncodeUtf
{-# INLINE ext #-}

hdr :: String -> FileSelector
hdr = Hdr . OS.unsafeEncodeUtf
{-# INLINE hdr #-}

name :: String -> FileSelector
name = Name . OS.unsafeEncodeUtf
{-# INLINE name #-}

instance Show FileSelector where
    show (Name x) = show x
    show (Ext e) = "*." <> show e
    show (Hdr e) = "*." <> show e

-- utility functions

readTypeList :: [String] -> ([FileType], [FileType], [FileType])
readTypeList = foldl run ([], [], [])
  where
    run :: ([FileType], [FileType], [FileType]) -> String -> ([FileType], [FileType], [FileType])
    run (l1, l2, l3) l
        | '+' : xs <- l = (l1, prettyRead xs "Type" : l2, l3)
        | '-' : xs <- l = (l1, l2, prettyRead xs "Type" : l3)
        | otherwise = (prettyRead l "Type" : l1, l2, l3)

readKindList :: [String] -> [FileKind]
readKindList = map (`prettyRead` "Kind")
