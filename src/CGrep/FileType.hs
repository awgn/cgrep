---
-- Copyright (c) 2013-2023 Nicola Bonelli <nicola@larthia.com>
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

module CGrep.FileType (
      FileType(..)
    , FileSelector(..)
    , readTypeList
    , readKindList )
    where

import qualified Data.Map as Map
import Control.Monad ( forM_ )
import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe ( fromJust )

import qualified Data.ByteString.Char8 as C
import Options ( Options(Options, type_force) )
import Util ( prettyRead )
import System.Posix.FilePath (RawFilePath)
import CGrep.FileKind ( FileKind )


data FileType = Agda | Assembly | Awk  | Bash | C | CMake | Cabal | Chapel | Clojure | Coffee | Conf | Cpp  | Csh | Csharp | Css | Cql |
                D | Dart | Dhall | Elm | Elixir | Erlang | Fish | Fortran | Fsharp | Go | GoMod | Haskell | Html | Idris | Java | Javascript | Json | Julia | Kotlin |
                Ksh | Latex | Lisp | Lua | Make | Nim | Nmap | OCaml | ObjectiveC | PHP | Perl | Python | R | Ruby | Rust | Scala | SmallTalk | Swift | Sql | Tcl |
                Text | Unison | VHDL | Verilog | Yaml | Toml | Ini | Zig | Zsh
                deriving stock (Read, Show, Eq, Ord, Bounded)

data FileSelector = Name RawFilePath| Ext C.ByteString
    deriving stock (Eq, Ord)


instance Show FileSelector where
    show (Name x) = C.unpack x
    show (Ext  e) = "*." <> C.unpack e


-- utility functions

readTypeList :: [String] -> ([FileType], [FileType], [FileType])
readTypeList  = foldl run ([],[],[])
    where run :: ([FileType], [FileType], [FileType]) -> String -> ([FileType], [FileType], [FileType])
          run (l1, l2, l3) l
            | '+':xs <- l = (l1, prettyRead xs "Type" : l2, l3)
            | '-':xs <- l = (l1, l2, prettyRead xs "Type" : l3)
            | otherwise   = (prettyRead l "Type" : l1, l2, l3)

readKindList :: [String] -> [FileKind]
readKindList = map (`prettyRead` "Kind")