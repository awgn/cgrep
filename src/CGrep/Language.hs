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

module CGrep.Language ( Language(..), FileType(..), splitLanguagesList) where

import qualified Data.Map as Map
import System.FilePath(takeExtension, takeFileName)
import Control.Monad ( forM_ )
import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe ( fromJust )

import qualified Data.ByteString.Char8 as C
import Options ( Options(Options, language_force) )
import Util ( prettyRead )


data Language = Agda | Assembly | Awk  | C | CMake | Cabal | Chapel | Clojure | Coffee | Conf | Cpp  | Csharp | Css |
                D | Dart | Elm | Elixir | Erlang | Eta | Fortran | Fsharp | Go | Haskell | Html | Idris | Java | Javascript | Json | Julia | Kotlin |
                Latex | Lisp | Lua | Make | Nim | Nmap | OCaml | ObjectiveC | PHP | Perl | Python | R | Ruby | Rust | Scala | SmallTalk | Shell | Swift | Tcl |
                Text | VHDL | Verilog | Vim | Yaml
                deriving (Read, Show, Eq, Ord, Bounded)


data FileType = Name String | Ext String
    deriving (Eq, Ord)


instance Show FileType where
    show (Name x) = x
    show (Ext  e) = "*." <> e


-- utility functions

splitLanguagesList :: [String] -> ([Language], [Language], [Language])
splitLanguagesList  = foldl run ([],[],[])
    where run :: ([Language], [Language], [Language]) -> String -> ([Language], [Language], [Language])
          run (l1, l2, l3) l
            | '+':xs <- l = (l1, prettyRead xs "Lang" : l2, l3)
            | '-':xs <- l = (l1, l2, prettyRead xs "Lang" : l3)
            | otherwise   = (prettyRead l  "Lang" : l1, l2, l3)
