--
-- Copyright (c) 2013 Bonelli Nicola <bonelli@antifork.org>
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

module CGrep.Lang (Lang(..), langMap, getLang, splitLangList,
                   dumpLangMap, dumpLangRevMap) where

import qualified Data.Map as Map
import System.FilePath(takeExtension, takeFileName)
import Control.Monad
import Control.Applicative
import Data.Maybe

import Options
import Util

data Lang = Awk | C | Cpp | Csharp | Chapel | Coffee | Css | CMake | D | Erlang | Fsharp | Go | Haskell |
                Html | Java | Javascript | Latex | Lua | Make | OCaml | ObjectiveC |
                Perl | PHP | Python | Ruby | Scala | Tcl | Text | Shell | Verilog | VHDL | Vim
                    deriving (Read, Show, Eq, Ord, Bounded)


data FileType = Name String | Ext String
                    deriving (Eq, Ord)

instance Show FileType where
    show (Name x) = x
    show (Ext  e) = "*." ++ e


type LangMapType    = Map.Map Lang [FileType]
type LangRevMapType = Map.Map FileType Lang


langMap :: LangMapType
langMap = Map.fromList [
            (Awk,       [Ext "awk", Ext "mawk", Ext "gawk"]),
            (C,         [Ext "c", Ext "C"]),
            (Cpp,       [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc", Ext "cp", Ext "tcc", Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP", Ext "hxx", Ext "hh", Ext "hp"]),
            (Csharp,    [Ext "cs", Ext "CS"]),
            (Coffee,    [Ext "coffee"]),
            (Chapel,    [Ext "chpl"]),
            (Css,       [Ext "css"]),
            (CMake,     [Name "CMakeLists.txt", Ext "cmake"]),
            (D,         [Ext "d", Ext "D"]),
            (Erlang,    [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"]),
            (Fsharp,    [Ext "fs", Ext "fsx", Ext "fsi"]),
            (Go,        [Ext "go"]),
            (Haskell,   [Ext "hs", Ext "lhs", Ext "hsc"]),
            (Html,      [Ext "htm", Ext "html"]),
            (Java,      [Ext "java"]),
            (Javascript,[Ext "js"]),
            (Latex,     [Ext "latex", Ext "tex"]),
            (Lua,       [Ext "lua"]),
            (Make,      [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak"]),
            (OCaml ,    [Ext "ml", Ext "mli"]),
            (ObjectiveC,[Ext "m", Ext "mi"]),
            (Perl,      [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]),
            (PHP,       [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"]),
            (Python,    [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]),
            (Ruby,      [Ext "rb", Ext "ruby"]),
            (Scala,     [Ext "scala"]),
            (Tcl,       [Ext "tcl", Ext "tk"]),
            (Text,      [Ext "txt", Ext "md", Name "README", Name "INSTALL"]),
            (Shell,     [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"]),
            (Verilog,   [Ext "v", Ext "vh", Ext "sv"]),
            (VHDL,      [Ext "vhd", Ext "vhdl"]),
            (Vim,       [Ext "vim"])
          ]


langRevMap :: LangRevMapType
langRevMap = Map.fromList $ concatMap (\(l, xs) -> map (\x -> (x,l)) xs ) $ Map.toList langMap

-- utility functions

lookupLang :: FilePath -> Maybe Lang
lookupLang f = Map.lookup (Name $ takeFileName f) langRevMap <|> Map.lookup (Ext (let name = takeExtension f in case name of ('.':xs) -> xs; _ -> name )) langRevMap


forcedLang :: Options -> Maybe Lang
forcedLang Options{ force_language = l }
    | Nothing <- l = Nothing
    | otherwise    = Map.lookup (Ext $ fromJust l) langRevMap <|> Map.lookup (Name $ fromJust l) langRevMap


getLang :: Options -> FilePath -> Maybe Lang
getLang opts f = forcedLang opts <|> lookupLang f



dumpLangMap :: LangMapType -> IO ()
dumpLangMap m = forM_ (Map.toList m) $ \(l, ex) ->
                putStrLn $ show l ++ [ ' ' | _ <- [length (show l)..12]] ++ "-> " ++ show ex


dumpLangRevMap :: LangRevMapType -> IO ()
dumpLangRevMap m = forM_ (Map.toList m) $ \(ext, l) ->
                    putStrLn $ show ext ++ [ ' ' | _ <- [length (show ext)..12 ]] ++ "-> " ++ show l


splitLangList :: [String] -> ([Lang], [Lang], [Lang])
splitLangList  = foldl run ([],[],[])
    where run :: ([Lang], [Lang], [Lang]) -> String -> ([Lang], [Lang], [Lang])
          run (l1, l2, l3) l
            | '+':xs <- l = (l1, prettyRead xs "Lang" : l2, l3)
            | '-':xs <- l = (l1, l2, prettyRead xs "Lang" : l3)
            | otherwise   = (prettyRead l  "Lang" : l1, l2, l3)

