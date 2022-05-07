--
-- Copyright (c) 2013-2019 Nicola Bonelli <nicola@pfq.io>
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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module CGrep.Languages ( Language(..)
                  , languagesMap
                  , languageLookup
                  , splitLanguagesList
                  , dumpLanguagesMap
                  , dumpLanguagesRevMap) where

import qualified Data.Map as Map
import System.FilePath(takeExtension, takeFileName)
import Control.Monad ( forM_ )
import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe ( fromJust )

import Options ( Options(Options, language_force) )
import Util ( prettyRead )


data Language = Agda | Assembly | Awk  | C | CMake | Cabal | Chapel | Clojure | Coffee | Conf | Cpp  | Csharp | Css |
                D | Dart | Elm | Elixir | Erlang | Eta | Fortran | Fsharp | Go | Haskell | Html | Idris | Java | Javascript | Json | Kotlin |
                Latex | Lisp | Lua | Make | Nmap | OCaml | ObjectiveC | PHP | Perl | Python | R | Ruby | Rust | Scala | SmallTalk | Shell | Swift | Tcl |
                Text | VHDL | Verilog | Vim | Yaml
                deriving (Read, Show, Eq, Ord, Bounded)


data FileType = Name String | Ext String
    deriving (Eq, Ord)


instance Show FileType where
    show (Name x) = x
    show (Ext  e) = "*." ++ e


newtype LanguageInfo = LanguageInfo {
    langExtensions :: [FileType]
} deriving (Show, Eq)


type LanguagesMapType    = Map.Map Language LanguageInfo
type LanguagesRevMapType = Map.Map FileType Language

languagesMap :: LanguagesMapType
languagesMap = Map.fromList
    [  (Agda,      LanguageInfo {
        langExtensions = [Ext "agda", Ext "lagda"]
    })
    ,  (Assembly,  LanguageInfo { langExtensions = [Ext "s", Ext "S"] })
    ,  (Awk,       LanguageInfo { langExtensions = [Ext "awk", Ext "mawk", Ext "gawk"] })
    ,  (C,         LanguageInfo { langExtensions = [Ext "c", Ext "C", Ext "inc"] })
    ,  (CMake,     LanguageInfo { langExtensions = [Name "CMakeLists.txt", Ext "cmake"] })
    ,  (Cabal,     LanguageInfo { langExtensions = [Ext "cabal"] })
    ,  (Chapel,    LanguageInfo { langExtensions = [Ext "chpl"] })
    ,  (Clojure,   LanguageInfo { langExtensions = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"] })
    ,  (Coffee,    LanguageInfo { langExtensions = [Ext "coffee"] })
    ,  (Conf,      LanguageInfo { langExtensions = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"] })
    ,  (Cpp,       LanguageInfo { langExtensions = [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc",
                            Ext "cp", Ext "c++", Ext "tcc",
                            Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP",
                            Ext "hxx", Ext "hh", Ext "hp", Ext "h++", Ext "cu", Ext "cuh"] })
    ,  (Csharp,    LanguageInfo { langExtensions = [Ext "cs", Ext "CS"]})
    ,  (Css,       LanguageInfo { langExtensions = [Ext "css"] })
    ,  (D,         LanguageInfo { langExtensions = [Ext "d", Ext "D"] })
    ,  (Dart,      LanguageInfo { langExtensions = [Ext "dart"] })
    ,  (Elixir,    LanguageInfo { langExtensions = [Ext "ex", Ext "exs"] })
    ,  (Elm,       LanguageInfo { langExtensions = [Ext "elm"] })
    ,  (Eta,       LanguageInfo { langExtensions = [Ext "eta"] })
    ,  (Erlang,    LanguageInfo { langExtensions = [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"] })
    ,  (Fortran,   LanguageInfo { langExtensions = [Ext "f", Ext "for", Ext "ftn",
                    Ext "F", Ext "FOR", Ext "FTN", Ext "fpp", Ext "FPP",
                    Ext "f90", Ext "f95", Ext "f03", Ext "f08",
                    Ext "F90", Ext "F95", Ext "F03", Ext "F08"] })
    ,  (Fsharp,    LanguageInfo { langExtensions = [Ext "fs", Ext "fsx", Ext "fsi"]})
    ,  (Go,        LanguageInfo { langExtensions = [Ext "go"] })
    ,  (Haskell,   LanguageInfo { langExtensions = [Ext "hs", Ext "lhs", Ext "hsc"]})
    ,  (Html,      LanguageInfo { langExtensions = [Ext "htm", Ext "html"] })
    ,  (Idris,     LanguageInfo { langExtensions = [Ext "idr", Ext "lidr"] })
    ,  (Java,      LanguageInfo { langExtensions = [Ext "java"] })
    ,  (Javascript,LanguageInfo { langExtensions = [Ext "js"] })
    ,  (Json      ,LanguageInfo { langExtensions = [Ext "json", Ext "ndjson"] })
    ,  (Kotlin,    LanguageInfo { langExtensions = [Ext "kt", Ext "kts", Ext "ktm"] })
    ,  (Latex,     LanguageInfo { langExtensions = [Ext "latex", Ext "tex"] })
    ,  (Lisp,      LanguageInfo { langExtensions = [Ext "lisp", Ext "cl"] })
    ,  (Lua,       LanguageInfo { langExtensions = [Ext "lua"] })
    ,  (Make,      LanguageInfo { langExtensions = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak"] })
    ,  (Nmap,      LanguageInfo { langExtensions = [Ext "nse"] })
    ,  (OCaml ,    LanguageInfo { langExtensions = [Ext "ml", Ext "mli"] })
    ,  (ObjectiveC,LanguageInfo { langExtensions = [Ext "m", Ext "mi"] })
    ,  (PHP,       LanguageInfo { langExtensions = [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"] })
    ,  (Perl,      LanguageInfo { langExtensions = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"] })
    ,  (Python,    LanguageInfo { langExtensions = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"] })
    ,  (Ruby,      LanguageInfo { langExtensions = [Ext "rb", Ext "ruby"] })
    ,  (R,         LanguageInfo { langExtensions = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"] })
    ,  (Rust,      LanguageInfo { langExtensions = [Ext "rs", Ext "rlib"] })
    ,  (Scala,     LanguageInfo { langExtensions = [Ext "scala"]  })
    ,  (SmallTalk, LanguageInfo { langExtensions = [Ext "st", Ext "gst"] })
    ,  (Shell,     LanguageInfo { langExtensions = [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"] })
    ,  (Swift,     LanguageInfo { langExtensions = [Ext "swift"] })
    ,  (Tcl,       LanguageInfo { langExtensions = [Ext "tcl", Ext "tk"] })
    ,  (Text,      LanguageInfo { langExtensions = [Ext "txt", Ext "md", Ext "markdown", Ext "mdown", Ext "mkdn", Ext "mkd", Ext "mdwn", Ext "mdtxt", Ext "mdtext", Ext "text", Name "README", Name "INSTALL", Name "VERSION", Name "LICENSE", Name "AUTHORS", Name "CHANGELOG"] })
    ,  (VHDL,      LanguageInfo { langExtensions = [Ext "vhd", Ext "vhdl"] })
    ,  (Verilog,   LanguageInfo { langExtensions = [Ext "v", Ext "vh", Ext "sv"] })
    ,  (Vim,       LanguageInfo { langExtensions = [Ext "vim"] })
    ,  (Yaml,      LanguageInfo { langExtensions = [Ext "yaml", Ext "yml"] })
    ]


languagesRevMap :: LanguagesRevMapType
languagesRevMap = Map.fromList $ concatMap (\(l, LanguageInfo{..}) -> map (,l) langExtensions ) $ Map.toList languagesMap
{-# INLINE languagesRevMap #-}

-- utility functions

lookupFileLang :: FilePath -> Maybe Language
lookupFileLang f = Map.lookup (Name $ takeFileName f) languagesRevMap <|> Map.lookup (Ext (let name = takeExtension f in case name of ('.':xs) -> xs; _ -> name )) languagesRevMap
{-# INLINE lookupFileLang #-}


forcedLang :: Options -> Maybe Language
forcedLang Options{ language_force = l }
    | Nothing <- l = Nothing
    | otherwise    = Map.lookup (Ext $ fromJust l) languagesRevMap <|> Map.lookup (Name $ fromJust l) languagesRevMap


languageLookup :: Options -> FilePath -> Maybe Language
languageLookup opts f = forcedLang opts <|> lookupFileLang f
{-# INLINE languageLookup #-}


dumpLanguagesMap :: LanguagesMapType -> IO ()
dumpLanguagesMap m = forM_ (Map.toList m) $ \(l, ex) ->
                putStrLn $ show l ++ [ ' ' | _ <- [length (show l)..12]] ++ "-> " ++ show ex


dumpLanguagesRevMap :: LanguagesRevMapType -> IO ()
dumpLanguagesRevMap m = forM_ (Map.toList m) $ \(ext, l) ->
                    putStrLn $ show ext ++ [ ' ' | _ <- [length (show ext)..12 ]] ++ "-> " ++ show l


splitLanguagesList :: [String] -> ([Language], [Language], [Language])
splitLanguagesList  = foldl run ([],[],[])
    where run :: ([Language], [Language], [Language]) -> String -> ([Language], [Language], [Language])
          run (l1, l2, l3) l
            | '+':xs <- l = (l1, prettyRead xs "Lang" : l2, l3)
            | '-':xs <- l = (l1, l2, prettyRead xs "Lang" : l3)
            | otherwise   = (prettyRead l  "Lang" : l1, l2, l3)
