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

module CGrep.LanguagesMap where

import CGrep.Languages ( Language(..), FileType(..) )
import CGrep.ContextFilter
    ( ParConf(ParConf),
      Boundary(Boundary),
      ContextFilter(ContextFilter),
      FilterFunction,
      contextFilterFun,
      findIndex' )

import CGrep.Types ( Text8 )
import qualified Data.Map as Map
import System.FilePath ( takeExtension, takeFileName )
import Control.Monad ( forM_ )
import Data.Maybe ( fromJust )
import Control.Applicative ( Alternative((<|>)) )
import Options ( Options(Options, language_force) )
import qualified Data.ByteString.Char8 as C
import Data.Array.Base ( listArray, UArray )


type LanguagesMapType = Map.Map Language LanguageInfo
type LanguagesRevMapType = Map.Map FileType Language

type StringBoundary = (String, String)

data LanguageInfo = LanguageInfo {
    langExtensions :: [FileType]
,   langFilter     :: Maybe FilterFunction
}

(~~) :: a -> b -> (a, b)
(~~) = (,)
{-# INLINE (~~) #-}


languagesMap :: LanguagesMapType
languagesMap = Map.fromList
    [  (Agda,      LanguageInfo {
        langExtensions = [Ext "agda", Ext "lagda"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
    })
    ,  (Assembly,  LanguageInfo {
        langExtensions = [Ext "s", Ext "S"]
    ,   langFilter = mkFilter [("#", "\n"), (";", "\n"), ("|", "\n"), ("!", "\n"), ("/*", "*/")]  [("\"", "\"")]
    })
    ,  (Awk,       LanguageInfo {
        langExtensions = [Ext "awk", Ext "mawk", Ext "gawk"]
    ,   langFilter = mkFilter [("{-", "-}"), ("--", "\n")]  [("\"", "\"")]
    })
    ,  (C,         LanguageInfo {
        langExtensions = [Ext "c", Ext "C", Ext "inc"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (CMake,     LanguageInfo {
        langExtensions = [Name "CMakeLists.txt", Ext "cmake"]
    ,   langFilter = mkFilter [("#", "\n")]  [("\"", "\"")]
    })
    ,  (Cabal,     LanguageInfo {
        langExtensions = [Ext "cabal"]
    ,   langFilter = mkFilter [("--", "\n")] [("\"", "\"")]
    })
    ,  (Chapel,    LanguageInfo {
        langExtensions = [Ext "chpl"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Clojure,   LanguageInfo {
        langExtensions = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"]
    ,   langFilter = mkFilter [(";", "\n")] [("\"", "\"")]
    })
    ,  (Coffee,    LanguageInfo {
        langExtensions = [Ext "coffee"]
    ,   langFilter = mkFilter [("#", "\n")]  [("'", "'"), ("\"", "\"")]
    })
    ,  (Conf,      LanguageInfo {
        langExtensions = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"]
    ,   langFilter = mkFilter [("#", "\n")]  [("'", "'"), ("\"", "\"")]
    })
    ,  (Cpp,    LanguageInfo {
        langExtensions = [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc",
                          Ext "cp", Ext "c++", Ext "tcc",
                          Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP",
                          Ext "hxx", Ext "hh", Ext "hp", Ext "h++", Ext "cu", Ext "cuh"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Csharp,    LanguageInfo {
        langExtensions = [Ext "cs", Ext "CS"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Css,       LanguageInfo {
        langExtensions = [Ext "css"]
    ,   langFilter = mkFilter [("/*", "*/")] [("\"", "\"")]
    })
    ,  (D,         LanguageInfo {
        langExtensions = [Ext "d", Ext "D"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Dart,      LanguageInfo {
        langExtensions = [Ext "dart"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\""), ("'", "'")]
    })
    ,  (Elixir,    LanguageInfo {
        langExtensions = [Ext "ex", Ext "exs"]
    ,   langFilter = mkFilter [("#", "\n")]  [("\"", "\"")]
    })
    ,  (Elm,       LanguageInfo {
        langExtensions = [Ext "elm"]
    ,   langFilter =  mkFilter [("{-", "-}"), ("--", "\n")]  [("\"", "\""), ("\"\"\"", "\"\"\"")]
    })
    ,  (Erlang,    LanguageInfo {
        langExtensions = [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"]
    ,   langFilter = mkFilter [("%", "\n")]  [("\"", "\"")]
    })
    ,  (Eta,       LanguageInfo {
        langExtensions = [Ext "eta"]
    ,   langFilter = mkFilter [("{-", "-}"), ("--", "\n")]  [("\"", "\"")]
    })
    ,  (Fortran,   LanguageInfo {
        langExtensions = [Ext "f", Ext "for", Ext "ftn",
                    Ext "F", Ext "FOR", Ext "FTN", Ext "fpp", Ext "FPP",
                    Ext "f90", Ext "f95", Ext "f03", Ext "f08",
                    Ext "F90", Ext "F95", Ext "F03", Ext "F08"]
    ,   langFilter = Nothing
    })
    ,  (Fsharp,    LanguageInfo {
        langExtensions = [Ext "fs", Ext "fsx", Ext "fsi"]
    ,   langFilter =  mkFilter [("(*", "*)"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Go,        LanguageInfo {
        langExtensions = [Ext "go"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\""), ("`", "`")]
    })
    ,  (Haskell,   LanguageInfo {
        langExtensions = [Ext "hs", Ext "lhs", Ext "hsc"]
    ,   langFilter = mkFilter [("{-", "-}"), ("--", "\n")]  [("\"", "\""), ("[r|", "|]"), ("[q|", "|]"), ("[s|", "|]"), ("[here|","|]"), ("[i|", "|]")]
    })
    ,  (Html,      LanguageInfo {
        langExtensions = [Ext "htm", Ext "html"]
    ,   langFilter = mkFilter [("<!--", "-->")]  [("\"", "\"")]
    })
    ,  (Idris,     LanguageInfo {
        langExtensions = [Ext "idr", Ext "lidr"]
    ,   langFilter = mkFilter [("{-", "-}"), ("--", "\n"), ("|||", "\n")] [("\"", "\"")]
    })
    ,  (Java,      LanguageInfo {
        langExtensions = [Ext "java"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Javascript,LanguageInfo {
        langExtensions = [Ext "js"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Json,      LanguageInfo {
        langExtensions = [Ext "json", Ext "ndjson"]
    ,   langFilter = mkFilter []  [("\"", "\"")]
    })
    ,  (Kotlin,    LanguageInfo {
        langExtensions = [Ext "kt", Ext "kts", Ext "ktm"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\""), ("'","'"), ("\"\"\"", "\"\"\"")]
    })
    ,  (Latex,     LanguageInfo {
        langExtensions = [Ext "latex", Ext "tex"]
    ,   langFilter = mkFilter [("%", "\n")]  [("\"", "\"")]
    })
    ,  (Lisp,      LanguageInfo {
        langExtensions = [Ext "lisp", Ext "cl"]
    ,   langFilter = mkFilter [(";", "\n"), ("#|","|#")]  [("\"", "\"")]
    })
    ,  (Lua,       LanguageInfo {
        langExtensions = [Ext "lua"]
    ,   langFilter = mkFilter [("--[[","--]]"), ("--", "\n")]    [("'", "'"), ("\"", "\""), ("[===[", "]===]"), ("[==[", "]==]"), ("[=[", "]=]"), ("[[", "]]") ]
    })
    ,  (Make,      LanguageInfo {
        langExtensions = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak"]
    ,   langFilter = mkFilter [("#", "\n")]  [("'", "'"), ("\"", "\"")]
    })
    ,  (Nmap,      LanguageInfo {
        langExtensions = [Ext "nse"]
    ,   langFilter = mkFilter [("--", "\n"), ("[[","]]")] [("'", "'"), ("\"", "\"")]
    })
    ,  (OCaml ,    LanguageInfo {
        langExtensions = [Ext "ml", Ext "mli"]
    ,   langFilter = mkFilter [("(*", "*)")] [("\"", "\"")]
    })
    ,  (ObjectiveC,LanguageInfo {
        langExtensions = [Ext "m", Ext "mi"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (PHP,       LanguageInfo {
        langExtensions = [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n"), ("#", "\n") ]  [("'", "'"), ("\"", "\"")]
    })
    ,  (Perl,      LanguageInfo {
        langExtensions = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]
    ,   langFilter = mkFilter [("=pod", "=cut"), ("#", "\n")]   [("'", "'"), ("\"", "\"")]
    })
    ,  (Python,    LanguageInfo {
        langExtensions = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]
    ,   langFilter = mkFilter [("#", "\n")]  [("\"\"\"", "\"\"\""), ("'''", "'''"), ("'", "'"), ("\"", "\"")]
    })
    ,  (R,         LanguageInfo {
        langExtensions = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"]
    ,   langFilter = mkFilter [("#", "\n")]  [("\"", "\""), ("'", "'")]
    })
    ,  (Ruby,      LanguageInfo {
        langExtensions = [Ext "rb", Ext "ruby"]
    ,   langFilter = mkFilter [("=begin", "=end"), ("#", "\n")] [("'", "'"), ("\"", "\""), ("%|", "|"), ("%q(", ")"), ("%Q(", ")") ]
    })
    ,  (Rust,      LanguageInfo {
        langExtensions = [Ext "rs", Ext "rlib"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Scala,     LanguageInfo {
        langExtensions = [Ext "scala"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (SmallTalk, LanguageInfo {
        langExtensions = [Ext "st", Ext "gst"]
    ,   langFilter = mkFilter [("\"", "\"")] [("'", "'")]
    })
    ,  (Shell,     LanguageInfo {
        langExtensions = [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"]
    ,   langFilter = mkFilter [("#", "\n")]  [("'", "'"), ("\"", "\"")]
    })
    ,  (Swift,     LanguageInfo {
        langExtensions = [Ext "swift"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Tcl,       LanguageInfo {
        langExtensions = [Ext "tcl", Ext "tk"]
    ,   langFilter = mkFilter [("#", "\n")]  [("\"", "\"")]
    })
    ,  (Text,  LanguageInfo {
        langExtensions = [Ext "txt", Ext "md", Ext "markdown", Ext "mdown", Ext "mkdn", Ext "mkd", Ext "mdwn", Ext "mdtxt", Ext "mdtext", Ext "text", Name "README", Name "INSTALL", Name "VERSION", Name "LICENSE", Name "AUTHORS", Name "CHANGELOG"]
    ,   langFilter = Nothing
    })
    ,  (VHDL,      LanguageInfo {
        langExtensions = [Ext "vhd", Ext "vhdl"]
    ,   langFilter = mkFilter [("--", "\n")] [("\"", "\"")]
    })
    ,  (Verilog,   LanguageInfo {
        langExtensions = [Ext "v", Ext "vh", Ext "sv"]
    ,   langFilter = mkFilter [("/*", "*/"), ("//", "\n")]  [("\"", "\"")]
    })
    ,  (Vim,       LanguageInfo {
        langExtensions = [Ext "vim"]
    ,   langFilter = mkFilter [("\"", "\n")] [("'", "'")]
    })
    ,  (Yaml,      LanguageInfo {
        langExtensions = [Ext "yaml", Ext "yml"]
    ,   langFilter = mkFilter [("#", "\n")]  [("\"", "\"")]
    })
    ]


contextFilter :: Maybe Language -> ContextFilter -> Text8 -> Text8
contextFilter _ (ContextFilter True True True) txt = txt
contextFilter Nothing _ txt = txt
contextFilter (Just language) filt txt
   | Just fun <- parFunc = fun filt txt
   | otherwise = txt
        where parFunc = langFilter =<< Map.lookup language languagesMap
{-# INLINE contextFilter #-}


lookupFileLang :: FilePath -> Maybe Language
lookupFileLang f = Map.lookup (Name $ takeFileName f) languagesRevMap <|> Map.lookup (Ext (let name = takeExtension f in case name of ('.':xs) -> xs; _ -> name )) languagesRevMap
{-# INLINE lookupFileLang #-}


languagesRevMap :: LanguagesRevMapType
languagesRevMap = Map.fromList $ concatMap (\(l, LanguageInfo{..}) -> map (,l) langExtensions ) $ Map.toList languagesMap
{-# INLINE languagesRevMap #-}


dumpLanguagesMap :: LanguagesMapType -> IO ()
dumpLanguagesMap m = forM_ (Map.toList m) $ \(l, ex) ->
                putStrLn $ show l ++ [ ' ' | _ <- [length (show l)..12]] ++ "-> " ++ show (langExtensions ex)


dumpLanguagesRevMap :: LanguagesRevMapType -> IO ()
dumpLanguagesRevMap m = forM_ (Map.toList m) $ \(ext, l) ->
                    putStrLn $ show ext ++ [ ' ' | _ <- [length (show ext)..12 ]] ++ "-> " ++ show l

forcedLang :: Options -> Maybe Language
forcedLang Options{ language_force = l }
    | Nothing <- l = Nothing
    | otherwise    = Map.lookup (Ext $ fromJust l) languagesRevMap <|> Map.lookup (Name $ fromJust l) languagesRevMap


languageLookup :: Options -> FilePath -> Maybe Language
languageLookup opts f = forcedLang opts <|> lookupFileLang f
{-# INLINE languageLookup #-}


mkBloom :: [StringBoundary] -> UArray Char Bool
mkBloom bs = listArray ('\0', '\255') (map (\c -> findIndex' (\(b,_) -> c == head b) bs >= 0 ) ['\0'..'\255'])
{-# INLINE mkBloom #-}


mkFilter :: [StringBoundary] -> [StringBoundary] -> Maybe FilterFunction
mkFilter cs ls =
  Just $ contextFilterFun (ParConf (map (\(a,b) -> Boundary (C.pack a) (C.pack b)) cs)
                            (map (\(a,b) -> Boundary (C.pack a) (C.pack b)) ls)
                            (mkBloom (cs ++ ls)))
