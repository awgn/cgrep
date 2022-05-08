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

import CGrep.Language ( Language(..), FileType(..) )
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
import qualified Data.Set as S

type LanguagesMapType = Map.Map Language LanguageInfo
type FileMapType = Map.Map FileType Language

type StringBoundary = (String, String)

data LanguageInfo = LanguageInfo {
    langExtensions  :: [FileType]
,   langFilter      :: Maybe FilterFunction
,   langResKeywords :: S.Set String
}

languagesMap :: LanguagesMapType
languagesMap = Map.fromList
    [  (Agda,      LanguageInfo {
        langExtensions = [Ext "agda", Ext "lagda"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Assembly,  LanguageInfo {
        langExtensions = [Ext "s", Ext "S"]
    ,   langFilter = mkFilter ["#" ~~ "\n", ";" ~~ "\n", "|" ~~ "\n", "!" ~~ "\n", "/*" ~~ "*/"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Awk,       LanguageInfo {
        langExtensions = [Ext "awk", Ext "mawk", Ext "gawk"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (C,         LanguageInfo {
        langExtensions = [Ext "c", Ext "C", Ext "inc"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (CMake,     LanguageInfo {
        langExtensions = [Name "CMakeLists.txt", Ext "cmake"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Cabal,     LanguageInfo {
        langExtensions = [Ext "cabal"]
    ,   langFilter = mkFilter ["--" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Chapel,    LanguageInfo {
        langExtensions = [Ext "chpl"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Clojure,   LanguageInfo {
        langExtensions = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"]
    ,   langFilter = mkFilter [";" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Coffee,    LanguageInfo {
        langExtensions = [Ext "coffee"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Conf,      LanguageInfo {
        langExtensions = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Cpp,    LanguageInfo {
        langExtensions = [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc",
                          Ext "cp", Ext "c++", Ext "tcc",
                          Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP",
                          Ext "hxx", Ext "hh", Ext "hp", Ext "h++", Ext "cu", Ext "cuh"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList ["alignas", "continue", "friend", "alignof", "decltype", "goto", "asm",
                       "default", "if", "auto", "delete", "inline", "bool", "do", "int", "break",
                       "double", "long", "case", "dynamic_cast", "mutable", "catch", "else",
                       "namespace", "char", "enum", "new", "char16_t", "explicit", "noexcept",
                       "char32_t", "export", "nullptr", "class", "extern", "operator", "const",
                       "false", "private", "constexpr", "float", "protected", "const_cast", "for",
                       "public", "register", "true", "reinterpret_cast", "try", "return", "typedef",
                       "short", "typeid", "signed", "typename", "sizeof", "union", "static", "unsigned",
                       "static_assert", "using", "static_cast", "virtual", "struct", "void", "switch",
                       "volatile", "template", "wchar_t", "this", "while", "thread_local", "throw",
                       "and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or", "or_eq",
                       "xor", "xor_eq"]
    })
    ,  (Csharp,    LanguageInfo {
        langExtensions = [Ext "cs", Ext "CS"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Css,       LanguageInfo {
        langExtensions = [Ext "css"]
    ,   langFilter = mkFilter ["/*" ~~ "*/"] ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (D,         LanguageInfo {
        langExtensions = [Ext "d", Ext "D"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Dart,      LanguageInfo {
        langExtensions = [Ext "dart"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langResKeywords = S.fromList []
    })
    ,  (Elixir,    LanguageInfo {
        langExtensions = [Ext "ex", Ext "exs"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Elm,       LanguageInfo {
        langExtensions = [Ext "elm"]
    ,   langFilter =  mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Erlang,    LanguageInfo {
        langExtensions = [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"]
    ,   langFilter = mkFilter ["%" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Eta,       LanguageInfo {
        langExtensions = [Ext "eta"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Fortran,   LanguageInfo {
        langExtensions = [Ext "f", Ext "for", Ext "ftn",
                    Ext "F", Ext "FOR", Ext "FTN", Ext "fpp", Ext "FPP",
                    Ext "f90", Ext "f95", Ext "f03", Ext "f08",
                    Ext "F90", Ext "F95", Ext "F03", Ext "F08"]
    ,   langFilter = Nothing
    ,   langResKeywords = S.fromList []
    })
    ,  (Fsharp,    LanguageInfo {
        langExtensions = [Ext "fs", Ext "fsx", Ext "fsi"]
    ,   langFilter =  mkFilter ["(*" ~~ "*)", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Go,        LanguageInfo {
        langExtensions = [Ext "go"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\"", "`" ~~ "`"]
    ,   langResKeywords = S.fromList []
    })
    ,  (Haskell,   LanguageInfo {
        langExtensions = [Ext "hs", Ext "lhs", Ext "hsc"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\"", "[r|" ~~ "|]", "[q|" ~~ "|]", "[s|" ~~ "|]", "[here|" ~~"|]",  "[i|" ~~ "|]"]
    ,   langResKeywords = S.fromList []
    })
    ,  (Html,      LanguageInfo {
        langExtensions = [Ext "htm", Ext "html"]
    ,   langFilter = mkFilter ["<!--" ~~ "-->"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Idris,     LanguageInfo {
        langExtensions = [Ext "idr", Ext "lidr"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n", "|||" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Java,      LanguageInfo {
        langExtensions = [Ext "java"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Javascript,LanguageInfo {
        langExtensions = [Ext "js"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Json,      LanguageInfo {
        langExtensions = [Ext "json", Ext "ndjson"]
    ,   langFilter = mkFilter []  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Kotlin,    LanguageInfo {
        langExtensions = [Ext "kt", Ext "kts", Ext "ktm"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\"", "'" ~~ "'", "\"\"\"" ~~ "\"\"\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Latex,     LanguageInfo {
        langExtensions = [Ext "latex", Ext "tex"]
    ,   langFilter = mkFilter ["%" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Lisp,      LanguageInfo {
        langExtensions = [Ext "lisp", Ext "cl"]
    ,   langFilter = mkFilter [";" ~~ "\n", "#|" ~~ "|#"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Lua,       LanguageInfo {
        langExtensions = [Ext "lua"]
    ,   langFilter = mkFilter ["--[[" ~~ "--]]", "--" ~~ "\n"] ["'" ~~ "'", "\"" ~~ "\"", "[===[" ~~ "]===]", "[==[" ~~ "]==]", "[=[" ~~ "]=]", "[[" ~~ "]]" ]
    ,   langResKeywords = S.fromList []
    })
    ,  (Make,      LanguageInfo {
        langExtensions = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Nmap,      LanguageInfo {
        langExtensions = [Ext "nse"]
    ,   langFilter = mkFilter ["--" ~~ "\n", "[[" ~~"]]"] ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (OCaml ,    LanguageInfo {
        langExtensions = [Ext "ml", Ext "mli"]
    ,   langFilter = mkFilter ["(*" ~~ "*)"] ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (ObjectiveC,LanguageInfo {
        langExtensions = [Ext "m", Ext "mi"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (PHP,       LanguageInfo {
        langExtensions = [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n", "#" ~~ "\n" ]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Perl,      LanguageInfo {
        langExtensions = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]
    ,   langFilter = mkFilter ["=pod" ~~ "=cut", "#" ~~ "\n"]   ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Python,    LanguageInfo {
        langExtensions = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''", "'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (R,         LanguageInfo {
        langExtensions = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langResKeywords = S.fromList []
    })
    ,  (Ruby,      LanguageInfo {
        langExtensions = [Ext "rb", Ext "ruby"]
    ,   langFilter = mkFilter ["=begin" ~~ "=end", "#" ~~ "\n"] ["'" ~~ "'", "\"" ~~ "\"", "%|" ~~ "|", "%q(" ~~ ")", "%Q(" ~~ ")" ]
    ,   langResKeywords = S.fromList []
    })
    ,  (Rust,      LanguageInfo {
        langExtensions = [Ext "rs", Ext "rlib"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Scala,     LanguageInfo {
        langExtensions = [Ext "scala"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (SmallTalk, LanguageInfo {
        langExtensions = [Ext "st", Ext "gst"]
    ,   langFilter = mkFilter ["\"" ~~ "\""] ["'" ~~ "'"]
    ,   langResKeywords = S.fromList []
    })
    ,  (Shell,     LanguageInfo {
        langExtensions = [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Swift,     LanguageInfo {
        langExtensions = [Ext "swift"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Tcl,       LanguageInfo {
        langExtensions = [Ext "tcl", Ext "tk"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Text,  LanguageInfo {
        langExtensions = [Ext "txt", Ext "md", Ext "markdown", Ext "mdown", Ext "mkdn", Ext "mkd", Ext "mdwn", Ext "mdtxt", Ext "mdtext", Ext "text", Name "README", Name "INSTALL", Name "VERSION", Name "LICENSE", Name "AUTHORS", Name "CHANGELOG"]
    ,   langFilter = Nothing
    ,   langResKeywords = S.fromList []
    })
    ,  (VHDL,      LanguageInfo {
        langExtensions = [Ext "vhd", Ext "vhdl"]
    ,   langFilter = mkFilter ["--" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Verilog,   LanguageInfo {
        langExtensions = [Ext "v", Ext "vh", Ext "sv"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "/n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
    })
    ,  (Vim,       LanguageInfo {
        langExtensions = [Ext "vim"]
    ,   langFilter = mkFilter ["\"" ~~ "\n"] ["'" ~~ "'"]
    ,   langResKeywords = S.fromList []
    })
    ,  (Yaml,      LanguageInfo {
        langExtensions = [Ext "yaml", Ext "yml"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = S.fromList []
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
                putStrLn $ show l ++ [ ' ' | _ <- [length (show l)..12]] ++ "-> " ++ show (langExtensions ex)


dumpLanguagesFileMap :: FileMapType -> IO ()
dumpLanguagesFileMap m = forM_ (Map.toList m) $ \(ext, l) ->
                    putStrLn $ show ext ++ [ ' ' | _ <- [length (show ext)..12 ]] ++ "-> " ++ show l


mkFilter :: [StringBoundary] -> [StringBoundary] -> Maybe FilterFunction
mkFilter cs ls =
  Just $ contextFilterFun (ParConf (map (\(a,b) -> Boundary (C.pack a) (C.pack b)) cs)
                            (map (\(a,b) -> Boundary (C.pack a) (C.pack b)) ls)
                            (mkBloom (cs ++ ls)))

mkBloom :: [StringBoundary] -> UArray Char Bool
mkBloom bs = listArray ('\0', '\255') (map (\c -> findIndex' (\(b,_) -> c == head b) bs >= 0 ) ['\0'..'\255'])
{-# INLINE mkBloom #-}


forcedLang :: Options -> Maybe Language
forcedLang Options{ language_force = l }
    | Nothing <- l = Nothing
    | otherwise    = Map.lookup (Ext $ fromJust l) languagesFileMap <|> Map.lookup (Name $ fromJust l) languagesFileMap


(~~) :: a -> b -> (a, b)
(~~) = (,)
{-# INLINE (~~) #-}
