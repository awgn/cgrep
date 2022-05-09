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
    ,   langFilter = mkFilter ["#" ~~ "\n", ";" ~~ "\n", "|" ~~ "\n", "!" ~~ "\n", "/*" ~~ "*/"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Awk,       LanguageInfo {
        langExtensions = [Ext "awk", Ext "mawk", Ext "gawk"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords [
            "BEGIN", "END", "if", "else", "while", "do", "for", "in", "break", "continue",
                "delete", "next", "nextfile", "function", "func", "exit"]
    })
    ,  (C,         LanguageInfo {
        langExtensions = [Ext "c", Ext "C", Ext "inc"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
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
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Cabal,     LanguageInfo {
        langExtensions = [Ext "cabal"]
    ,   langFilter = mkFilter ["--" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Chapel,    LanguageInfo {
        langExtensions = [Ext "chpl"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords [
            "atomic", "begin", "bool", "break", "by", "class", "cobegin", "coforall", "complex", "config",
            "const", "continue", "def", "distributed", "do", "domain", "else", "enum", "false", "for",
            "forall", "goto", "if", "imag", "in", "int", "inout", "let", "locale", "module",
            "nil", "of", "on", "ordered", "otherwise", "out", "param", "pragma", "range", "real",
            "record", "reduce", "return", "scan", "select", "serial", "single", "sync", "then", "true",
            "type", "uint", "union", "use", "var", "when", "where", "while", "yield"
        ]
    })
    ,  (Clojure,   LanguageInfo {
        langExtensions = [Ext "clj", Ext "cljs", Ext "cljc", Ext "edn"]
    ,   langFilter = mkFilter [";" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Coffee,    LanguageInfo {
        langExtensions = [Ext "coffee"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Conf,      LanguageInfo {
        langExtensions = [Ext "config", Ext "conf", Ext "cfg", Ext "doxy"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Cpp,    LanguageInfo {
        langExtensions = [Ext "cpp", Ext "CPP", Ext "cxx", Ext "cc",
                          Ext "cp", Ext "c++", Ext "tcc",
                          Ext "h", Ext "H", Ext "hpp", Ext "ipp", Ext "HPP",
                          Ext "hxx", Ext "hh", Ext "hp", Ext "h++", Ext "cu", Ext "cuh"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
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
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Css,       LanguageInfo {
        langExtensions = [Ext "css"]
    ,   langFilter = mkFilter ["/*" ~~ "*/"] ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (D,         LanguageInfo {
        langExtensions = [Ext "d", Ext "D"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Dart,      LanguageInfo {
        langExtensions = [Ext "dart"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langResKeywords = keywords []
    })
    ,  (Elixir,    LanguageInfo {
        langExtensions = [Ext "ex", Ext "exs"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Elm,       LanguageInfo {
        langExtensions = [Ext "elm"]
    ,   langFilter =  mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\"", "\"\"\"" ~~ "\"\"\""]
    ,   langResKeywords = keywords []
    })
    ,  (Erlang,    LanguageInfo {
        langExtensions = [Ext "erl", Ext "ERL",Ext "hrl", Ext "HRL"]
    ,   langFilter = mkFilter ["%" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Eta,       LanguageInfo {
        langExtensions = [Ext "eta"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Fortran,   LanguageInfo {
        langExtensions = [Ext "f", Ext "for", Ext "ftn",
                    Ext "F", Ext "FOR", Ext "FTN", Ext "fpp", Ext "FPP",
                    Ext "f90", Ext "f95", Ext "f03", Ext "f08",
                    Ext "F90", Ext "F95", Ext "F03", Ext "F08"]
    ,   langFilter = Nothing
    ,   langResKeywords = keywords []
    })
    ,  (Fsharp,    LanguageInfo {
        langExtensions = [Ext "fs", Ext "fsx", Ext "fsi"]
    ,   langFilter =  mkFilter ["(*" ~~ "*)", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Go,        LanguageInfo {
        langExtensions = [Ext "go"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "`" ~~ "`"]
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
    ,  (Haskell,   LanguageInfo {
        langExtensions = [Ext "hs", Ext "lhs", Ext "hsc"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n"]  ["\"" ~~ "\"", "[r|" ~~ "|]", "[q|" ~~ "|]", "[s|" ~~ "|]", "[here|" ~~"|]",  "[i|" ~~ "|]"]
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
    ,   langFilter = mkFilter ["<!--" ~~ "-->"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Idris,     LanguageInfo {
        langExtensions = [Ext "idr", Ext "lidr"]
    ,   langFilter = mkFilter ["{-" ~~ "-}", "--" ~~ "\n", "|||" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Java,      LanguageInfo {
        langExtensions = [Ext "java"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Javascript,LanguageInfo {
        langExtensions = [Ext "js"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Json,      LanguageInfo {
        langExtensions = [Ext "json", Ext "ndjson"]
    ,   langFilter = mkFilter []  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Kotlin,    LanguageInfo {
        langExtensions = [Ext "kt", Ext "kts", Ext "ktm"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'", "\"\"\"" ~~ "\"\"\""]
    ,   langResKeywords = keywords []
    })
    ,  (Latex,     LanguageInfo {
        langExtensions = [Ext "latex", Ext "tex"]
    ,   langFilter = mkFilter ["%" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Lisp,      LanguageInfo {
        langExtensions = [Ext "lisp", Ext "cl"]
    ,   langFilter = mkFilter [";" ~~ "\n", "#|" ~~ "|#"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Lua,       LanguageInfo {
        langExtensions = [Ext "lua"]
    ,   langFilter = mkFilter ["--[[" ~~ "--]]", "--" ~~ "\n"] ["'" ~~ "'", "\"" ~~ "\"", "[===[" ~~ "]===]", "[==[" ~~ "]==]", "[=[" ~~ "]=]", "[[" ~~ "]]" ]
    ,   langResKeywords = keywords []
    })
    ,  (Make,      LanguageInfo {
        langExtensions = [Name "Makefile", Name "makefile", Name "GNUmakefile", Ext "mk", Ext  "mak"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Nmap,      LanguageInfo {
        langExtensions = [Ext "nse"]
    ,   langFilter = mkFilter ["--" ~~ "\n", "[[" ~~"]]"] ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (OCaml ,    LanguageInfo {
        langExtensions = [Ext "ml", Ext "mli"]
    ,   langFilter = mkFilter ["(*" ~~ "*)"] ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (ObjectiveC,LanguageInfo {
        langExtensions = [Ext "m", Ext "mi"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (PHP,       LanguageInfo {
        langExtensions = [Ext "php", Ext "php3", Ext "php4", Ext "php5",Ext "phtml"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n", "#" ~~ "\n" ]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Perl,      LanguageInfo {
        langExtensions = [Ext "pl", Ext "pm", Ext "pm6", Ext "plx", Ext "perl"]
    ,   langFilter = mkFilter ["=pod" ~~ "=cut", "#" ~~ "\n"]   ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Python,    LanguageInfo {
        langExtensions = [Ext "py", Ext "pyx", Ext "pxd", Ext "pxi", Ext "scons"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"\"\"" ~~ "\"\"\"", "'''" ~~ "'''", "'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (R,         LanguageInfo {
        langExtensions = [Ext "r", Ext "rdata", Ext "rds", Ext "rda"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\"", "'" ~~ "'"]
    ,   langResKeywords = keywords []
    })
    ,  (Ruby,      LanguageInfo {
        langExtensions = [Ext "rb", Ext "ruby"]
    ,   langFilter = mkFilter ["=begin" ~~ "=end", "#" ~~ "\n"] ["'" ~~ "'", "\"" ~~ "\"", "%|" ~~ "|", "%q(" ~~ ")", "%Q(" ~~ ")" ]
    ,   langResKeywords = keywords []
    })
    ,  (Rust,      LanguageInfo {
        langExtensions = [Ext "rs", Ext "rlib"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Scala,     LanguageInfo {
        langExtensions = [Ext "scala"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (SmallTalk, LanguageInfo {
        langExtensions = [Ext "st", Ext "gst"]
    ,   langFilter = mkFilter ["\"" ~~ "\""] ["'" ~~ "'"]
    ,   langResKeywords = keywords []
    })
    ,  (Shell,     LanguageInfo {
        langExtensions = [Ext "sh", Ext "bash", Ext "csh", Ext "tcsh", Ext "ksh", Ext "zsh"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["'" ~~ "'", "\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Swift,     LanguageInfo {
        langExtensions = [Ext "swift"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Tcl,       LanguageInfo {
        langExtensions = [Ext "tcl", Ext "tk"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Text,  LanguageInfo {
        langExtensions = [
            Ext "txt", Ext "md", Ext "markdown", Ext "mdown", Ext "mkdn", Ext "mkd", Ext "mdwn",
            Ext "mdtxt", Ext "mdtext", Ext "text", Name "README", Name "INSTALL", Name "VERSION",
            Name "LICENSE", Name "AUTHORS", Name "CHANGELOG"
        ]
    ,   langFilter = Nothing
    ,   langResKeywords = keywords []
    })
    ,  (VHDL,      LanguageInfo {
        langExtensions = [Ext "vhd", Ext "vhdl"]
    ,   langFilter = mkFilter ["--" ~~ "\n"] ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Verilog,   LanguageInfo {
        langExtensions = [Ext "v", Ext "vh", Ext "sv"]
    ,   langFilter = mkFilter ["/*" ~~ "*/", "//" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
    })
    ,  (Vim,       LanguageInfo {
        langExtensions = [Ext "vim"]
    ,   langFilter = mkFilter ["\"" ~~ "\n"] ["'" ~~ "'"]
    ,   langResKeywords = keywords []
    })
    ,  (Yaml,      LanguageInfo {
        langExtensions = [Ext "yaml", Ext "yml"]
    ,   langFilter = mkFilter ["#" ~~ "\n"]  ["\"" ~~ "\""]
    ,   langResKeywords = keywords []
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


keywords :: (Ord a) => [a] -> S.Set a
keywords = S.fromList