{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module CGrep.Semantic.Tests (
    filterTests,
    LanguageTestFilter (..),
) where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (Token)
import CGrep.Semantic.Tests.Common (LanguageTestFilter (..), runLangFilter)
import System.OsPath (OsPath)

import CGrep.Semantic.Tests.C ()
import CGrep.Semantic.Tests.Clojure ()
import CGrep.Semantic.Tests.Csharp ()
import CGrep.Semantic.Tests.D ()
import CGrep.Semantic.Tests.Dart ()
import CGrep.Semantic.Tests.Elixir ()
import CGrep.Semantic.Tests.Erlang ()
import CGrep.Semantic.Tests.Fsharp ()
import CGrep.Semantic.Tests.Go ()
import CGrep.Semantic.Tests.Haskell ()
import CGrep.Semantic.Tests.Java ()
import CGrep.Semantic.Tests.Javascript ()
import CGrep.Semantic.Tests.Julia ()
import CGrep.Semantic.Tests.Kotlin ()
import CGrep.Semantic.Tests.Lua ()
import CGrep.Semantic.Tests.Nim ()
import CGrep.Semantic.Tests.OCaml ()
import CGrep.Semantic.Tests.PHP ()
import CGrep.Semantic.Tests.Perl ()
import CGrep.Semantic.Tests.Python ()
import CGrep.Semantic.Tests.R ()
import CGrep.Semantic.Tests.Ruby ()
import CGrep.Semantic.Tests.Rust ()
import CGrep.Semantic.Tests.Scala ()
import CGrep.Semantic.Tests.Swift ()
import CGrep.Semantic.Tests.Zig ()

filterTests :: OsPath -> FileType -> Maybe Bool -> [Token] -> [Token]
filterTests filename fileType flag tokens =
    case fileType of
        Rust       -> runLangFilter @'Rust filename flag tokens
        Go         -> runLangFilter @'Go filename flag tokens
        Java       -> runLangFilter @'Java filename flag tokens
        Kotlin     -> runLangFilter @'Kotlin filename flag tokens
        C          -> runLangFilter @'C filename flag tokens
        Cpp        -> runLangFilter @'Cpp filename flag tokens
        Python     -> runLangFilter @'Python filename flag tokens
        Zig        -> runLangFilter @'Zig filename flag tokens
        Javascript -> runLangFilter @'Javascript filename flag tokens
        Scala      -> runLangFilter @'Scala filename flag tokens
        Haskell    -> runLangFilter @'Haskell filename flag tokens
        Csharp     -> runLangFilter @'Csharp filename flag tokens
        Fsharp     -> runLangFilter @'Fsharp filename flag tokens
        Dart       -> runLangFilter @'Dart filename flag tokens
        Elixir     -> runLangFilter @'Elixir filename flag tokens
        Ruby       -> runLangFilter @'Ruby filename flag tokens
        PHP        -> runLangFilter @'PHP filename flag tokens
        Swift      -> runLangFilter @'Swift filename flag tokens
        ObjectiveC -> runLangFilter @'ObjectiveC filename flag tokens
        R          -> runLangFilter @'R filename flag tokens
        Julia      -> runLangFilter @'Julia filename flag tokens
        Perl       -> runLangFilter @'Perl filename flag tokens
        OCaml      -> runLangFilter @'OCaml filename flag tokens
        Erlang     -> runLangFilter @'Erlang filename flag tokens
        Nim        -> runLangFilter @'Nim filename flag tokens
        Clojure    -> runLangFilter @'Clojure filename flag tokens
        D          -> runLangFilter @'D filename flag tokens
        Lua        -> runLangFilter @'Lua filename flag tokens
        _          -> tokens
