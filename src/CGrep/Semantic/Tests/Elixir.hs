{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Elixir () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T

instance LanguageTestFilter 'Elixir where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideElixir keepTests False tokens

-- | Extract a dotted Elixir module alias `Foo.Bar.BazTest` starting at the head of
-- the token stream, returning `(modulePathTokens, lastSegmentName, remainingTokens)`.
extractElixirModulePath :: [Token] -> Maybe ([Token], T.Text, [Token])
extractElixirModulePath (t1 : ts)
    | isTokenIdentifier t1 = Just (go [t1] (tToken t1) ts)
  where
    go acc _ (tDot : tNext : rest)
        | isTokenOperator tDot && tToken tDot == "." && isTokenIdentifier tNext =
            go (acc ++ [tDot, tNext]) (tToken tNext) rest
    go acc lastSeg rest = (acc, lastSeg, rest)
extractElixirModulePath _ = Nothing

-- | Find the opening `do` keyword of an ExUnit `test`, `describe`, `setup`, or `setup_all`
-- block within a small lookahead, stopping if a declaration keyword (`defmodule`, `def`,
-- `defp`, `defmacro`) is encountered before `do`.
-- This ensures that `setup :some_atom` (without a `do...end` block) does NOT swallow
-- the `do...end` of a subsequent `defmodule` or `def`!
findElixirDo :: Int -> [Token] -> Maybe ([Token], [Token])
findElixirDo = go 0 []
  where
    go :: Int -> [Token] -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ _ 0 _ = Nothing
    go _ _ _ [] = Nothing
    go parenDepth acc n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (parenDepth + 1) (t : acc) (n - 1) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (parenDepth - 1)) (t : acc) (n - 1) ts
        | parenDepth == 0 && (isTokenKeyword t || isTokenIdentifier t) && tToken t == "do" =
            Just (reverse (t : acc), ts)
        | parenDepth == 0
            && (isTokenKeyword t || isTokenIdentifier t)
            && ( tToken t == "defmodule"
                    || tToken t == "def"
                    || tToken t == "defp"
                    || tToken t == "defmacro"
                    || tToken t == "test"
                    || tToken t == "describe"
                    || tToken t == "setup"
                    || tToken t == "setup_all"
               ) =
            Nothing
        | otherwise =
            go parenDepth (t : acc) (n - 1) ts

-- | (Elixir) Helper: Processes tokens *outside* a test block.
-- `prevDefOrDot` is `True` when the immediately preceding token was `def`, `defp`,
-- `defmacro`, or `.`, preventing `def test(x) do ... end` or `Mod.test(x)` from
-- being mistaken for an ExUnit test block.
processOutsideElixir :: Bool -> Bool -> [Token] -> [Token]
processOutsideElixir _ _ [] = []

-- Pattern 1: ExUnit `test "..." do ... end` or `describe "..." do ... end` or `property "..." do ... end`
-- Requires a string literal after `test`/`describe`/`property` so local variables
-- like `test = x > 0` are NEVER mistaken for an ExUnit test block!
processOutsideElixir keepTests False (t1 : t2 : ts)
    | isTokenIdentifier t1
        && (tToken t1 == "test" || tToken t1 == "describe" || tToken t1 == "property")
    , isTokenString t2
    , Just (headerAndDo, afterDo) <- findElixirDo 20 ts =
        let (bodyAndEnd, remainingTokens) = collectUntilElixirEnd 1 afterDo
         in if keepTests
                then t1 : t2 : headerAndDo ++ bodyAndEnd ++ processOutsideElixir keepTests False remainingTokens
                else processOutsideElixir keepTests False remainingTokens

-- Pattern 2: ExUnit `setup` / `setup_all` block (`setup do ... end` or `setup context do ... end`)
processOutsideElixir keepTests False (t1 : ts)
    | isTokenIdentifier t1 && (tToken t1 == "setup" || tToken t1 == "setup_all")
    , Just (headerAndDo, afterDo) <- findElixirDo 15 ts =
        let (bodyAndEnd, remainingTokens) = collectUntilElixirEnd 1 afterDo
         in if keepTests
                then t1 : headerAndDo ++ bodyAndEnd ++ processOutsideElixir keepTests False remainingTokens
                else processOutsideElixir keepTests False remainingTokens

-- Pattern 3: `defmodule ...Test do ... end` (supports dotted module names like `MyApp.CalculatorTest`)
processOutsideElixir keepTests _ (t1 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "defmodule"
    , Just (modPathTokens, lastSeg, afterModPath) <- extractElixirModulePath ts
    , "Test" `T.isSuffixOf` lastSeg
    , Just (doTokens, afterDo) <- findElixirDo 5 afterModPath =
        let (moduleTokens, remainingTokens) = collectUntilElixirEnd 1 afterDo
         in if keepTests
                then t1 : modPathTokens ++ doTokens ++ moduleTokens ++ processOutsideElixir keepTests False remainingTokens
                else processOutsideElixir keepTests False remainingTokens

-- No test found, process the current token
processOutsideElixir keepTests _ (t : ts) =
    let isBlocker =
            (isTokenOperator t && tToken t == ".")
                || ( (isTokenKeyword t || isTokenIdentifier t)
                        && (tToken t == "def" || tToken t == "defp" || tToken t == "defmacro")
                   )
     in if keepTests
            then processOutsideElixir keepTests isBlocker ts
            else t : processOutsideElixir keepTests isBlocker ts

-- | Helper: Collect tokens until the matching `end` keyword in Elixir, starting
-- at nesting depth `depth` (typically `1` right after the opening `do`).
-- Note: In Elixir, inline `do:` is a keyword list key (`do` followed by `:`) and
-- does NOT have a matching `end`! We only increment depth on `fn` or `do` NOT followed by `:`.
collectUntilElixirEnd :: Int -> [Token] -> ([Token], [Token])
collectUntilElixirEnd _ [] = ([], [])
collectUntilElixirEnd depth (t : ts)
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "fn" =
        let (collected, remaining) = collectUntilElixirEnd (depth + 1) ts
         in (t : collected, remaining)
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "do" =
        let isKeywordDo = case ts of
                (tNext : _) -> isTokenOperator tNext && tToken tNext == ":"
                [] -> False
            nextDepth = if isKeywordDo then depth else depth + 1
            (collected, remaining) = collectUntilElixirEnd nextDepth ts
         in (t : collected, remaining)
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
        if depth <= 1
            then ([t], ts)
            else
                let (collected, remaining) = collectUntilElixirEnd (depth - 1) ts
                 in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilElixirEnd depth ts
         in (t : collected, remaining)
