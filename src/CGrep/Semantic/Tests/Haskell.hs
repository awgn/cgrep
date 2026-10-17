{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Haskell () where

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
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Haskell where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideHaskell keepTests tokens

-- | HSpec / Tasty / HUnit / QuickCheck / Hedgehog test builder functions
-- that take a test name (string literal) as their first argument.
isHaskellTestBuilder :: Text -> Bool
isHaskellTestBuilder name =
    name
        `elem` ([ "describe"
                , "xdescribe"
                , "fdescribe"
                , "it"
                , "xit"
                , "fit"
                , "specify"
                , "xspecify"
                , "fspecify"
                , "context"
                , "xcontext"
                , "fcontext"
                , "testCase"
                , "testCaseInfo"
                , "testCaseSteps"
                , "testGroup"
                , "testProperty"
                , "testPropertyNamed"
                ] :: [Text])

-- | Check whether a token sequence starts with `::` (either as a single token `"::"`
-- in `--strict` mode or two adjacent `":"` tokens in normal mode).
startsWithDoubleColon :: [Token] -> Bool
startsWithDoubleColon (x1 : x2 : _)
    | isTokenOperator x1 && tToken x1 == ":"
    , isTokenOperator x2 && tToken x2 == ":" =
        True
startsWithDoubleColon (x : _)
    | isTokenOperator x && tToken x == "::" =
        True
startsWithDoubleColon _ = False

-- | (Haskell) Helper: Processes tokens *outside* a test block.
processOutsideHaskell :: Bool -> [Token] -> [Token]
processOutsideHaskell _ [] = []

-- Pattern 1: HSpec / Tasty / QuickCheck test builder followed by a string literal
-- (or parenthesized string expression), preventing local variables named `it` or `context`
-- (e.g. `let it = x + 1`) from being mistaken for test blocks.
processOutsideHaskell keepTests (t1 : t2 : ts)
    | isTokenIdentifier t1 && isHaskellTestBuilder (tToken t1)
    , isTokenString t2 =
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef 0 (t2 : ts)
         in if keepTests
                then t1 : testTokens ++ processOutsideHaskell keepTests remainingTokens
                else processOutsideHaskell keepTests remainingTokens

-- Pattern 2: prop_* function definition or type signature (QuickCheck / Hedgehog convention).
-- Only triggers when `prop_*` is followed by `::` or parameters + `=`, so referencing
-- `prop_foo` as an argument inside another expression does not misbehave.
processOutsideHaskell keepTests (t1 : ts)
    | isTokenIdentifier t1 && "prop_" `T.isPrefixOf` tToken t1 =
        let (propTokens, remainingTokens) = collectPropDef 0 ts
         in if keepTests
                then t1 : propTokens ++ processOutsideHaskell keepTests remainingTokens
                else processOutsideHaskell keepTests remainingTokens

-- No test found, process the current token
processOutsideHaskell keepTests (t : ts) =
    if keepTests
        then processOutsideHaskell keepTests ts
        else t : processOutsideHaskell keepTests ts

-- | Collect a `prop_*` definition. If `prop_foo` is starts with a type signature `prop_foo :: ...`,
-- we consume the type signature AND the immediately following `prop_foo ... = ...` body!
collectPropDef :: Int -> [Token] -> ([Token], [Token])
collectPropDef depth ts
    | startsWithDoubleColon ts =
        -- Consume the `::` tokens first, then collect until the next top-level definition.
        -- If that next definition is the implementation of the same `prop_*`, Pattern 2 in
        -- `processOutsideHaskell` will immediately pick it up!
        case ts of
            (c1 : c2 : rest)
                | tToken c1 == ":" && tToken c2 == ":" ->
                    let (body, remTokens) = collectUntilNextHaskellDef depth rest
                     in (c1 : c2 : body, remTokens)
            (c1 : rest) ->
                let (body, remTokens) = collectUntilNextHaskellDef depth rest
                 in (c1 : body, remTokens)
            [] -> ([], [])
    | otherwise = collectUntilNextHaskellDef depth ts

-- | Helper: Collect tokens until we find the next top-level Haskell definition,
-- tracking bracket nesting depth `(...)`, `[...]`, `{...}` so that inline type
-- annotations like `(1 :: Int)` or list elements `[ testCase ... ]` are not
-- prematurely split!
collectUntilNextHaskellDef :: Int -> [Token] -> ([Token], [Token])
collectUntilNextHaskellDef _ [] = ([], [])
collectUntilNextHaskellDef depth (x : xs)
    -- Track opening brackets
    | isTokenBracket x && (tToken x == "(" || tToken x == "[" || tToken x == "{") =
        let (collected, remaining) = collectUntilNextHaskellDef (depth + 1) xs
         in (x : collected, remaining)
    -- Track closing brackets
    | isTokenBracket x && (tToken x == ")" || tToken x == "]" || tToken x == "}") =
        let newDepth = max 0 (depth - 1)
            (collected, remaining) = collectUntilNextHaskellDef newDepth xs
         in (x : collected, remaining)
    -- At top level (depth == 0), a `ident :: ...` signals the start of a new top-level type signature!
    -- Wait: we stop BEFORE `x` (the function/value name) if `xs` starts with `::`!
    | depth == 0 && isTokenIdentifier x && startsWithDoubleColon xs =
        ([], x : xs)
    -- Also stop at `::` if depth == 0 (in case the identifier was already consumed)
    | depth == 0 && startsWithDoubleColon (x : xs) =
        ([], x : xs)
    -- Stop at another test builder or `prop_*` or `main` at depth == 0
    | depth == 0
        && isTokenIdentifier x
        && ( (isHaskellTestBuilder (tToken x) && case xs of (t2 : _) -> isTokenString t2; [] -> False)
                || tToken x == "main"
                || "prop_" `T.isPrefixOf` tToken x
           ) =
        ([], x : xs)
    -- Stop at top-level declaration keywords (`data`, `type`, `newtype`, `class`, `instance`, `module`, `import`, `foreign`)
    | depth == 0
        && isTokenKeyword x
        && ( tToken x == "data"
                || tToken x == "type"
                || tToken x == "class"
                || tToken x == "instance"
                || tToken x == "newtype"
                || tToken x == "module"
                || tToken x == "import"
                || tToken x == "foreign"
           ) =
        ([], x : xs)
    | otherwise =
        let (collected, remaining) = collectUntilNextHaskellDef depth xs
         in (x : collected, remaining)
