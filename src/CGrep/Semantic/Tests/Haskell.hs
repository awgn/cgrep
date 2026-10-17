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
    langFilter (Just keepTests) tokens = processOutsideHaskell keepTests False tokens

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

-- | Check if a `prop_*` token is the LHS of a type signature (`prop_foo :: ...`) or
-- function/property binding (`prop_foo x y = ...`), rather than an expression reference
-- (e.g. `let ok = prop_reverseReverse [1, 2, 3]`).
isPropDefinition :: [Token] -> Bool
isPropDefinition ts
    | startsWithDoubleColon ts = True
    | otherwise = checkParamsBeforeEquals 10 ts
  where
    checkParamsBeforeEquals :: Int -> [Token] -> Bool
    checkParamsBeforeEquals 0 _ = False
    checkParamsBeforeEquals _ [] = False
    checkParamsBeforeEquals n (t : rest)
        | isTokenOperator t && tToken t == "=" = True
        | isTokenIdentifier t = checkParamsBeforeEquals (n - 1) rest
        | isTokenBracket t && tToken t == "(" =
            let (_, afterParen) = processInsideBrackets "(" ")" 1 rest
             in checkParamsBeforeEquals (n - 1) afterParen
        | otherwise = False

-- | (Haskell) Helper: Processes tokens *outside* a test block.
-- `afterEquals` is `True` when the immediately preceding token was `=` (so `let ok = prop_foo ...`
-- is recognized as a RHS reference rather than a `prop_foo` definition).
processOutsideHaskell :: Bool -> Bool -> [Token] -> [Token]
processOutsideHaskell _ _ [] = []
-- Pattern 1: HSpec / Tasty / QuickCheck test builder followed by a string literal
processOutsideHaskell keepTests _ (t1 : t2 : ts)
    | isTokenIdentifier t1 && isHaskellTestBuilder (tToken t1)
    , isTokenString t2 =
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef 0 (t2 : ts)
         in if keepTests
                then t1 : testTokens ++ processOutsideHaskell keepTests False remainingTokens
                else processOutsideHaskell keepTests False remainingTokens
-- Pattern 2: prop_* function definition or type signature (QuickCheck / Hedgehog convention).
-- Only triggers when `prop_*` is not immediately after `=` and is followed by `::` or parameters + `=`.
processOutsideHaskell keepTests False (t1 : ts)
    | isTokenIdentifier t1 && "prop_" `T.isPrefixOf` tToken t1
    , isPropDefinition ts =
        let (propTokens, remainingTokens) = collectPropDef 0 ts
         in if keepTests
                then t1 : propTokens ++ processOutsideHaskell keepTests False remainingTokens
                else processOutsideHaskell keepTests False remainingTokens
-- No test found, process the current token
processOutsideHaskell keepTests _ (t : ts) =
    let isEq = isTokenOperator t && tToken t == "="
     in if keepTests
            then processOutsideHaskell keepTests isEq ts
            else t : processOutsideHaskell keepTests isEq ts

-- | Collect a `prop_*` definition. If `prop_foo` starts with a type signature `prop_foo :: ...`,
-- we consume the type signature AND the immediately following `prop_foo ... = ...` body!
collectPropDef :: Int -> [Token] -> ([Token], [Token])
collectPropDef depth ts
    | startsWithDoubleColon ts =
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
    | depth == 0 && isTokenIdentifier x && startsWithDoubleColon xs =
        ([], x : xs)
    -- Also stop at `::` if depth == 0 (in case the identifier was already consumed)
    | depth == 0 && startsWithDoubleColon (x : xs) =
        ([], x : xs)
    -- Stop at another test builder or `prop_*` definition or `main` at depth == 0
    | depth == 0
        && isTokenIdentifier x
        && ( (isHaskellTestBuilder (tToken x) && case xs of (t2 : _) -> isTokenString t2; [] -> False)
                || tToken x == "main"
                || ("prop_" `T.isPrefixOf` tToken x && isPropDefinition xs)
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
