{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.R () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenOperator,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)

instance LanguageTestFilter 'R where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideR keepTests tokens

-- | Check if the tokens inside `(` start with a string literal (e.g. `describe("...", { ... })`)
-- or a named string argument `desc = "..."` / `description = "..."`.
startsWithStringArg :: [Token] -> Bool
startsWithStringArg (t1 : _)
    | isTokenString t1 = True
startsWithStringArg (t1 : t2 : t3 : _)
    | isTokenIdentifier t1
    , isTokenOperator t2 && tToken t2 == "="
    , isTokenString t3 =
        True
startsWithStringArg _ = False

-- | Check if a token sequence starting at a function name is a `testthat` call,
-- returning the matched prefix tokens (up to and including `(`) and remaining tokens after `(`.
matchRTestCall :: [Token] -> Maybe ([Token], [Token])
-- Optional `testthat::` namespace prefix
matchRTestCall (ns : col : rest)
    | isTokenIdentifier ns && tToken ns == "testthat"
    , isTokenOperator col && (tToken col == "::" || tToken col == ":::") =
        case matchRTestCall rest of
            Just (pre, post) -> Just (ns : col : pre, post)
            Nothing -> Nothing
-- Direct `test_that(`, `test_dir(`, `test_file(`
matchRTestCall (t1 : t2 : ts)
    | isTokenIdentifier t1
    , tToken t1 `elem` (["test_that", "test_dir", "test_file"] :: [Text])
    , isTokenBracket t2 && tToken t2 == "(" =
        Just ([t1, t2], ts)
-- `describe("...", ...)`, `it("...", ...)`, `context("...")`:
-- Must be immediately followed by `(` AND take a string description as first argument
-- (avoiding false positives on `psych::describe(df)` or variables named `it`/`context`).
matchRTestCall (t1 : t2 : ts)
    | isTokenIdentifier t1
    , tToken t1 `elem` (["describe", "it", "context"] :: [Text])
    , isTokenBracket t2 && tToken t2 == "("
    , startsWithStringArg ts =
        Just ([t1, t2], ts)
-- Fallback when `test_that` / `test_dir` / `test_file` is tokenized as `test`, `_`, `that`
matchRTestCall (t1 : t2 : t3 : t4 : ts)
    | isTokenIdentifier t1 && tToken t1 == "test"
    , (isTokenOperator t2 || isTokenIdentifier t2) && tToken t2 == "_"
    , isTokenIdentifier t3 && (tToken t3 `elem` (["that", "dir", "file"] :: [Text]))
    , isTokenBracket t4 && tToken t4 == "(" =
        Just ([t1, t2, t3, t4], ts)
matchRTestCall _ = Nothing

-- | (R) Helper: Processes tokens *outside* a test block.
processOutsideR :: Bool -> [Token] -> [Token]
processOutsideR _ [] = []
-- Avoid matching method/member access like `obj$describe(...)` or `obj@describe(...)`
processOutsideR keepTests (t1 : t2 : ts)
    | isTokenOperator t1 && (tToken t1 == "$" || tToken t1 == "@")
    , isTokenIdentifier t2 =
        if keepTests
            then processOutsideR keepTests ts
            else t1 : t2 : processOutsideR keepTests ts
processOutsideR keepTests tokens@(t : ts) =
    case matchRTestCall tokens of
        Just (prefixTokens, tokensAfterParen) ->
            let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterParen
             in if keepTests
                    then prefixTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                    else processOutsideR keepTests remainingTokens
        Nothing ->
            if keepTests
                then processOutsideR keepTests ts
                else t : processOutsideR keepTests ts
