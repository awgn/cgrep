{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import qualified Data.Text as T
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'R where
    isTestFile = isRTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideR keepTests tokens

-- | Recognizes `testthat` and `tinytest` test files (`test-*.R`, `test_*.R`, `test-*.r`, `test_*.r`).
isRTestFilePath :: OsPath -> Bool
isRTestFilePath path =
    (ext == [osp|.R|] || ext == [osp|.r|])
        && ( [osp|test-|] `OS.isPrefixOf` base
                || [osp|test_|] `OS.isPrefixOf` base
           )
  where
    ext = takeExtension path
    base = takeBaseName path

-- | Matches R's `::` or `:::` namespace operator in both default mode (2 or 3 adjacent `":"` tokens)
-- and strict mode (`"::"` / `":::"`).
matchRNamespaceSep :: [Token] -> Maybe ([Token], [Token])
matchRNamespaceSep (c : rest)
    | isTokenOperator c && (tToken c == "::" || tToken c == ":::") =
        Just ([c], rest)
matchRNamespaceSep (c1 : c2 : c3 : rest)
    | isTokenOperator c1 && tToken c1 == ":"
    , isTokenOperator c2 && tToken c2 == ":"
    , isTokenOperator c3 && tToken c3 == ":"
    , areAdjacentTokens c1 c2
    , areAdjacentTokens c2 c3 =
        Just ([c1, c2, c3], rest)
matchRNamespaceSep (c1 : c2 : rest)
    | isTokenOperator c1 && tToken c1 == ":"
    , isTokenOperator c2 && tToken c2 == ":"
    , areAdjacentTokens c1 c2 =
        Just ([c1, c2], rest)
matchRNamespaceSep _ = Nothing

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

-- | Unambiguous `testthat` / `tinytest` expectation and skip functions.
isRExpectationOrSkip :: Text -> Bool
isRExpectationOrSkip name =
    "expect_" `T.isPrefixOf` name
        || name
            `elem` ([ "skip_if"
                    , "skip_if_not"
                    , "skip_if_not_installed"
                    , "skip_on_cran"
                    , "skip_on_ci"
                    , "skip_on_os"
                    , "skip_on_bioc"
                    , "skip_on_covr"
                    ] :: [Text])

-- | Check if a token sequence starting at an unqualified function name is a `testthat` / `tinytest` call,
-- returning the matched prefix tokens (up to and including `(`) and remaining tokens after `(`.
matchUnqualifiedRTestCall :: [Token] -> Maybe ([Token], [Token])
-- Direct `test_that(`, `test_dir(`, `test_file(`, `expect_*(`, `skip_*(`
matchUnqualifiedRTestCall (t1 : t2 : ts)
    | isTokenIdentifier t1
    , tToken t1 `elem` (["test_that", "test_dir", "test_file"] :: [Text])
        || isRExpectationOrSkip (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        Just ([t1, t2], ts)
-- `describe("...", ...)`, `it("...", ...)`, `context("...")`
matchUnqualifiedRTestCall (t1 : t2 : ts)
    | isTokenIdentifier t1
    , tToken t1 `elem` (["describe", "it", "context"] :: [Text])
    , isTokenBracket t2 && tToken t2 == "("
    , startsWithStringArg ts =
        Just ([t1, t2], ts)
-- Fallback when `test _ that` is written with separated `_`
matchUnqualifiedRTestCall (t1 : t2 : t3 : t4 : ts)
    | isTokenIdentifier t1 && tToken t1 == "test"
    , (isTokenOperator t2 || isTokenIdentifier t2) && tToken t2 == "_"
    , isTokenIdentifier t3 && (tToken t3 `elem` (["that", "dir", "file"] :: [Text]))
    , isTokenBracket t4 && tToken t4 == "(" =
        Just ([t1, t2, t3, t4], ts)
matchUnqualifiedRTestCall _ = Nothing

-- | (R) Helper: Processes tokens *outside* a test block.
processOutsideR :: Bool -> [Token] -> [Token]
processOutsideR _ [] = []
-- Avoid matching member access (`obj$describe(...)`, `obj@describe(...)`) or R dot-identifiers (`custom.describe(...)`)
processOutsideR keepTests (t1 : t2 : ts)
    | isTokenOperator t1 && (tToken t1 == "$" || tToken t1 == "@" || tToken t1 == ".")
    , isTokenIdentifier t2 =
        if keepTests
            then processOutsideR keepTests ts
            else t1 : t2 : processOutsideR keepTests ts
-- Package-qualified call `pkg::fn(...)` or `pkg:::fn(...)`
processOutsideR keepTests (ns : restAfterNs)
    | isTokenIdentifier ns
    , Just (sepToks, afterSep) <- matchRNamespaceSep restAfterNs =
        if tToken ns == "testthat" || tToken ns == "tinytest"
            then case matchUnqualifiedRTestCall afterSep of
                Just (prefixTokens, tokensAfterParen) ->
                    let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterParen
                     in if keepTests
                            then ns : sepToks ++ prefixTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                            else processOutsideR keepTests remainingTokens
                Nothing ->
                    if keepTests
                        then processOutsideR keepTests afterSep
                        else ns : sepToks ++ processOutsideR keepTests afterSep
            else case afterSep of
                (fnTok : afterFn) | isTokenIdentifier fnTok ->
                    -- Consume `otherpkg::describe` together so `describe` is not mistaken for `testthat::describe`!
                    if keepTests
                        then processOutsideR keepTests afterFn
                        else ns : sepToks ++ [fnTok] ++ processOutsideR keepTests afterFn
                _ ->
                    if keepTests
                        then processOutsideR keepTests afterSep
                        else ns : sepToks ++ processOutsideR keepTests afterSep
-- Unqualified call
processOutsideR keepTests tokens@(t : ts) =
    case matchUnqualifiedRTestCall tokens of
        Just (prefixTokens, tokensAfterParen) ->
            let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterParen
             in if keepTests
                    then prefixTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                    else processOutsideR keepTests remainingTokens
        Nothing ->
            if keepTests
                then processOutsideR keepTests ts
                else t : processOutsideR keepTests ts
