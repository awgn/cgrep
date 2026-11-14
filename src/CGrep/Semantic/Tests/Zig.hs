{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Zig () where

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
import System.OsPath (OsPath, osp, splitDirectories, takeBaseName, takeExtension, takeFileName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Zig where
    isTestFile = isZigTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideZig keepTests False tokens

-- | Zig standalone test module conventions (`tests/` directory, `tests.zig`,
-- `test_*.zig`, `*_test.zig`, `*_tests.zig`).
isZigTestFilePath :: OsPath -> Bool
isZigTestFilePath path =
    let ext = takeExtension path
        base = takeBaseName path
        dirs = map takeFileName (dropLastPart (splitDirectories path))
        inTestsDir = any (== [osp|tests|]) dirs
        isTestBase =
            base == [osp|tests|]
                || ([osp|test_|] `OS.isPrefixOf` base)
                || ([osp|_test|] `OS.isSuffixOf` base)
                || ([osp|_tests|] `OS.isSuffixOf` base)
     in ext == [osp|.zig|] && (inTestsDir || isTestBase)
  where
    dropLastPart [] = []
    dropLastPart [_] = []
    dropLastPart (x : xs) = x : dropLastPart xs

-- | Match a single Zig identifier or `@"escaped"` identifier at the start of a decltest path.
consumeZigDeclSegment :: [Token] -> Maybe ([Token], [Token])
consumeZigDeclSegment (t : ts)
    | isTokenIdentifier t = Just ([t], ts)
consumeZigDeclSegment (tAt : tStr : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenString tStr =
        Just ([tAt, tStr], ts)
consumeZigDeclSegment _ = Nothing

-- | Match a Zig `TestDecl` header right after the `test` keyword:
--   1. `{` (unnamed test `test { ... }`)
--   2. `"description" {` (`test "addition" { ... }`)
--   3. `<identOrEscaped>(.<identOrEscaped>)* {` (dotted decltest `test a.b.c.fn { ... }`)
matchZigTestHeader :: [Token] -> Maybe ([Token], [Token])
matchZigTestHeader (tBrace : ts)
    | isTokenBracket tBrace && tToken tBrace == "{" =
        Just ([tBrace], ts)
matchZigTestHeader (tStr : tBrace : ts)
    | isTokenString tStr
    , isTokenBracket tBrace && tToken tBrace == "{" =
        Just ([tStr, tBrace], ts)
matchZigTestHeader ts =
    case consumeZigDeclSegment ts of
        Nothing -> Nothing
        Just (firstSeg, rest0) -> goPath (40 :: Int) firstSeg rest0
  where
    goPath :: Int -> [Token] -> [Token] -> Maybe ([Token], [Token])
    goPath 0 _ _ = Nothing
    goPath _ acc (tBrace : rest)
        | isTokenBracket tBrace && tToken tBrace == "{" =
            Just (acc ++ [tBrace], rest)
    goPath n acc (tDot : rest)
        | isTokenOperator tDot && tToken tDot == "."
        , Just (segToks, afterSeg) <- consumeZigDeclSegment rest =
            goPath (n - 1) (acc ++ (tDot : segToks)) afterSeg
    goPath _ _ _ = Nothing

-- | Check whether parenthesized `if (...)` tokens represent a positive Zig
-- `builtin.is_test` or `@import("builtin").is_test` guard (rejecting `!builtin.is_test`).
isPositiveZigIsTestCond :: [Token] -> Bool
isPositiveZigIsTestCond [t1, tDot, tProp] =
    isTokenIdentifier t1
        && isTokenOperator tDot
        && tToken tDot == "."
        && isTokenIdentifier tProp
        && tToken tProp == "is_test"
isPositiveZigIsTestCond [tAt, tImp, tPOpen, tStr, tPClose, tDot, tProp] =
    isTokenOperator tAt
        && tToken tAt == "@"
        && (isTokenIdentifier tImp || isTokenKeyword tImp)
        && tToken tImp == ("import" :: Text)
        && isTokenBracket tPOpen
        && tToken tPOpen == "("
        && isTokenString tStr
        && isTokenBracket tPClose
        && tToken tPClose == ")"
        && isTokenOperator tDot
        && tToken tDot == "."
        && isTokenIdentifier tProp
        && tToken tProp == "is_test"
isPositiveZigIsTestCond _ = False

dropClosingParen :: [Token] -> [Token]
dropClosingParen [] = []
dropClosingParen [t] | isTokenBracket t && tToken t == ")" = []
dropClosingParen (t : ts) = t : dropClosingParen ts

-- | (Zig) Helper: Processes tokens *outside* a test block.
processOutsideZig :: Bool -> Bool -> [Token] -> [Token]
processOutsideZig _ _ [] = []

-- Pattern 1: `test [string | dotted.decl.path] { ... }`
processOutsideZig keepTests False (t1 : ts)
    | isTokenKeyword t1 && tToken t1 == "test"
    , Just (hdrAndBrace, afterBrace) <- matchZigTestHeader ts =
        let (bodyTokens, remainingTokens) = processInsideBraces 1 afterBrace
         in if keepTests
                then t1 : hdrAndBrace ++ bodyTokens ++ processOutsideZig keepTests False remainingTokens
                else processOutsideZig keepTests False remainingTokens

-- Pattern 2: `if (builtin.is_test) { ... }` (keeping any `else { ... }` branch as prod!)
processOutsideZig keepTests False (tIf : tOpen : ts)
    | isTokenKeyword tIf && tToken tIf == "if"
    , isTokenBracket tOpen && tToken tOpen == "("
    , let (condToks, afterParens) = processInsideBrackets "(" ")" 1 ts
    , isPositiveZigIsTestCond (dropClosingParen condToks)
    , (tBrace : afterBrace) <- afterParens
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (thenBody, remainingTokens) = processInsideBraces 1 afterBrace
            testToks = tIf : tOpen : condToks ++ (tBrace : thenBody)
         in if keepTests
                then testToks ++ processOutsideZig keepTests False remainingTokens
                else processOutsideZig keepTests False remainingTokens

-- Fallback: advance one token and track `.`
processOutsideZig keepTests _ (t : ts) =
    let nextPrevDot = isTokenOperator t && tToken t == "."
     in if keepTests
            then processOutsideZig keepTests nextPrevDot ts
            else t : processOutsideZig keepTests nextPrevDot ts
