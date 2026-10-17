{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.OCaml () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'OCaml where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideOCaml keepTests tokens

-- | Check if a module name is a test module (`Test`, `Tests`, `TestSuite`, `Test_foo`, `FooTest`)
-- without matching words like `Testimonial`.
isOCamlTestModuleName :: Text -> Bool
isOCamlTestModuleName name =
    let prefixOk = case T.stripPrefix "Test" name of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
     in prefixOk
            || "Test" `T.isSuffixOf` name
            || "Tests" `T.isSuffixOf` name
            || "_test" `T.isSuffixOf` name
            || "_tests" `T.isSuffixOf` name

-- | (OCaml) Helper: Processes tokens *outside* a test block.
processOutsideOCaml :: Bool -> [Token] -> [Token]
processOutsideOCaml _ [] = []
-- Pattern 1: `module Test... = struct ... end`
processOutsideOCaml keepTests (t1 : t2 : t3 : t4 : ts)
    | isTokenKeyword t1 && tToken t1 == "module"
    , isTokenIdentifier t2 && isOCamlTestModuleName (tToken t2)
    , isTokenOperator t3 && tToken t3 == "="
    , isTokenKeyword t4 && tToken t4 == "struct" =
        let (testTokens, remainingTokens) = collectUntilOCamlEnd 1 ts
         in if keepTests
                then t1 : t2 : t3 : t4 : testTokens ++ processOutsideOCaml keepTests remainingTokens
                else processOutsideOCaml keepTests remainingTokens
-- Pattern 2: `let%test`, `let%test_unit`, `let%test_module`, `let%expect_test`
processOutsideOCaml keepTests (t1 : t2 : t3 : ts)
    | isTokenKeyword t1 && tToken t1 == "let"
    , isTokenOperator t2 && tToken t2 == "%"
    , isTokenIdentifier t3
        && ( tToken t3 == "test"
                || "test_" `T.isPrefixOf` tToken t3
                || "expect_test" `T.isPrefixOf` tToken t3
           ) =
        let (testTokens, remainingTokens) = collectOCamlTestDef ts
         in if keepTests
                then t1 : t2 : t3 : testTokens ++ processOutsideOCaml keepTests remainingTokens
                else processOutsideOCaml keepTests remainingTokens
-- No test found, process the current token
processOutsideOCaml keepTests (t : ts) =
    if keepTests
        then processOutsideOCaml keepTests ts
        else t : processOutsideOCaml keepTests ts

-- | Collect tokens until the matching `end`, tracking nested `struct`/`sig`/`begin`/`object` blocks.
collectUntilOCamlEnd :: Int -> [Token] -> ([Token], [Token])
collectUntilOCamlEnd 0 ts = ([], ts)
collectUntilOCamlEnd _ [] = ([], [])
collectUntilOCamlEnd depth (t : ts)
    | isTokenKeyword t && (tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text])) =
        let (collected, remaining) = collectUntilOCamlEnd (depth + 1) ts
         in (t : collected, remaining)
    | isTokenKeyword t && tToken t == "end" =
        if depth == 1
            then ([t], ts)
            else
                let (collected, remaining) = collectUntilOCamlEnd (depth - 1) ts
                 in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilOCamlEnd depth ts
         in (t : collected, remaining)

-- | Check whether a `let` token (whose following tokens are `ts`) is a local
-- `let ... in` expression (as opposed to a top-level `let` item).
-- Because every nested local `let ... in` inside the binding RHS is balanced
-- (`+1` on `let`, `-1` on `in`), `letDepth` reaches 0 if and only if the starting
-- `let` has its own matching `in`!
isLocalLetIn :: [Token] -> Bool
isLocalLetIn (op : _)
    | isTokenOperator op && tToken op == "%" = False
isLocalLetIn ts0 = go 1 0 ts0
  where
    go :: Int -> Int -> [Token] -> Bool
    go _ _ [] = False
    go letDepth blockDepth (t : ts)
        | isTokenKeyword t && (tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text])) =
            go letDepth (blockDepth + 1) ts
        | isTokenKeyword t && tToken t == "end" =
            go letDepth (max 0 (blockDepth - 1)) ts
        | blockDepth > 0 =
            go letDepth blockDepth ts
        | isTokenKeyword t && tToken t == "in" =
            if letDepth <= 1
                then True
                else go (letDepth - 1) blockDepth ts
        | isTokenKeyword t && tToken t == "let" =
            case ts of
                (op : _) | isTokenOperator op && tToken op == "%" -> False
                _ -> go (letDepth + 1) blockDepth ts
        | isTokenKeyword t && (tToken t `elem` (["module", "type", "exception", "external", "class", "open", "include"] :: [Text])) =
            False
        | isTokenOperator t && tToken t == ";;" =
            False
        | otherwise =
            go letDepth blockDepth ts

-- | Collect tokens of a top-level `let%test` / `let%expect_test` definition until
-- the next top-level declaration (`let` without matching `in`, `let%...`, `module`, `type`, etc.).
collectOCamlTestDef :: [Token] -> ([Token], [Token])
collectOCamlTestDef [] = ([], [])
collectOCamlTestDef (t : ts)
    | isTokenKeyword t && (tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text])) =
        let (blockTokens, afterBlock) = collectUntilOCamlEnd 1 ts
            (restTokens, remaining) = collectOCamlTestDef afterBlock
         in (t : blockTokens ++ restTokens, remaining)
    | isTokenBracket t && (tToken t `elem` (["(", "[", "{"] :: [Text])) =
        let closeB = case tToken t of
                "(" -> ")"
                "[" -> "]"
                _ -> "}"
            (bracketTokens, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts
            (restTokens, remaining) = collectOCamlTestDef afterBracket
         in (t : bracketTokens ++ restTokens, remaining)
    | isTokenKeyword t && tToken t == "let" =
        if isLocalLetIn ts
            then
                let (collected, remaining) = collectOCamlTestDef ts
                 in (t : collected, remaining)
            else ([], t : ts)
    | isTokenKeyword t && (tToken t `elem` (["module", "type", "exception", "external", "class", "open", "include"] :: [Text])) =
        ([], t : ts)
    | otherwise =
        let (collected, remaining) = collectOCamlTestDef ts
         in (t : collected, remaining)
