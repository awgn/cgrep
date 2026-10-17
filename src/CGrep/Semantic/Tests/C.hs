{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.C () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Chunk (pattern ChunkNativeType)
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    tToken,
    tTyp,
 )
import CGrep.Semantic.Tests.Common
import Data.Char (isAsciiLower)
import qualified Data.Text as T

instance LanguageTestFilter 'C where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideC keepTests False tokens

instance LanguageTestFilter 'Cpp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideC keepTests False tokens

-- | Checks if an identifier is a known C/C++ test macro (GoogleTest, Catch2, doctest, Boost.Test, CppUTest, Criterion).
isCTestMacro :: T.Text -> Bool
isCTestMacro n =
    -- GoogleTest
    n == "TEST" || n == "TEST_F" || n == "TEST_P"
        || n == "TYPED_TEST" || n == "TYPED_TEST_P"
        || n == "FRIEND_TEST"
        -- Catch2 / doctest
        || n == "TEST_CASE" || n == "TEST_CASE_METHOD"
        || n == "TEMPLATE_TEST_CASE" || n == "TEMPLATE_PRODUCT_TEST_CASE"
        || n == "SCENARIO" || n == "SCENARIO_METHOD"
        || n == "SECTION" || n == "SUBCASE"
        || n == "SUITE" || n == "TEST_SUITE" || n == "TEST_SUITE_BEGIN"
        -- Boost.Test
        || n == "BOOST_AUTO_TEST_CASE" || n == "BOOST_FIXTURE_TEST_CASE"
        || n == "BOOST_DATA_TEST_CASE" || n == "BOOST_AUTO_TEST_SUITE"
        || n == "BOOST_FIXTURE_TEST_SUITE"
        -- CppUTest / Criterion / Unity
        || n == "TEST_GROUP" || n == "Test"

-- | Checks if a C/C++ function name is a test function (`test_*` or `Test*` where `"Test"`
-- is NOT followed by a lowercase ASCII letter, avoiding domain words like `TestifyAccount`).
isCTestFuncName :: T.Text -> Bool
isCTestFuncName name
    | "test_" `T.isPrefixOf` name = True
    | Just rest <- T.stripPrefix "Test" name =
        case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAsciiLower c)
    | otherwise = False

-- | Whether a token can be the end of a C/C++ return type immediately before a function name
-- (preventing C++ constructors like `public: TestRunner(int v) { ... }` from being matched as functions).
canPrecedeCFuncName :: Token -> Bool
canPrecedeCFuncName t =
    isTokenIdentifier t
        || isTokenKeyword t
        || tTyp t == ChunkNativeType
        || (isTokenOperator t && (tToken t == "*" || tToken t == "&" || tToken t == ">"))

-- | Finds the opening `{` of a C/C++ function/macro body after its `(...)` parameter list,
-- stopping if `;` or `)` is encountered at depth 0 (preventing calls inside `if (test_foo()) { ... }`
-- or C++ constructor initializer lists `TestRunner() : a(1) { ... }` from being misidentified as test definitions).
findOpeningBraceCDecl :: Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceCDecl = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go depth n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            if depth == 0
                then Nothing -- Unmatched closing paren (e.g. inside `if (test_fn()) {`)
                else step (depth - 1) (n - 1) t ts
        | depth == 0 && tToken t == ":" =
            Nothing -- C++ constructor member initializer list `TestFoo() : member(0) {`
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0 && tToken t == ";" =
            Nothing
        | otherwise =
            step depth (n - 1) t ts

    step :: Int -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d m tok rest =
        case go d m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

processOutsideC :: Bool -> Bool -> [Token] -> [Token]
processOutsideC _ _ [] = []
-- Pattern 1: C/C++ Test Macros (Google Test, Catch2, doctest, Boost.Test, etc.)
processOutsideC keepTests _ (t1 : t2 : ts)
    | isTokenIdentifier t1
    , isCTestMacro (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 ts
         in case findOpeningBraceCDecl 50 afterParams of
                Nothing ->
                    if keepTests then processOutsideC keepTests False (t2 : ts) else t1 : processOutsideC keepTests False (t2 : ts)
                Just (signatureTokens, tokensAfterBrace) ->
                    let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                     in if keepTests
                            then t1 : t2 : paramTokens ++ signatureTokens ++ bodyTokens ++ processOutsideC keepTests False remainingTokens
                            else processOutsideC keepTests False remainingTokens
-- Pattern 2: C++ destructor `~TestFoo()` (never a test function, skip past it)
processOutsideC keepTests _ (t0 : t1 : t2 : ts)
    | tToken t0 == "~"
    , isTokenIdentifier t1
    , isTokenBracket t2 && tToken t2 == "(" =
        if keepTests
            then processOutsideC keepTests False ts
            else t0 : t1 : t2 : processOutsideC keepTests False ts
-- Pattern 3: Functions starting with "test_" or "Test" (must be preceded by a return type token!)
processOutsideC keepTests True (t1 : t2 : ts)
    | isTokenIdentifier t1
    , isCTestFuncName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 ts
         in case findOpeningBraceCDecl 25 afterParams of
                Nothing ->
                    if keepTests then processOutsideC keepTests False (t2 : ts) else t1 : processOutsideC keepTests False (t2 : ts)
                Just (signatureTokens, tokensAfterBrace) ->
                    let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                     in if keepTests
                            then t1 : t2 : paramTokens ++ signatureTokens ++ bodyTokens ++ processOutsideC keepTests False remainingTokens
                            else processOutsideC keepTests False remainingTokens
-- No test found, process the current token
processOutsideC keepTests _ (t : ts) =
    let hasRetType = canPrecedeCFuncName t
     in if keepTests
            then processOutsideC keepTests hasRetType ts
            else t : processOutsideC keepTests hasRetType ts
