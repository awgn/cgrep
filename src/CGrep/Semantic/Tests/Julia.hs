{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Julia () where

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
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Julia where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJulia keepTests tokens

-- | Check if a macro identifier (after `@`) is a Julia test macro
-- (`@test`, `@testset`, `@test_throws`, `@test_broken`, `@test_skip`, `@test_warn`,
-- `@test_nowarn`, `@test_logs`, `@test_deprecated`, `@inferred`, `@testitem`, etc.)
-- without matching unrelated words like `@testify`.
isJuliaTestMacro :: Text -> Bool
isJuliaTestMacro name =
    name
        `elem` ([ "test"
                , "testset"
                , "inferred"
                , "testitem"
                , "testmodule"
                , "testsnippet"
                ] :: [Text])
        || "test_" `T.isPrefixOf` name

-- | (Julia) Helper: Processes tokens *outside* a test block.
processOutsideJulia :: Bool -> [Token] -> [Token]
processOutsideJulia _ [] = []
-- Pattern 1: @test, @testset, @test_throws, etc.
processOutsideJulia keepTests (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "@"
    , isTokenIdentifier t2 && isJuliaTestMacro (tToken t2) =
        let (testTokens, remainingTokens) = collectJuliaTest ts
         in if keepTests
                then t1 : t2 : testTokens ++ processOutsideJulia keepTests remainingTokens
                else processOutsideJulia keepTests remainingTokens
-- No test found, process the current token
processOutsideJulia keepTests (t : ts) =
    if keepTests
        then processOutsideJulia keepTests ts
        else t : processOutsideJulia keepTests ts

collectJuliaTest :: [Token] -> ([Token], [Token])
collectJuliaTest [] = ([], [])
collectJuliaTest (t : ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "begin" || tToken t == "for" || tToken t == "let") =
        let (bodyTokens, remaining) = collectJuliaBlock 1 ts
         in (t : bodyTokens, remaining)
    | isTokenBracket t && tToken t == "(" =
        let (parenTokens, afterParen) = processInsideBrackets "(" ")" 1 ts
            (restTokens, remaining) = collectJuliaTest afterParen
         in (t : parenTokens ++ restTokens, remaining)
    | isTokenBracket t && tToken t == "[" =
        let (bracketTokens, afterBracket) = processInsideBrackets "[" "]" 1 ts
            (restTokens, remaining) = collectJuliaTest afterBracket
         in (t : bracketTokens ++ restTokens, remaining)
    | isTokenOperator t && tToken t == "@" = ([], t : ts)
    | (isTokenKeyword t || isTokenIdentifier t)
        && ( tToken t
                `elem` ([ "function"
                        , "struct"
                        , "mutable"
                        , "abstract"
                        , "primitive"
                        , "macro"
                        , "module"
                        , "baremodule"
                        , "end"
                        ] :: [Text])
           ) =
        ([], t : ts)
    | otherwise =
        let (collected, remaining) = collectJuliaTest ts
         in (t : collected, remaining)

-- | Collect tokens inside a Julia `begin ... end` / `for ... end` block,
-- consuming `[...]` index expressions via `processInsideBrackets` so that
-- array slice indexing like `arr[2:end]` does NOT decrement the `begin ... end` depth!
collectJuliaBlock :: Int -> [Token] -> ([Token], [Token])
collectJuliaBlock 0 ts = ([], ts)
collectJuliaBlock _ [] = ([], [])
collectJuliaBlock depth (t : ts)
    | isTokenBracket t && tToken t == "[" =
        let (bracketTokens, afterBracket) = processInsideBrackets "[" "]" 1 ts
            (collected, remaining) = collectJuliaBlock depth afterBracket
         in (t : bracketTokens ++ collected, remaining)
    | (isTokenKeyword t || isTokenIdentifier t)
        && ( tToken t
                `elem` ([ "begin"
                        , "function"
                        , "struct"
                        , "macro"
                        , "let"
                        , "quote"
                        , "do"
                        , "if"
                        , "for"
                        , "while"
                        , "try"
                        , "module"
                        , "baremodule"
                        ] :: [Text])
           ) =
        let (collected, remaining) = collectJuliaBlock (depth + 1) ts
         in (t : collected, remaining)
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
        if depth == 1
            then ([t], ts)
            else
                let (collected, remaining) = collectJuliaBlock (depth - 1) ts
                 in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectJuliaBlock depth ts
         in (t : collected, remaining)
