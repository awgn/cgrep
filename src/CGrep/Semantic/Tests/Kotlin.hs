{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Kotlin () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T

instance LanguageTestFilter 'Kotlin where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideKotlin keepTests tokens

-- | Helper for Kotlin to avoid swallowing braces of subsequent functions after abstract methods,
-- while ignoring braces/keywords nested inside `(...)` or `[...]` (e.g. default parameter lambdas
-- like `fun myTest(cb: () -> Unit = {}) { ... }`).
findOpeningBraceKotlin :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceKotlin canSeeDecl0 n0 = go 0 canSeeDecl0 n0
  where
    go :: Int -> Bool -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ _ 0 _ = Nothing
    go _ _ _ [] = Nothing
    go depth canSeeDecl n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) canSeeDecl (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) canSeeDecl (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0 && tToken t == ";" =
            Nothing
        | depth == 0 && isTokenKeyword t && (tToken t == "fun" || tToken t == "class" || tToken t == "interface" || tToken t == "val" || tToken t == "var") =
            if canSeeDecl
                then step depth False (n - 1) t ts
                else Nothing
        | otherwise =
            step depth canSeeDecl (n - 1) t ts

    step :: Int -> Bool -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d c m tok rest =
        case go d c m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

isKotestBlockName :: T.Text -> Bool
isKotestBlockName n =
    n == "test" || n == "xtest"
        || n == "describe" || n == "xdescribe"
        || n == "it" || n == "xit"
        || n == "context" || n == "xcontext"
        || n == "should" || n == "xshould"
        || n == "given" || n == "xgiven"
        || n == "when" || n == "xwhen"
        || n == "then" || n == "xthen"
        || n == "feature" || n == "xfeature"
        || n == "scenario" || n == "xscenario"
        || n == "expect" || n == "xexpect"

processOutsideKotlin :: Bool -> [Token] -> [Token]
processOutsideKotlin _ [] = []
-- Pattern 1: Annotated method or class (@Test, @BeforeEach, @Nested, etc.)
processOutsideKotlin keepTests tokens
    | let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens
    , not (null attrTokens) =
        if isTest
            then case findOpeningBraceKotlin True 100 restAfterAttrs of
                Nothing ->
                    if keepTests
                        then processOutsideKotlin keepTests restAfterAttrs
                        else attrTokens ++ processOutsideKotlin keepTests restAfterAttrs
                Just (sigTokens, restAfterBrace) ->
                    let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                     in if keepTests
                            then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                            else processOutsideKotlin keepTests finalRest
            else
                if keepTests
                    then processOutsideKotlin keepTests restAfterAttrs
                    else attrTokens ++ processOutsideKotlin keepTests restAfterAttrs
-- Pattern 2: Kotest / Spek DSL blocks with string description in parentheses:
-- `test("...") { ... }`, `describe("...") { ... }`, `when("...") { ... }`
-- Requiring the first argument inside `(...)` to be a string literal prevents Kotlin's
-- built-in `when (status) { ... }` expression from being mistaken for a Kotest block!
processOutsideKotlin keepTests (t1 : t2 : ts@(firstArg : _))
    | (isTokenIdentifier t1 || isTokenKeyword t1)
    , isKotestBlockName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenString firstArg =
        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 ts
         in case findOpeningBraceKotlin False 20 afterArgs of
                Nothing ->
                    if keepTests then processOutsideKotlin keepTests (t2 : ts) else t1 : processOutsideKotlin keepTests (t2 : ts)
                Just (sigTokens, restAfterBrace) ->
                    let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                     in if keepTests
                            then t1 : t2 : argTokens ++ sigTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                            else processOutsideKotlin keepTests finalRest
-- Pattern 3: Kotest StringSpec / FreeSpec / WordSpec DSL blocks:
-- `"test description" { ... }`, `"container" - { ... }`, `"subject" should { ... }`
processOutsideKotlin keepTests (t1 : ts)
    | isTokenString t1 =
        case matchKotestStringBlock ts of
            Just (hdrTokens, restAfterBrace) ->
                let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                 in if keepTests
                        then t1 : hdrTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                        else processOutsideKotlin keepTests finalRest
            Nothing ->
                if keepTests
                    then processOutsideKotlin keepTests ts
                    else t1 : processOutsideKotlin keepTests ts
  where
    matchKotestStringBlock (b : rest)
        | isTokenBracket b && tToken b == "{" = Just ([b], rest)
    matchKotestStringBlock (op : b : rest)
        | tToken op == "-" && isTokenBracket b && tToken b == "{" = Just ([op, b], rest)
        | isTokenIdentifier op
        , tToken op == "should" || tToken op == "xshould" || tToken op == "config"
        , isTokenBracket b && tToken b == "{" =
            Just ([op, b], rest)
    matchKotestStringBlock _ = Nothing
processOutsideKotlin keepTests (t : ts) =
    if keepTests
        then processOutsideKotlin keepTests ts
        else t : processOutsideKotlin keepTests ts
