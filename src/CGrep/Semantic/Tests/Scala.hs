{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Scala () where

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

instance LanguageTestFilter 'Scala where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideScala keepTests False tokens

-- | ScalaTest / MUnit / Specs2 / ZiO Test block names
isScalaTestBlockName :: Text -> Bool
isScalaTestBlockName name =
    name
        `elem` ([ "test"
                , "ignore"
                , "it"
                , "they"
                , "describe"
                , "scenario"
                , "feature"
                , "Scenario"
                , "Feature"
                , "property"
                , "suite"
                , "testM"
                ] :: [Text])

-- | FlatSpec / WordSpec / FreeSpec / ShouldMatchers infix verbs
isScalaSpecVerb :: Text -> Bool
isScalaSpecVerb name =
    name
        `elem` ([ "should"
                , "must"
                , "can"
                , "in"
                , "ignore"
                , "of"
                , "when"
                , "which"
                , "that"
                ] :: [Text])

-- | Find opening `{` for an annotated Scala `def` / `class`, stopping if another
-- declaration (`def`, `val`, `var`, `class`, `object`, `trait`) is encountered
-- after the first one, while ignoring `(...)` and `[...]`.
findOpeningBraceScala :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceScala canSeeDecl0 n0 = go 0 canSeeDecl0 n0
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
        | depth == 0
            && isTokenKeyword t
            && (tToken t == "def" || tToken t == "val" || tToken t == "var" || tToken t == "class" || tToken t == "object" || tToken t == "trait") =
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

-- | Match FlatSpec / WordSpec / FreeSpec header tokens after a string literal or `it`/`they`
-- followed by a spec verb (e.g. `"A Calc" should "add" in { ... }` or `"test" - { ... }`),
-- stopping if `;`, `def`, `val`, `var`, `class`, `object`, or `trait` appears at top level
-- before `{`.
findFlatSpecBrace :: Int -> [Token] -> Maybe ([Token], [Token])
findFlatSpecBrace = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go depth n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0
            && ( tToken t == ";"
                    || ( isTokenKeyword t
                            && (tToken t == "def" || tToken t == "val" || tToken t == "var" || tToken t == "class" || tToken t == "object" || tToken t == "trait")
                       )
               ) =
            Nothing
        | otherwise =
            step depth (n - 1) t ts

    step :: Int -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d m tok rest =
        case go d m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | After `test(...)` / `it(...)` / `describe(...)`, consume any chained `.tag(...)` or
-- curried `(tag1, tag2)` argument lists, and require an immediate `{` for the test body.
matchAfterScalaTestParens :: [Token] -> Maybe ([Token], [Token])
matchAfterScalaTestParens (tBrace : ts)
    | isTokenBracket tBrace && tToken tBrace == "{" =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 ts
         in Just (tBrace : bodyTokens, remainingTokens)
matchAfterScalaTestParens (tOpen : ts)
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 ts
         in case matchAfterScalaTestParens afterArgs of
                Just (moreTokens, remaining) ->
                    Just (tOpen : argTokens ++ moreTokens, remaining)
                Nothing -> Nothing
matchAfterScalaTestParens (tDot : tMod : tOpen : ts)
    | isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tMod
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 ts
         in case matchAfterScalaTestParens afterArgs of
                Just (moreTokens, remaining) ->
                    Just (tDot : tMod : tOpen : argTokens ++ moreTokens, remaining)
                Nothing -> Nothing
matchAfterScalaTestParens _ = Nothing

-- | (Scala) Helper: Processes tokens *outside* a test block.
-- The `Bool` parameter `prevBlock` is `True` when the immediately preceding token was
-- `.` (method call like `calc.test(...)`) or `def`/`val`/`var` (declaration like `def test(...)`).
processOutsideScala :: Bool -> Bool -> [Token] -> [Token]
processOutsideScala _ _ [] = []

-- Pattern 1: Annotated method or class (@Test, @BeforeEach, etc.)
processOutsideScala keepTests _ tokens
    | let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens
    , not (null attrTokens) =
        if isTest
            then case findOpeningBraceScala True 100 restAfterAttrs of
                Nothing ->
                    if keepTests
                        then processOutsideScala keepTests False restAfterAttrs
                        else attrTokens ++ processOutsideScala keepTests False restAfterAttrs
                Just (signatureTokens, tokensAfterBrace) ->
                    let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                     in if keepTests
                            then attrTokens ++ signatureTokens ++ bodyTokens ++ processOutsideScala keepTests False remainingTokens
                            else processOutsideScala keepTests False remainingTokens
            else
                if keepTests
                    then processOutsideScala keepTests False restAfterAttrs
                    else attrTokens ++ processOutsideScala keepTests False restAfterAttrs

-- Pattern 2: FunSuite / FunSpec / FeatureSpec / MUnit:
-- test("...") { ... }, ignore("...") { ... }, describe("...") { ... }, it("...") { ... },
-- scenario("...") { ... }, feature("...") { ... }, Scenario("...") { ... }, Feature("...") { ... }
processOutsideScala keepTests False (t1 : t2 : ts)
    | isTokenIdentifier t1 && isScalaTestBlockName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , let (argTokens, afterParens) = processInsideBrackets "(" ")" 1 ts
    , Just (braceAndBody, remainingTokens) <- matchAfterScalaTestParens afterParens =
        if keepTests
            then t1 : t2 : argTokens ++ braceAndBody ++ processOutsideScala keepTests False remainingTokens
            else processOutsideScala keepTests False remainingTokens

-- Pattern 3: FlatSpec / WordSpec / FreeSpec starting with a String literal or `it`/`they`
-- e.g. "A Calculator" should "add numbers" in { ... }
--      it should "handle zero" in { ... }
--      "A FreeSpec" - { ... }
processOutsideScala keepTests False (t1 : t2 : ts)
    | (isTokenString t1 || (isTokenIdentifier t1 && (tToken t1 == "it" || tToken t1 == "they" || tToken t1 == "ignore")))
    , (isTokenIdentifier t2 && isScalaSpecVerb (tToken t2)) || (isTokenOperator t2 && tToken t2 == "-")
    , Just (headerAndBrace, afterBrace) <- findFlatSpecBrace 15 ts =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
         in if keepTests
                then t1 : t2 : headerAndBrace ++ bodyTokens ++ processOutsideScala keepTests False remainingTokens
                else processOutsideScala keepTests False remainingTokens

-- Fallback: advance one token and update `prevBlock`
processOutsideScala keepTests _ (t : ts) =
    let isBlocker =
            (isTokenOperator t && tToken t == ".")
                || (isTokenKeyword t && (tToken t == "def" || tToken t == "val" || tToken t == "var"))
     in if keepTests
            then processOutsideScala keepTests isBlocker ts
            else t : processOutsideScala keepTests isBlocker ts
