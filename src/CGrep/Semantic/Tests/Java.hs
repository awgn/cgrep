{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Java () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T

instance LanguageTestFilter 'Java where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJava keepTests tokens

-- | Check if a Java class header before `{` extends `*TestCase` (supporting generic classes `class Foo<T> extends TestCase`).
extendsJUnitTestCase :: [Token] -> Bool
extendsJUnitTestCase tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "{") && tToken t /= ";") tokens
     in hasExtendsTestCase headerTokens
  where
    hasExtendsTestCase (t1 : t2 : ts)
        | isTokenKeyword t1 && tToken t1 == "extends"
        , isTokenIdentifier t2 && "TestCase" `T.isSuffixOf` tToken t2 =
            True
        | otherwise = hasExtendsTestCase (t2 : ts)
    hasExtendsTestCase _ = False

-- | (Java) Helper: Processes tokens *outside* a test method.
processOutsideJava :: Bool -> [Token] -> [Token]
processOutsideJava _ [] = []
-- Pattern 1: Annotated method or class (JUnit 4/5, TestNG, etc.)
processOutsideJava keepTests tokens
    | let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens
    , not (null attrTokens) =
        if isTest
            then case findOpeningBraceC 100 restAfterAttrs of
                Nothing ->
                    if keepTests
                        then processOutsideJava keepTests restAfterAttrs
                        else attrTokens ++ processOutsideJava keepTests restAfterAttrs
                Just (sigTokens, restAfterBrace) ->
                    let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                     in if keepTests
                            then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideJava keepTests finalRest
                            else processOutsideJava keepTests finalRest
            else
                if keepTests
                    then processOutsideJava keepTests restAfterAttrs
                    else attrTokens ++ processOutsideJava keepTests restAfterAttrs
-- Pattern 2: JUnit 3 class extending TestCase (`class Foo extends TestCase { ... }` or `class Foo<T> extends TestCase { ... }`)
processOutsideJava keepTests (t1 : t2 : ts)
    | isTokenKeyword t1 && tToken t1 == "class"
    , isTokenIdentifier t2
    , extendsJUnitTestCase ts =
        case findOpeningBraceC 100 ts of
            Nothing ->
                if keepTests then processOutsideJava keepTests (t2 : ts) else t1 : processOutsideJava keepTests (t2 : ts)
            Just (sigTokens, restAfterBrace) ->
                let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                 in if keepTests
                        then t1 : t2 : sigTokens ++ bodyTokens ++ processOutsideJava keepTests finalRest
                        else processOutsideJava keepTests finalRest
-- No test found, process the current token
processOutsideJava keepTests (t : ts) =
    if keepTests
        then processOutsideJava keepTests ts
        else t : processOutsideJava keepTests ts
