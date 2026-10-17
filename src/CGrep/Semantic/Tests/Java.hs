{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Java () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T

instance LanguageTestFilter 'Java where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJava keepTests tokens

-- | (Java) Helper: Processes tokens *outside* a test method.
processOutsideJava :: Bool -> [Token] -> [Token]
processOutsideJava _ [] = [] -- End of stream

-- Pattern 1: Annotated method or class (JUnit 4/5, TestNG, etc.)
processOutsideJava keepTests tokens
    | let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens
    , not (null attrTokens) =
           if isTest
           then
               case findOpeningBraceC 100 restAfterAttrs of
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

-- Pattern 2: JUnit 3 class extending TestCase (`class Foo extends TestCase { ... }`)
processOutsideJava keepTests (t1:t2:t3:t4:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      isTokenKeyword t3 && tToken t3 == "extends" &&
      isTokenIdentifier t4 && "TestCase" `T.isSuffixOf` tToken t4
    =
        case findOpeningBraceC 100 ts of
            Nothing ->
                if keepTests then processOutsideJava keepTests (t2:t3:t4:ts) else t1 : processOutsideJava keepTests (t2:t3:t4:ts)
            Just (sigTokens, restAfterBrace) ->
                let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : sigTokens ++ bodyTokens ++ processOutsideJava keepTests finalRest
                   else processOutsideJava keepTests finalRest

-- No test found, process the current token
processOutsideJava keepTests (t:ts) =
    if keepTests
    then processOutsideJava keepTests ts
    else t : processOutsideJava keepTests ts

-- ------------------------------------------------------------------
