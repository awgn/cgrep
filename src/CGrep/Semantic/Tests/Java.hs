{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import System.OsPath (OsPath, osp, splitDirectories, takeBaseName, takeFileName)
import qualified System.OsString as OS
instance LanguageTestFilter 'Java where
    isTestFile = isJavaTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJava keepTests tokens

-- | Checks if a Java file path is a test file according to Maven/Gradle conventions
-- (e.g. under `test/` or `tests/` directory, or named `*Test.java`, `*Tests.java`,
-- `*TestCase.java`, `Test*.java`, `*IT.java`).
isJavaTestFilePath :: OsPath -> Bool
isJavaTestFilePath path =
    let parts = splitDirectories path
        dirs  = case reverse parts of
                    (_:ds) -> ds
                    []     -> []
        base  = takeBaseName path
        inTestDir  = any (\d -> let n = takeFileName d in n == [osp|test|] || n == [osp|tests|]) dirs
        isTestName =
            base == [osp|Test|]
                || base == [osp|Tests|]
                || ([osp|Test|] `OS.isPrefixOf` base)
                || ([osp|Test|] `OS.isSuffixOf` base)
                || ([osp|Tests|] `OS.isSuffixOf` base)
                || ([osp|TestCase|] `OS.isSuffixOf` base)
                || ([osp|IT|] `OS.isSuffixOf` base)
    in inTestDir || isTestName

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
