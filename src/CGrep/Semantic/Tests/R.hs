{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.R () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'R where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideR keepTests tokens

-- | (R) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test_that("...") - testthat
--   2. describe("...") - testthat
--   3. context("...") - testthat
--   4. it("...") - testthat
--   5. test_dir/test_file
--
-- R uses testthat framework.
processOutsideR :: Bool -> [Token] -> [Token]
processOutsideR _ [] = [] -- End of stream

-- Pattern 1: test_that( or describe( or context( or it( etc.
processOutsideR keepTests (t1:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test_that" || tToken t1 == "describe" || tToken t1 == "context" ||
       tToken t1 == "it" || tToken t1 == "test_dir" || tToken t1 == "test_file")
    =
        case findOpeningBracketBounded "(" 20 ts of
            Nothing ->
                if keepTests then t1 : processOutsideR keepTests ts else t1 : processOutsideR keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                   else processOutsideR keepTests remainingTokens

-- Pattern 2: Fallback for when test_that is tokenized as test, _, that
processOutsideR keepTests (t1:t2:t3:ts)
    | isTokenIdentifier t1 && tToken t1 == "test" &&
      (isTokenOperator t2 || isTokenIdentifier t2) && tToken t2 == "_" &&
      isTokenIdentifier t3 && (tToken t3 == "that" || tToken t3 == "dir" || tToken t3 == "file")
    =
        case findOpeningBracketBounded "(" 20 ts of
            Nothing ->
                if keepTests then t1 : t2 : t3 : processOutsideR keepTests ts else t1 : t2 : t3 : processOutsideR keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : signatureTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                   else processOutsideR keepTests remainingTokens

-- No test found, process the current token
processOutsideR keepTests (t:ts) =
    if keepTests
    then processOutsideR keepTests ts
    else t : processOutsideR keepTests ts

-- ------------------------------------------------------------------
