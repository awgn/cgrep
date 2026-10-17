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
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Scala where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideScala keepTests tokens

-- | (Scala) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test("...") { ... } - ScalaTest FunSuite, MUnit
--   2. it("...") { ... } - ScalaTest FunSpec
--   3. describe("...") { ... } - ScalaTest FunSpec
--   4. scenario("...") { ... } - ScalaTest FeatureSpec
--   5. feature("...") { ... } - ScalaTest FeatureSpec
--
-- ScalaTest and MUnit are the most popular testing frameworks for Scala.
processOutsideScala :: Bool -> [Token] -> [Token]
processOutsideScala _ [] = [] -- End of stream

-- Pattern: test( or it( or describe( or scenario( or feature(
processOutsideScala keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test" || tToken t1 == "it" || tToken t1 == "describe" ||
       tToken t1 == "scenario" || tToken t1 == "feature") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test function call. Find the opening brace of the test body.
        case findOpeningBracketBounded "{" 20 ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideScala keepTests (t2:ts) else t1 : processOutsideScala keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes everything up to and including the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then -- Keep test function call + signature + body
                        t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideScala keepTests remainingTokens
                   else -- Discard the whole test block
                        processOutsideScala keepTests remainingTokens

-- No test found, process the current token
processOutsideScala keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideScala keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideScala keepTests ts
-- ------------------------------------------------------------------
