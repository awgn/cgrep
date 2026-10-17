{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.C () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'C where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideC keepTests tokens

instance LanguageTestFilter 'Cpp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideC keepTests tokens

processOutsideC :: Bool -> [Token] -> [Token]
processOutsideC _ [] = [] -- End of stream

-- Pattern 1: C/C++ Test Macros (Google Test, Catch2, etc.)
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "TEST" || tToken t1 == "TEST_F" || tToken t1 == "TEST_CASE" ||
       tToken t1 == "TYPED_TEST" || tToken t1 == "SECTION" || tToken t1 == "SUITE") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test macro. Look ahead for the opening brace of the test body (limit 100 tokens).
        case findOpeningBraceC 100 ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- Pattern 2: Functions starting with "test_" or "Test"
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (("test_" `T.isPrefixOf` tToken t1) || ("Test" `T.isPrefixOf` tToken t1)) &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test function. Require the next token to be '(' to avoid matching variables.
        -- Find the opening brace within a bounded lookahead to avoid swallowing forward declarations.
        case findOpeningBraceC 50 ts of
            Nothing -> -- Malformed or forward declaration. Treat as non-test code.
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- No test found, process the current token
processOutsideC keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideC keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideC keepTests ts

-- ------------------------------------------------------------------
