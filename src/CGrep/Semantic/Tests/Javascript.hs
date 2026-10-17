{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Javascript () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Javascript where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJavascript keepTests tokens

-- | (Javascript) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. describe('...', function() { ... }) - Mocha/Jasmine/Jest
--   2. describe('...', () => { ... }) - Modern syntax
--   3. it('...', function() { ... }) - Test cases
--   4. it('...', () => { ... }) - Modern syntax
--   5. test('...', function() { ... }) - Jest
--   6. test('...', () => { ... }) - Jest modern syntax
--   7. context('...', ...) - Mocha/Jasmine
--
-- All these frameworks use similar patterns with function calls.
processOutsideJavascript :: Bool -> [Token] -> [Token]
processOutsideJavascript _ [] = [] -- End of stream

-- Pattern 1: describe( or it( or test( or context(
processOutsideJavascript keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "describe" || tToken t1 == "it" || tToken t1 == "test" || tToken t1 == "context") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test function call. Capture everything inside the parentheses.
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then -- Keep test function call + signature + body
                t1 : t2 : bodyTokens ++ processOutsideJavascript keepTests remainingTokens
           else -- Discard the whole test block
                processOutsideJavascript keepTests remainingTokens

-- No test found, process the current token
processOutsideJavascript keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideJavascript keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideJavascript keepTests ts
-- ------------------------------------------------------------------
