{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Dart () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Dart where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideDart keepTests tokens

-- | (Dart) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test('...') or test("...") - Dart test package
--   2. group('...') or group("...") - Dart test package
--   3. testWidgets('...') - Flutter test package
--   4. setUp(...), tearDown(...), setUpAll(...), tearDownAll(...)
--
-- Similar to JavaScript but with Dart-specific functions.
processOutsideDart :: Bool -> [Token] -> [Token]
processOutsideDart _ [] = [] -- End of stream

-- Pattern: test( or group( or testWidgets( or setUp( etc.
processOutsideDart keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test" || tToken t1 == "group" || tToken t1 == "testWidgets" ||
       tToken t1 == "setUp" || tToken t1 == "tearDown" ||
       tToken t1 == "setUpAll" || tToken t1 == "tearDownAll") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then t1 : t2 : bodyTokens ++ processOutsideDart keepTests remainingTokens
           else processOutsideDart keepTests remainingTokens

-- No test found, process the current token
processOutsideDart keepTests (t:ts) =
    if keepTests
    then processOutsideDart keepTests ts
    else t : processOutsideDart keepTests ts

-- ------------------------------------------------------------------
