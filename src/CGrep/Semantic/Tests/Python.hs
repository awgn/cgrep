{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Python () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'Python where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePython keepTests tokens

-- | (Python) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. Functions starting with "test_" (pytest, unittest convention)
--   2. Classes starting with "Test" (pytest convention)
--   3. Decorators @pytest, @unittest, @pytest.mark.* (common test decorators)
--
processOutsidePython :: Bool -> [Token] -> [Token]
processOutsidePython _ [] = [] -- End of stream

-- Pattern 1: async def test_*
processOutsidePython keepTests (t1:t2:t3:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" &&
      isTokenKeyword t2 && tToken t2 == "def" &&
      isTokenIdentifier t3 && "test_" `T.isPrefixOf` tToken t3
    =
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : t3 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 2: @pytest or @unittest decorator
processOutsidePython keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && (tToken t2 == "pytest" || tToken t2 == "unittest" || tToken t2 == "patch" || tToken t2 == "mock")
    =
        let (decoratorTokens, afterDecorator) = collectDecoratorAndFunction ts
        in if keepTests
           then t1 : t2 : decoratorTokens ++ processOutsidePython keepTests afterDecorator
           else processOutsidePython keepTests afterDecorator

-- Pattern 3: def test_*
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "def" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 4: class Test* or class *(unittest.TestCase)
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      ("Test" `T.isPrefixOf` tToken t2 || hasUnittestBase ts)
    =
        let (testTokens, remainingTokens) = collectTestClassBody ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens
  where
    hasUnittestBase (t3:t4:t5:t6:t7:_) =
        isTokenBracket t3 && tToken t3 == "(" &&
        isTokenIdentifier t4 && tToken t4 == "unittest" &&
        isTokenOperator t5 && tToken t5 == "." &&
        isTokenIdentifier t6 && tToken t6 == "TestCase" &&
        isTokenBracket t7 && tToken t7 == ")"
    hasUnittestBase _ = False

-- No test found, process the current token
processOutsidePython keepTests (t:ts) =
    if keepTests
    then processOutsidePython keepTests ts
    else t : processOutsidePython keepTests ts

-- | Helper: Collect decorator tokens and the following function/class definition.
collectDecoratorAndFunction :: [Token] -> ([Token], [Token])
collectDecoratorAndFunction ts =
    let (beforeDef, fromDef) = collectUntilDefOrClass ts
    in case fromDef of
        [] -> (beforeDef, [])
        (t1:t2:rest) | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" && isTokenKeyword t2 && tToken t2 == "def" ->
            let (bodyTokens, remaining) = collectUntilNextDefOrClass rest
            in (beforeDef ++ [t1, t2] ++ bodyTokens, remaining)
        (t:rest) ->
            let (bodyTokens, remaining) = collectUntilNextDefOrClass rest
            in (beforeDef ++ [t] ++ bodyTokens, remaining)

-- | Helper: Collect tokens until we find 'def' or 'class' keyword.
collectUntilDefOrClass :: [Token] -> ([Token], [Token])
collectUntilDefOrClass [] = ([], [])
collectUntilDefOrClass (t:ts)
    | isTokenKeyword t && (tToken t == "def" || tToken t == "class") =
        ([], t:ts)
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:_) | isTokenKeyword t2 && tToken t2 == "def" -> ([], t:ts)
            _ ->
                let (collected, remaining) = collectUntilDefOrClass ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilDefOrClass ts
        in (t : collected, remaining)

-- | Helper: Collect tokens until we find 'def' or 'class' keyword.
collectUntilNextDefOrClass :: [Token] -> ([Token], [Token])
collectUntilNextDefOrClass [] = ([], [])
collectUntilNextDefOrClass (t:ts)
    | isTokenKeyword t && (tToken t == "def" || tToken t == "class") =
        ([], t:ts)
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:_) | isTokenKeyword t2 && tToken t2 == "def" -> ([], t:ts)
            _ ->
                let (collected, remaining) = collectUntilNextDefOrClass ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilNextDefOrClass ts
        in (t : collected, remaining)

-- | Helper: Collect test class body based on method signatures (self/cls)
collectTestClassBody :: [Token] -> ([Token], [Token])
collectTestClassBody [] = ([], [])
collectTestClassBody (t:ts)
    | isTokenKeyword t && tToken t == "class" = ([], t:ts)
    | isTokenKeyword t && tToken t == "def" =
        case ts of
            (name:openParen:firstArg:_)
                | isTokenIdentifier name &&
                  isTokenBracket openParen && tToken openParen == "(" &&
                  isTokenIdentifier firstArg && (tToken firstArg == "self" || tToken firstArg == "cls") ->
                    let (methodTokens, remaining) = collectUntilNextDefOrClass ts
                        (restClass, finalRest) = collectTestClassBody remaining
                    in (t : methodTokens ++ restClass, finalRest)
            _ -> ([], t:ts) -- Not a method with self/cls, so it's a new top-level def
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:ts') | isTokenKeyword t2 && tToken t2 == "def" ->
                case ts' of
                    (name:openParen:firstArg:_)
                        | isTokenIdentifier name &&
                          isTokenBracket openParen && tToken openParen == "(" &&
                          isTokenIdentifier firstArg && (tToken firstArg == "self" || tToken firstArg == "cls") ->
                            let (methodTokens, remaining) = collectUntilNextDefOrClass ts
                                (restClass, finalRest) = collectTestClassBody remaining
                            in (t : methodTokens ++ restClass, finalRest)
                    _ -> ([], t:ts)
            _ ->
                let (collected, remaining) = collectTestClassBody ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectTestClassBody ts
        in (t : collected, remaining)

-- ------------------------------------------------------------------
