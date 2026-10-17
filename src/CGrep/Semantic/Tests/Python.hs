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

-- | Extracts a single Python decorator `@name[.sub][(...)]`
extractPythonDecorator :: [Token] -> Maybe ([Token], [Token])
extractPythonDecorator (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" && isTokenIdentifier t2 =
        let (pathTokens, rest) = collectDotPath ts
        in case rest of
            (t3:ts') | isTokenBracket t3 && tToken t3 == "(" ->
                let (insideTokens, remaining) = processInsideBrackets "(" ")" 1 ts'
                in Just (t1 : t2 : pathTokens ++ t3 : insideTokens, remaining)
            _ -> Just (t1 : t2 : pathTokens, rest)
  where
    collectDotPath (d:i:rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (more, final) = collectDotPath rest
            in (d : i : more, final)
    collectDotPath rest = ([], rest)
extractPythonDecorator _ = Nothing

-- | Checks if a decorator is a known Python test decorator (examining only the decorator
-- path before `(`, not arguments).
isPythonTestDecorator :: [Token] -> Bool
isPythonTestDecorator tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "(")) tokens
        idents = [tToken t | t <- headerTokens, isTokenIdentifier t]
    in case idents of
        (first:_) ->
            first == "pytest"
                || first == "unittest"
                || first == "patch"
                || first == "mock"
                || first == "fixture"
                || first == "given"
                || first == "parametrize"
                || first == "skip"
                || first == "skipIf"
                || first == "skipUnless"
                || first == "expectedFailure"
                || any (\i -> i == "fixture" || i == "parametrize") idents
        [] -> False

-- | Collects a contiguous stack of Python decorators (`@a @b ...`).
collectPythonDecorators :: [Token] -> ([[Token]], Bool, [Token])
collectPythonDecorators = go [] False
  where
    go acc isTest remaining =
        case extractPythonDecorator remaining of
            Just (dec, rest) ->
                let testDec = isPythonTestDecorator dec
                in go (acc ++ [dec]) (isTest || testDec) rest
            Nothing -> (acc, isTest, remaining)

-- | Checks if a Python `def` or `class` target immediately following decorators is a test construct.
isPythonTestDefOrClass :: [Token] -> Bool
isPythonTestDefOrClass (t1:t2:t3:_)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" &&
      isTokenKeyword t2 && tToken t2 == "def" &&
      isTokenIdentifier t3 && "test_" `T.isPrefixOf` tToken t3 = True
isPythonTestDefOrClass (t1:t2:_)
    | isTokenKeyword t1 && tToken t1 == "def" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2 = True
isPythonTestDefOrClass (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 && ("Test" `T.isPrefixOf` tToken t2 || hasUnittestBase ts) = True
isPythonTestDefOrClass _ = False

-- | Checks if a Python class inherits from `*TestCase` (e.g. `(unittest.TestCase)` or `(TestCase)`).
hasUnittestBase :: [Token] -> Bool
hasUnittestBase (t:ts)
    | isTokenBracket t && tToken t == "(" =
        let (baseTokens, _) = processInsideBrackets "(" ")" 1 ts
        in any (\b -> isTokenIdentifier b && ("TestCase" `T.isSuffixOf` tToken b)) baseTokens
hasUnittestBase _ = False

-- | Consumes a Python `def` (or `async def`) or `class` following decorators.
consumeDecoratedTarget :: [Token] -> ([Token], [Token])
consumeDecoratedTarget (t1:t2:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" &&
      isTokenKeyword t2 && tToken t2 == "def" =
        let (body, remaining) = collectUntilNextDefOrClass ts
        in (t1 : t2 : body, remaining)
consumeDecoratedTarget (t1:ts)
    | isTokenKeyword t1 && tToken t1 == "def" =
        let (body, remaining) = collectUntilNextDefOrClass ts
        in (t1 : body, remaining)
    | isTokenKeyword t1 && tToken t1 == "class" =
        let (body, remaining) = collectTestClassBody ts
        in (t1 : body, remaining)
consumeDecoratedTarget ts = ([], ts)

-- | (Python) Helper: Processes tokens *outside* a test function.
processOutsidePython :: Bool -> [Token] -> [Token]
processOutsidePython _ [] = [] -- End of stream

-- Pattern 1: One or more decorators (`@pytest.fixture`, `@my_dec def test_foo()`, etc.)
processOutsidePython keepTests tokens@(t1:_)
    | isTokenOperator t1 && tToken t1 == "@" =
        let (decs, hasTestDec, afterDecs) = collectPythonDecorators tokens
            flatDecs = concat decs
        in if not (null decs)
           then
               if hasTestDec || isPythonTestDefOrClass afterDecs
               then
                   let (targetTokens, remaining) = consumeDecoratedTarget afterDecs
                   in if keepTests
                      then flatDecs ++ targetTokens ++ processOutsidePython keepTests remaining
                      else processOutsidePython keepTests remaining
               else
                   if keepTests
                   then processOutsidePython keepTests afterDecs
                   else flatDecs ++ processOutsidePython keepTests afterDecs
           else
               let (t:ts) = tokens
               in if keepTests
                  then processOutsidePython keepTests ts
                  else t : processOutsidePython keepTests ts

-- Pattern 2: async def test_*
processOutsidePython keepTests (t1:t2:t3:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" &&
      isTokenKeyword t2 && tToken t2 == "def" &&
      isTokenIdentifier t3 && "test_" `T.isPrefixOf` tToken t3
    =
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : t3 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 3: def test_*
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "def" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 4: class Test* or class *(...TestCase...)
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      ("Test" `T.isPrefixOf` tToken t2 || hasUnittestBase ts)
    =
        let (testTokens, remainingTokens) = collectTestClassBody ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- No test found, process the current token
processOutsidePython keepTests (t:ts) =
    if keepTests
    then processOutsidePython keepTests ts
    else t : processOutsidePython keepTests ts

-- | Helper: Collect tokens until we find the next top-level `def`, `async def`, `class`, or `@` decorator.
collectUntilNextDefOrClass :: [Token] -> ([Token], [Token])
collectUntilNextDefOrClass [] = ([], [])
collectUntilNextDefOrClass (t:ts)
    | isTokenOperator t && tToken t == "@" =
        ([], t:ts)
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

-- | Helper: Collect test class body based on method signatures (`self` or `cls`),
-- including decorated methods inside the test class (`@staticmethod`, `@classmethod`, `@pytest.mark.*`, etc.).
collectTestClassBody :: [Token] -> ([Token], [Token])
collectTestClassBody [] = ([], [])
collectTestClassBody tokens@(t:ts)
    | isTokenKeyword t && tToken t == "class" = ([], tokens)
    | isTokenOperator t && tToken t == "@" =
        let (decs, _, afterDecs) = collectPythonDecorators tokens
            flatDecs = concat decs
        in if not (null decs) && isClassMethodOrStatic decs afterDecs
           then
               case afterDecs of
                   (d1:d2:rest) | (isTokenIdentifier d1 || isTokenKeyword d1) && tToken d1 == "async" && isTokenKeyword d2 && tToken d2 == "def" ->
                       let (methodTokens, remaining) = collectUntilNextDefOrClass rest
                           (restClass, finalRest) = collectTestClassBody remaining
                       in (flatDecs ++ d1 : d2 : methodTokens ++ restClass, finalRest)
                   (d1:rest) | isTokenKeyword d1 && tToken d1 == "def" ->
                       let (methodTokens, remaining) = collectUntilNextDefOrClass rest
                           (restClass, finalRest) = collectTestClassBody remaining
                       in (flatDecs ++ d1 : methodTokens ++ restClass, finalRest)
                   _ -> ([], tokens)
           else ([], tokens)
    | isTokenKeyword t && tToken t == "def" =
        if hasSelfOrCls ts
        then
            let (methodTokens, remaining) = collectUntilNextDefOrClass ts
                (restClass, finalRest) = collectTestClassBody remaining
            in (t : methodTokens ++ restClass, finalRest)
        else ([], tokens) -- Not a method with self/cls, so it's a new top-level def
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:ts') | isTokenKeyword t2 && tToken t2 == "def" ->
                if hasSelfOrCls ts'
                then
                    let (methodTokens, remaining) = collectUntilNextDefOrClass ts'
                        (restClass, finalRest) = collectTestClassBody remaining
                    in (t : t2 : methodTokens ++ restClass, finalRest)
                else ([], tokens)
            _ ->
                let (collected, remaining) = collectTestClassBody ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectTestClassBody ts
        in (t : collected, remaining)
  where
    hasSelfOrCls (name:openParen:firstArg:_) =
        isTokenIdentifier name &&
        isTokenBracket openParen && tToken openParen == "(" &&
        isTokenIdentifier firstArg && (tToken firstArg == "self" || tToken firstArg == "cls")
    hasSelfOrCls _ = False

    isClassMethodOrStatic decs afterDecs =
        let hasStatic = any (any (\tok -> isTokenIdentifier tok && tToken tok == "staticmethod")) decs
        in case afterDecs of
            (d1:d2:rest) | (isTokenIdentifier d1 || isTokenKeyword d1) && tToken d1 == "async" && isTokenKeyword d2 && tToken d2 == "def" ->
                hasStatic || hasSelfOrCls rest
            (d1:rest) | isTokenKeyword d1 && tToken d1 == "def" ->
                hasStatic || hasSelfOrCls rest
            _ -> False
