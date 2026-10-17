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
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)

instance LanguageTestFilter 'Dart where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideDart keepTests False tokens

-- | Dart `package:test` / `package:flutter_test` block functions
isDartTestFunction :: Text -> Bool
isDartTestFunction name =
    name
        `elem` ([ "test"
                , "group"
                , "testWidgets"
                , "setUp"
                , "tearDown"
                , "setUpAll"
                , "tearDownAll"
                ] :: [Text])

-- | Check whether the tokens inside a call's parentheses `(...)` contain a Dart
-- callback (`{` for block callback `() { ... }` or `=>` for expression callback `() => ...`).
-- This prevents regular method declarations like `bool test(String s) { ... }` or
-- normal function calls `test(x)` from having their parameter list mistaken for a test.
hasDartCallbackTokens :: [Token] -> Bool
hasDartCallbackTokens [] = False
hasDartCallbackTokens (t1 : t2 : ts)
    | tToken t1 == "=" && tToken t2 == ">" = True
    | otherwise = isDartCallbackSingle t1 || hasDartCallbackTokens (t2 : ts)
hasDartCallbackTokens [t1] = isDartCallbackSingle t1

isDartCallbackSingle :: Token -> Bool
isDartCallbackSingle t =
    (isTokenBracket t && tToken t == "{")
        || tToken t == "=>"

-- | (Dart) Helper: Processes tokens *outside* a test block.
-- `afterDot` is `True` when the immediately preceding token was `.` (or `?.` / `..`),
-- preventing `calc.test(...)` or `match?.group(...)` from being mistaken for a test block.
processOutsideDart :: Bool -> Bool -> [Token] -> [Token]
processOutsideDart _ _ [] = []
processOutsideDart keepTests False (t1 : t2 : ts)
    | isTokenIdentifier t1 && isDartTestFunction (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
    , hasDartCallbackTokens bodyTokens =
        if keepTests
            then t1 : t2 : bodyTokens ++ processOutsideDart keepTests False remainingTokens
            else processOutsideDart keepTests False remainingTokens
processOutsideDart keepTests _ (t : ts) =
    let isDot = isTokenOperator t && (tToken t == "." || tToken t == "?." || tToken t == "..")
     in if keepTests
            then processOutsideDart keepTests isDot ts
            else t : processOutsideDart keepTests isDot ts
