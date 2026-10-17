{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Csharp () where

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
import qualified Data.Text as T

instance LanguageTestFilter 'Csharp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideCsharp keepTests False tokens

-- | Extracts a single C# attribute block `[...]`, including nested brackets/parens.
extractCsharpAttribute :: [Token] -> Maybe ([Token], [Token])
extractCsharpAttribute (t1 : ts)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
         in Just (t1 : insideTokens, remaining)
extractCsharpAttribute _ = Nothing

-- | Strip parenthesized arguments `(...)` from an attribute token stream so that
-- identifiers inside attribute arguments (e.g. `[Obsolete(nameof(Test))]`) are
-- NEVER mistaken for the attribute's own name!
stripAttrArguments :: [Token] -> [Token]
stripAttrArguments [] = []
stripAttrArguments (t : ts)
    | isTokenBracket t && tToken t == "(" =
        let (_, remaining) = processInsideBrackets "(" ")" 1 ts
         in stripAttrArguments remaining
    | otherwise = t : stripAttrArguments ts

-- | True if any attribute name (outside `(...)` arguments) inside `[...]` is a
-- known C# test-framework attribute (NUnit, xUnit, MSTest).
isCsharpTestAttribute :: [Token] -> Bool
isCsharpTestAttribute attrTokens =
    let headerTokens = stripAttrArguments attrTokens
     in any (\t -> isTokenIdentifier t && isCsharpTestAttrName (dropEndAttribute (tToken t))) headerTokens
  where
    isCsharpTestAttrName :: Text -> Bool
    isCsharpTestAttrName n =
        n
            `elem` ([ -- NUnit
                      "Test"
                    , "TestFixture"
                    , "TestCase"
                    , "TestCaseSource"
                    , "SetUp"
                    , "TearDown"
                    , "OneTimeSetUp"
                    , "OneTimeTearDown"
                    , "SetUpFixture"
                    , "Theory"
                    , -- xUnit
                      "Fact"
                    , "InlineData"
                    , "MemberData"
                    , "ClassData"
                    , -- MSTest
                      "TestClass"
                    , "TestMethod"
                    , "DataTestMethod"
                    , "DataRow"
                    , "DynamicData"
                    , "TestInitialize"
                    , "TestCleanup"
                    , "ClassInitialize"
                    , "ClassCleanup"
                    , "AssemblyInitialize"
                    , "AssemblyCleanup"
                    ] :: [Text])

dropEndAttribute :: Text -> Text
dropEndAttribute txt
    | "Attribute" `T.isSuffixOf` txt && T.length txt > 9 = T.dropEnd 9 txt
    | otherwise = txt

-- | Collects a contiguous run of C# attributes at the head of the stream.
collectCsharpAttributes :: [Token] -> ([[Token]], [Token])
collectCsharpAttributes ts = case extractCsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectCsharpAttributes rest
         in (attr : more, final)
    Nothing -> ([], ts)

data CsharpBodyKind
    = BracedBody [Token] [Token] -- (tokens up to and including `{`, tokens after `{`)
    | ExprOrAbstractBody [Token] [Token] -- (tokens up to and including `;`, tokens after `;`)

-- | Find the body of an attributed C# declaration within `n` tokens, tracking
-- `(...)` and `[...]` depth so parameter default values or array types are ignored.
-- Stops at top-level `{` (braced method/class body) OR top-level `;` (abstract/extern
-- declaration or expression-bodied member `=> ...;`).
findCsharpDeclBody :: Int -> [Token] -> Maybe CsharpBodyKind
findCsharpDeclBody = go 0 []
  where
    go :: Int -> [Token] -> Int -> [Token] -> Maybe CsharpBodyKind
    go _ _ 0 _ = Nothing
    go _ _ _ [] = Nothing
    go depth acc n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            go (depth + 1) (t : acc) (n - 1) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            go (max 0 (depth - 1)) (t : acc) (n - 1) ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just (BracedBody (reverse (t : acc)) ts)
        | depth == 0 && isTokenOperator t && tToken t == ";" =
            Just (ExprOrAbstractBody (reverse (t : acc)) ts)
        | otherwise =
            go depth (t : acc) (n - 1) ts

-- | `canStartAttr` is `False` right after an identifier, number, `)`, or `]`,
-- preventing array indexing like `arr[Test]` from being mistaken for an attribute `[Test]`.
processOutsideCsharp :: Bool -> Bool -> [Token] -> [Token]
processOutsideCsharp _ _ [] = []
processOutsideCsharp keepTests False tokens@(t1 : _)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (attrs, afterAttrs) = collectCsharpAttributes tokens
            flatAttrs = concat attrs
         in if not (null attrs) && any isCsharpTestAttribute attrs
                then case findCsharpDeclBody 200 afterAttrs of
                    Nothing ->
                        if keepTests
                            then processOutsideCsharp keepTests False afterAttrs
                            else flatAttrs ++ processOutsideCsharp keepTests False afterAttrs
                    Just (BracedBody signatureTokens tokensAfterBrace) ->
                        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                         in if keepTests
                                then flatAttrs ++ signatureTokens ++ bodyTokens ++ processOutsideCsharp keepTests False remainingTokens
                                else processOutsideCsharp keepTests False remainingTokens
                    Just (ExprOrAbstractBody exprTokens remainingTokens) ->
                        if keepTests
                            then flatAttrs ++ exprTokens ++ processOutsideCsharp keepTests False remainingTokens
                            else processOutsideCsharp keepTests False remainingTokens
                else
                    if keepTests
                        then processOutsideCsharp keepTests False afterAttrs
                        else flatAttrs ++ processOutsideCsharp keepTests False afterAttrs
processOutsideCsharp keepTests _ (t : ts) =
    let isIndexerPrefix =
            isTokenIdentifier t
                || (isTokenBracket t && (tToken t == ")" || tToken t == "]"))
     in if keepTests
            then processOutsideCsharp keepTests isIndexerPrefix ts
            else t : processOutsideCsharp keepTests isIndexerPrefix ts
