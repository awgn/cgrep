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
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Csharp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideCsharp keepTests tokens

-- | (C#) Helper: Processes tokens *outside* a test method.
-- Recognizes C# attributes from multiple test frameworks:
--   NUnit:  [Test], [TestFixture], [TestCase(...)], [TestCaseSource(...)],
--           [SetUp], [TearDown], [OneTimeSetUp], [OneTimeTearDown]
--   xUnit:  [Fact], [Theory], [InlineData(...)], [MemberData(...)], [ClassData(...)]
--   MSTest: [TestClass], [TestMethod], [DataTestMethod], [DataRow(...)],
--           [TestInitialize], [TestCleanup], [ClassInitialize], [ClassCleanup],
--           [AssemblyInitialize], [AssemblyCleanup]
--
-- Handles:
--   - Parameterized attributes, e.g. [Fact(Skip = "reason")], [TestMethod("Name")]
--   - Fully-qualified attributes, e.g. [NUnit.Framework.Test]
--   - Stacked attributes, e.g. [Theory] [InlineData(1,2)] [InlineData(3,4)]

-- | Extracts a single C# attribute block `[...]`, including nested brackets/parens.
extractCsharpAttribute :: [Token] -> Maybe ([Token], [Token])
extractCsharpAttribute (t1:ts)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : insideTokens, remaining)
extractCsharpAttribute _ = Nothing

-- | True if any identifier inside the attribute is a known C# test-framework attribute.
isCsharpTestAttribute :: [Token] -> Bool
isCsharpTestAttribute = any (\t -> isTokenIdentifier t && isCsharpTestAttrName (tToken t))
  where
    isCsharpTestAttrName n =
        -- NUnit
        n == "Test" || n == "TestFixture" ||
        n == "TestCase" || n == "TestCaseSource" ||
        n == "SetUp" || n == "TearDown" ||
        n == "OneTimeSetUp" || n == "OneTimeTearDown" ||
        -- xUnit
        n == "Fact" || n == "Theory" ||
        n == "InlineData" || n == "MemberData" || n == "ClassData" ||
        -- MSTest
        n == "TestClass" || n == "TestMethod" || n == "DataTestMethod" ||
        n == "DataRow" ||
        n == "TestInitialize" || n == "TestCleanup" ||
        n == "ClassInitialize" || n == "ClassCleanup" ||
        n == "AssemblyInitialize" || n == "AssemblyCleanup"

-- | Collects a contiguous run of C# attributes at the head of the stream.
collectCsharpAttributes :: [Token] -> ([[Token]], [Token])
collectCsharpAttributes ts = case extractCsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectCsharpAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

processOutsideCsharp :: Bool -> [Token] -> [Token]
processOutsideCsharp _ [] = [] -- End of stream

-- Pattern: one or more contiguous attributes, at least one of which is a test attribute.
processOutsideCsharp keepTests tokens@(t1:_)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (attrs, afterAttrs) = collectCsharpAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isCsharpTestAttribute attrs
           then
               -- Found at least one test attribute in the stack.
               -- Skip any surrounding modifiers/signature and consume the following braced body.
               case findOpeningBraceBounded 200 afterAttrs of
                   Nothing ->
                       -- No brace found within lookahead: drop attributes gracefully.
                       if keepTests
                       then processOutsideCsharp keepTests afterAttrs
                       else flatAttrs ++ processOutsideCsharp keepTests afterAttrs
                   Just (signatureTokens, tokensAfterBrace) ->
                       let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                       in if keepTests
                          then flatAttrs ++ signatureTokens ++ bodyTokens ++ processOutsideCsharp keepTests remainingTokens
                          else processOutsideCsharp keepTests remainingTokens
           else
               -- Attributes are present but none are test-related: emit them as-is and continue.
               if keepTests
               then processOutsideCsharp keepTests afterAttrs
               else flatAttrs ++ processOutsideCsharp keepTests afterAttrs

-- No attribute / no test: pass through
processOutsideCsharp keepTests (t:ts) =
    if keepTests
    then processOutsideCsharp keepTests ts
    else t : processOutsideCsharp keepTests ts

-- ------------------------------------------------------------------
