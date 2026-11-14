{-# LANGUAGE BangPatterns #-}
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
    isTokenKeyword,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI

instance LanguageTestFilter 'Csharp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideCsharp keepTests False tokens

-- | True if `t` is the first non-whitespace token on its physical line.
isFirstTokenOnLine :: Token -> Bool
isFirstTokenOnLine t =
    case tToken t of
        TI.Text arr off _ -> scanBack arr (off - 1)
  where
    scanBack !arr !i
        | i < 0 = True
        | otherwise =
            let !b = A.unsafeIndex arr i
             in if b == 10
                    then True
                    else (b == 32 || b == 9 || b == 13) && scanBack arr (i - 1)

-- | True if no newline (`\n`) separates `t1` and `t2`.
onSameLine :: Token -> Token -> Bool
onSameLine t1 t2 =
    case (tToken t1, tToken t2) of
        (TI.Text arr off1 len1, TI.Text _ off2 _) ->
            scanNoNewline arr (off1 + len1) off2
  where
    scanNoNewline !arr !p !end
        | p >= end = True
        | A.unsafeIndex arr p == 10 = False
        | otherwise = scanNoNewline arr (p + 1) end

consumeSameLine :: Token -> [Token] -> ([Token], [Token])
consumeSameLine prev (t : ts)
    | onSameLine prev t =
        let (more, rest) = consumeSameLine t ts
         in (t : more, rest)
consumeSameLine _ ts = ([], ts)

-- | Extracts a single C# attribute block `[...]`, including nested brackets/parens.
extractCsharpAttribute :: [Token] -> Maybe ([Token], [Token])
extractCsharpAttribute (t1 : ts)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
         in Just (t1 : insideTokens, remaining)
extractCsharpAttribute _ = Nothing

-- | Extract the interior tokens of `[...]` (dropping outer `[` and `]`) and any
-- optional C# attribute target specifier (`assembly:`, `method:`, `return:`, etc.),
-- taking care not to confuse `global::Foo` with a target specifier.
unwrapAttributeSection :: [Token] -> (Maybe Text, [Token])
unwrapAttributeSection attrTokens =
    let inner = case attrTokens of
            (_ : xs) -> case reverse xs of
                (lastTok : revMid)
                    | isTokenBracket lastTok && tToken lastTok == "]" -> reverse revMid
                _ -> xs
            [] -> []
     in extractAttrTarget inner
  where
    extractAttrTarget :: [Token] -> (Maybe Text, [Token])
    extractAttrTarget (tTarget : tColon : rest)
        | (isTokenIdentifier tTarget || isTokenKeyword tTarget)
        , isTokenOperator tColon && tToken tColon == ":"
        , not (startsAdjacentColon tColon rest) =
            (Just (tToken tTarget), rest)
    extractAttrTarget ts = (Nothing, ts)

    startsAdjacentColon :: Token -> [Token] -> Bool
    startsAdjacentColon prevColon (c : _) =
        isTokenOperator c && tToken c == ":" && areAdjacentTokens prevColon c
    startsAdjacentColon _ [] = False

-- | Compilation-unit attributes (`[assembly: ...]`, `[module: ...]`) never attach
-- to a following class or method declaration.
isAssemblyOrModuleAttr :: [Token] -> Bool
isAssemblyOrModuleAttr attrTokens =
    case fst (unwrapAttributeSection attrTokens) of
        Just target -> target == "assembly" || target == "module"
        Nothing -> False

-- | Split an attribute section's inner token stream at top-level commas
-- (outside `(...)`, `[...]`, `{...}`, and generic `<...>`).
splitTopLevelCommas :: [Token] -> [[Token]]
splitTopLevelCommas = go 0 0 []
  where
    go :: Int -> Int -> [Token] -> [Token] -> [[Token]]
    go _ _ cur [] = [reverse cur | not (null cur)]
    go !pDepth !aDepth cur (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (pDepth + 1) aDepth (t : cur) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (pDepth - 1)) aDepth (t : cur) ts
        | pDepth == 0 && isTokenOperator t && tToken t == "<" =
            go pDepth (aDepth + 1) (t : cur) ts
        | pDepth == 0 && isTokenOperator t && tToken t == ">" =
            go pDepth (max 0 (aDepth - 1)) (t : cur) ts
        | pDepth == 0 && isTokenOperator t && tToken t == ">>" =
            go pDepth (max 0 (aDepth - 2)) (t : cur) ts
        | pDepth == 0 && aDepth == 0 && isTokenOperator t && tToken t == "," =
            let restItems = go 0 0 [] ts
             in if null cur then restItems else reverse cur : restItems
        | otherwise =
            go pDepth aDepth (t : cur) ts

-- | Strip parenthesized arguments `(...)` and C# 11 generic type arguments `<...>`
-- from a single attribute item so identifiers inside `[Obsolete(nameof(Test))]`
-- or `[Validator<Test>]` are never mistaken for the attribute's own name.
stripAttrArgsAndGenerics :: [Token] -> [Token]
stripAttrArgsAndGenerics [] = []
stripAttrArgsAndGenerics (t : ts)
    | isTokenBracket t && tToken t == "(" =
        let (_, remaining) = processInsideBrackets "(" ")" 1 ts
         in stripAttrArgsAndGenerics remaining
    | isTokenOperator t && tToken t == "<" =
        let remaining = skipAngleBrackets 1 ts
         in stripAttrArgsAndGenerics remaining
    | otherwise = t : stripAttrArgsAndGenerics ts
  where
    skipAngleBrackets :: Int -> [Token] -> [Token]
    skipAngleBrackets 0 rest = rest
    skipAngleBrackets _ [] = []
    skipAngleBrackets !depth (x : xs)
        | isTokenOperator x && tToken x == "<" = skipAngleBrackets (depth + 1) xs
        | isTokenOperator x && tToken x == ">" = skipAngleBrackets (depth - 1) xs
        | isTokenOperator x && tToken x == ">>" = skipAngleBrackets (max 0 (depth - 2)) xs
        | otherwise = skipAngleBrackets depth xs

-- | True if any comma-separated attribute inside `[...]` is a known C# test-framework
-- attribute (NUnit, xUnit, MSTest, SpecFlow/Reqnroll) targeting a method or type.
isCsharpTestAttribute :: [Token] -> Bool
isCsharpTestAttribute attrTokens =
    let (mTarget, bodyTokens) = unwrapAttributeSection attrTokens
        targetOk = case mTarget of
            Nothing -> True
            Just target -> target `elem` (["method", "type", "class"] :: [Text])
     in targetOk && any isSingleTestAttr (splitTopLevelCommas bodyTokens)
  where
    isSingleTestAttr :: [Token] -> Bool
    isSingleTestAttr itemTokens =
        let nameTokens = filter isTokenIdentifier (stripAttrArgsAndGenerics itemTokens)
         in case reverse nameTokens of
                (lastIdent : _) -> isCsharpTestAttrName (dropEndAttribute (tToken lastIdent))
                [] -> False

    isCsharpTestAttrName :: Text -> Bool
    isCsharpTestAttrName n =
        n
            `elem` ([ -- NUnit
                      "Test"
                    , "TestFixture"
                    , "TestFixtureSource"
                    , "TestCase"
                    , "TestCaseSource"
                    , "SetUp"
                    , "TearDown"
                    , "OneTimeSetUp"
                    , "OneTimeTearDown"
                    , "SetUpFixture"
                    , "Theory"
                    , -- xUnit (including SkippableFact & StaFact extensions)
                      "Fact"
                    , "InlineData"
                    , "MemberData"
                    , "ClassData"
                    , "MatrixTheoryData"
                    , "SkippableFact"
                    , "SkippableTheory"
                    , "StaFact"
                    , "StaTheory"
                    , "WpfFact"
                    , "WpfTheory"
                    , "UIFact"
                    , "UITheory"
                    , -- MSTest
                      "TestClass"
                    , "STATestClass"
                    , "TestMethod"
                    , "STATestMethod"
                    , "UITestMethod"
                    , "DataTestMethod"
                    , "DataRow"
                    , "DynamicData"
                    , "TestInitialize"
                    , "TestCleanup"
                    , "ClassInitialize"
                    , "ClassCleanup"
                    , "AssemblyInitialize"
                    , "AssemblyCleanup"
                    , -- SpecFlow / Reqnroll (BDD)
                      "Given"
                    , "When"
                    , "Then"
                    , "StepDefinition"
                    , "BeforeScenario"
                    , "AfterScenario"
                    , "BeforeScenarioBlock"
                    , "AfterScenarioBlock"
                    , "BeforeFeature"
                    , "AfterFeature"
                    , "BeforeTestRun"
                    , "AfterTestRun"
                    , "BeforeStep"
                    , "AfterStep"
                    ] :: [Text])

dropEndAttribute :: Text -> Text
dropEndAttribute txt
    | "Attribute" `T.isSuffixOf` txt && T.length txt > 9 = T.dropEnd 9 txt
    | otherwise = txt

-- | Collects a contiguous run of declaration-attached C# attributes at the head
-- of the stream, stopping before `[assembly: ...]` or `[module: ...]`.
collectCsharpAttributes :: [Token] -> ([[Token]], [Token])
collectCsharpAttributes ts = case extractCsharpAttribute ts of
    Just (attr, rest)
        | not (isAssemblyOrModuleAttr attr) ->
            let (more, final) = collectCsharpAttributes rest
             in (attr : more, final)
    _ -> ([], ts)

data CsharpBodyKind
    = BracedBody [Token] [Token] -- (tokens up to and including `{`, tokens after `{`)
    | ExprOrAbstractBody [Token] [Token] -- (tokens up to and including `;`, tokens after `;`)

-- | Match C# `=>` in both `--strict` (`"=>"`) and default (`"="` adjacent to `">"`) modes.
matchFatArrow :: [Token] -> Maybe ([Token], [Token])
matchFatArrow (t : ts)
    | isTokenOperator t && tToken t == "=>" = Just ([t], ts)
matchFatArrow (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "="
    , isTokenOperator t2 && tToken t2 == ">"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], ts)
matchFatArrow _ = Nothing

-- | Find the body of an attributed C# declaration within `n` signature tokens,
-- tracking `(...)` and `[...]` depth so parameter defaults or array types are ignored.
-- Handles:
--   * Braced bodies `{ ... }`
--   * Abstract / extern declarations `;`
--   * Expression-bodied members `=> expr;`, balancing inner `{ ... }` (e.g. object
--     initializers `new Order { Id = 1 }.Verify()` or lambda blocks) up to top-level `;`.
findCsharpDeclBody :: Int -> [Token] -> Maybe CsharpBodyKind
findCsharpDeclBody = goSig 0 []
  where
    goSig :: Int -> [Token] -> Int -> [Token] -> Maybe CsharpBodyKind
    goSig _ _ 0 _ = Nothing
    goSig _ _ _ [] = Nothing
    goSig !depth acc !n tokens@(t : ts)
        | depth == 0
        , Just (arrowToks, afterArrow) <- matchFatArrow tokens =
            collectExprBody 0 (reverse arrowToks ++ acc) afterArrow
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            goSig (depth + 1) (t : acc) (n - 1) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            goSig (max 0 (depth - 1)) (t : acc) (n - 1) ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just (BracedBody (reverse (t : acc)) ts)
        | depth == 0 && isTokenOperator t && tToken t == ";" =
            Just (ExprOrAbstractBody (reverse (t : acc)) ts)
        | otherwise =
            goSig depth (t : acc) (n - 1) ts

    collectExprBody :: Int -> [Token] -> [Token] -> Maybe CsharpBodyKind
    collectExprBody _ _ [] = Nothing
    collectExprBody !exprDepth acc (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            collectExprBody (exprDepth + 1) (t : acc) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            collectExprBody (max 0 (exprDepth - 1)) (t : acc) ts
        | exprDepth == 0 && isTokenOperator t && tToken t == ";" =
            Just (ExprOrAbstractBody (reverse (t : acc)) ts)
        | otherwise =
            collectExprBody exprDepth (t : acc) ts

-- | `isIndexerPrefix` is `True` only when the next token on the SAME physical line
-- immediately follows an identifier, `)`, or `]`, preventing array indexing like
-- `arr[Test]` from being mistaken for an attribute `[Test]` without letting
-- `#region Name` / `#nullable enable` hide attributes on subsequent lines.
processOutsideCsharp :: Bool -> Bool -> [Token] -> [Token]
processOutsideCsharp _ _ [] = []
processOutsideCsharp keepTests _ (t : ts)
    | isTokenOperator t && tToken t == "#" && isFirstTokenOnLine t =
        let (lineToks, remaining) = consumeSameLine t ts
            restOut = processOutsideCsharp keepTests False remaining
         in if keepTests
                then restOut
                else (t : lineToks) ++ restOut
processOutsideCsharp keepTests False tokens@(t1 : _)
    | isTokenBracket t1 && tToken t1 == "["
    , Just (firstAttr, afterFirst) <- extractCsharpAttribute tokens
    , isAssemblyOrModuleAttr firstAttr =
        let restOut = processOutsideCsharp keepTests False afterFirst
         in if keepTests
                then restOut
                else firstAttr ++ restOut
    | isTokenBracket t1 && tToken t1 == "[" =
        let (attrs, afterAttrs) = collectCsharpAttributes tokens
            flatAttrs = concat attrs
         in if not (null attrs) && any isCsharpTestAttribute attrs
                then case findCsharpDeclBody 500 afterAttrs of
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
    let sameLineNext = case ts of
            (nextTok : _) -> onSameLine t nextTok
            [] -> False
        isIndexerPrefix =
            sameLineNext
                && ( isTokenIdentifier t
                        || (isTokenBracket t && (tToken t == ")" || tToken t == "]"))
                   )
     in if keepTests
            then processOutsideCsharp keepTests isIndexerPrefix ts
            else t : processOutsideCsharp keepTests isIndexerPrefix ts
