{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Fsharp () where

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
instance LanguageTestFilter 'Fsharp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideFsharp keepTests tokens

-- | (F#) Helper: Processes tokens *outside* a test function.
-- Recognizes F# attributes from multiple test frameworks:
--   NUnit:   [<Test>], [<TestFixture>], [<TestCase(...)>], [<TestCaseSource(...)>],
--            [<SetUp>], [<TearDown>], [<OneTimeSetUp>], [<OneTimeTearDown>], [<Category(...)>]
--   xUnit:   [<Fact>], [<Theory>], [<InlineData(...)>], [<MemberData(...)>], [<ClassData(...)>]
--   FsCheck: [<Property>]
--   Expecto: testCase, testCaseAsync, testList, test, ftest, ptest, ftestCase, ptestCase, testProperty
--
-- Notes on F# tokenization:
--   In --strict mode, [< and >] are single operator tokens.
--   In non-strict mode, [< splits into `[` (bracket) + `<` (op), and >] splits into `>` (op) + `]` (bracket).
-- Handles parameterized, fully-qualified, and stacked attributes.
--
-- F# has no braces for function bodies: we consume tokens until the next
-- top-level 'let', 'module', 'type', 'namespace', 'open', or another attribute start.

-- | True if the stream starts with an F# attribute opener: `[<` (single op token) or `[` + `<` (split).
isFsharpAttrOpen :: [Token] -> Bool
isFsharpAttrOpen (t1:t2:_)
    | isTokenOperator t1 && tToken t1 == "[<" = True
    | isTokenBracket  t1 && tToken t1 == "["
      && isTokenOperator t2 && tToken t2 == "<" = True
isFsharpAttrOpen (t1:_)
    | isTokenOperator t1 && tToken t1 == "[<" = True
isFsharpAttrOpen _ = False

-- | True if the given 2 tokens form an F# attribute closer: `>]` or `>` + `]`.
isFsharpAttrClose2 :: Token -> Token -> Bool
isFsharpAttrClose2 a b =
    isTokenOperator a && tToken a == ">" &&
    isTokenBracket  b && tToken b == "]"

isFsharpAttrClose1 :: Token -> Bool
isFsharpAttrClose1 t = isTokenOperator t && tToken t == ">]"

-- | Extracts a single F# attribute block `[< ... >]`, including nested parens.
-- Returns (attributeTokens, tokensAfterAttribute).
extractFsharpAttribute :: [Token] -> Maybe ([Token], [Token])
-- Case A: opener is single token "[<"
extractFsharpAttribute (t1:ts)
    | isTokenOperator t1 && tToken t1 == "[<" =
        let (inside, rest) = consumeFsharpAttrBody ts
        in case rest of
             (c:rs) | isFsharpAttrClose1 c -> Just (t1 : inside ++ [c], rs)
             (c1:c2:rs) | isFsharpAttrClose2 c1 c2 -> Just (t1 : inside ++ [c1, c2], rs)
             _ -> Nothing
-- Case B: opener is two tokens "[" + "<"
extractFsharpAttribute (t1:t2:ts)
    | isTokenBracket t1 && tToken t1 == "[" &&
      isTokenOperator t2 && tToken t2 == "<" =
        let (inside, rest) = consumeFsharpAttrBody ts
        in case rest of
             (c:rs) | isFsharpAttrClose1 c -> Just (t1 : t2 : inside ++ [c], rs)
             (c1:c2:rs) | isFsharpAttrClose2 c1 c2 -> Just (t1 : t2 : inside ++ [c1, c2], rs)
             _ -> Nothing
extractFsharpAttribute _ = Nothing

-- | Consumes the body of an F# attribute, balancing parentheses,
-- stopping just before the `>]` (or `>` + `]`) closer.
consumeFsharpAttrBody :: [Token] -> ([Token], [Token])
consumeFsharpAttrBody [] = ([], [])
consumeFsharpAttrBody (t:rest)
    | isFsharpAttrClose1 t = ([], t:rest)
consumeFsharpAttrBody (t1:t2:rest)
    | isFsharpAttrClose2 t1 t2 = ([], t1:t2:rest)
consumeFsharpAttrBody (t:rest)
    | isTokenBracket t && tToken t == "(" =
        let (paren, afterParen) = processInsideBrackets "(" ")" 1 rest
            (more, final) = consumeFsharpAttrBody afterParen
        in (t : paren ++ more, final)
    | otherwise =
        let (more, final) = consumeFsharpAttrBody rest
        in (t : more, final)

-- | True if any identifier inside the attribute is a known F# test-framework attribute.
isFsharpTestAttribute :: [Token] -> Bool
isFsharpTestAttribute = any (\t -> isTokenIdentifier t && isFsharpTestAttrName (tToken t))
  where
    isFsharpTestAttrName n =
        -- NUnit
        n == "Test" || n == "TestFixture" ||
        n == "TestCase" || n == "TestCaseSource" ||
        n == "SetUp" || n == "TearDown" ||
        n == "OneTimeSetUp" || n == "OneTimeTearDown" ||
        n == "Category" ||
        -- xUnit
        n == "Fact" || n == "Theory" ||
        n == "InlineData" || n == "MemberData" || n == "ClassData" ||
        -- FsCheck
        n == "Property"

-- | Collect a contiguous run of F# attributes.
collectFsharpAttributes :: [Token] -> ([[Token]], [Token])
collectFsharpAttributes ts = case extractFsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectFsharpAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

-- | Collect tokens that are part of a single F# binding (after a `let` / `member`
-- / `type` header). Stops when the next top-level construct, a new attribute
-- stack, or a new Expecto test function is encountered.
collectFsharpBindingBody :: [Token] -> ([Token], [Token])
collectFsharpBindingBody [] = ([], [])
collectFsharpBindingBody (first:rest) =
    let (body, remaining) = go rest
    in (first : body, remaining)
  where
    go [] = ([], [])
    go toks@(t:ts)
        -- Stop at the start of a new attribute.
        | isFsharpAttrOpen toks = ([], toks)
        -- Stop at a new top-level keyword (excluding 'let' since it's used for local bindings).
        | isTokenKeyword t &&
          (tToken t == "module" || tToken t == "namespace" ||
           tToken t == "type" ||
           tToken t == "member" || tToken t == "static" || tToken t == "abstract" ||
           tToken t == "override" || tToken t == "interface") = ([], toks)
        -- Stop at a new top-level Expecto test starter.
        | isTokenIdentifier t && isFsharpExpectoStarter (tToken t) = ([], toks)
        | otherwise =
            let (more, r) = go ts
            in (t : more, r)

-- | Names of Expecto test combinators that start a test block.
isFsharpExpectoStarter :: T.Text -> Bool
isFsharpExpectoStarter n =
    n == "testCase" || n == "testCaseAsync" ||
    n == "testList" || n == "test" ||
    n == "ftest" || n == "ptest" ||
    n == "ftestCase" || n == "ptestCase" ||
    n == "testProperty" || n == "testPropertyWithConfig" ||
    n == "testAsync" || n == "testSequenced"

-- | Consume a single Expecto test block, which may look like:
--     testCase "name" <| fun () -> ...
--     test "name" { ... }
--     testList "name" [ ... ]
--   We skip the string literal (if present), then:
--     - if the next token is `{`, consume the balanced block;
--     - if it's `[`, consume the balanced list (which itself may contain nested test combinators);
--     - otherwise, consume a binding body until the next top-level construct.
collectFsharpExpectoBody :: [Token] -> ([Token], [Token])
collectFsharpExpectoBody ts =
    -- skip an optional string literal (the test name is often a bare string token)
    let afterName = case ts of
                      (n:rest) | not (isTokenIdentifier n) && not (isTokenKeyword n)
                                 && not (isTokenOperator n) && not (isTokenBracket n) -> rest
                      _ -> ts
        prefix    = take (length ts - length afterName) ts
    in case afterName of
        -- Block form: test "..." { ... }
        (b:rest) | isTokenBracket b && tToken b == "{" ->
            let (body, final) = processInsideBrackets "{" "}" 1 rest
            in (prefix ++ b : body, final)
        -- List form: testList "..." [ ... ]
        (b:rest) | isTokenBracket b && tToken b == "[" ->
            let (body, final) = processInsideBrackets "[" "]" 1 rest
            in (prefix ++ b : body, final)
        -- Fallback: accumulate until next top-level construct.
        _ ->
            let (body, final) = collectFsharpBindingBody afterName
            in (prefix ++ body, final)

processOutsideFsharp :: Bool -> [Token] -> [Token]
processOutsideFsharp _ [] = [] -- End of stream

-- Pattern 1: one or more contiguous F# attributes, at least one of which is a test attribute.
processOutsideFsharp keepTests tokens
    | isFsharpAttrOpen tokens =
        let (attrs, afterAttrs) = collectFsharpAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isFsharpTestAttribute attrs
           then
               -- Consume the binding that follows (everything until the next
               -- attribute / top-level keyword / Expecto starter).
               let (body, remaining) = collectFsharpBindingBody afterAttrs
               in if keepTests
                  then flatAttrs ++ body ++ processOutsideFsharp keepTests remaining
                  else processOutsideFsharp keepTests remaining
           else
               -- No test attribute in the stack: emit them as-is and continue.
               if keepTests
               then processOutsideFsharp keepTests afterAttrs
               else flatAttrs ++ processOutsideFsharp keepTests afterAttrs

-- Pattern 2: Expecto test combinators: testCase "..." ..., test "..." { ... }, etc.
processOutsideFsharp keepTests (t1:ts)
    | isTokenIdentifier t1 && isFsharpExpectoStarter (tToken t1) =
        let (body, remaining) = collectFsharpExpectoBody ts
        in if keepTests
           then t1 : body ++ processOutsideFsharp keepTests remaining
           else processOutsideFsharp keepTests remaining

-- No test found, process the current token
processOutsideFsharp keepTests (t:ts) =
    if keepTests
    then processOutsideFsharp keepTests ts
    else t : processOutsideFsharp keepTests ts

-- ------------------------------------------------------------------
