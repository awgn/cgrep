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
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)

instance LanguageTestFilter 'Fsharp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideFsharp keepTests tokens

-- | True if the stream starts with an F# attribute opener: `[<` (single op token) or `[` + `<` (split).
isFsharpAttrOpen :: [Token] -> Bool
isFsharpAttrOpen (t1 : t2 : _)
    | isTokenOperator t1 && tToken t1 == "[<" = True
    | isTokenBracket t1 && tToken t1 == "["
        && isTokenOperator t2 && tToken t2 == "<" =
        True
isFsharpAttrOpen (t1 : _)
    | isTokenOperator t1 && tToken t1 == "[<" = True
isFsharpAttrOpen _ = False

-- | True if the given 2 tokens form an F# attribute closer: `>]` or `>` + `]`.
isFsharpAttrClose2 :: Token -> Token -> Bool
isFsharpAttrClose2 a b =
    isTokenOperator a
        && tToken a == ">"
        && isTokenBracket b
        && tToken b == "]"

isFsharpAttrClose1 :: Token -> Bool
isFsharpAttrClose1 t = isTokenOperator t && tToken t == ">]"

-- | Extracts a single F# attribute block `[< ... >]`, including nested parens.
extractFsharpAttribute :: [Token] -> Maybe ([Token], [Token])
extractFsharpAttribute (t1 : ts)
    | isTokenOperator t1 && tToken t1 == "[<" =
        let (inside, rest) = consumeFsharpAttrBody ts
         in case rest of
                (c : rs) | isFsharpAttrClose1 c -> Just (t1 : inside ++ [c], rs)
                (c1 : c2 : rs) | isFsharpAttrClose2 c1 c2 -> Just (t1 : inside ++ [c1, c2], rs)
                _ -> Nothing
extractFsharpAttribute (t1 : t2 : ts)
    | isTokenBracket t1
        && tToken t1 == "["
        && isTokenOperator t2
        && tToken t2 == "<" =
        let (inside, rest) = consumeFsharpAttrBody ts
         in case rest of
                (c : rs) | isFsharpAttrClose1 c -> Just (t1 : t2 : inside ++ [c], rs)
                (c1 : c2 : rs) | isFsharpAttrClose2 c1 c2 -> Just (t1 : t2 : inside ++ [c1, c2], rs)
                _ -> Nothing
extractFsharpAttribute _ = Nothing

-- | Consumes the body of an F# attribute, balancing parentheses,
-- stopping just before the `>]` (or `>` + `]`) closer.
consumeFsharpAttrBody :: [Token] -> ([Token], [Token])
consumeFsharpAttrBody [] = ([], [])
consumeFsharpAttrBody (t : rest)
    | isFsharpAttrClose1 t = ([], t : rest)
consumeFsharpAttrBody (t1 : t2 : rest)
    | isFsharpAttrClose2 t1 t2 = ([], t1 : t2 : rest)
consumeFsharpAttrBody (t : rest)
    | isTokenBracket t && tToken t == "(" =
        let (paren, afterParen) = processInsideBrackets "(" ")" 1 rest
            (more, final) = consumeFsharpAttrBody afterParen
         in (t : paren ++ more, final)
    | otherwise =
        let (more, final) = consumeFsharpAttrBody rest
         in (t : more, final)

-- | Strip parenthesized arguments `(...)` from an F# attribute token stream so
-- that identifiers inside attribute arguments (e.g. `[<System.Obsolete(nameof(Test))>]`)
-- are NEVER mistaken for the attribute's own name!
stripFsharpAttrArgs :: [Token] -> [Token]
stripFsharpAttrArgs [] = []
stripFsharpAttrArgs (t : ts)
    | isTokenBracket t && tToken t == "(" =
        let (_, remaining) = processInsideBrackets "(" ")" 1 ts
         in stripFsharpAttrArgs remaining
    | otherwise = t : stripFsharpAttrArgs ts

-- | True if any attribute name (outside `(...)` arguments) inside the attribute
-- is a known F# test-framework attribute.
isFsharpTestAttribute :: [Token] -> Bool
isFsharpTestAttribute attrTokens =
    let headerTokens = stripFsharpAttrArgs attrTokens
     in any (\t -> isTokenIdentifier t && isFsharpTestAttrName (tToken t)) headerTokens
  where
    isFsharpTestAttrName :: Text -> Bool
    isFsharpTestAttrName n =
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
                    , -- xUnit
                      "Fact"
                    , "Theory"
                    , "InlineData"
                    , "MemberData"
                    , "ClassData"
                    , -- FsCheck
                      "Property"
                    , -- MSTest
                      "TestClass"
                    , "TestMethod"
                    ] :: [Text])

-- | Collect a contiguous run of F# attributes.
collectFsharpAttributes :: [Token] -> ([[Token]], [Token])
collectFsharpAttributes ts = case extractFsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectFsharpAttributes rest
         in (attr : more, final)
    Nothing -> ([], ts)

-- | Check whether a `let` token starts a function definition (`let [rec] [inline] name param1 ... =`)
-- rather than a simple local variable binding (`let x = ...`).
-- A function binding has at least two tokens (or `()` unit) between `let` and `=` at paren depth 0,
-- OR a parameter list `(x: int)`.
isTopLevelFunctionLet :: [Token] -> Bool
isTopLevelFunctionLet ts =
    let afterModifiers = dropWhile (\t -> isTokenKeyword t && (tToken t == "rec" || tToken t == "inline" || tToken t == "private" || tToken t == "internal" || tToken t == "public")) ts
     in case afterModifiers of
            -- `let name param1 ... =` where `param1` is NOT `=` and NOT `:` (return type on value)
            (tName : tNext : _)
                | (isTokenIdentifier tName || isTokenString tName)
                , not (isTokenOperator tNext && (tToken tNext == "=" || tToken tNext == ":")) ->
                    True
            _ -> False

-- | Collect tokens that are part of a single F# binding (after a `let` / `member`
-- / `type` header). Stops when the next top-level construct, a new attribute
-- stack, a new top-level `let func param = ...`, or a new Expecto test function is encountered.
collectFsharpBindingBody :: [Token] -> ([Token], [Token])
collectFsharpBindingBody [] = ([], [])
collectFsharpBindingBody (first : rest) =
    let (body, remaining) = go rest
     in (first : body, remaining)
  where
    go [] = ([], [])
    go toks@(t : ts)
        -- Stop at the start of a new attribute.
        | isFsharpAttrOpen toks = ([], toks)
        -- Stop at a new top-level keyword.
        | isTokenKeyword t
            && ( tToken t == "module"
                    || tToken t == "namespace"
                    || tToken t == "type"
                    || tToken t == "open"
                    || tToken t == "member"
                    || tToken t == "static"
                    || tToken t == "abstract"
                    || tToken t == "override"
                    || tToken t == "interface"
               ) =
            ([], toks)
        -- Stop at a new top-level function `let name param = ...`!
        | isTokenKeyword t && tToken t == "let" && isTopLevelFunctionLet ts =
            ([], toks)
        -- Stop at a new valid Expecto test starter.
        | isValidExpectoStart toks = ([], toks)
        | otherwise =
            let (more, r) = go ts
             in (t : more, r)

-- | Names of Expecto test combinators that start a test block.
isFsharpExpectoStarter :: Text -> Bool
isFsharpExpectoStarter n =
    n
        `elem` ([ "testCase"
                , "testCaseAsync"
                , "testList"
                , "test"
                , "ftest"
                , "ptest"
                , "ftestCase"
                , "ptestCase"
                , "ftestList"
                , "ptestList"
                , "testProperty"
                , "testPropertyWithConfig"
                , "testAsync"
                , "testSequenced"
                ] :: [Text])

-- | An identifier is a real Expecto test starter ONLY if it is followed by a string literal
-- (the test name) or `{` (computation expression `test { ... }`).
-- This prevents local variables named `test` (e.g. `let test = x > 0`) from being mistaken
-- for an Expecto test block!
isValidExpectoStart :: [Token] -> Bool
isValidExpectoStart (t1 : t2 : _)
    | isTokenIdentifier t1 && isFsharpExpectoStarter (tToken t1) =
        isTokenString t2 || (isTokenBracket t2 && tToken t2 == "{")
isValidExpectoStart _ = False

-- | Consume a single Expecto test block.
collectFsharpExpectoBody :: [Token] -> ([Token], [Token])
collectFsharpExpectoBody ts =
    let (prefix, afterName) = case ts of
            (n : rest) | isTokenString n -> ([n], rest)
            _ -> ([], ts)
     in case afterName of
            -- Block form: test "..." { ... } or test { ... }
            (b : rest)
                | isTokenBracket b && tToken b == "{" ->
                    let (body, final) = processInsideBrackets "{" "}" 1 rest
                     in (prefix ++ b : body, final)
            -- List form: testList "..." [ ... ]
            (b : rest)
                | isTokenBracket b && tToken b == "[" ->
                    let (body, final) = processInsideBrackets "[" "]" 1 rest
                     in (prefix ++ b : body, final)
            -- Fallback: accumulate until next top-level construct.
            _ ->
                let (body, final) = collectFsharpBindingBody afterName
                 in (prefix ++ body, final)

processOutsideFsharp :: Bool -> [Token] -> [Token]
processOutsideFsharp _ [] = []

-- Pattern 1: one or more contiguous F# attributes, at least one of which is a test attribute.
processOutsideFsharp keepTests tokens
    | isFsharpAttrOpen tokens =
        let (attrs, afterAttrs) = collectFsharpAttributes tokens
            flatAttrs = concat attrs
         in if not (null attrs) && any isFsharpTestAttribute attrs
                then
                    let (body, remaining) = collectFsharpBindingBody afterAttrs
                     in if keepTests
                            then flatAttrs ++ body ++ processOutsideFsharp keepTests remaining
                            else processOutsideFsharp keepTests remaining
                else
                    if keepTests
                        then processOutsideFsharp keepTests afterAttrs
                        else flatAttrs ++ processOutsideFsharp keepTests afterAttrs

-- Pattern 2: Expecto test combinators followed by a string literal or `{`:
-- e.g. `testCase "..." <| fun () -> ...`, `test "..." { ... }`, `testList "..." [ ... ]`
processOutsideFsharp keepTests tokens@(t1 : ts)
    | isValidExpectoStart tokens =
        let (body, remaining) = collectFsharpExpectoBody ts
         in if keepTests
                then t1 : body ++ processOutsideFsharp keepTests remaining
                else processOutsideFsharp keepTests remaining

-- No test found, process the current token
processOutsideFsharp keepTests (t : ts) =
    if keepTests
        then processOutsideFsharp keepTests ts
        else t : processOutsideFsharp keepTests ts
