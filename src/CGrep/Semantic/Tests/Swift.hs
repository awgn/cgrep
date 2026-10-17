{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Swift () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Swift where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens

instance LanguageTestFilter 'ObjectiveC where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens

-- | Extracts a Swift attribute `@Foo.Bar(...)`
extractSwiftAttribute :: [Token] -> Maybe ([Token], [Token])
extractSwiftAttribute (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "@" && (isTokenIdentifier t2 || isTokenKeyword t2) =
        let (pathTokens, rest) = collectDotPath ts
         in case rest of
                (t3 : ts')
                    | isTokenBracket t3 && tToken t3 == "(" ->
                        let (insideTokens, remaining) = processInsideBrackets "(" ")" 1 ts'
                         in Just (t1 : t2 : pathTokens ++ t3 : insideTokens, remaining)
                _ -> Just (t1 : t2 : pathTokens, rest)
  where
    collectDotPath (d : i : rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (more, final) = collectDotPath rest
             in (d : i : more, final)
    collectDotPath rest = ([], rest)
extractSwiftAttribute _ = Nothing

-- | Checks if a Swift attribute is `@Test` or `@Suite` (or `@Testing.Test` / `@Testing.Suite`),
-- examining ONLY the attribute name before `(` so arguments like `@CustomAttr(mode: Test)`
-- are never mistaken for test attributes!
isSwiftTestAttribute :: [Token] -> Bool
isSwiftTestAttribute tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "(")) tokens
        identTokens = filter isTokenIdentifier headerTokens
     in any (\t -> tToken t == "Test" || tToken t == "Suite") identTokens

-- | Collects contiguous Swift attributes
collectSwiftAttributes :: [Token] -> ([[Token]], [Token])
collectSwiftAttributes ts = case extractSwiftAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectSwiftAttributes rest
         in (attr : more, final)
    Nothing -> ([], ts)

-- | Find the opening `{` of a Swift declaration while:
--   1. Ignoring `{}` nested inside `(...)` or `[...]` (e.g. default closure parameters `= {}`).
--   2. Stopping if `;` or a second declaration keyword (`func`, `struct`, `class`, `enum`,
--      `protocol`, `extension`, `var`, `let`) is encountered at depth 0 (e.g. a protocol
--      method requirement `func testConnection() -> Bool` that has no body `{}`).
findOpeningBraceSwift :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceSwift canSeeDecl0 n0 = go 0 canSeeDecl0 n0
  where
    go :: Int -> Bool -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ _ 0 _ = Nothing
    go _ _ _ [] = Nothing
    go depth canSeeDecl n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) canSeeDecl (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) canSeeDecl (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0 && tToken t == ";" =
            Nothing
        | depth == 0 && isSwiftDeclKeyword t =
            if canSeeDecl
                then step depth False (n - 1) t ts
                else Nothing
        | otherwise =
            step depth canSeeDecl (n - 1) t ts

    isSwiftDeclKeyword :: Token -> Bool
    isSwiftDeclKeyword tok =
        (isTokenKeyword tok || isTokenIdentifier tok)
            && ( tToken tok
                    `elem` ([ "func"
                            , "struct"
                            , "class"
                            , "enum"
                            , "protocol"
                            , "extension"
                            , "var"
                            , "let"
                            ] :: [Text])
               )

    step :: Int -> Bool -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d c m tok rest =
        case go d c m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | Check if a Swift/ObjC function name is an XCTest method (`test*` where `"test"` is
-- NOT followed by a lowercase ASCII letter, or `setUp` / `tearDown` / `setUpWithError` / `tearDownWithError`).
isSwiftTestMethodName :: Text -> Bool
isSwiftTestMethodName name
    | name `elem` (["setUp", "tearDown", "setUpWithError", "tearDownWithError"] :: [Text]) = True
    | Just rest <- T.stripPrefix "test" name =
        case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAsciiLower c)
    | otherwise = False

-- | Check if a Swift `class` / `struct` / `extension` is a test type:
--   1. Type name starts with `"Test"` (not followed by lowercase, e.g. `TestSuite`, NOT `Testimonial`),
--      or ends with `"Test"` / `"Tests"` / `"TestCase"`, or contains `"Test_"` / `"_Test"`, OR
--   2. Type header inherits (`:`) from `XCTestCase` (even after generic parameters `<T>`).
isSwiftTestType :: Token -> [Token] -> Bool
isSwiftTestType typeNameTok afterTypeName =
    let tname = tToken typeNameTok
        prefixOk = case T.stripPrefix "Test" tname of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
        nameMatch =
            prefixOk
                || "Test" `T.isSuffixOf` tname
                || "Tests" `T.isSuffixOf` tname
                || "TestCase" `T.isSuffixOf` tname
                || "Test_" `T.isInfixOf` tname
                || "_Test" `T.isInfixOf` tname
        headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "{") && tToken t /= ";") afterTypeName
        inheritsXCTestCase = any (\t -> isTokenIdentifier t && tToken t == "XCTestCase") headerTokens
     in nameMatch || inheritsXCTestCase

-- | (Swift / Objective-C) Helper: Processes tokens *outside* a test function.
processOutsideSwift :: Bool -> [Token] -> [Token]
processOutsideSwift _ [] = []

-- Pattern 1: Swift Testing Attributes @Test, @Suite
processOutsideSwift keepTests tokens@(t1 : ts)
    | isTokenOperator t1 && tToken t1 == "@" =
        let (attrs, afterAttrs) = collectSwiftAttributes tokens
            flatAttrs = concat attrs
         in if not (null attrs) && any isSwiftTestAttribute attrs
                then case findOpeningBraceSwift True 200 afterAttrs of
                    Nothing ->
                        if keepTests
                            then processOutsideSwift keepTests afterAttrs
                            else flatAttrs ++ processOutsideSwift keepTests afterAttrs
                    Just (sigTokens, tokensAfterBrace) ->
                        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                         in if keepTests
                                then flatAttrs ++ sigTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                                else processOutsideSwift keepTests remainingTokens
                else
                    if null attrs
                        then
                            if keepTests
                                then processOutsideSwift keepTests ts
                                else t1 : processOutsideSwift keepTests ts
                        else
                            if keepTests
                                then processOutsideSwift keepTests afterAttrs
                                else flatAttrs ++ processOutsideSwift keepTests afterAttrs

-- Pattern 2: `func test*`, `setUp`, `tearDown`, `setUpWithError`, `tearDownWithError`
-- Note: `canSeeDecl` starts at `False` because `func` has ALREADY been consumed!
processOutsideSwift keepTests (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "func"
    , isTokenIdentifier t2 && isSwiftTestMethodName (tToken t2) =
        case findOpeningBraceSwift False 100 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2 : ts) else t1 : processOutsideSwift keepTests (t2 : ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                        else processOutsideSwift keepTests remainingTokens

-- Pattern 3: `class *Tests`, `struct *Tests`, or `class ... : XCTestCase` (including generics `class Foo<T>: XCTestCase`)
processOutsideSwift keepTests (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && (tToken t1 == "class" || tToken t1 == "struct" || tToken t1 == "extension")
    , isTokenIdentifier t2 && isSwiftTestType t2 ts =
        case findOpeningBraceSwift False 200 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2 : ts) else t1 : processOutsideSwift keepTests (t2 : ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                        else processOutsideSwift keepTests remainingTokens

-- Pattern 4: Objective-C `- (void)testSomething` / `- (void)setUp` / `- (void)tearDown`
processOutsideSwift keepTests (t1 : t2 : t3 : t4 : t5 : ts)
    | isTokenOperator t1 && (tToken t1 == "-" || tToken t1 == "+")
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenBracket t4 && tToken t4 == ")"
    , isTokenIdentifier t5 && isSwiftTestMethodName (tToken t5) =
        case findOpeningBraceC 100 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2 : t3 : t4 : t5 : ts) else t1 : processOutsideSwift keepTests (t2 : t3 : t4 : t5 : ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : t3 : t4 : t5 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                        else processOutsideSwift keepTests remainingTokens

-- No test found, process the current token
processOutsideSwift keepTests (t : ts) =
    if keepTests
        then processOutsideSwift keepTests ts
        else t : processOutsideSwift keepTests ts
