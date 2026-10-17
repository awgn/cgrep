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
import qualified Data.Text as T
instance LanguageTestFilter 'Swift where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens

instance LanguageTestFilter 'ObjectiveC where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens

-- | (Swift/Objective-C) Helper: Processes tokens *outside* a test method.
-- | Extracts a Swift attribute `@...`
extractSwiftAttribute :: [Token] -> Maybe ([Token], [Token])
extractSwiftAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" && isTokenIdentifier t2 =
        case ts of
            (t3:ts') | isTokenBracket t3 && tToken t3 == "(" ->
                let (insideTokens, remaining) = processInsideBrackets "(" ")" 1 ts'
                in Just (t1 : t2 : t3 : insideTokens, remaining)
            _ -> Just (t1 : t2 : [], ts)
extractSwiftAttribute _ = Nothing

-- | Checks if a Swift attribute is test-related
isSwiftTestAttribute :: [Token] -> Bool
isSwiftTestAttribute = any (\t -> isTokenIdentifier t && (tToken t == "Test" || tToken t == "Suite"))

-- | Collects contiguous Swift attributes
collectSwiftAttributes :: [Token] -> ([[Token]], [Token])
collectSwiftAttributes ts = case extractSwiftAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectSwiftAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

-- | (Swift) Helper: Processes tokens *outside* a test function.
--
-- Swift and Objective-C use XCTest framework and Swift Testing framework.
processOutsideSwift :: Bool -> [Token] -> [Token]
processOutsideSwift _ [] = [] -- End of stream

-- Pattern 1: Swift Testing Attributes @Test, @Suite
processOutsideSwift keepTests tokens@(t1:ts)
    | isTokenOperator t1 && tToken t1 == "@" =
        let (attrs, afterAttrs) = collectSwiftAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isSwiftTestAttribute attrs
           then
               case findOpeningBraceBounded 200 afterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsideSwift keepTests afterAttrs
                       else flatAttrs ++ processOutsideSwift keepTests afterAttrs
                   Just (sigTokens, tokensAfterBrace) ->
                       let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                       in if keepTests
                          then flatAttrs ++ sigTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                          else processOutsideSwift keepTests remainingTokens
           else if null attrs then
               if keepTests
               then processOutsideSwift keepTests ts
               else t1 : processOutsideSwift keepTests ts
           else
               if keepTests
               then processOutsideSwift keepTests afterAttrs
               else flatAttrs ++ processOutsideSwift keepTests afterAttrs

-- Pattern 2: func test*, setUp, tearDown
processOutsideSwift keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "func" &&
      isTokenIdentifier t2 &&
      ("test" `T.isPrefixOf` tToken t2 || tToken t2 == "setUp" || tToken t2 == "tearDown" || tToken t2 == "setUpWithError" || tToken t2 == "tearDownWithError")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:ts) else t1 : processOutsideSwift keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 3: class *Test* or struct *Test*
processOutsideSwift keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && (tToken t1 == "class" || tToken t1 == "struct" || tToken t1 == "extension") &&
      isTokenIdentifier t2 && "Test" `T.isInfixOf` tToken t2
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:ts) else t1 : processOutsideSwift keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 4: class ... : XCTestCase
processOutsideSwift keepTests (t1:t2:t3:t4:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      isTokenOperator t3 && tToken t3 == ":" &&
      isTokenIdentifier t4 && tToken t4 == "XCTestCase"
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:t3:t4:ts) else t1 : processOutsideSwift keepTests (t2:t3:t4:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 5: Objective-C - (void)testSomething
processOutsideSwift keepTests (t1:t2:t3:t4:t5:ts)
    | isTokenOperator t1 && (tToken t1 == "-" || tToken t1 == "+") &&
      isTokenBracket t2 && tToken t2 == "(" &&
      isTokenBracket t4 && tToken t4 == ")" &&
      isTokenIdentifier t5 && ("test" `T.isPrefixOf` tToken t5 || tToken t5 == "setUp" || tToken t5 == "tearDown")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:t3:t4:t5:ts) else t1 : processOutsideSwift keepTests (t2:t3:t4:t5:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : t5 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- No test found, process the current token
processOutsideSwift keepTests (t:ts) =
    if keepTests
    then processOutsideSwift keepTests ts
    else t : processOutsideSwift keepTests ts

-- ------------------------------------------------------------------
