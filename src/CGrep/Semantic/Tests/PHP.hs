{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.PHP () where

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

instance LanguageTestFilter 'PHP where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePHP keepTests False tokens

-- | Pest PHP test & lifecycle functions
isPestFunctionName :: Text -> Bool
isPestFunctionName name =
    name
        `elem` ([ "test"
                , "it"
                , "describe"
                , "beforeEach"
                , "afterEach"
                , "beforeAll"
                , "afterAll"
                ] :: [Text])

-- | Check whether the tokens inside a Pest call's parentheses `(...)` contain a closure
-- (`function` or `fn` or `=>`) OR whether the call is followed by chained Pest methods `->...`.
hasPestClosure :: [Token] -> Bool
hasPestClosure [] = False
hasPestClosure (t1 : t2 : ts)
    | tToken t1 == "=" && tToken t2 == ">" = True
    | otherwise = isPestClosureSingle t1 || hasPestClosure (t2 : ts)
hasPestClosure [t1] = isPestClosureSingle t1

isPestClosureSingle :: Token -> Bool
isPestClosureSingle t =
    tToken t == "=>"
        || ((isTokenKeyword t || isTokenIdentifier t) && (tToken t == "function" || tToken t == "fn"))

-- | Consume any trailing chained Pest calls (`->throws(...)`, `->skip(...)`, `->group(...)`, `->todo()`)
-- up to the statement-terminating `;`.
consumePestChain :: [Token] -> ([Token], [Token])
consumePestChain (tDash : tGt : tMethod : rest)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenOperator tGt && tToken tGt == ">"
    , isTokenIdentifier tMethod =
        case rest of
            (tOpen : afterOpen)
                | isTokenBracket tOpen && tToken tOpen == "(" ->
                    let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 afterOpen
                        (moreChain, final) = consumePestChain afterArgs
                     in (tDash : tGt : tMethod : tOpen : argTokens ++ moreChain, final)
            _ ->
                let (moreChain, final) = consumePestChain rest
                 in (tDash : tGt : tMethod : moreChain, final)
consumePestChain (tArrow : tMethod : rest)
    | isTokenOperator tArrow && tToken tArrow == "->"
    , isTokenIdentifier tMethod =
        case rest of
            (tOpen : afterOpen)
                | isTokenBracket tOpen && tToken tOpen == "(" ->
                    let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 afterOpen
                        (moreChain, final) = consumePestChain afterArgs
                     in (tArrow : tMethod : tOpen : argTokens ++ moreChain, final)
            _ ->
                let (moreChain, final) = consumePestChain rest
                 in (tArrow : tMethod : moreChain, final)
consumePestChain ts = ([], ts)

-- | Check if a PHP function/method name is a PHPUnit test or lifecycle method.
-- Requires `"test"` to NOT be followed by a lowercase ASCII letter (so `testAddition`
-- and `test_addition` match, while `testifyUser` does NOT match!).
isPhpTestMethodName :: Text -> Bool
isPhpTestMethodName name
    | name `elem` (["setUp", "tearDown", "setUpBeforeClass", "tearDownAfterClass"] :: [Text]) = True
    | Just rest <- T.stripPrefix "test" name =
        case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAsciiLower c)
    | otherwise = False

-- | Check if a PHP class name or `extends` clause denotes a PHPUnit test class.
-- Note: we require `extends` to be followed by a class name ending in `TestCase` or `Test`
-- OR the class's own name to end in `Test` / `Tests` / `TestCase` or start with `Test_` / `Test`
-- (excluding classes named `...After...Test` which are production classes in test suites!).
isPhpTestClass :: Token -> [Token] -> Bool
isPhpTestClass classNameTok afterClassName =
    let cname = tToken classNameTok
        prefixOk = case T.stripPrefix "Test" cname of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
        nameMatch =
            ( prefixOk
                || "Test" `T.isSuffixOf` cname
                || "Tests" `T.isSuffixOf` cname
                || "TestCase" `T.isSuffixOf` cname
                || "Test_" `T.isInfixOf` cname
                || "_Test" `T.isInfixOf` cname
            )
                
        extendsMatch = case afterClassName of
            (tExt : rest)
                | (isTokenKeyword tExt || isTokenIdentifier tExt) && tToken tExt == "extends" ->
                    let superTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "{") && tToken t /= ";") rest
                     in any (\t -> isTokenIdentifier t && ("TestCase" `T.isSuffixOf` tToken t || "Test" `T.isSuffixOf` tToken t)) superTokens
            _ -> False
     in nameMatch || extendsMatch

-- | (PHP) Helper: Processes tokens *outside* a test method.
processOutsidePHP :: Bool -> Bool -> [Token] -> [Token]
processOutsidePHP _ _ [] = []

-- Pattern 1: Pest functions `test(`, `it(`, `describe(`, `beforeEach(`, `afterEach(`, `beforeAll(`, `afterAll(`
processOutsidePHP keepTests False (t1 : t2 : ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && isPestFunctionName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , let (bodyTokens, afterParens) = processInsideBrackets "(" ")" 1 ts
    , let (chainTokens, remainingTokens) = consumePestChain afterParens
    , hasPestClosure bodyTokens || not (null chainTokens) =
        if keepTests
            then t1 : t2 : bodyTokens ++ chainTokens ++ processOutsidePHP keepTests False remainingTokens
            else processOutsidePHP keepTests False remainingTokens

-- Pattern 2: `function test*` or PHPUnit lifecycle methods (`setUp`, `tearDown`, etc.)
-- Uses `findOpeningBraceC` so abstract methods (`abstract public function testFoo();`) stop at `;`!
processOutsidePHP keepTests _ (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "function"
    , isTokenIdentifier t2 && isPhpTestMethodName (tToken t2) =
        case findOpeningBraceC 100 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests False (t2 : ts) else t1 : processOutsidePHP keepTests False (t2 : ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests False remainingTokens
                        else processOutsidePHP keepTests False remainingTokens

-- Pattern 3: `class *Test` or `class ... extends ...\TestCase`
processOutsidePHP keepTests _ (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class"
    , isTokenIdentifier t2 && isPhpTestClass t2 ts =
        case findOpeningBraceC 200 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests False (t2 : ts) else t1 : processOutsidePHP keepTests False (t2 : ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests False remainingTokens
                        else processOutsidePHP keepTests False remainingTokens

-- No test found, process the current token
processOutsidePHP keepTests _ (t : ts) =
    let isBlocker =
            ((isTokenKeyword t || isTokenIdentifier t) && tToken t == "function")
                || (isTokenOperator t && (tToken t == ">" || tToken t == "->" || tToken t == "?->" || tToken t == ":" || tToken t == "::"))
     in if keepTests
            then processOutsidePHP keepTests isBlocker ts
            else t : processOutsidePHP keepTests isBlocker ts
