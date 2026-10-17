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
import qualified Data.Text as T
instance LanguageTestFilter 'PHP where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePHP keepTests tokens

-- | Extracts a PHP attribute block `#[...]`
extractPhpAttribute :: [Token] -> Maybe ([Token], [Token])
extractPhpAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : t2 : insideTokens, remaining)
extractPhpAttribute _ = Nothing

-- | Checks if an attribute is test-related
isPhpTestAttribute :: [Token] -> Bool
isPhpTestAttribute = any (\t -> isTokenIdentifier t &&
    (tToken t == "Test" || tToken t == "DataProvider" || tToken t == "Depends" ||
     tToken t == "Before" || tToken t == "After" || tToken t == "BeforeClass" || tToken t == "AfterClass"))

-- | Collects all PHP attributes before a declaration
collectPhpAttributes :: [Token] -> ([[Token]], [Token])
collectPhpAttributes ts = case extractPhpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectPhpAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

-- | (PHP) Helper: Processes tokens *outside* a test method.
--
-- PHP uses PHPUnit and Pest frameworks.
processOutsidePHP :: Bool -> [Token] -> [Token]
processOutsidePHP _ [] = [] -- End of stream

-- Pattern 1: Pest functions `test(`, `it(`, `describe(`
processOutsidePHP keepTests (t1:t2:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && (tToken t1 == "test" || tToken t1 == "it" || tToken t1 == "describe") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then t1 : t2 : bodyTokens ++ processOutsidePHP keepTests remainingTokens
           else processOutsidePHP keepTests remainingTokens

-- Pattern 2: PHP Attributes #[Test] etc.
processOutsidePHP keepTests tokens@(t1:t2:_)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (attrs, afterAttrs) = collectPhpAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isPhpTestAttribute attrs
           then
               case findOpeningBraceBounded 100 afterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsidePHP keepTests afterAttrs
                       else flatAttrs ++ processOutsidePHP keepTests afterAttrs
                   Just (sigTokens, tokensAfterBrace) ->
                       let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                       in if keepTests
                          then flatAttrs ++ sigTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                          else processOutsidePHP keepTests remainingTokens
           else
               if keepTests
               then processOutsidePHP keepTests afterAttrs
               else flatAttrs ++ processOutsidePHP keepTests afterAttrs

-- Pattern 3: @test annotation (in comment/docblock)
processOutsidePHP keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && (tToken t2 == "test" || tToken t2 == "dataProvider" || tToken t2 == "depends" || tToken t2 == "before" || tToken t2 == "after")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 4: function test* or lifecycle methods
processOutsidePHP keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "function" &&
      isTokenIdentifier t2 &&
      ("test" `T.isPrefixOf` tToken t2 || tToken t2 == "setUp" || tToken t2 == "tearDown" || tToken t2 == "setUpBeforeClass" || tToken t2 == "tearDownAfterClass")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 5: class *Test*
processOutsidePHP keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 && ("Test" `T.isInfixOf` tToken t2)
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 6: class ... extends TestCase
processOutsidePHP keepTests (t1:t2:t3:t4:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      (isTokenKeyword t3 || isTokenIdentifier t3) && tToken t3 == "extends" &&
      isTokenIdentifier t4 && ("TestCase" `T.isSuffixOf` tToken t4)
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:t3:t4:ts) else t1 : processOutsidePHP keepTests (t2:t3:t4:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- No test found, process the current token
processOutsidePHP keepTests (t:ts) =
    if keepTests
    then processOutsidePHP keepTests ts
    else t : processOutsidePHP keepTests ts

-- ------------------------------------------------------------------
