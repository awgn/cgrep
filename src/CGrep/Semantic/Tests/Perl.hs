{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Perl () where

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

instance LanguageTestFilter 'Perl where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePerl keepTests tokens

-- | Find `sub {` within `n` tokens before any `;`, returning tokens up to and including `{`
-- and remaining tokens after `{`.
findPerlAnonSubBrace :: Int -> [Token] -> Maybe ([Token], [Token])
findPerlAnonSubBrace 0 _ = Nothing
findPerlAnonSubBrace _ [] = Nothing
findPerlAnonSubBrace _ (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "sub"
    , isTokenBracket t2 && tToken t2 == "{" =
        Just ([t1, t2], ts)
findPerlAnonSubBrace n (t : ts)
    | tToken t == ";" = Nothing
    | isTokenBracket t && tToken t == "{" = Nothing
    | otherwise =
        case findPerlAnonSubBrace (n - 1) ts of
            Nothing -> Nothing
            Just (pre, post) -> Just (t : pre, post)

-- | Check if a `sub <name> : Test... {` header has a `Test` or `Tests` attribute before `{`.
findTestClassSubBrace :: Int -> Bool -> [Token] -> Maybe ([Token], [Token])
findTestClassSubBrace 0 _ _ = Nothing
findTestClassSubBrace _ _ [] = Nothing
findTestClassSubBrace _ seenTestAttr (t : ts)
    | isTokenBracket t && tToken t == "{" =
        if seenTestAttr then Just ([t], ts) else Nothing
    | tToken t == ";" = Nothing
findTestClassSubBrace n _ (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == ":"
    , isTokenIdentifier t2 && (tToken t2 == "Test" || tToken t2 == "Tests") =
        case findTestClassSubBrace (n - 2) True ts of
            Nothing -> Nothing
            Just (pre, post) -> Just (t1 : t2 : pre, post)
findTestClassSubBrace n seenTestAttr (t : ts) =
    case findTestClassSubBrace (n - 1) seenTestAttr ts of
        Nothing -> Nothing
        Just (pre, post) -> Just (t : pre, post)

-- | Check if a DSL keyword (`subtest`, `describe`, `it`, `context`, `test`) is followed
-- by a string description (optionally inside `(`) and `sub {`.
isPerlTestBlockKeyword :: Token -> [Token] -> Bool
isPerlTestBlockKeyword t1 ts
    | not (isTokenIdentifier t1 || isTokenKeyword t1) = False
    | tToken t1 == "subtest" = True
    | tToken t1 `elem` (["describe", "it", "context", "test"] :: [Text]) =
        case ts of
            (s : _) | isTokenString s -> True
            (p : s : _) | isTokenBracket p && tToken p == "(" && isTokenString s -> True
            _ -> False
    | otherwise = False

-- | (Perl) Helper: Processes tokens *outside* a test block.
processOutsidePerl :: Bool -> [Token] -> [Token]
processOutsidePerl _ [] = []
-- Avoid matching method calls `$self->test(...)` or sigil variables `$test`, `@test`, `%test`
processOutsidePerl keepTests (t1 : t2 : ts)
    | isTokenOperator t1 && (tToken t1 `elem` (["->", "$", "@", "%", "&"] :: [Text]))
    , isTokenIdentifier t2 =
        if keepTests
            then processOutsidePerl keepTests ts
            else t1 : t2 : processOutsidePerl keepTests ts
-- Pattern 1: Test::Class subroutine with `: Test` or `: Tests(...)` attribute (`sub foo : Test(2) { ... }`)
processOutsidePerl keepTests (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "sub"
    , isTokenIdentifier t2 =
        case findTestClassSubBrace 20 False ts of
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePerl keepTests remainingTokens
                        else processOutsidePerl keepTests remainingTokens
            Nothing ->
                if keepTests
                    then processOutsidePerl keepTests (t2 : ts)
                    else t1 : processOutsidePerl keepTests (t2 : ts)
-- Pattern 2: `subtest 'name' => sub { ... }`, `describe 'name' => sub { ... }`, `it 'name' => sub { ... }`
processOutsidePerl keepTests (t1 : ts)
    | isPerlTestBlockKeyword t1 ts =
        case findPerlAnonSubBrace 20 ts of
            Nothing ->
                if keepTests
                    then processOutsidePerl keepTests ts
                    else t1 : processOutsidePerl keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : signatureTokens ++ bodyTokens ++ processOutsidePerl keepTests remainingTokens
                        else processOutsidePerl keepTests remainingTokens
-- No test found, process the current token
processOutsidePerl keepTests (t : ts) =
    if keepTests
        then processOutsidePerl keepTests ts
        else t : processOutsidePerl keepTests ts
