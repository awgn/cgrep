{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import System.OsPath (osp, takeExtension)

instance LanguageTestFilter 'Perl where
    isTestFile path = takeExtension path == [osp|.t|]
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

-- | Check if a `sub <name> : ... : Test... {` header has a `Test` or `Tests` attribute before `{`.
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

-- | Check if a DSL keyword (`subtest`, `describe`, `it`, `before`, etc.) is followed
-- by expected arguments and `sub {`.
isPerlTestBlockKeyword :: Token -> [Token] -> Bool
isPerlTestBlockKeyword t1 ts
    | not (isTokenIdentifier t1 || isTokenKeyword t1) = False
    | name == "subtest" = True
    | name `elem` (["describe", "it", "context", "test", "tests", "shared_examples_for"] :: [Text]) =
        case ts of
            (s : _) | isTokenString s -> True
            (p : s : _) | isTokenBracket p && tToken p == "(" && isTokenString s -> True
            _ -> False
    | name
        `elem` ([ "before"
                , "after"
                , "around"
                , "before_each"
                , "after_each"
                , "before_all"
                , "after_all"
                , "around_each"
                ] :: [Text]) =
        True
    | otherwise = False
  where
    name = tToken t1

-- | Test::Exception / Test::Warn block-first assertion functions (`dies_ok { ... } 'msg';`).
isPerlBlockAssertion :: Token -> Bool
isPerlBlockAssertion t =
    isTokenIdentifier t
        && tToken t
            `elem` ([ "dies_ok"
                    , "lives_ok"
                    , "throws_ok"
                    , "lives_and"
                    , "warnings_exist"
                    , "warning_is"
                    , "warning_like"
                    ] :: [Text])

-- | Consumes trailing arguments of a Perl block assertion up to and including `;`.
consumeUntilPerlSemicolon :: [Token] -> ([Token], [Token])
consumeUntilPerlSemicolon [] = ([], [])
consumeUntilPerlSemicolon (t : ts)
    | tToken t == ";" = ([t], ts)
    | isTokenBracket t && tToken t `elem` (["(", "[", "{"] :: [Text]) =
        let closeB = case tToken t of
                "(" -> ")"
                "[" -> "]"
                _ -> "}"
            (brk, afterBrk) = processInsideBrackets (tToken t) closeB 1 ts
            (rest, remToks) = consumeUntilPerlSemicolon afterBrk
         in (t : brk ++ rest, remToks)
    | otherwise =
        let (rest, remToks) = consumeUntilPerlSemicolon ts
         in (t : rest, remToks)

-- | (Perl) Helper: Processes tokens *outside* a test block.
processOutsidePerl :: Bool -> [Token] -> [Token]
processOutsidePerl _ [] = []
-- Avoid matching method calls `$self->test(...)` in non-strict mode (`"-"` + `">"`)
processOutsidePerl keepTests (tDash : tGt : tMethod : ts)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenOperator tGt && tToken tGt == ">"
    , areAdjacentTokens tDash tGt
    , isTokenIdentifier tMethod || isTokenKeyword tMethod =
        if keepTests
            then processOutsidePerl keepTests ts
            else tDash : tGt : tMethod : processOutsidePerl keepTests ts
-- Avoid matching method calls `$self->test(...)` in strict mode or sigil variables `$test`, `@test`, `%test`
processOutsidePerl keepTests (t1 : t2 : ts)
    | isTokenOperator t1 && (tToken t1 `elem` (["->", "$", "@", "%", "&"] :: [Text]))
    , isTokenIdentifier t2 || isTokenKeyword t2 =
        if keepTests
            then processOutsidePerl keepTests ts
            else t1 : t2 : processOutsidePerl keepTests ts
-- Pattern 1: Test::Class subroutine with `: Test` or `: Tests(...)` attribute
processOutsidePerl keepTests (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "sub"
    , isTokenIdentifier t2 =
        case findTestClassSubBrace 50 False ts of
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in if keepTests
                        then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePerl keepTests remainingTokens
                        else processOutsidePerl keepTests remainingTokens
            Nothing ->
                if keepTests
                    then processOutsidePerl keepTests (t2 : ts)
                    else t1 : processOutsidePerl keepTests (t2 : ts)
-- Pattern 2: `subtest`, `describe`, `it`, `before each => sub { ... }`, etc.
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
-- Pattern 3: `dies_ok { ... } 'msg';`, `throws_ok { ... } qr/.../, 'msg';`
processOutsidePerl keepTests (t1 : tBrace : ts)
    | isPerlBlockAssertion t1
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (blockTokens, afterBlock) = processInsideBrackets "{" "}" 1 ts
            (tailTokens, remainingTokens) = consumeUntilPerlSemicolon afterBlock
         in if keepTests
                then t1 : tBrace : blockTokens ++ tailTokens ++ processOutsidePerl keepTests remainingTokens
                else processOutsidePerl keepTests remainingTokens
-- Fallback
processOutsidePerl keepTests (t : ts) =
    if keepTests
        then processOutsidePerl keepTests ts
        else t : processOutsidePerl keepTests ts
