{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Zig () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Zig where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideZig keepTests tokens

-- | (Zig) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test "description" { ... } - Zig's built-in test syntax
--
-- Zig has a very clean built-in test syntax that's easy to recognize.
processOutsideZig :: Bool -> [Token] -> [Token]
processOutsideZig _ [] = [] -- End of stream

-- Pattern: test "..." {
processOutsideZig keepTests (t1:ts)
    | isTokenKeyword t1 && tToken t1 == "test"
    =
        -- Found a test block. Find the opening brace.
        case findOpeningBracketBounded "{" 20 ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideZig keepTests ts else t1 : processOutsideZig keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes the test name/description and the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then -- Keep test keyword + description + body
                        t1 : signatureTokens ++ bodyTokens ++ processOutsideZig keepTests remainingTokens
                   else -- Discard the whole test block
                        processOutsideZig keepTests remainingTokens

-- No test found, process the current token
processOutsideZig keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideZig keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideZig keepTests ts
-- ------------------------------------------------------------------
