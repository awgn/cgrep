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
-- Recognizes Zig built-in test blocks:
--   1. `test "description" { ... }`
--   2. `test { ... }` (unnamed test)
--   3. `test identifier { ... }` or `test @"identifier" { ... }` (decltest in Zig 0.11+)
processOutsideZig :: Bool -> [Token] -> [Token]
processOutsideZig _ [] = [] -- End of stream

-- Pattern: test [name] {
processOutsideZig keepTests (t1:ts)
    | isTokenKeyword t1 && tToken t1 == "test"
    =
        -- Find the opening brace within a tight lookahead (max 5 tokens), stopping at `;`.
        case findOpeningBraceC 5 ts of
            Nothing -> -- Malformed or not a test block. Treat as non-test code.
                if keepTests then processOutsideZig keepTests ts else t1 : processOutsideZig keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideZig keepTests remainingTokens
                   else processOutsideZig keepTests remainingTokens

-- No test found, process the current token
processOutsideZig keepTests (t:ts) =
    if keepTests
    then processOutsideZig keepTests ts
    else t : processOutsideZig keepTests ts
