{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.D () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'D where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideD keepTests tokens

-- | (D) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. unittest { ... } - D's built-in test blocks
--
-- D has built-in unittest blocks.
processOutsideD :: Bool -> [Token] -> [Token]
processOutsideD _ [] = [] -- End of stream

-- Pattern 1: unittest { ... }
processOutsideD keepTests (t1:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "unittest"
    =
        case findOpeningBracketBounded "{" 20 ts of
            Nothing ->
                if keepTests
                then t1 : processOutsideD keepTests ts
                else t1 : processOutsideD keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideD keepTests remainingTokens
                   else processOutsideD keepTests remainingTokens

-- No test found, process the current token
processOutsideD keepTests (t:ts) =
    if keepTests
    then processOutsideD keepTests ts
    else t : processOutsideD keepTests ts

-- ------------------------------------------------------------------
