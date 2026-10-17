{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Perl () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Perl where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePerl keepTests tokens

-- | (Perl) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. Test files (*.t)
--   2. subtest blocks
--
-- Perl uses Test::More, Test::Simple frameworks.
-- Note: Perl tests are less structured, so this is a simplified approach.
processOutsidePerl :: Bool -> [Token] -> [Token]
processOutsidePerl _ [] = [] -- End of stream

-- Pattern 1: subtest
processOutsidePerl keepTests (t1:ts)
    | isTokenIdentifier t1 && (tToken t1 == "subtest" || tToken t1 == "test")
    =
        case findOpeningBracketBounded "{" 20 ts of
            Nothing ->
                if keepTests
                then t1 : processOutsidePerl keepTests ts
                else t1 : processOutsidePerl keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsidePerl keepTests remainingTokens
                   else processOutsidePerl keepTests remainingTokens

-- No test found, process the current token
processOutsidePerl keepTests (t:ts) =
    if keepTests
    then processOutsidePerl keepTests ts
    else t : processOutsidePerl keepTests ts

-- ------------------------------------------------------------------
