{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Clojure () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Clojure where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideClojure keepTests tokens

-- | (Clojure) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. (deftest ...)
--   2. (testing ...)
--
-- Clojure uses clojure.test.
processOutsideClojure :: Bool -> [Token] -> [Token]
processOutsideClojure _ [] = [] -- End of stream

-- Pattern 1: (deftest ...) or (testing ...)
processOutsideClojure keepTests (t1:t2:ts)
    | isTokenBracket t1 && tToken t1 == "(" &&
      isTokenIdentifier t2 && (tToken t2 == "deftest" || tToken t2 == "testing")
    =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then t1 : t2 : bodyTokens ++ processOutsideClojure keepTests remainingTokens
           else processOutsideClojure keepTests remainingTokens

-- No test found, process the current token
processOutsideClojure keepTests (t:ts) =
    if keepTests
    then processOutsideClojure keepTests ts
    else t : processOutsideClojure keepTests ts

-- | Helper: Collect tokens until matching closing parenthesis
collectUntilMatchingParen :: Int -> [Token] -> ([Token], [Token])
collectUntilMatchingParen 0 ts = ([], ts)
collectUntilMatchingParen _ [] = ([], [])
collectUntilMatchingParen depth (t:ts)
    | isTokenBracket t && tToken t == "(" =
        let (collected, remaining) = collectUntilMatchingParen (depth + 1) ts
        in (t : collected, remaining)
    | isTokenBracket t && tToken t == ")" =
        if depth == 1
        then ([t], ts)
        else let (collected, remaining) = collectUntilMatchingParen (depth - 1) ts
             in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilMatchingParen depth ts
        in (t : collected, remaining)

-- ------------------------------------------------------------------
