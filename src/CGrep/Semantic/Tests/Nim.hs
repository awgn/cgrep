{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Nim () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
instance LanguageTestFilter 'Nim where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideNim keepTests tokens

-- | (Nim) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. suite "..."
--   2. test "..."
--
-- Nim uses unittest module.
processOutsideNim :: Bool -> [Token] -> [Token]
processOutsideNim _ [] = [] -- End of stream

-- Pattern 1: suite "name": or test "name":
processOutsideNim keepTests (t1:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && (tToken t1 == "suite" || tToken t1 == "test")
    =
        let (testTokens, remainingTokens) = collectUntilNimDef ts
        in if keepTests
           then t1 : testTokens ++ processOutsideNim keepTests remainingTokens
           else processOutsideNim keepTests remainingTokens

-- No test found, process the current token
processOutsideNim keepTests (t:ts) =
    if keepTests
    then processOutsideNim keepTests ts
    else t : processOutsideNim keepTests ts

-- ------------------------------------------------------------------

-- | (Nim) Helper: Collects tokens until the next top-level def.
collectUntilNimDef :: [Token] -> ([Token], [Token])
collectUntilNimDef [] = ([], [])
collectUntilNimDef (t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "proc" || tToken t == "func" || tToken t == "iterator" || tToken t == "macro" || tToken t == "template" || tToken t == "method" || tToken t == "suite") =
        ([], t:ts)
    | otherwise =
        let (collected, remaining) = collectUntilNimDef ts
        in (t : collected, remaining)

