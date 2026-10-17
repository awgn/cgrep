{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Haskell () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'Haskell where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideHaskell keepTests tokens

-- | (Haskell) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. describe "..." - HSpec
--   2. it "..." - HSpec
--   3. context "..." - HSpec (alias for describe)
--   4. testCase "..." - Tasty/HUnit
--   5. testGroup "..." - Tasty
--   6. testProperty "..." - Tasty/QuickCheck
--   7. prop_* functions - QuickCheck convention
--
-- Haskell tests often use do-notation or $ operator, so we look for
-- these patterns and collect tokens until the next top-level definition.
processOutsideHaskell :: Bool -> [Token] -> [Token]
processOutsideHaskell _ [] = [] -- End of stream

-- Pattern 1: describe/it/context/testCase/testGroup/testProperty followed by string
processOutsideHaskell keepTests (t1:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "describe" || tToken t1 == "it" || tToken t1 == "context" ||
       tToken t1 == "testCase" || tToken t1 == "testGroup" || tToken t1 == "testProperty")
    =
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : testTokens ++ processOutsideHaskell keepTests remainingTokens
           else processOutsideHaskell keepTests remainingTokens

-- Pattern 2: prop_* function (QuickCheck convention)
processOutsideHaskell keepTests (t1:ts)
    | isTokenIdentifier t1 && "prop_" `T.isPrefixOf` tToken t1
    =
        let (propTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : propTokens ++ processOutsideHaskell keepTests remainingTokens
           else processOutsideHaskell keepTests remainingTokens

-- No test found, process the current token
processOutsideHaskell keepTests (t:ts) =
    if keepTests
    then processOutsideHaskell keepTests ts
    else t : processOutsideHaskell keepTests ts

-- | Helper: Collect tokens until we find the next top-level Haskell definition.
collectUntilNextHaskellDef :: [Token] -> ([Token], [Token])
collectUntilNextHaskellDef [] = ([], [])
-- The Haskell tokenizer splits the `::` operator into two adjacent `:` tokens.
-- Detect that sequence here to recognize the start of a new top-level type signature.
collectUntilNextHaskellDef (x1:x2:xs)
    | isTokenOperator x1 && tToken x1 == ":" &&
      isTokenOperator x2 && tToken x2 == ":" =
        ([], x1:x2:xs)
collectUntilNextHaskellDef (x:xs)
    | isTokenIdentifier x &&
      (tToken x == "describe" || tToken x == "it" || tToken x == "context" ||
       tToken x == "testCase" || tToken x == "testGroup" || tToken x == "testProperty" ||
       tToken x == "main" || "prop_" `T.isPrefixOf` tToken x) =
        ([], x:xs)
    | isTokenOperator x && tToken x == "::" =
        ([], x:xs)
    | isTokenKeyword x && (tToken x == "data" || tToken x == "type" || tToken x == "class" || tToken x == "instance" || tToken x == "newtype" || tToken x == "module") =
        ([], x:xs)
    | isTokenIdentifier x && (tToken x == "let" || tToken x == "in") =
        let (collected, remaining) = collectUntilNextHaskellDef xs
        in (x : collected, remaining)
    | isTokenOperator x && tToken x == "=" =
        let (collected, remaining) = collectUntilNextHaskellDef xs
        in (x : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilNextHaskellDef xs
        in (x : collected, remaining)
-- ------------------------------------------------------------------
