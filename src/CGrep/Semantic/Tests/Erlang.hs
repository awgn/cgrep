{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Erlang () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'Erlang where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideErlang keepTests tokens

-- | (Erlang) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. *_test() - EUnit convention
--   2. *_test_() - EUnit generator convention
--
-- Erlang uses EUnit framework.

collectUntilErlangEnd :: [Token] -> ([Token], [Token])
collectUntilErlangEnd [] = ([], [])
collectUntilErlangEnd (t:ts)
    | isTokenOperator t && tToken t == "." =
        ([t], ts)
    | otherwise =
        let (collected, remaining) = collectUntilErlangEnd ts
        in (t : collected, remaining)

processOutsideErlang :: Bool -> [Token] -> [Token]
processOutsideErlang _ [] = [] -- End of stream

-- Pattern 1: *_test() or *_test_()
processOutsideErlang keepTests (t1:ts)
    | isTokenIdentifier t1 && ("_test" `T.isSuffixOf` tToken t1 || "_test_" `T.isSuffixOf` tToken t1)
    =
        let (testTokens, remainingTokens) = collectUntilErlangEnd ts
        in if keepTests
           then t1 : testTokens ++ processOutsideErlang keepTests remainingTokens
           else processOutsideErlang keepTests remainingTokens

-- No test found, process the current token
processOutsideErlang keepTests (t:ts) =
    if keepTests
    then processOutsideErlang keepTests ts
    else t : processOutsideErlang keepTests ts


-- ------------------------------------------------------------------
