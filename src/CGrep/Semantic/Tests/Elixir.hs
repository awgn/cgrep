{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Elixir () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'Elixir where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideElixir keepTests tokens

-- | (Elixir) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test "..." - ExUnit test
--   2. describe "..." - ExUnit describe block
--   3. defmodule *Test - Test module
--
-- Elixir uses ExUnit with do...end blocks.
processOutsideElixir :: Bool -> [Token] -> [Token]
processOutsideElixir _ [] = [] -- End of stream

-- Pattern 1: test, describe, setup, setup_all followed by string/block
processOutsideElixir keepTests (t1:ts)
    | isTokenIdentifier t1 && (tToken t1 == "test" || tToken t1 == "describe" || tToken t1 == "setup" || tToken t1 == "setup_all")
    =
        -- Collect until we find 'end' keyword
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : testTokens ++ processOutsideElixir keepTests remainingTokens
           else processOutsideElixir keepTests remainingTokens

-- Pattern 2: defmodule ...Test
processOutsideElixir keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "defmodule" &&
      isTokenIdentifier t2 && "Test" `T.isSuffixOf` tToken t2
    =
        -- Collect test module until 'end'
        let (moduleTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : t2 : moduleTokens ++ processOutsideElixir keepTests remainingTokens
           else processOutsideElixir keepTests remainingTokens

-- No test found, process the current token
processOutsideElixir keepTests (t:ts) =
    if keepTests
    then processOutsideElixir keepTests ts
    else t : processOutsideElixir keepTests ts

-- | Helper: Collect tokens until we find 'end' keyword in Elixir.
-- This handles do...end blocks.
collectUntilElixirEnd :: [Token] -> ([Token], [Token])
collectUntilElixirEnd = go 0
  where
    go :: Int -> [Token] -> ([Token], [Token])
    go _ [] = ([], [])
    go depth (t:ts)
        -- Found 'do' or 'fn' - increase depth
        | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "do" || tToken t == "fn") =
            let (collected, remaining) = go (depth + 1) ts
            in (t : collected, remaining)
        -- Found 'end' - decrease depth or finish
        | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
            if depth <= 1
            then ([t], ts) -- Include final 'end' and finish
            else
                let (collected, remaining) = go (depth - 1) ts
                in (t : collected, remaining)
        -- Found another definition that doesn't use 'do' but might need tracking
        -- Note: this is simplified, Elixir macros can be complex
        | otherwise =
            let (collected, remaining) = go depth ts
            in (t : collected, remaining)

-- ------------------------------------------------------------------
