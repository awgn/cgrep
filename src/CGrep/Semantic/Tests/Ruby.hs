{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Ruby () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'Ruby where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideRuby keepTests tokens

-- | (Ruby) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. describe "..." - RSpec
--   2. context "..." - RSpec (alias for describe)
--   3. it "..." - RSpec
--   4. def test_* - Minitest
--   5. class Test* - Minitest
--
-- Ruby uses RSpec (BDD) and Minitest frameworks.
processOutsideRuby :: Bool -> [Token] -> [Token]
processOutsideRuby _ [] = [] -- End of stream

-- Pattern 1: describe/context/it (RSpec)
processOutsideRuby keepTests tokens@(t1:_)
    | isTokenIdentifier t1 &&
      (tToken t1 == "describe" || tToken t1 == "context" || tToken t1 == "it")
    =
        -- Collect until 'end' keyword
        let (testTokens, remainingTokens) = collectUntilRubyEnd tokens
        in if keepTests
           then testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- Pattern 2: def test_*, setup, teardown (Minitest)
processOutsideRuby keepTests tokens@(t1:t2:_)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "def" &&
      isTokenIdentifier t2 && ("test_" `T.isPrefixOf` tToken t2 || tToken t2 == "setup" || tToken t2 == "teardown")
    =
        let (testTokens, remainingTokens) = collectUntilRubyEnd tokens
        in if keepTests
           then testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- Pattern 3: class Test* (Minitest)
processOutsideRuby keepTests tokens@(t1:t2:_)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 && "Test" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilRubyEnd tokens
        in if keepTests
           then testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- No test found, process the current token
processOutsideRuby keepTests (t:ts) =
    if keepTests
    then processOutsideRuby keepTests ts
    else t : processOutsideRuby keepTests ts

-- | Helper: Collect tokens until we find 'end' keyword in Ruby.
-- This handles do...end, def...end, class...end blocks.
collectUntilRubyEnd :: [Token] -> ([Token], [Token])
collectUntilRubyEnd = go 0
  where
    go :: Int -> [Token] -> ([Token], [Token])
    go _ [] = ([], [])
    go depth (t:ts)
        -- Found block starter - increase depth
        | (isTokenKeyword t || isTokenIdentifier t) &&
          (tToken t == "do" || tToken t == "def" || tToken t == "class" || tToken t == "module" ||
           tToken t == "if" || tToken t == "unless" || tToken t == "while" || tToken t == "until" ||
           tToken t == "for" || tToken t == "case" || tToken t == "begin") =
            let (collected, remaining) = go (depth + 1) ts
            in (t : collected, remaining)
        -- Found 'end' - decrease depth or finish
        | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
            if depth <= 1
            then ([t], ts) -- Include final 'end' and finish
            else let (collected, remaining) = go (depth - 1) ts
                 in (t : collected, remaining)
        -- Other tokens
        | otherwise =
            let (collected, remaining) = go depth ts
            in (t : collected, remaining)

-- ------------------------------------------------------------------
