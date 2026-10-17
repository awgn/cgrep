{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.OCaml () where

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
instance LanguageTestFilter 'OCaml where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideOCaml keepTests tokens

-- | (OCaml) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. let test_* = ... - OUnit convention
--   2. test_case - Alcotest
--
-- OCaml uses OUnit and Alcotest frameworks.
processOutsideOCaml :: Bool -> [Token] -> [Token]
processOutsideOCaml _ [] = [] -- End of stream

-- Pattern 1: let%test or let%expect_test
processOutsideOCaml keepTests (t1:t2:t3:t4:ts)
    | isTokenKeyword t1 && tToken t1 == "module" &&
      isTokenIdentifier t2 && (tToken t2 == "Test" || "Test" `T.isPrefixOf` tToken t2) &&
      isTokenOperator t3 && tToken t3 == "=" &&
      isTokenKeyword t4 && tToken t4 == "struct"
    =
        let (testTokens, remainingTokens) = collectUntilOCamlEnd ts
        in if keepTests
           then t1 : t2 : t3 : t4 : testTokens ++ processOutsideOCaml keepTests remainingTokens
           else processOutsideOCaml keepTests remainingTokens

processOutsideOCaml keepTests (t1:t2:t3:ts)
    | isTokenKeyword t1 && tToken t1 == "let" &&
      isTokenOperator t2 && tToken t2 == "%" &&
      isTokenIdentifier t3 && (tToken t3 == "test" || "test_" `T.isPrefixOf` tToken t3 || "expect_test" `T.isPrefixOf` tToken t3)
    =
        let (testTokens, remainingTokens) = collectUntilOCamlLet ts
        in if keepTests
           then t1 : t2 : t3 : testTokens ++ processOutsideOCaml keepTests remainingTokens
           else processOutsideOCaml keepTests remainingTokens

-- No test found, process the current token
processOutsideOCaml keepTests (t:ts) =
    if keepTests
    then processOutsideOCaml keepTests ts
    else t : processOutsideOCaml keepTests ts

-- ------------------------------------------------------------------

collectUntilOCamlEnd :: [Token] -> ([Token], [Token])
collectUntilOCamlEnd [] = ([], [])
collectUntilOCamlEnd (t:ts)
    | isTokenKeyword t && tToken t == "end" =
        ([t], ts)
    | otherwise =
        let (collected, remaining) = collectUntilOCamlEnd ts
        in (t : collected, remaining)


-- | (OCaml) Helper: Collects tokens until the next top-level let.
collectUntilOCamlLet :: [Token] -> ([Token], [Token])
collectUntilOCamlLet [] = ([], [])
collectUntilOCamlLet (t:ts)
    | isTokenKeyword t && tToken t == "let" =
        ([], t:ts)
    | otherwise =
        let (collected, remaining) = collectUntilOCamlLet ts
        in (t : collected, remaining)
