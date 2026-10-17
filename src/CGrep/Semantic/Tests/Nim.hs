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
    isTokenOperator,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)

instance LanguageTestFilter 'Nim where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideNim keepTests tokens

-- | (Nim) Helper: Processes tokens *outside* a test block.
-- Recognizes `std/unittest` blocks:
--   1. `suite "..." :`
--   2. `test "..." :`
processOutsideNim :: Bool -> [Token] -> [Token]
processOutsideNim _ [] = []
-- Pattern 1: `suite "name":` or `test "name":`
processOutsideNim keepTests (t1 : t2 : t3 : ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1)
        && (tToken t1 == "suite" || tToken t1 == "test")
    , isTokenString t2
    , isTokenOperator t3 && tToken t3 == ":" =
        let (testTokens, remainingTokens) = collectUntilNimDef ts
         in if keepTests
                then t1 : t2 : t3 : testTokens ++ processOutsideNim keepTests remainingTokens
                else processOutsideNim keepTests remainingTokens
-- No test found, process the current token
processOutsideNim keepTests (t : ts) =
    if keepTests
        then processOutsideNim keepTests ts
        else t : processOutsideNim keepTests ts

-- | (Nim) Helper: Collects tokens until the next top-level named routine/type definition
-- or `suite "..." :`, ignoring anonymous `proc(...)` expressions/types inside tests.
collectUntilNimDef :: [Token] -> ([Token], [Token])
collectUntilNimDef [] = ([], [])
collectUntilNimDef tokens@(t1 : t2 : t3 : _)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "suite"
    , isTokenString t2
    , isTokenOperator t3 && tToken t3 == ":" =
        ([], tokens)
collectUntilNimDef tokens@(t1 : t2 : _)
    | (isTokenKeyword t1 || isTokenIdentifier t1)
        && ( tToken t1
                `elem` ([ "proc"
                        , "func"
                        , "iterator"
                        , "macro"
                        , "template"
                        , "method"
                        , "converter"
                        , "type"
                        ] :: [Text])
           )
    , isTokenIdentifier t2 =
        ([], tokens)
collectUntilNimDef (t : ts) =
    let (collected, remaining) = collectUntilNimDef ts
     in (t : collected, remaining)
