{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Julia () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import qualified Data.Text as T
instance LanguageTestFilter 'Julia where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJulia keepTests tokens

-- | (Julia) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. @testset "..." begin ... end
--   2. @test expression
--
-- Julia uses Test standard library.
processOutsideJulia :: Bool -> [Token] -> [Token]
processOutsideJulia _ [] = [] -- End of stream

-- Pattern 1: @test, @testset, @test_throws, etc.
processOutsideJulia keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && ("test" `T.isPrefixOf` tToken t2)
    =
        let (testTokens, remainingTokens) = collectJuliaTest ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideJulia keepTests remainingTokens
           else processOutsideJulia keepTests remainingTokens

-- No test found, process the current token
processOutsideJulia keepTests (t:ts) =
    if keepTests
    then processOutsideJulia keepTests ts
    else t : processOutsideJulia keepTests ts

collectJuliaTest :: [Token] -> ([Token], [Token])
collectJuliaTest [] = ([], [])
collectJuliaTest (t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "begin" || tToken t == "for") =
        let (bodyTokens, remaining) = collectJuliaBlock 1 ts
        in (t : bodyTokens, remaining)
    | isTokenBracket t && tToken t == "(" =
        let (bodyTokens, remaining) = processInsideBrackets "(" ")" 1 ts
        in (t : bodyTokens, remaining)
    | isTokenOperator t && tToken t == "@" = ([], t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "function" || tToken t == "struct" || tToken t == "mutable" || tToken t == "macro" || tToken t == "module" || tToken t == "end") = ([], t:ts)
    | otherwise =
        let (collected, remaining) = collectJuliaTest ts
        in (t : collected, remaining)

collectJuliaBlock :: Int -> [Token] -> ([Token], [Token])
collectJuliaBlock 0 ts = ([], ts)
collectJuliaBlock _ [] = ([], [])
collectJuliaBlock depth (t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "begin" || tToken t == "function" || tToken t == "struct" || tToken t == "macro" || tToken t == "let" || tToken t == "quote" || tToken t == "do" || tToken t == "if" || tToken t == "for" || tToken t == "while" || tToken t == "try") =
        let (collected, remaining) = collectJuliaBlock (depth + 1) ts
        in (t : collected, remaining)
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
        if depth == 1
        then ([t], ts)
        else
            let (collected, remaining) = collectJuliaBlock (depth - 1) ts
            in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectJuliaBlock depth ts
        in (t : collected, remaining)

-- ------------------------------------------------------------------
