{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.D () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    tToken,
 )
import CGrep.Semantic.Tests.Common

instance LanguageTestFilter 'D where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideD keepTests tokens

-- | (D) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. `unittest { ... }` - D's built-in test blocks (where `{` immediately follows `unittest`)
--   2. `version ( unittest ) { ... }` - D's conditional compilation block for unit tests
processOutsideD :: Bool -> [Token] -> [Token]
processOutsideD _ [] = []
-- Pattern 1: `version(unittest) { ... }` (or single statement `version(unittest) import ...;`)
processOutsideD keepTests (t1 : t2 : t3 : t4 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "version"
    , isTokenBracket t2 && tToken t2 == "("
    , (isTokenKeyword t3 || isTokenIdentifier t3) && tToken t3 == "unittest"
    , isTokenBracket t4 && tToken t4 == ")" =
        case ts of
            (b : afterBrace)
                | isTokenBracket b && tToken b == "{" ->
                    let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
                     in if keepTests
                            then t1 : t2 : t3 : t4 : b : bodyTokens ++ processOutsideD keepTests remainingTokens
                            else processOutsideD keepTests remainingTokens
            _ ->
                let (stmtTokens, remainingTokens) = collectUntilSemicolon ts
                 in if keepTests
                        then t1 : t2 : t3 : t4 : stmtTokens ++ processOutsideD keepTests remainingTokens
                        else processOutsideD keepTests remainingTokens
-- Pattern 2: `unittest { ... }` (`{` must immediately follow `unittest` in D grammar)
processOutsideD keepTests (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "unittest"
    , isTokenBracket t2 && tToken t2 == "{" =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 ts
         in if keepTests
                then t1 : t2 : bodyTokens ++ processOutsideD keepTests remainingTokens
                else processOutsideD keepTests remainingTokens
-- No test found, process the current token
processOutsideD keepTests (t : ts) =
    if keepTests
        then processOutsideD keepTests ts
        else t : processOutsideD keepTests ts

-- | Collect tokens of a single non-braced statement up to `;`.
collectUntilSemicolon :: [Token] -> ([Token], [Token])
collectUntilSemicolon [] = ([], [])
collectUntilSemicolon (t : ts)
    | tToken t == ";" = ([t], ts)
    | isTokenBracket t && tToken t == "{" = ([], t : ts)
    | otherwise =
        let (collected, remaining) = collectUntilSemicolon ts
         in (t : collected, remaining)
