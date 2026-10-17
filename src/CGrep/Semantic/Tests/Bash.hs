{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Bash () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Bash where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens =
        let isBats = hasBatsTestBlock tokens
         in processOutsideBash keepTests isBats tokens

-- | Checks whether the token stream contains at least one BATS `@test "..." {` block.
hasBatsTestBlock :: [Token] -> Bool
hasBatsTestBlock (tAt : tTest : tDesc : tBrace : _)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tTest && tToken tTest == "test"
    , isTokenString tDesc
    , isTokenBracket tBrace && tToken tBrace == "{" =
        True
hasBatsTestBlock (_ : ts) = hasBatsTestBlock ts
hasBatsTestBlock [] = False

-- | Checks if a Bash function name is a shUnit2 / bashunit test function (`test_foo`, `testFoo`, `test1`),
-- avoiding false positives on words like `testify`, `testing`, or bare `test`.
isBashTestFuncName :: Bool -> Text -> Bool
isBashTestFuncName isBats name =
    hasTestPrefix name
        || isTestLifecycleName name
        || (isBats && (name == "setup" || name == "teardown"))
  where
    hasTestPrefix txt =
        case T.stripPrefix "test" txt of
            Just rest ->
                case T.uncons rest of
                    Just (c, _) -> not (isAsciiLower c)
                    Nothing -> False
            Nothing -> False

    isTestLifecycleName n =
        n
            `elem` ([ "oneTimeSetUp"
                    , "oneTimeTearDown"
                    , "setUp"
                    , "tearDown"
                    , "set_up"
                    , "tear_down"
                    , "set_up_before_script"
                    , "tear_down_after_script"
                    , "setup_file"
                    , "teardown_file"
                    ] :: [Text])

-- | Processes Bash tokens outside of a test block.
processOutsideBash :: Bool -> Bool -> [Token] -> [Token]
processOutsideBash _ _ [] = []
-- Pattern 1: BATS `@test "description" { ... }`
processOutsideBash keepTests isBats (tAt : tTest : tDesc : tBrace : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tTest && tToken tTest == "test"
    , isTokenString tDesc
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tAt : tTest : tDesc : tBrace : insideBraces ++ processOutsideBash keepTests isBats remaining
                else processOutsideBash keepTests isBats remaining
-- Pattern 2a: `function test_foo() { ... }`
processOutsideBash keepTests isBats (tFunc : tName : tLParen : tRParen : tBrace : ts)
    | isTokenKeyword tFunc && tToken tFunc == "function"
    , isTokenIdentifier tName && isBashTestFuncName isBats (tToken tName)
    , isTokenBracket tLParen && tToken tLParen == "("
    , isTokenBracket tRParen && tToken tRParen == ")"
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tFunc : tName : tLParen : tRParen : tBrace : insideBraces ++ processOutsideBash keepTests isBats remaining
                else processOutsideBash keepTests isBats remaining
-- Pattern 2b: `function test_foo { ... }` (ksh/bash style without parens)
processOutsideBash keepTests isBats (tFunc : tName : tBrace : ts)
    | isTokenKeyword tFunc && tToken tFunc == "function"
    , isTokenIdentifier tName && isBashTestFuncName isBats (tToken tName)
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tFunc : tName : tBrace : insideBraces ++ processOutsideBash keepTests isBats remaining
                else processOutsideBash keepTests isBats remaining
-- Pattern 3: `test_foo() { ... }` (POSIX/shUnit2/bashunit style)
processOutsideBash keepTests isBats (tName : tLParen : tRParen : tBrace : ts)
    | isTokenIdentifier tName && isBashTestFuncName isBats (tToken tName)
    , isTokenBracket tLParen && tToken tLParen == "("
    , isTokenBracket tRParen && tToken tRParen == ")"
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tName : tLParen : tRParen : tBrace : insideBraces ++ processOutsideBash keepTests isBats remaining
                else processOutsideBash keepTests isBats remaining
-- Fallback
processOutsideBash keepTests isBats (t : ts) =
    if keepTests
        then processOutsideBash keepTests isBats ts
        else t : processOutsideBash keepTests isBats ts
