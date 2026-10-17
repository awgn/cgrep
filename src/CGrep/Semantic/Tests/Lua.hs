{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Lua () where

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

instance LanguageTestFilter 'Lua where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideLua keepTests False tokens

-- | Busted / Lust block builders that take a description string and a callback function.
isBustedBlock :: Text -> Bool
isBustedBlock name =
    name
        `elem` ([ "describe"
                , "insulate"
                , "expose"
                , "context"
                , "it"
                , "spec"
                , "test"
                , "pending"
                ] :: [Text])

-- | Busted lifecycle hooks that take a callback function `setup(function() ... end)`.
isBustedHook :: Text -> Bool
isBustedHook name =
    name
        `elem` ([ "setup"
                , "teardown"
                , "before_each"
                , "after_each"
                , "lazy_setup"
                , "lazy_teardown"
                , "finally"
                ] :: [Text])

-- | LuaUnit test function/method name (`test_foo`, `testFoo`, `TestFoo`, `test1`),
-- avoiding false positives on words like `testify`, `testing`, or bare `test`.
isLuaUnitTestName :: Text -> Bool
isLuaUnitTestName name =
    checkPrefix "test" name || checkPrefix "Test" name
  where
    checkPrefix pref txt =
        case T.stripPrefix pref txt of
            Just rest ->
                case T.uncons rest of
                    Just (c, _) -> not (isAsciiLower c)
                    Nothing -> False
            Nothing -> False

-- | LuaUnit suite table name (`TestCalculator`, `Test_Math`, `CalculatorTest`, `CalculatorTests`).
isLuaUnitSuiteName :: Text -> Bool
isLuaUnitSuiteName name =
    checkTestPrefix name
        || ("Test" `T.isSuffixOf` name && T.length name > 4)
        || ("Tests" `T.isSuffixOf` name && T.length name > 5)
  where
    checkTestPrefix txt =
        case T.stripPrefix "Test" txt of
            Just rest ->
                case T.uncons rest of
                    Just (c, _) -> not (isAsciiLower c)
                    Nothing -> False
            Nothing -> False

-- | LuaUnit lifecycle method names on a `Test*` suite table.
isLuaUnitLifecycle :: Text -> Bool
isLuaUnitLifecycle name =
    name `elem` (["setUp", "tearDown", "setup", "teardown"] :: [Text])

-- | Checks if a token list inside `(...)` contains a `function` keyword.
hasFunctionKeyword :: [Token] -> Bool
hasFunctionKeyword = any (\t -> isTokenKeyword t && tToken t == "function")

-- | Processes Lua tokens outside of a test block.
-- `prevSub` tracks whether the previous token was `.` or `:` (so `obj:test("abc")` is not
-- mistaken for a Busted `test(...)` block).
processOutsideLua :: Bool -> Bool -> [Token] -> [Token]
processOutsideLua _ _ [] = []
-- Pattern 1a: Busted block `describe("...", function() ... end)` or `pending("...")`
processOutsideLua keepTests False (t1 : t2 : ts@(t3 : _))
    | isTokenIdentifier t1 && isBustedBlock (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenString t3 =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
            isValidBusted = tToken t1 == "pending" || hasFunctionKeyword insideParens
         in if isValidBusted
                then
                    if keepTests
                        then t1 : t2 : insideParens ++ processOutsideLua keepTests False remaining
                        else processOutsideLua keepTests False remaining
                else
                    if keepTests
                        then processOutsideLua keepTests False (t2 : ts)
                        else t1 : processOutsideLua keepTests False (t2 : ts)
-- Pattern 1b: Paren-less Busted string call `describe "suite" (function() ... end)`
processOutsideLua keepTests False (t1 : t2 : t3 : ts)
    | isTokenIdentifier t1 && isBustedBlock (tToken t1)
    , isTokenString t2
    , isTokenBracket t3 && tToken t3 == "(" =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
         in if hasFunctionKeyword insideParens
                then
                    if keepTests
                        then t1 : t2 : t3 : insideParens ++ processOutsideLua keepTests False remaining
                        else processOutsideLua keepTests False remaining
                else
                    if keepTests
                        then processOutsideLua keepTests False (t2 : t3 : ts)
                        else t1 : processOutsideLua keepTests False (t2 : t3 : ts)
-- Pattern 2: Busted lifecycle hook `setup(function() ... end)`
processOutsideLua keepTests False (t1 : t2 : ts@(t3 : _))
    | isTokenIdentifier t1 && isBustedHook (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenKeyword t3 && tToken t3 == "function" =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
         in if keepTests
                then t1 : t2 : insideParens ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 3: `local function test_foo(...) ... end`
processOutsideLua keepTests _ (tLocal : tFunc : tName : ts)
    | isTokenKeyword tLocal && tToken tLocal == "local"
    , isTokenKeyword tFunc && tToken tFunc == "function"
    , isTokenIdentifier tName && isLuaUnitTestName (tToken tName) =
        let (bodyTokens, remaining) = processInsideLuaEndBlock 1 False ts
         in if keepTests
                then tLocal : tFunc : tName : bodyTokens ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 4a: `function TestSuite:test_foo(...) ... end` or `function TestSuite.test_foo(...) ... end`
-- or `function TestSuite:setUp() ... end`
processOutsideLua keepTests _ (tFunc : tSuite : tSep : tMethod : ts)
    | isTokenKeyword tFunc && tToken tFunc == "function"
    , isTokenIdentifier tSuite
    , isTokenOperator tSep && (tToken tSep == ":" || tToken tSep == ".")
    , isTokenIdentifier tMethod
    , isLuaUnitTestName (tToken tMethod)
        || (isLuaUnitSuiteName (tToken tSuite) && isLuaUnitLifecycle (tToken tMethod)) =
        let (bodyTokens, remaining) = processInsideLuaEndBlock 1 False ts
         in if keepTests
                then tFunc : tSuite : tSep : tMethod : bodyTokens ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 4b: `function test_foo(...) ... end`
processOutsideLua keepTests _ (tFunc : tName : ts)
    | isTokenKeyword tFunc && tToken tFunc == "function"
    , isTokenIdentifier tName && isLuaUnitTestName (tToken tName) =
        let (bodyTokens, remaining) = processInsideLuaEndBlock 1 False ts
         in if keepTests
                then tFunc : tName : bodyTokens ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 5: LuaUnit table declaration `TestCalculator = { ... }` (or `local TestCalculator = { ... }`)
processOutsideLua keepTests _ (tLocal : tSuite : tEq : tBrace : ts)
    | isTokenKeyword tLocal && tToken tLocal == "local"
    , isTokenIdentifier tSuite && isLuaUnitSuiteName (tToken tSuite)
    , isTokenOperator tEq && tToken tEq == "="
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tLocal : tSuite : tEq : tBrace : insideBraces ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
processOutsideLua keepTests False (tSuite : tEq : tBrace : ts)
    | isTokenIdentifier tSuite && isLuaUnitSuiteName (tToken tSuite)
    , isTokenOperator tEq && tToken tEq == "="
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tSuite : tEq : tBrace : insideBraces ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Fallback
processOutsideLua keepTests _ (t : ts) =
    let isSub = isTokenOperator t && (tToken t == "." || tToken t == ":")
     in if keepTests
            then processOutsideLua keepTests isSub ts
            else t : processOutsideLua keepTests isSub ts

-- | Balances a Lua block terminated by `end`.
-- In Lua, only `function`, `if`, and `do` open a block that is closed by `end`
-- (`while ... do ... end` and `for ... do ... end` include `do`, while `repeat ... until`
-- is closed by `until`, not `end`).
processInsideLuaEndBlock :: Int -> Bool -> [Token] -> ([Token], [Token])
processInsideLuaEndBlock 0 _ ts = ([], ts)
processInsideLuaEndBlock _ _ [] = ([], [])
processInsideLuaEndBlock level prevSub (t : ts)
    | not prevSub
    , isTokenKeyword t
    , tToken t `elem` (["function", "if", "do"] :: [Text]) =
        let (rest, remTokens) = processInsideLuaEndBlock (level + 1) False ts
         in (t : rest, remTokens)
    | not prevSub
    , isTokenKeyword t
    , tToken t == "end" =
        if level == 1
            then ([t], ts)
            else
                let (rest, remTokens) = processInsideLuaEndBlock (level - 1) False ts
                 in (t : rest, remTokens)
    | otherwise =
        let isSub = isTokenOperator t && (tToken t == "." || tToken t == ":")
            (rest, remTokens) = processInsideLuaEndBlock level isSub ts
         in (t : rest, remTokens)
