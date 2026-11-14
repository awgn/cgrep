{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import System.OsPath (osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Lua where
    isTestFile path =
        let ext = takeExtension path
            base = takeBaseName path
         in ext == [osp|.lua|]
                && ( [osp|_spec|] `OS.isSuffixOf` base
                        || [osp|_test|] `OS.isSuffixOf` base
                        || [osp|test_|] `OS.isPrefixOf` base
                   )

    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideLua keepTests False tokens

-- | Busted / Lust / Plenary block builders that take a description string and a callback.
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

-- | Busted block names where a 2-arg tear-off `it("desc", fn_ref)` is unambiguous
-- (excludes bare `test(a, b)` which could be a 2-arg production helper).
isBustedTearoffBlock :: Text -> Bool
isBustedTearoffBlock name =
    name
        `elem` ([ "describe"
                , "insulate"
                , "expose"
                , "context"
                , "it"
                , "spec"
                ] :: [Text])

-- | Busted lifecycle hooks that take `function() ... end`.
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

-- | Lust-specific lifecycle hooks (`lust.before(function() ... end)`).
isLustQualifiedHook :: Text -> Bool
isLustQualifiedHook name =
    isBustedHook name || name `elem` (["before", "after"] :: [Text])

-- | Unambiguous Busted lifecycle hooks that also accept tear-off function references
-- (`before_each(reset_db)`), excluding `setup(opts)` which is ubiquitous in Neovim/Lua configs.
isBustedTearoffHook :: Text -> Bool
isBustedTearoffHook name =
    name
        `elem` ([ "before_each"
                , "after_each"
                , "lazy_setup"
                , "lazy_teardown"
                ] :: [Text])

-- | Module table names commonly used for qualified Lust / Busted calls (`lust.describe(...)`).
isLuaTestModuleQualifier :: Text -> Bool
isLuaTestModuleQualifier name =
    name `elem` (["lust", "busted"] :: [Text])

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

-- | LuaUnit lifecycle method names on a `Test*` / `*Tests` suite table.
isLuaUnitLifecycle :: Text -> Bool
isLuaUnitLifecycle name =
    name `elem` (["setUp", "tearDown", "setup", "teardown"] :: [Text])

-- | Checks if a token list inside `(...)` contains a `function` keyword.
hasFunctionKeyword :: [Token] -> Bool
hasFunctionKeyword = any (\t -> isTokenKeyword t && tToken t == "function")

-- | Checks whether tokens before the closing `)` form a non-empty dotted identifier reference
-- (`fn_ref` or `M.helpers.verify`), used for tear-off callbacks.
isLuaFuncRefUntilCloseParen :: [Token] -> Bool
isLuaFuncRefUntilCloseParen (t0 : rest0)
    | isTokenIdentifier t0 = go rest0
  where
    go [c] = isTokenBracket c && tToken c == ")"
    go (d : ident : rs)
        | isTokenOperator d && tToken d == "."
        , isTokenIdentifier ident =
            go rs
    go _ = False
isLuaFuncRefUntilCloseParen _ = False

-- | Checks whether `insideParens` (which ends with `)`) is `("desc", fn_ref)`.
isStringPlusTearoff :: [Token] -> Bool
isStringPlusTearoff (tStr : tComma : rest)
    | isTokenString tStr
    , tToken tComma == "," =
        isLuaFuncRefUntilCloseParen rest
isStringPlusTearoff _ = False

-- | Parses `ident ("." ident)*` returning `(consumedTokens, mSecondToLastIdent, lastIdent, rest)`.
splitLuaIdentChain :: [Token] -> Maybe ([Token], Maybe Text, Text, [Token])
splitLuaIdentChain (t0 : ts0)
    | isTokenIdentifier t0 = Just (go [t0] Nothing (tToken t0) ts0)
  where
    go acc _ cur (tDot : tNext : rest)
        | isTokenOperator tDot && tToken tDot == "."
        , isTokenIdentifier tNext =
            go (acc ++ [tDot, tNext]) (Just cur) (tToken tNext) rest
    go acc mPrev cur rest = (acc, mPrev, cur, rest)
splitLuaIdentChain _ = Nothing

-- | Parses a Lua `function` target path (`foo`, `Suite.foo`, `M.Suite:foo`),
-- returning `(headerTokens, mSuiteName, methodName, afterTargetTokens)`.
splitLuaFunctionTarget :: [Token] -> Maybe ([Token], Maybe Text, Text, [Token])
splitLuaFunctionTarget ts = do
    (chainToks, mPrevSeg, lastSeg, afterDots) <- splitLuaIdentChain ts
    case afterDots of
        (tColon : tMethod : rest)
            | isTokenOperator tColon && tToken tColon == ":"
            , isTokenIdentifier tMethod ->
                Just (chainToks ++ [tColon, tMethod], Just lastSeg, tToken tMethod, rest)
        _ -> Just (chainToks, mPrevSeg, lastSeg, afterDots)

-- | Attempts to match a Busted or Lust block/hook call starting at the function name token.
matchBustedCall :: Bool -> [Token] -> Maybe ([Token], [Token])
-- Block `describe("...", function() ... end)`, `pending("...")`, or tear-off `it("...", fn_ref)`
matchBustedCall _ (t1 : t2 : ts@(t3 : _))
    | isTokenIdentifier t1 && isBustedBlock (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenString t3 =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
            name = tToken t1
            isValid =
                name == "pending"
                    || hasFunctionKeyword insideParens
                    || (isBustedTearoffBlock name && isStringPlusTearoff insideParens)
         in if isValid
                then Just (t1 : t2 : insideParens, remaining)
                else Nothing
-- Paren-less string call `describe "suite" (function() ... end)`
matchBustedCall _ (t1 : t2 : t3 : ts)
    | isTokenIdentifier t1 && isBustedBlock (tToken t1)
    , isTokenString t2
    , isTokenBracket t3 && tToken t3 == "(" =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
         in if hasFunctionKeyword insideParens
                then Just (t1 : t2 : t3 : insideParens, remaining)
                else Nothing
-- Lifecycle hook `setup(function() ... end)`, `lust.before(function() ... end)`, or `before_each(fn_ref)`
matchBustedCall allowLustHooks (t1 : t2 : ts@(t3 : _))
    | isTokenIdentifier t1
    , (if allowLustHooks then isLustQualifiedHook (tToken t1) else isBustedHook (tToken t1))
    , isTokenBracket t2 && tToken t2 == "(" =
        let name = tToken t1
         in if isTokenKeyword t3 && tToken t3 == "function"
                then
                    let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
                     in Just (t1 : t2 : insideParens, remaining)
                else
                    if isBustedTearoffHook name
                        then
                            let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
                             in if isLuaFuncRefUntilCloseParen insideParens
                                    then Just (t1 : t2 : insideParens, remaining)
                                    else Nothing
                        else Nothing
matchBustedCall _ _ = Nothing

-- | Processes Lua tokens outside of a test block.
-- `prevSub` tracks whether the previous token was `.` or `:` (so `obj:test("abc")` is not
-- mistaken for a Busted `test(...)` block).
processOutsideLua :: Bool -> Bool -> [Token] -> [Token]
processOutsideLua _ _ [] = []
-- Pattern 1a: Qualified Lust / Busted call `lust.describe(...)`, `lust.before(...)`
processOutsideLua keepTests False (tMod : tDot : ts)
    | isTokenIdentifier tMod && isLuaTestModuleQualifier (tToken tMod)
    , isTokenOperator tDot && tToken tDot == "."
    , Just (callToks, remaining) <- matchBustedCall True ts =
        if keepTests
            then tMod : tDot : callToks ++ processOutsideLua keepTests False remaining
            else processOutsideLua keepTests False remaining
-- Pattern 1b: Unqualified Busted / Plenary block or hook call
processOutsideLua keepTests False ts
    | Just (callToks, remaining) <- matchBustedCall False ts =
        if keepTests
            then callToks ++ processOutsideLua keepTests False remaining
            else processOutsideLua keepTests False remaining
-- Pattern 2: `local function test_foo(...) ... end`
processOutsideLua keepTests _ (tLocal : tFunc : ts)
    | isTokenKeyword tLocal && tToken tLocal == "local"
    , isTokenKeyword tFunc && tToken tFunc == "function"
    , Just (targetToks, mSuite, methodName, afterTarget) <- splitLuaFunctionTarget ts
    , isLuaUnitTestName methodName
        || (maybe False isLuaUnitSuiteName mSuite && isLuaUnitLifecycle methodName) =
        let (bodyTokens, remaining) = processInsideLuaEndBlock 1 False afterTarget
         in if keepTests
                then tLocal : tFunc : targetToks ++ bodyTokens ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 3: `function [M.]TestSuite:test_foo(...) ... end`, `function [M.]SuiteTests:setUp() ... end`,
-- or `function test_foo(...) ... end`
processOutsideLua keepTests _ (tFunc : ts)
    | isTokenKeyword tFunc && tToken tFunc == "function"
    , Just (targetToks, mSuite, methodName, afterTarget) <- splitLuaFunctionTarget ts
    , isLuaUnitTestName methodName
        || (maybe False isLuaUnitSuiteName mSuite && isLuaUnitLifecycle methodName) =
        let (bodyTokens, remaining) = processInsideLuaEndBlock 1 False afterTarget
         in if keepTests
                then tFunc : targetToks ++ bodyTokens ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 4a: `local TestCalculator = { ... }`
processOutsideLua keepTests _ (tLocal : tSuite : tEq : tBrace : ts)
    | isTokenKeyword tLocal && tToken tLocal == "local"
    , isTokenIdentifier tSuite && isLuaUnitSuiteName (tToken tSuite)
    , isTokenOperator tEq && tToken tEq == "="
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
         in if keepTests
                then tLocal : tSuite : tEq : tBrace : insideBraces ++ processOutsideLua keepTests False remaining
                else processOutsideLua keepTests False remaining
-- Pattern 4b: `[M.]TestCalculator = { ... }`
processOutsideLua keepTests False ts
    | Just (chainToks, _, lastSeg, tEq : tBrace : afterBrace) <- splitLuaIdentChain ts
    , isLuaUnitSuiteName lastSeg
    , isTokenOperator tEq && tToken tEq == "="
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 afterBrace
         in if keepTests
                then chainToks ++ (tEq : tBrace : insideBraces) ++ processOutsideLua keepTests False remaining
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
