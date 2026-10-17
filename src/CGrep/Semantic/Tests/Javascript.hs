{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Javascript () where

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
import Data.Text (Text)

instance LanguageTestFilter 'Javascript where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJavascript keepTests False tokens

-- | Standard Jest / Mocha / Jasmine / Vitest / Node test runner identifiers
isJsTestBlockName :: Text -> Bool
isJsTestBlockName name =
    name
        `elem` ([ "describe"
                , "fdescribe"
                , "xdescribe"
                , "it"
                , "fit"
                , "xit"
                , "test"
                , "xtest"
                , "context"
                , "suite"
                , "specify"
                , "beforeEach"
                , "afterEach"
                , "beforeAll"
                , "afterAll"
                , "before"
                , "after"
                ] :: [Text])

-- | Chained modifiers allowed on test/describe/it/suite/context
isJsTestModifier :: Text -> Bool
isJsTestModifier name =
    name
        `elem` ([ "only"
                , "skip"
                , "todo"
                , "concurrent"
                , "sequential"
                , "shuffle"
                , "failing"
                , "fails"
                , "skipIf"
                , "runIf"
                , "each"
                , "for"
                ] :: [Text])

-- | Check whether a sequence of tokens contains `=>` (either as single token `"= >"`
-- in strict mode or two tokens `"="` `">"` in non-strict mode) or the `function` keyword.
hasCallbackTokens :: [Token] -> Bool
hasCallbackTokens [] = False
hasCallbackTokens (t1 : t2 : ts)
    | tToken t1 == "=" && tToken t2 == ">" = True
    | otherwise = isCallbackSingleToken t1 || hasCallbackTokens (t2 : ts)
hasCallbackTokens [t1] = isCallbackSingleToken t1

isCallbackSingleToken :: Token -> Bool
isCallbackSingleToken t =
    tToken t == "=>"
        || (isTokenKeyword t && tToken t == "function")
        || tToken t == "function"

-- | Consume optional chained modifiers `.only`, `.skip`, `.each(...)`, etc.,
-- starting after the root test identifier (e.g. `describe`, `it`, `test`).
-- Returns `Just (capturedHeaderAndBodyTokens, remainingTokens)` if a valid
-- test call `(...)` is matched.
matchJsTestCall :: [Token] -> Bool -> Maybe ([Token], [Token])
matchJsTestCall (tDot : tMod : rest) _
    | isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tMod && isJsTestModifier (tToken tMod) =
        let isTodo = tToken tMod == "todo"
         in case rest of
                -- e.g. .each([1, 2, 3]) or .skipIf(cond) followed by ('name', () => ...) or .only(...)
                (tOpen : afterOpen)
                    | isTokenBracket tOpen && tToken tOpen == "(" ->
                        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 afterOpen
                         in case afterArgs of
                                -- Curried call: .each(...)('name', () => { ... })
                                (tOpen2 : afterOpen2)
                                    | isTokenBracket tOpen2 && tToken tOpen2 == "(" ->
                                        let (bodyTokens, remaining) = processInsideBrackets "(" ")" 1 afterOpen2
                                         in if isTodo || hasCallbackTokens argTokens || hasCallbackTokens bodyTokens
                                                then Just (tDot : tMod : tOpen : argTokens ++ (tOpen2 : bodyTokens), remaining)
                                                else Nothing
                                -- Chained after curried call: .each(...).only(...)
                                _ ->
                                    case matchJsTestCall afterArgs True of
                                        Just (moreTokens, remaining) ->
                                            Just (tDot : tMod : tOpen : argTokens ++ moreTokens, remaining)
                                        Nothing ->
                                            if isTodo || hasCallbackTokens argTokens
                                                then Just (tDot : tMod : tOpen : argTokens, afterArgs)
                                                else Nothing
                -- Chained modifier or tagged template literal
                _ ->
                    case matchJsTestCall rest True of
                        Just (moreTokens, remaining) ->
                            Just (tDot : tMod : moreTokens, remaining)
                        Nothing -> Nothing
matchJsTestCall (tOpen : afterOpen) seenMod
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (bodyTokens, remaining) = processInsideBrackets "(" ")" 1 afterOpen
         in if hasCallbackTokens bodyTokens || seenMod
                then Just (tOpen : bodyTokens, remaining)
                else Nothing
matchJsTestCall _ _ = Nothing

-- | (Javascript / TypeScript) Helper: Processes tokens *outside* a test block.
-- The `Bool` parameter `afterDot` tracks whether the immediately preceding token
-- was `.` (or `?.`), preventing `regex.test(...)` from being mistaken for a test block.
processOutsideJavascript :: Bool -> Bool -> [Token] -> [Token]
processOutsideJavascript _ _ [] = []
processOutsideJavascript keepTests False (t1 : ts)
    | isTokenIdentifier t1 && isJsTestBlockName (tToken t1)
    , Just (callTokens, remainingTokens) <- matchJsTestCall ts False =
        if keepTests
            then t1 : callTokens ++ processOutsideJavascript keepTests False remainingTokens
            else processOutsideJavascript keepTests False remainingTokens
processOutsideJavascript keepTests _ (t : ts) =
    let isDot = isTokenOperator t && (tToken t == "." || tToken t == "?.")
     in if keepTests
            then processOutsideJavascript keepTests isDot ts
            else t : processOutsideJavascript keepTests isDot ts
