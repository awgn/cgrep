{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Control.Applicative ((<|>))
import Data.Text (Text)
import System.OsPath (OsPath, osp, splitDirectories, takeBaseName, takeFileName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Javascript where
    isTestFile = isJavascriptTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJavascript keepTests False False tokens

-- | Jest / Vitest / Mocha / Playwright conventions (`*.test.*`, `*.spec.*`, `__tests__/`).
isJavascriptTestFilePath :: OsPath -> Bool
isJavascriptTestFilePath path =
    let base = takeBaseName path
        dirs = map takeFileName (dropLastPart (splitDirectories path))
     in [osp|.test|] `OS.isSuffixOf` base
            || [osp|.spec|] `OS.isSuffixOf` base
            || [osp|__tests__|] `elem` dirs
  where
    dropLastPart [] = []
    dropLastPart [_] = []
    dropLastPart (x : xs) = x : dropLastPart xs

isJsHookName :: Text -> Bool
isJsHookName = \case
    "beforeEach" -> True
    "afterEach" -> True
    "beforeAll" -> True
    "afterAll" -> True
    "before" -> True
    "after" -> True
    _ -> False

-- | Standard Jest / Mocha / Jasmine / Vitest / Playwright / Node test runner identifiers.
isJsTestBlockName :: Text -> Bool
isJsTestBlockName = \case
    "describe" -> True
    "fdescribe" -> True
    "xdescribe" -> True
    "it" -> True
    "fit" -> True
    "xit" -> True
    "test" -> True
    "ftest" -> True
    "xtest" -> True
    "context" -> True
    "fcontext" -> True
    "xcontext" -> True
    "suite" -> True
    "specify" -> True
    name -> isJsHookName name

-- | Chained modifiers & sub-APIs allowed on `test`/`describe`/`it`/`suite`/`Deno.test`/`QUnit`.
isJsTestModifier :: Text -> Bool
isJsTestModifier = \case
    "only" -> True
    "skip" -> True
    "todo" -> True
    "fixme" -> True
    "ignore" -> True
    "concurrent" -> True
    "sequential" -> True
    "serial" -> True
    "shuffle" -> True
    "failing" -> True
    "fails" -> True
    "skipIf" -> True
    "runIf" -> True
    "each" -> True
    "for" -> True
    "describe" -> True
    "beforeEach" -> True
    "afterEach" -> True
    "beforeAll" -> True
    "afterAll" -> True
    "step" -> True
    "slow" -> True
    "use" -> True
    "cb" -> True
    "always" -> True
    "macro" -> True
    _ -> False

isNoCallbackModifier :: Text -> Bool
isNoCallbackModifier = \case
    "todo" -> True
    "fixme" -> True
    "slow" -> True
    "use" -> True
    _ -> False

-- | Check whether a sequence of tokens contains `=>` or the `function` keyword.
hasCallbackTokens :: [Token] -> Bool
hasCallbackTokens [] = False
hasCallbackTokens (t1 : t2 : ts)
    | tToken t1 == "=" && tToken t2 == ">" && areAdjacentTokens t1 t2 = True
    | otherwise = isCallbackSingleToken t1 || hasCallbackTokens (t2 : ts)
hasCallbackTokens [t1] = isCallbackSingleToken t1

isCallbackSingleToken :: Token -> Bool
isCallbackSingleToken t =
    tToken t == "=>"
        || ((isTokenKeyword t || isTokenIdentifier t) && tToken t == "function")

-- | Detect TypeScript parameter type annotations (`test(cb: () => void);` or `test(cb?: () => void);`)
-- at paren depth 0 inside `(...)`, distinguishing them from ternary `cond ? a : b` expressions.
hasTopLevelTsParamColon :: [Token] -> Bool
hasTopLevelTsParamColon = go 0 0
  where
    go :: Int -> Int -> [Token] -> Bool
    go _ _ [] = False
    go !depth !pendingQ (t : rest)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (depth + 1) pendingQ rest
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (depth - 1)) pendingQ rest
        | depth == 0 && isTokenOperator t && tToken t == "?:" =
            True
        | depth == 0 && isTokenOperator t && tToken t == "?" =
            case rest of
                (nxt : _) | isTokenOperator nxt && tToken nxt == ":" -> True
                _ -> go 0 (pendingQ + 1) rest
        | depth == 0 && isTokenOperator t && tToken t == ":" =
            if pendingQ > 0
                then go 0 (pendingQ - 1) rest
                else True
        | otherwise =
            go depth pendingQ rest

-- | Drop the trailing closing `)` produced by `processInsideBrackets "(" ")" 1`.
dropClosingParen :: [Token] -> [Token]
dropClosingParen [] = []
dropClosingParen [t] | isTokenBracket t && tToken t == ")" = []
dropClosingParen (t : ts) = t : dropClosingParen ts

-- | Skip a leading JS string argument (either `ChunkString` `"..."` or single-quoted `'...'`
-- tokens when `'` is not yet in `ftString`).
skipLeadingJsString :: [Token] -> Maybe [Token]
skipLeadingJsString (s : rest)
    | isTokenString s = Just rest
skipLeadingJsString (q : rest)
    | isTokenOperator q && tToken q == "'" = findCloseSingleQuote rest
  where
    findCloseSingleQuote [] = Nothing
    findCloseSingleQuote (t : ts)
        | isTokenOperator t && tToken t == "'" = Just ts
        | isTokenBracket t = Nothing
        | tToken t == "," || tToken t == ";" = Nothing
        | otherwise = findCloseSingleQuote ts
skipLeadingJsString _ = Nothing

-- | Check whether arguments inside `(...)` form a valid tear-off test/hook invocation:
--   * Hooks (`beforeEach(resetMocks)`, `test.beforeEach(seed)`): starts with an identifier.
--   * Named test/suite (`it('name', checkFn)`): starts with string literal + `,` + second argument.
hasTearOffArgs :: Bool -> Bool -> [Token] -> Bool
hasTearOffArgs isHook allowSingleString innerArgs
    | isHook =
        case innerArgs of
            (firstTok : _) -> isTokenIdentifier firstTok
            [] -> False
    | otherwise =
        case skipLeadingJsString innerArgs of
            Just (comma : _ : _) | tToken comma == "," -> True
            Just [] | allowSingleString -> True
            _ -> False

-- | Check if the tokens immediately following `(...)` indicate a class method or function
-- declaration (`{ ... }` or `: ReturnType`) rather than a test runner call.
isFollowedByMethodBody :: [Token] -> Bool
isFollowedByMethodBody (t : _)
    | isTokenBracket t && tToken t == "{" = True
    | isTokenOperator t && tToken t == ":" = True
isFollowedByMethodBody _ = False

-- | Consume optional chained modifiers `.only`, `.skip`, `.describe`, `.beforeEach`, `.each(...)`, etc.
matchJsTestCall :: Bool -> Bool -> Bool -> [Token] -> Maybe ([Token], [Token])
matchJsTestCall _ _ _ (tDot : tMod : rest)
    | isTokenOperator tDot && tToken tDot == "."
    , (isTokenIdentifier tMod || isTokenKeyword tMod) && isJsTestModifier (tToken tMod) =
        let modName = tToken tMod
            isNoCb = isNoCallbackModifier modName
            isHook' = isJsHookName modName
         in case rest of
                (tOpen : afterOpen)
                    | isTokenBracket tOpen && tToken tOpen == "(" ->
                        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 afterOpen
                            innerArgs = dropClosingParen argTokens
                            noParamColon = not (hasTopLevelTsParamColon innerArgs)
                         in case afterArgs of
                                -- Curried call: .each(...)('name', () => { ... })
                                (tOpen2 : afterOpen2)
                                    | isTokenBracket tOpen2 && tToken tOpen2 == "(" ->
                                        let (bodyTokens, remaining) = processInsideBrackets "(" ")" 1 afterOpen2
                                            innerBody = dropClosingParen bodyTokens
                                         in if not (hasTopLevelTsParamColon innerBody)
                                                && (isNoCb || hasCallbackTokens argTokens || hasCallbackTokens bodyTokens || hasTearOffArgs isHook' False innerBody)
                                                then Just (tDot : tMod : tOpen : argTokens ++ (tOpen2 : bodyTokens), remaining)
                                                else Nothing
                                _ ->
                                    case matchJsTestCall isHook' True isNoCb afterArgs of
                                        Just (moreTokens, remaining) ->
                                            Just (tDot : tMod : tOpen : argTokens ++ moreTokens, remaining)
                                        Nothing ->
                                            if noParamColon
                                                && not (isFollowedByMethodBody afterArgs)
                                                && (isNoCb || hasCallbackTokens argTokens || hasTearOffArgs isHook' (modName == "skip") innerArgs)
                                                then Just (tDot : tMod : tOpen : argTokens, afterArgs)
                                                else Nothing
                _ ->
                    case matchJsTestCall isHook' True isNoCb rest of
                        Just (moreTokens, remaining) ->
                            Just (tDot : tMod : moreTokens, remaining)
                        Nothing -> Nothing
matchJsTestCall isHook allowSingleStr seenNoCb (tOpen : afterOpen)
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (bodyTokens, remaining) = processInsideBrackets "(" ")" 1 afterOpen
            innerArgs = dropClosingParen bodyTokens
         in if not (isFollowedByMethodBody remaining)
                && not (hasTopLevelTsParamColon innerArgs)
                && (seenNoCb || hasCallbackTokens bodyTokens || hasTearOffArgs isHook allowSingleStr innerArgs)
                then Just (tOpen : bodyTokens, remaining)
                else Nothing
matchJsTestCall _ _ _ _ = Nothing

matchQualifiedRunnerRoot :: [Token] -> Maybe ([Token], [Token])
matchQualifiedRunnerRoot (tNs : tDot : tSub : rest)
    | isTokenIdentifier tNs && tToken tNs == "Deno"
    , isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tSub && tToken tSub == "test" =
        fmap (\(callToks, remToks) -> (tNs : tDot : tSub : callToks, remToks)) (matchJsTestCall False False False rest)
    | isTokenIdentifier tNs && tToken tNs == "QUnit"
    , isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tSub
    , tToken tSub `elem` (["test", "module", "todo", "skip", "only"] :: [Text]) =
        let sub = tToken tSub
            isNoCb = sub == "todo"
            allowSingle = sub == "module" || sub == "skip"
         in fmap (\(callToks, remToks) -> (tNs : tDot : tSub : callToks, remToks)) (matchJsTestCall False allowSingle isNoCb rest)
matchQualifiedRunnerRoot _ = Nothing

matchBareRunnerRoot :: [Token] -> Maybe ([Token], [Token])
matchBareRunnerRoot (t1 : ts@(t2 : _))
    | (isTokenBracket t2 && tToken t2 == "(") || (isTokenOperator t2 && tToken t2 == ".")
    , (isTokenIdentifier t1 || isTokenKeyword t1)
    , isJsTestBlockName (tToken t1) =
        let isHook = isJsHookName (tToken t1)
         in fmap (\(callToks, remToks) -> (t1 : callToks, remToks)) (matchJsTestCall isHook False False ts)
matchBareRunnerRoot _ = Nothing

isJsMethodModifierName :: Text -> Bool
isJsMethodModifierName = \case
    "async" -> True
    "static" -> True
    "get" -> True
    "set" -> True
    "public" -> True
    "private" -> True
    "protected" -> True
    "readonly" -> True
    "abstract" -> True
    "override" -> True
    _ -> False

processOutsideJavascript :: Bool -> Bool -> Bool -> [Token] -> [Token]
processOutsideJavascript _ _ _ [] = []
processOutsideJavascript keepTests False !_ toks
    | Just (testTokens, remainingTokens) <- matchQualifiedRunnerRoot toks <|> matchBareRunnerRoot toks =
        if keepTests
            then testTokens ++ processOutsideJavascript keepTests False False remainingTokens
            else processOutsideJavascript keepTests False False remainingTokens
processOutsideJavascript keepTests !_ !afterFunc (t : ts) =
    let !isFuncKw = (isTokenKeyword t || isTokenIdentifier t) && tToken t == "function"
        isGenStar = afterFunc && isTokenOperator t && tToken t == "*"
        isDotOp = isTokenOperator t && (tToken t == "." || tToken t == "?.")
        isMethodMod = (isTokenKeyword t || isTokenIdentifier t) && isJsMethodModifierName (tToken t)
        !nextBlocker = isFuncKw || isGenStar || isDotOp || isMethodMod
     in if keepTests
            then processOutsideJavascript keepTests nextBlocker isFuncKw ts
            else t : processOutsideJavascript keepTests nextBlocker isFuncKw ts
