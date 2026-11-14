{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Dart () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Chunk (pattern ChunkNativeType)
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    isTokenString,
    tToken,
    tTyp,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Dart where
    isTestFile = isDartTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideDart keepTests True tokens

-- | Dart and Flutter test runners discover test suites via `*_test.dart`.
isDartTestFilePath :: OsPath -> Bool
isDartTestFilePath path =
    takeExtension path == [osp|.dart|]
        && [osp|_test|] `OS.isSuffixOf` takeBaseName path

-- | Dart `package:test`, `package:flutter_test`, `bloc_test`, `golden_toolkit`, and `patrol` functions.
isDartTestFunction :: Text -> Bool
isDartTestFunction name =
    name == "test"
        || name == "group"
        || isDartDedicatedTestFunction name

isDartDedicatedTestFunction :: Text -> Bool
isDartDedicatedTestFunction name =
    name
        `elem` ([ "testWidgets"
                , "setUp"
                , "tearDown"
                , "setUpAll"
                , "tearDownAll"
                , "solo_test"
                , "solo_group"
                , "blocTest"
                , "testGoldens"
                , "patrolTest"
                , "patrolWidgetTest"
                , "benchmarkWidgets"
                ] :: [Text])

isDartSetupLifecycle :: Text -> Bool
isDartSetupLifecycle name =
    name `elem` (["setUp", "tearDown", "setUpAll", "tearDownAll"] :: [Text])

-- | Match Dart `=>` in both `--strict` (`"=>"`) and default (`"="` adjacent to `">"`) modes.
matchFatArrow :: [Token] -> Maybe ([Token], [Token])
matchFatArrow (t : ts)
    | isTokenOperator t && tToken t == "=>" = Just ([t], ts)
matchFatArrow (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "="
    , isTokenOperator t2 && tToken t2 == ">"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], ts)
matchFatArrow _ = Nothing

-- | Consume balanced `< ... >` generic type arguments on a call (`blocTest<CounterBloc, int>(...)`),
-- aborting on `{`, `}`, or `;`.
consumeTypeArgs :: Int -> [Token] -> Maybe ([Token], [Token])
consumeTypeArgs 0 ts = Just ([], ts)
consumeTypeArgs _ [] = Nothing
consumeTypeArgs !depth (t : ts)
    | isTokenBracket t && (tToken t == "{" || tToken t == "}") = Nothing
    | isTokenOperator t && tToken t == ";" = Nothing
    | isTokenOperator t && tToken t == "<" =
        case consumeTypeArgs (depth + 1) ts of
            Just (more, rest) -> Just (t : more, rest)
            Nothing -> Nothing
    | isTokenOperator t && tToken t == ">" =
        if depth == 1
            then Just ([t], ts)
            else case consumeTypeArgs (depth - 1) ts of
                Just (more, rest) -> Just (t : more, rest)
                Nothing -> Nothing
    | isTokenOperator t && tToken t == ">>" =
        if depth <= 2
            then Just ([t], ts)
            else case consumeTypeArgs (depth - 2) ts of
                Just (more, rest) -> Just (t : more, rest)
                Nothing -> Nothing
    | otherwise =
        case consumeTypeArgs depth ts of
            Just (more, rest) -> Just (t : more, rest)
            Nothing -> Nothing

dropTrailingCloseParen :: [Token] -> [Token]
dropTrailingCloseParen tokens =
    case reverse tokens of
        (lastTok : revMid)
            | isTokenBracket lastTok && tToken lastTok == ")" -> reverse revMid
        _ -> tokens

-- | Check whether argument tokens contain an actual Dart closure literal
-- `(params) [async[*]|sync[*]] { ... }` or `(params) [async] => ...`.
-- Unlike naive `{` scanning, this never confuses Dart named parameters
-- (`bool test({required String s})`) or Map/Set literals (`group({'k': v})`)
-- with a test callback closure.
hasDartClosure :: [Token] -> Bool
hasDartClosure [] = False
hasDartClosure (t : ts)
    | isTokenBracket t && tToken t == "(" =
        let (insideParens, afterParens) = processInsideBrackets "(" ")" 1 ts
            inner = dropTrailingCloseParen insideParens
         in isClosureBodyAfterParens afterParens
                || hasDartClosure inner
                || hasDartClosure afterParens
    | otherwise = hasDartClosure ts
  where
    isClosureBodyAfterParens :: [Token] -> Bool
    isClosureBodyAfterParens tokens =
        let afterMod = skipClosureModifier tokens
         in case afterMod of
                (b : _) | isTokenBracket b && tToken b == "{" -> True
                _ | Just _ <- matchFatArrow afterMod -> True
                _ -> False

    skipClosureModifier :: [Token] -> [Token]
    skipClosureModifier (m : star : rest)
        | (isTokenKeyword m || isTokenIdentifier m)
        , tToken m == "async" || tToken m == "sync"
        , isTokenOperator star && tToken star == "*" =
            rest
    skipClosureModifier (m : rest)
        | (isTokenKeyword m || isTokenIdentifier m)
        , tToken m == "async" =
            rest
    skipClosureModifier tokens = tokens

-- | Split argument tokens at top-level commas (outside `(...)`, `[...]`, `{...}`).
splitTopLevelArgs :: [Token] -> [[Token]]
splitTopLevelArgs = go 0 []
  where
    go :: Int -> [Token] -> [Token] -> [[Token]]
    go _ cur [] = [reverse cur | not (null cur)]
    go !depth cur (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (depth + 1) (t : cur) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (depth - 1)) (t : cur) ts
        | depth == 0 && isTokenOperator t && tToken t == "," =
            let restItems = go 0 [] ts
             in if null cur then restItems else reverse cur : restItems
        | otherwise =
            go depth (t : cur) ts

-- | True if `tokens` is a Dart tear-off reference (`initDb` or `TestEnv.initDb`),
-- as opposed to a typed parameter declaration (`Config cfg`) or map literal (`{...}`).
isTearOffArg :: [Token] -> Bool
isTearOffArg [t] = isTokenIdentifier t
isTearOffArg (t1 : tDot : rest)
    | isTokenIdentifier t1
    , isTokenOperator tDot && tToken tDot == "." =
        isTearOffArg rest
isTearOffArg _ = False

-- | Validates the arguments inside `fn(...)` for Dart test calls:
--   * Any test function with a closure literal `(...) { ... }` / `(...) => ...`
--   * `setUp(tearOff)` / `tearDown(tearOff)` / `setUpAll(tearOff)` / `tearDownAll(tearOff)`
--   * `test('desc', tearOff)` / `group('desc', tearOff)` where arg 1 is a string literal and arg 2 is a tear-off
hasValidDartTestArgs :: Text -> [Token] -> Bool
hasValidDartTestArgs fnName innerToks
    | hasDartClosure innerToks = True
    | isDartSetupLifecycle fnName =
        case splitTopLevelArgs innerToks of
            [arg1] -> isTearOffArg arg1
            _ -> False
    | otherwise =
        case splitTopLevelArgs innerToks of
            (arg1 : arg2 : _)
                | (firstTok : _) <- arg1
                , isTokenString firstTok
                , isTearOffArg arg2 ->
                    True
            _ -> False

-- | True if tokens right after `)` start a Dart function/method declaration body
-- (`{ ... }`, `=> ...`, `async ...`, `sync* ...`), meaning `fn(...)` was a
-- declaration signature rather than a call statement.
startsDartFunctionBody :: [Token] -> Bool
startsDartFunctionBody (b : _)
    | isTokenBracket b && tToken b == "{" = True
startsDartFunctionBody (m : _)
    | (isTokenKeyword m || isTokenIdentifier m)
    , tToken m == "async" || tToken m == "sync" =
        True
startsDartFunctionBody tokens
    | Just _ <- matchFatArrow tokens = True
startsDartFunctionBody _ = False

consumeOptionalSemicolon :: [Token] -> ([Token], [Token])
consumeOptionalSemicolon (t : ts)
    | isTokenOperator t && tToken t == ";" = ([t], ts)
consumeOptionalSemicolon ts = ([], ts)

-- | Match `package:meta` `@isTest` or `@isTestGroup` (optionally qualified `@meta.isTest`
-- and optionally followed by `()`), returning `(annoTokens, remainingTokens)`.
matchDartIsTestAnnotation :: [Token] -> Maybe ([Token], [Token])
matchDartIsTestAnnotation (tAt : tId1 : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tId1 =
        let (qualToks, attrName, afterName) = case ts of
                (tDot : tId2 : rest)
                    | isTokenOperator tDot && tToken tDot == "."
                    , isTokenIdentifier tId2 ->
                        ([tDot, tId2], tToken tId2, rest)
                _ -> ([], tToken tId1, ts)
         in if attrName == "isTest" || attrName == "isTestGroup"
                then case afterName of
                    (openP : rest)
                        | isTokenBracket openP && tToken openP == "(" ->
                            let (parens, afterParens) = processInsideBrackets "(" ")" 1 rest
                             in Just (tAt : tId1 : qualToks ++ (openP : parens), afterParens)
                    _ -> Just (tAt : tId1 : qualToks, afterName)
                else Nothing
matchDartIsTestAnnotation _ = Nothing

-- | Consume a Dart function declaration annotated with `@isTest` / `@isTestGroup`
-- (handling braced bodies `{ ... }`, expression bodies `=> ...;`, or `;`).
consumeDartAnnotatedDecl :: [Token] -> ([Token], [Token])
consumeDartAnnotatedDecl = goSig 0 []
  where
    goSig :: Int -> [Token] -> [Token] -> ([Token], [Token])
    goSig _ acc [] = (reverse acc, [])
    goSig !pDepth acc tokens@(t : ts)
        | pDepth == 0
        , Just (arrowToks, afterArrow) <- matchFatArrow tokens =
            collectExpr 0 (reverse arrowToks ++ acc) afterArrow
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            goSig (pDepth + 1) (t : acc) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            goSig (max 0 (pDepth - 1)) (t : acc) ts
        | pDepth == 0 && isTokenBracket t && tToken t == "{" =
            let (bodyToks, remaining) = processInsideBrackets "{" "}" 1 ts
             in (reverse (t : acc) ++ bodyToks, remaining)
        | pDepth == 0 && isTokenOperator t && tToken t == ";" =
            (reverse (t : acc), ts)
        | otherwise =
            goSig pDepth (t : acc) ts

    collectExpr :: Int -> [Token] -> [Token] -> ([Token], [Token])
    collectExpr _ acc [] = (reverse acc, [])
    collectExpr !eDepth acc (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            collectExpr (eDepth + 1) (t : acc) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            collectExpr (max 0 (eDepth - 1)) (t : acc) ts
        | eDepth == 0 && isTokenOperator t && tToken t == ";" =
            (reverse (t : acc), ts)
        | otherwise =
            collectExpr eDepth (t : acc) ts

-- | Match optional `<TypeArgs>` followed by `( ... )` after a Dart test function identifier.
matchDartCallArgs :: [Token] -> Maybe ([Token], [Token], [Token])
matchDartCallArgs (tOpen : ts)
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (bodyToks, remaining) = processInsideBrackets "(" ")" 1 ts
         in Just ([tOpen], bodyToks, remaining)
matchDartCallArgs (tLt : ts)
    | isTokenOperator tLt && tToken tLt == "<"
    , Just (typeArgToks, tOpen : afterOpen) <- consumeTypeArgs 1 ts
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (bodyToks, remaining) = processInsideBrackets "(" ")" 1 afterOpen
         in Just (tLt : typeArgToks ++ [tOpen], bodyToks, remaining)
matchDartCallArgs _ = Nothing

-- | `canStartCall` is `False` right after receiver accessors (`.`, `?.`, `..`, `?..`)
-- and return-type / declaration-prefix tokens (`bool`, `void`, `Future<void>`, `String?`),
-- preventing method calls on objects (`router.group(...)`) and method declarations
-- (`bool test({required String s})`) from being misclassified as test blocks.
processOutsideDart :: Bool -> Bool -> [Token] -> [Token]
processOutsideDart _ _ [] = []
-- Pattern 1: `@isTest` / `@isTestGroup` custom test wrapper declarations (`package:meta`)
processOutsideDart keepTests True tokens
    | Just (annoToks, afterAnno) <- matchDartIsTestAnnotation tokens =
        let (declToks, remaining) = consumeDartAnnotatedDecl afterAnno
            restOut = processOutsideDart keepTests True remaining
         in if keepTests
                then annoToks ++ declToks ++ restOut
                else restOut
-- Pattern 2: `test(...)`, `group(...)`, `testWidgets(...)`, `blocTest<B, S>(...)`, `setUp(...)`, etc.
processOutsideDart keepTests True (t1 : ts)
    | isTokenIdentifier t1 && isDartTestFunction (tToken t1)
    , Just (callPrefixToks, bodyTokens, afterParen) <- matchDartCallArgs ts
    , not (startsDartFunctionBody afterParen)
    , hasValidDartTestArgs (tToken t1) (dropTrailingCloseParen bodyTokens) =
        let (semiToks, remainingTokens) = consumeOptionalSemicolon afterParen
            restOut = processOutsideDart keepTests True remainingTokens
         in if keepTests
                then (t1 : callPrefixToks ++ bodyTokens ++ semiToks) ++ restOut
                else restOut
-- Fat arrow `=>` (both `"=>"` and `"="` + `">"`) allows a test call right after it (`main() => group(...)`)
processOutsideDart keepTests _ tokens
    | Just (arrowToks, remaining) <- matchFatArrow tokens =
        let restOut = processOutsideDart keepTests True remaining
         in if keepTests
                then restOut
                else arrowToks ++ restOut
processOutsideDart keepTests _ (t : ts) =
    let blocksCall =
            ( isTokenOperator t
                && tToken t `elem` ([".", "?.", "..", "?..", ">", ">>", "?"] :: [Text])
            )
                || tTyp t == ChunkNativeType
                || isTokenIdentifier t
                || ( isTokenKeyword t
                        && tToken t `notElem` (["await", "return", "else", "do"] :: [Text])
                   )
     in if keepTests
            then processOutsideDart keepTests (not blocksCall) ts
            else t : processOutsideDart keepTests (not blocksCall) ts
