{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Bash () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenNumber,
    isTokenOperator,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (osp, takeExtension)

instance LanguageTestFilter 'Bash where
    isTestFile path = takeExtension path == [osp|.bats|]
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens =
        let isBats = hasBatsIndicator tokens
         in processOutsideBash keepTests isBats Nothing tokens

-- | Checks whether the token stream contains BATS indicators (`@test "..." {`,
-- BATS file/suite lifecycle functions, or `bats_test_function`), enabling `setup`
-- and `teardown` recognition in BATS scripts and helper files.
isBatsIndicatorIdent :: Text -> Bool
isBatsIndicatorIdent = \case
    "setup_file" -> True
    "teardown_file" -> True
    "setup_suite" -> True
    "teardown_suite" -> True
    "bats_test_function" -> True
    _ -> False

hasBatsIndicator :: [Token] -> Bool
hasBatsIndicator (tAt : tTest : tDesc : tBrace : _)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tTest && tToken tTest == "test"
    , isTokenString tDesc
    , isTokenBracket tBrace && tToken tBrace == "{" =
        True
hasBatsIndicator (t : ts)
    | isTokenIdentifier t && isBatsIndicatorIdent (tToken t) =
        True
    | otherwise = hasBatsIndicator ts
hasBatsIndicator [] = False

isTestLifecycleName :: Text -> Bool
isTestLifecycleName = \case
    "oneTimeSetUp" -> True
    "oneTimeTearDown" -> True
    "setUp" -> True
    "tearDown" -> True
    "set_up" -> True
    "tear_down" -> True
    "set_up_before_script" -> True
    "tear_down_after_script" -> True
    "setup_file" -> True
    "teardown_file" -> True
    "setup_suite" -> True
    "teardown_suite" -> True
    _ -> False

-- | Checks if a Bash function name (including hyphenated `test-foo` or namespaced `pkg::test_foo`)
-- is a BATS / shUnit2 / bashunit test or lifecycle function, avoiding false positives on words
-- like `testify`, `testing`, `non-test_runner`, or bare `test`.
isBashTestFuncName :: Bool -> Text -> Bool
isBashTestFuncName isBats fullName =
    let localName = snd (T.breakOnEnd "::" fullName)
     in checkSingle fullName || (localName /= fullName && checkSingle localName)
  where
    checkSingle name =
        hasTestPrefix name
            || isTestLifecycleName name
            || (isBats && (name == "setup" || name == "teardown"))

    hasTestPrefix txt =
        case T.stripPrefix "test" txt of
            Just rest ->
                case T.uncons rest of
                    Just (c, _) -> not (isAsciiLower c)
                    Nothing -> False
            Nothing -> False

isWordOrNumToken :: Token -> Bool
isWordOrNumToken t = isTokenIdentifier t || isTokenKeyword t || isTokenNumber t

isFuncNameSepToken :: Token -> Bool
isFuncNameSepToken t =
    isTokenOperator t && tToken t `elem` (["-", ":", "::", "."] :: [Text])

-- | Extracts a contiguous Bash function name starting with an identifier token,
-- joining immediately adjacent `-`, `:`, or `.` segments (e.g. `test-for-empty`, `auth::test_login`).
extractBashFuncName :: [Token] -> Maybe ([Token], Text, [Token])
extractBashFuncName (t1 : rest)
    | isTokenIdentifier t1 =
        let (extraToks, remaining) = collectAdjSegments t1 rest
            allNameToks = t1 : extraToks
            fullText = if null extraToks then tToken t1 else T.concat (map tToken allNameToks)
         in Just (allNameToks, fullText, remaining)
  where
    collectAdjSegments :: Token -> [Token] -> ([Token], [Token])
    collectAdjSegments prev (t : ts)
        | areAdjacentTokens prev t && isWordOrNumToken t =
            let (more, remToks) = collectAdjSegments t ts
             in (t : more, remToks)
        | areAdjacentTokens prev t && isFuncNameSepToken t =
            let (seps, lastSep, afterSeps) = collectAdjSeps t ts
             in case afterSeps of
                    (tWord : afterWord)
                        | areAdjacentTokens lastSep tWord && isWordOrNumToken tWord ->
                            let (more, remToks) = collectAdjSegments tWord afterWord
                             in (seps ++ (tWord : more), remToks)
                    _ -> ([], t : ts)
    collectAdjSegments _ ts = ([], ts)

    collectAdjSeps :: Token -> [Token] -> ([Token], Token, [Token])
    collectAdjSeps prevSep (t : ts)
        | areAdjacentTokens prevSep t && isFuncNameSepToken t =
            let (more, finalSep, remToks) = collectAdjSeps t ts
             in (prevSep : more, finalSep, remToks)
    collectAdjSeps prevSep ts = ([prevSep], prevSep, ts)
extractBashFuncName _ = Nothing

-- | Balances a subshell function body `( ... )`, tracking `case ... esac` blocks so
-- unbalanced pattern-arm closing parens `pat)` do not prematurely close the subshell.
processBashSubshell :: Int -> [Int] -> [Token] -> ([Token], [Token])
processBashSubshell 0 _ ts = ([], ts)
processBashSubshell _ _ [] = ([], [])
processBashSubshell depth caseStack (t : ts)
    | isTokenKeyword t && tToken t == "case" =
        let (inside, remToks) = processBashSubshell depth (depth : caseStack) ts
         in (t : inside, remToks)
    | isTokenKeyword t && tToken t == "esac" =
        let (inside, remToks) = processBashSubshell depth (drop 1 caseStack) ts
         in (t : inside, remToks)
    | isTokenBracket t && tToken t == "(" =
        let (inside, remToks) = processBashSubshell (depth + 1) caseStack ts
         in (t : inside, remToks)
    | isTokenBracket t && tToken t == ")" =
        case caseStack of
            (floorDepth : _)
                | depth <= floorDepth ->
                    let (inside, remToks) = processBashSubshell depth caseStack ts
                     in (t : inside, remToks)
            _
                | depth == 1 -> ([t], ts)
                | otherwise ->
                    let (inside, remToks) = processBashSubshell (depth - 1) caseStack ts
                     in (t : inside, remToks)
    | otherwise =
        let (inside, remToks) = processBashSubshell depth caseStack ts
         in (t : inside, remToks)

-- | Matches a Bash function compound body: `{ ... }` or subshell `( ... )`.
matchBashCompoundBody :: [Token] -> Maybe ([Token], [Token])
matchBashCompoundBody (tOpen : ts)
    | isTokenBracket tOpen && tToken tOpen == "{" =
        let (inside, remToks) = processInsideBraces 1 ts
         in Just (tOpen : inside, remToks)
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (inside, remToks) = processBashSubshell 1 [] ts
         in Just (tOpen : inside, remToks)
matchBashCompoundBody _ = Nothing

-- | Matches `() <compoundBody>`, or (when `allowNoParens` is `True` after `function`)
-- directly `<compoundBody>`.
matchBashFunctionAfterName :: Bool -> [Token] -> Maybe ([Token], [Token])
matchBashFunctionAfterName _ (tLParen : tRParen : afterParens)
    | isTokenBracket tLParen && tToken tLParen == "("
    , isTokenBracket tRParen && tToken tRParen == ")"
    , Just (bodyToks, remToks) <- matchBashCompoundBody afterParens =
        Just (tLParen : tRParen : bodyToks, remToks)
matchBashFunctionAfterName True rest = matchBashCompoundBody rest
matchBashFunctionAfterName False _ = Nothing

startsWithEmptyParens :: [Token] -> Bool
startsWithEmptyParens (tLParen : tRParen : _) =
    isTokenBracket tLParen
        && tToken tLParen == "("
        && isTokenBracket tRParen
        && tToken tRParen == ")"
startsWithEmptyParens _ = False

-- | Processes Bash tokens outside of a test block.
processOutsideBash :: Bool -> Bool -> Maybe Token -> [Token] -> [Token]
processOutsideBash _ _ _ [] = []
-- Pattern 1: BATS `@test "description" { ... }`
processOutsideBash keepTests !isBats _ (tAt : tTest : tDesc : tBrace : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tTest && tToken tTest == "test"
    , isTokenString tDesc
    , isTokenBracket tBrace && tToken tBrace == "{" =
        let (insideBraces, remaining) = processInsideBraces 1 ts
            captured = tAt : tTest : tDesc : tBrace : insideBraces
            !nextPrev = Just (last captured)
         in if keepTests
                then captured ++ processOutsideBash keepTests isBats nextPrev remaining
                else processOutsideBash keepTests isBats nextPrev remaining
-- Pattern 2: `function <name>[()] { ... }` or `function <name>[()] ( ... )`
processOutsideBash keepTests !isBats _ (tFunc : ts)
    | isTokenKeyword tFunc && tToken tFunc == "function"
    , Just (nameToks, fullName, afterName) <- extractBashFuncName ts
    , isBashTestFuncName isBats fullName
    , Just (sigAndBody, remaining) <- matchBashFunctionAfterName True afterName =
        let captured = tFunc : nameToks ++ sigAndBody
            !nextPrev = Just (last captured)
         in if keepTests
                then captured ++ processOutsideBash keepTests isBats nextPrev remaining
                else processOutsideBash keepTests isBats nextPrev remaining
-- Pattern 3: `<name>() { ... }` or `<name>() ( ... )` (POSIX / shUnit2 / bashunit style)
processOutsideBash keepTests !isBats prevTok (t1 : ts)
    | isTokenIdentifier t1
    , maybe True (\p -> not (areAdjacentTokens p t1)) prevTok
    , Just (nameToks, fullName, afterName) <- extractBashFuncName (t1 : ts)
    , startsWithEmptyParens afterName
    , isBashTestFuncName isBats fullName
    , Just (sigAndBody, remaining) <- matchBashFunctionAfterName False afterName =
        let captured = nameToks ++ sigAndBody
            !nextPrev = Just (last captured)
         in if keepTests
                then captured ++ processOutsideBash keepTests isBats nextPrev remaining
                else processOutsideBash keepTests isBats nextPrev remaining
-- Fallback
processOutsideBash keepTests !isBats _ (t : ts) =
    if keepTests
        then processOutsideBash keepTests isBats (Just t) ts
        else t : processOutsideBash keepTests isBats (Just t) ts
