{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.OCaml () where

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
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'OCaml where
    isTestFile = isOCamlTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideOCaml keepTests False tokens

-- | Recognizes Dune / OCaml test files (`test_*.ml`, `*_test.ml`).
isOCamlTestFilePath :: OsPath -> Bool
isOCamlTestFilePath path =
    takeExtension path == [osp|.ml|]
        && ( [osp|test_|] `OS.isPrefixOf` base
                || [osp|_test|] `OS.isSuffixOf` base
           )
  where
    base = takeBaseName path

-- | Matches OCaml's `;;` item terminator in both default mode (two adjacent `";"` punctuation tokens)
-- and strict mode (`";;"`).
matchDoubleSemi :: [Token] -> Maybe ([Token], [Token])
matchDoubleSemi (t1 : ts)
    | tToken t1 == ";;" = Just ([t1], ts)
matchDoubleSemi (t1 : t2 : ts)
    | tToken t1 == ";"
    , tToken t2 == ";"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], ts)
matchDoubleSemi _ = Nothing

-- | Checks whether `[` starts a top-level structure item extension `[%%...]` or floating attribute `[@@@...]`.
isTopLevelBracketItem :: [Token] -> Bool
isTopLevelBracketItem (tOpen : rest)
    | isTokenBracket tOpen && tToken tOpen == "[" =
        case rest of
            (t1 : _) | tToken t1 == "%%" || tToken t1 == "@@@" -> True
            (t1 : t2 : _)
                | tToken t1 == "%" && tToken t2 == "%" && areAdjacentTokens t1 t2 -> True
            (t1 : t2 : t3 : _)
                | tToken t1 == "@"
                , tToken t2 == "@"
                , tToken t3 == "@"
                , areAdjacentTokens t1 t2
                , areAdjacentTokens t2 t3 ->
                    True
            _ -> False
isTopLevelBracketItem _ = False

-- | Matches `[%%test ...]`, `[%%test_unit ...]`, `[%%test_module ...]`, `[%%expect_test ...]`.
matchItemTestExtension :: [Token] -> Maybe ([Token], [Token])
matchItemTestExtension (tOpen : rest)
    | isTokenBracket tOpen && tToken tOpen == "[" =
        let mExtTok = case rest of
                (tPct : tExt : _)
                    | tToken tPct == "%%" && isTokenIdentifier tExt ->
                        Just tExt
                (tP1 : tP2 : tExt : _)
                    | tToken tP1 == "%"
                    , tToken tP2 == "%"
                    , areAdjacentTokens tP1 tP2
                    , isTokenIdentifier tExt ->
                        Just tExt
                _ -> Nothing
         in case mExtTok of
                Just tExt
                    | isOCamlPpxTestName (tToken tExt) ->
                        let (insideToks, remaining) = processInsideBrackets "[" "]" 1 rest
                         in Just (tOpen : insideToks, remaining)
                _ -> Nothing
matchItemTestExtension _ = Nothing

isOCamlPpxTestName :: Text -> Bool
isOCamlPpxTestName name =
    name == "test"
        || "test_" `T.isPrefixOf` name
        || "expect_test" `T.isPrefixOf` name

-- | Check if a module name is a test module (`Test`, `Tests`, `TestSuite`, `Test_foo`, `FooTest`)
-- without matching words like `Testimonial`.
isOCamlTestModuleName :: Text -> Bool
isOCamlTestModuleName name =
    let prefixOk = case T.stripPrefix "Test" name of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
     in prefixOk
            || "Test" `T.isSuffixOf` name
            || "Tests" `T.isSuffixOf` name
            || "_test" `T.isSuffixOf` name
            || "_tests" `T.isSuffixOf` name

isOCamlTopItemKeyword :: Text -> Bool
isOCamlTopItemKeyword kw =
    kw `elem` (["module", "type", "exception", "external", "class", "open", "include"] :: [Text])

-- | Scans module header tokens (allowing optional `: sig ... end`, functor parameters `(...)`,
-- and `[@...]` attributes) up to `= struct`, then collects through the matching `end`.
findModuleStructBody :: Int -> [Token] -> Maybe ([Token], [Token])
findModuleStructBody maxToks = go maxToks
  where
    go :: Int -> [Token] -> Maybe ([Token], [Token])
    go n _ | n <= 0 = Nothing
    go _ [] = Nothing
    go _ ts | Just _ <- matchDoubleSemi ts = Nothing
    go _ (tEq : tStruct : ts)
        | isTokenOperator tEq && tToken tEq == "="
        , isTokenKeyword tStruct && tToken tStruct == "struct" =
            let (bodyToks, remaining) = collectUntilOCamlEnd 1 ts
             in Just (tEq : tStruct : bodyToks, remaining)
    go n (t : ts)
        | isTokenKeyword t && tToken t `elem` (["sig", "struct", "begin", "object"] :: [Text]) =
            let (blk, afterBlk) = collectUntilOCamlEnd 1 ts
             in do
                    (restHdr, remaining) <- go (n - 1) afterBlk
                    Just (t : blk ++ restHdr, remaining)
        | isTokenBracket t && tToken t `elem` (["(", "["] :: [Text]) =
            let closeB = if tToken t == "(" then ")" else "]"
                (brk, afterBrk) = processInsideBrackets (tToken t) closeB 1 ts
             in do
                    (restHdr, remaining) <- go (n - 1) afterBrk
                    Just (t : brk ++ restHdr, remaining)
        | isTokenKeyword t && (tToken t == "let" || isOCamlTopItemKeyword (tToken t)) =
            Nothing
        | otherwise = do
            (restHdr, remaining) <- go (n - 1) ts
            Just (t : restHdr, remaining)

-- | Collects the curried arguments of an `Alcotest.test_case` call up to the next
-- unbracketed separator (`;`, `;;`, `,`, `]`, `)`, `}`) or top-level item.
collectAlcotestArgs :: [Token] -> ([Token], [Token])
collectAlcotestArgs [] = ([], [])
collectAlcotestArgs ts
    | Just _ <- matchDoubleSemi ts = ([], ts)
    | isTopLevelBracketItem ts = ([], ts)
collectAlcotestArgs allToks@(t : ts)
    | tToken t `elem` ([";", ",", ")", "]", "}"] :: [Text]) =
        ([], allToks)
    | isTokenKeyword t && (tToken t == "in" || isOCamlTopItemKeyword (tToken t)) =
        ([], allToks)
    | isTokenKeyword t && tToken t == "let" =
        if isLocalLetIn ts
            then
                let (rest, remToks) = collectAlcotestArgs ts
                 in (t : rest, remToks)
            else ([], allToks)
    | isTokenKeyword t && tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text]) =
        let (blk, afterBlk) = collectUntilOCamlEnd 1 ts
            (rest, remToks) = collectAlcotestArgs afterBlk
         in (t : blk ++ rest, remToks)
    | isTokenBracket t && tToken t `elem` (["(", "[", "{"] :: [Text]) =
        let closeB = case tToken t of
                "(" -> ")"
                "[" -> "]"
                _ -> "}"
            (brk, afterBrk) = processInsideBrackets (tToken t) closeB 1 ts
            (rest, remToks) = collectAlcotestArgs afterBrk
         in (t : brk ++ rest, remToks)
    | otherwise =
        let (rest, remToks) = collectAlcotestArgs ts
         in (t : rest, remToks)

-- | Processes OCaml tokens outside a test block.
-- `prevDot` prevents matching field/module projections like `Other.test_case`.
processOutsideOCaml :: Bool -> Bool -> [Token] -> [Token]
processOutsideOCaml _ _ [] = []
-- Pattern 1a: `module%test [Name | _] ... = struct ... end`
processOutsideOCaml keepTests _ (tMod : tPct : tExt : ts)
    | isTokenKeyword tMod && tToken tMod == "module"
    , isTokenOperator tPct && tToken tPct == "%"
    , isTokenIdentifier tExt && isOCamlPpxTestName (tToken tExt)
    , Just (modBodyToks, remaining) <- findModuleStructBody 80 ts =
        if keepTests
            then tMod : tPct : tExt : modBodyToks ++ processOutsideOCaml keepTests False remaining
            else processOutsideOCaml keepTests False remaining
-- Pattern 1b: `module Test... [: sig ... end] = struct ... end`
processOutsideOCaml keepTests _ (tMod : tName : ts)
    | isTokenKeyword tMod && tToken tMod == "module"
    , isTokenIdentifier tName && isOCamlTestModuleName (tToken tName)
    , Just (modBodyToks, remaining) <- findModuleStructBody 80 ts =
        if keepTests
            then tMod : tName : modBodyToks ++ processOutsideOCaml keepTests False remaining
            else processOutsideOCaml keepTests False remaining
-- Pattern 2: `let%test`, `let%test_unit`, `let%test_module`, `let%expect_test`
processOutsideOCaml keepTests _ (t1 : t2 : t3 : ts)
    | isTokenKeyword t1 && tToken t1 == "let"
    , isTokenOperator t2 && tToken t2 == "%"
    , isTokenIdentifier t3 && isOCamlPpxTestName (tToken t3) =
        let (testTokens, remainingTokens) = collectOCamlTestDef ts
         in if keepTests
                then t1 : t2 : t3 : testTokens ++ processOutsideOCaml keepTests False remainingTokens
                else processOutsideOCaml keepTests False remainingTokens
-- Pattern 3: `[%%expect_test ...]` / `[%%test ...]` item extensions
processOutsideOCaml keepTests _ allToks
    | Just (extToks, remaining) <- matchItemTestExtension allToks =
        if keepTests
            then extToks ++ processOutsideOCaml keepTests False remaining
            else processOutsideOCaml keepTests False remaining
-- Pattern 4a: Qualified `Alcotest.test_case "..." ...` / `Alcotest_lwt.test_case ...`
processOutsideOCaml keepTests False (tMod : tDot : tFn : ts)
    | isTokenIdentifier tMod
    , tToken tMod `elem` (["Alcotest", "Alcotest_lwt", "Alcotest_async"] :: [Text])
    , isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tFn && (tToken tFn == "test_case" || tToken tFn == "test_case_sync") =
        let (argToks, remaining) = collectAlcotestArgs ts
         in if keepTests
                then tMod : tDot : tFn : argToks ++ processOutsideOCaml keepTests False remaining
                else processOutsideOCaml keepTests False remaining
-- Pattern 4b: Unqualified `test_case "..." \`Quick|\`Slow ...`
processOutsideOCaml keepTests False (tFn : ts@(tStr : tTick : tSpeed : _))
    | isTokenIdentifier tFn && (tToken tFn == "test_case" || tToken tFn == "test_case_sync")
    , isTokenString tStr
    , tToken tTick == "`"
    , isTokenIdentifier tSpeed && (tToken tSpeed == "Quick" || tToken tSpeed == "Slow") =
        let (argToks, remaining) = collectAlcotestArgs ts
         in if keepTests
                then tFn : argToks ++ processOutsideOCaml keepTests False remaining
                else processOutsideOCaml keepTests False remaining
-- Fallback
processOutsideOCaml keepTests _ (t : ts) =
    let isDot = isTokenOperator t && tToken t == "."
     in if keepTests
            then processOutsideOCaml keepTests isDot ts
            else t : processOutsideOCaml keepTests isDot ts

-- | Collect tokens until the matching `end`, tracking nested `struct`/`sig`/`begin`/`object` blocks.
collectUntilOCamlEnd :: Int -> [Token] -> ([Token], [Token])
collectUntilOCamlEnd 0 ts = ([], ts)
collectUntilOCamlEnd _ [] = ([], [])
collectUntilOCamlEnd depth (t : ts)
    | isTokenKeyword t && (tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text])) =
        let (collected, remaining) = collectUntilOCamlEnd (depth + 1) ts
         in (t : collected, remaining)
    | isTokenKeyword t && tToken t == "end" =
        if depth == 1
            then ([t], ts)
            else
                let (collected, remaining) = collectUntilOCamlEnd (depth - 1) ts
                 in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilOCamlEnd depth ts
         in (t : collected, remaining)

-- | Check whether a `let` token (whose following tokens are `ts`) is a local
-- `let ... in` expression (as opposed to a top-level `let` item).
isLocalLetIn :: [Token] -> Bool
isLocalLetIn (op : _)
    | isTokenOperator op && tToken op == "%" = False
isLocalLetIn ts0 = go 1 0 ts0
  where
    go :: Int -> Int -> [Token] -> Bool
    go _ _ [] = False
    go letDepth blockDepth allToks@(t : ts)
        | blockDepth == 0 && (case matchDoubleSemi allToks of Just _ -> True; Nothing -> False) =
            False
        | blockDepth == 0 && isTopLevelBracketItem allToks =
            False
        | isTokenKeyword t && (tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text])) =
            go letDepth (blockDepth + 1) ts
        | isTokenKeyword t && tToken t == "end" =
            go letDepth (max 0 (blockDepth - 1)) ts
        | blockDepth > 0 =
            go letDepth blockDepth ts
        | isTokenKeyword t && tToken t == "in" =
            if letDepth <= 1
                then True
                else go (letDepth - 1) blockDepth ts
        | isTokenKeyword t && tToken t == "let" =
            case ts of
                (op : _) | isTokenOperator op && tToken op == "%" -> False
                _ -> go (letDepth + 1) blockDepth ts
        | isTokenKeyword t && isOCamlTopItemKeyword (tToken t) =
            False
        | otherwise =
            go letDepth blockDepth ts

-- | Collect tokens of a top-level `let%test` / `let%expect_test` definition until `;;`,
-- a top-level `[%%...]` / `[@@@...]` item, or the next top-level declaration.
collectOCamlTestDef :: [Token] -> ([Token], [Token])
collectOCamlTestDef [] = ([], [])
collectOCamlTestDef allToks
    | Just (semiToks, remaining) <- matchDoubleSemi allToks =
        (semiToks, remaining)
    | isTopLevelBracketItem allToks =
        ([], allToks)
collectOCamlTestDef (t : ts)
    | isTokenKeyword t && (tToken t `elem` (["struct", "sig", "begin", "object"] :: [Text])) =
        let (blockTokens, afterBlock) = collectUntilOCamlEnd 1 ts
            (restTokens, remaining) = collectOCamlTestDef afterBlock
         in (t : blockTokens ++ restTokens, remaining)
    | isTokenBracket t && (tToken t `elem` (["(", "[", "{"] :: [Text])) =
        let closeB = case tToken t of
                "(" -> ")"
                "[" -> "]"
                _ -> "}"
            (bracketTokens, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts
            (restTokens, remaining) = collectOCamlTestDef afterBracket
         in (t : bracketTokens ++ restTokens, remaining)
    | isTokenKeyword t && tToken t == "let" =
        if isLocalLetIn ts
            then
                let (collected, remaining) = collectOCamlTestDef ts
                 in (t : collected, remaining)
            else ([], t : ts)
    | isTokenKeyword t && isOCamlTopItemKeyword (tToken t) =
        ([], t : ts)
    | otherwise =
        let (collected, remaining) = collectOCamlTestDef ts
         in (t : collected, remaining)
