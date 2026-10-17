{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Elm () where

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
import Data.Text (Text)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI

instance LanguageTestFilter 'Elm where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideElm keepTests False tokens

-- | Checks in O(1) whether a token starts at column 0 (beginning of file or immediately after '\n').
-- In Elm, all top-level declarations must start at column 0.
isAtColumnZero :: Token -> Bool
isAtColumnZero tok =
    case tToken tok of
        TI.Text arr off _ ->
            off == 0 || A.unsafeIndex arr (off - 1) == 10

-- | `elm-test` functions that take a string description as their first argument.
isElmStringTestBuilder :: Text -> Bool
isElmStringTestBuilder name =
    name `elem` (["describe", "test", "todo"] :: [Text])

-- | `elm-test` fuzz builders (`fuzz`, `fuzz2`, `fuzz3`, `fuzzWith`).
isElmFuzzBuilder :: Text -> Bool
isElmFuzzBuilder name =
    name `elem` (["fuzz", "fuzz2", "fuzz3", "fuzzWith"] :: [Text])

-- | Checks if a token stream starts with a single `:` (and not `::`).
startsWithSingleColon :: [Token] -> Bool
startsWithSingleColon (x1 : x2 : _)
    | isTokenOperator x1 && tToken x1 == ":"
    , isTokenOperator x2 && tToken x2 == ":" =
        False
startsWithSingleColon (x : _)
    | isTokenOperator x && tToken x == ":" =
        True
startsWithSingleColon _ = False

-- | Checks if a token stream starts with a single `=` (and not `==` or `=>`).
startsWithSingleEquals :: [Token] -> Bool
startsWithSingleEquals (x1 : x2 : _)
    | isTokenOperator x1 && tToken x1 == "="
    , isTokenOperator x2 && (tToken x2 == "=" || tToken x2 == ">") =
        False
startsWithSingleEquals (x : _)
    | isTokenOperator x && tToken x == "=" =
        True
startsWithSingleEquals _ = False

-- | Checks if `: Test` or `: Test.Test` follows an identifier at top level.
isElmTestTypeSignature :: [Token] -> Bool
isElmTestTypeSignature (tColon : tType : rest)
    | isTokenOperator tColon && tToken tColon == ":"
    , not (startsWithSingleColon rest)
    , isTokenIdentifier tType && tToken tType == "Test" =
        case rest of
            (tDot : tSub : _)
                | isTokenOperator tDot && tToken tDot == "." ->
                    isTokenIdentifier tSub && tToken tSub == "Test"
            _ -> True
isElmTestTypeSignature _ = False

-- | Checks if a token sequence starts with an `elm-test` builder call
-- (`describe "..."`, `test "..."`, `todo "..."`, `fuzz ... "..."`, or `Test.describe "..."` etc.).
startsWithElmTestBuilder :: [Token] -> Bool
startsWithElmTestBuilder (tMod : tDot : rest)
    | isTokenIdentifier tMod && tToken tMod == "Test"
    , isTokenOperator tDot && tToken tDot == "." =
        startsWithUnqualifiedElmBuilder rest
startsWithElmTestBuilder ts = startsWithUnqualifiedElmBuilder ts

startsWithUnqualifiedElmBuilder :: [Token] -> Bool
startsWithUnqualifiedElmBuilder (t1 : t2 : _)
    | isTokenIdentifier t1 && isElmStringTestBuilder (tToken t1)
    , isTokenString t2 =
        True
startsWithUnqualifiedElmBuilder (t1 : rest)
    | isTokenIdentifier t1 && isElmFuzzBuilder (tToken t1) =
        any isTokenString (take 8 rest)
startsWithUnqualifiedElmBuilder _ = False

-- | Checks if a token sequence starting with a column-0 identifier is a top-level Elm binding
-- `<ident> <0..N params> = ...`.
isTopLevelElmBinding :: [Token] -> Bool
isTopLevelElmBinding (tName : rest)
    | isTokenIdentifier tName && isAtColumnZero tName = checkParams 8 rest
  where
    checkParams :: Int -> [Token] -> Bool
    checkParams 0 _ = False
    checkParams _ [] = False
    checkParams n ts@(t : ts')
        | startsWithSingleEquals ts = True
        | isTokenIdentifier t && not (isAtColumnZero t) = checkParams (n - 1) ts'
        | isTokenBracket t && (tToken t == "(" || tToken t == "{") && not (isAtColumnZero t) =
            let closeB = if tToken t == "(" then ")" else "}"
                (_, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts'
             in checkParams (n - 1) afterBracket
        | otherwise = False
isTopLevelElmBinding _ = False

-- | Processes Elm tokens outside of a test block.
processOutsideElm :: Bool -> Bool -> [Token] -> [Token]
processOutsideElm _ _ [] = []
-- Pattern 1: Top-level `<name> : Test` or `<name> : Test.Test` followed by `<name> = ...`
processOutsideElm keepTests False (tName : ts)
    | isTokenIdentifier tName && isAtColumnZero tName && isElmTestTypeSignature ts =
        let (testTokens, remaining) = collectUntilNextElmDef (Just (tToken tName)) 0 0 False ts
         in if keepTests
                then tName : testTokens ++ processOutsideElm keepTests False remaining
                else processOutsideElm keepTests False remaining
-- Pattern 2: Top-level unannotated `<name> = describe "..." ...` / `<name> = test "..." ...`
processOutsideElm keepTests False (tName : tEq : ts)
    | isTokenIdentifier tName && isAtColumnZero tName
    , startsWithSingleEquals (tEq : ts)
    , startsWithElmTestBuilder ts =
        let (testTokens, remaining) = collectUntilNextElmDef Nothing 0 0 True ts
         in if keepTests
                then tName : tEq : testTokens ++ processOutsideElm keepTests False remaining
                else processOutsideElm keepTests False remaining
-- Pattern 3: Standalone `describe "..."` / `test "..."` / `fuzz ...`
processOutsideElm keepTests False ts@(t1 : rest)
    | startsWithElmTestBuilder ts =
        let (testTokens, remaining) = collectUntilNextElmDef Nothing 0 0 True rest
         in if keepTests
                then t1 : testTokens ++ processOutsideElm keepTests False remaining
                else processOutsideElm keepTests False remaining
-- Fallback
processOutsideElm keepTests _ (t : ts) =
    let isDot = isTokenOperator t && tToken t == "."
     in if keepTests
            then processOutsideElm keepTests isDot ts
            else t : processOutsideElm keepTests isDot ts

-- | Collects tokens of an Elm top-level test definition until the next top-level declaration.
-- Tracks bracket depth (`()`, `[]`, `{}`), `let ... in` nesting depth, and column-0 boundaries.
collectUntilNextElmDef :: Maybe Text -> Int -> Int -> Bool -> [Token] -> ([Token], [Token])
collectUntilNextElmDef _ _ _ _ [] = ([], [])
collectUntilNextElmDef mName bracketDepth letDepth passedBodyEq (x : xs)
    -- Track brackets
    | isTokenBracket x && (tToken x == "(" || tToken x == "[" || tToken x == "{") =
        let (col, remTokens) = collectUntilNextElmDef mName (bracketDepth + 1) letDepth passedBodyEq xs
         in (x : col, remTokens)
    | isTokenBracket x && (tToken x == ")" || tToken x == "]" || tToken x == "}") =
        let newBracketDepth = max 0 (bracketDepth - 1)
            (col, remTokens) = collectUntilNextElmDef mName newBracketDepth letDepth passedBodyEq xs
         in (x : col, remTokens)
    -- Track `let ... in`
    | isTokenKeyword x && tToken x == "let" =
        let (col, remTokens) = collectUntilNextElmDef mName bracketDepth (letDepth + 1) passedBodyEq xs
         in (x : col, remTokens)
    | isTokenKeyword x && tToken x == "in" =
        let newLetDepth = max 0 (letDepth - 1)
            (col, remTokens) = collectUntilNextElmDef mName bracketDepth newLetDepth passedBodyEq xs
         in (x : col, remTokens)
    -- At top level (bracketDepth == 0 && letDepth == 0 && column 0), stop at top-level keywords
    | bracketDepth == 0
        && letDepth == 0
        && isAtColumnZero x
        && isTokenKeyword x
        && (tToken x `elem` (["type", "port", "module", "import", "infix", "effect"] :: [Text])) =
        ([], x : xs)
    -- At top level (column 0), stop at the next type signature `<otherIdent> : ...`
    | bracketDepth == 0
        && letDepth == 0
        && isAtColumnZero x
        && isTokenIdentifier x
        && startsWithSingleColon xs
        && (passedBodyEq || Just (tToken x) /= mName) =
        ([], x : xs)
    -- At top level (column 0), if we are in `<name> : Test` and haven't passed `<name> =` yet,
    -- recognize `<name> =` as the start of the body (flipping `passedBodyEq` to `True`)
    | bracketDepth == 0
        && letDepth == 0
        && not passedBodyEq
        && isAtColumnZero x
        && isTokenIdentifier x
        && Just (tToken x) == mName
        && startsWithSingleEquals xs =
        case xs of
            (tEq : rest) ->
                let (col, remTokens) = collectUntilNextElmDef mName bracketDepth letDepth True rest
                 in (x : tEq : col, remTokens)
            [] -> ([x], [])
    -- At top level (column 0), once `passedBodyEq` is True, stop at the next top-level binding `<nextIdent> <params> =`
    | bracketDepth == 0
        && letDepth == 0
        && passedBodyEq
        && isTopLevelElmBinding (x : xs) =
        ([], x : xs)
    | otherwise =
        let (col, remTokens) = collectUntilNextElmDef mName bracketDepth letDepth passedBodyEq xs
         in (x : col, remTokens)
