{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Erlang () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Erlang where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideErlang keepTests tokens

-- | Match Erlang's `->` arrow operator in both `--strict` mode (`"->"`)
-- and non-strict mode (`"-"` followed by `">"`).
matchErlangArrow :: [Token] -> Maybe ([Token], [Token])
matchErlangArrow (t1 : ts)
    | isTokenOperator t1 && tToken t1 == "->" = Just ([t1], ts)
matchErlangArrow (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "-"
    , isTokenOperator t2 && tToken t2 == ">" =
        Just ([t1, t2], ts)
matchErlangArrow _ = Nothing

-- | Collect tokens of an Erlang function clause until its terminating `.`,
-- ignoring `.` used in record field access `#record.field` and inside `(...)`/`[...]`/`{...}`.
collectUntilErlangEnd :: [Token] -> ([Token], [Token])
collectUntilErlangEnd [] = ([], [])
-- Record field access `#rec.field`: `.` here is NOT the end of the function!
collectUntilErlangEnd (t1 : t2 : t3 : t4 : ts)
    | isTokenOperator t1 && tToken t1 == "#"
    , isTokenIdentifier t2
    , isTokenOperator t3 && tToken t3 == "."
    , isTokenIdentifier t4 =
        let (collected, remaining) = collectUntilErlangEnd ts
         in (t1 : t2 : t3 : t4 : collected, remaining)
collectUntilErlangEnd (t : ts)
    | isTokenBracket t && (tToken t `elem` (["(", "[", "{"] :: [Text])) =
        let closeB = case tToken t of
                "(" -> ")"
                "[" -> "]"
                _ -> "}"
            (bracketTokens, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts
            (restTokens, remaining) = collectUntilErlangEnd afterBracket
         in (t : bracketTokens ++ restTokens, remaining)
    | isTokenOperator t && tToken t == "." =
        ([t], ts)
    | otherwise =
        let (collected, remaining) = collectUntilErlangEnd ts
         in (t : collected, remaining)

-- | (Erlang) Helper: Processes tokens *outside* a test function.
-- Recognizes 0-arity EUnit test function definitions:
--   1. `*_test() -> ... .`
--   2. `*_test_() -> ... .`
processOutsideErlang :: Bool -> [Token] -> [Token]
processOutsideErlang _ [] = []
-- Pattern 1: 0-arity `*_test() ->` or `*_test_() ->`
processOutsideErlang keepTests (t1 : t2 : t3 : ts)
    | isTokenIdentifier t1 && ("_test" `T.isSuffixOf` tToken t1 || "_test_" `T.isSuffixOf` tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenBracket t3 && tToken t3 == ")"
    , Just (arrowTokens, afterArrow) <- matchErlangArrow ts =
        let (testTokens, remainingTokens) = collectUntilErlangEnd afterArrow
         in if keepTests
                then t1 : t2 : t3 : arrowTokens ++ testTokens ++ processOutsideErlang keepTests remainingTokens
                else processOutsideErlang keepTests remainingTokens
-- No test found, process the current token
processOutsideErlang keepTests (t : ts) =
    if keepTests
        then processOutsideErlang keepTests ts
        else t : processOutsideErlang keepTests ts
