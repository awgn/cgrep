{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Erlang () where

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
import qualified Data.Text as T
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Erlang where
    isTestFile = isErlangTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideErlang keepTests True tokens

-- | Common Test (`*_SUITE.erl`) and Rebar3 EUnit (`*_tests.erl`) file conventions.
isErlangTestFilePath :: OsPath -> Bool
isErlangTestFilePath path =
    let ext = takeExtension path
        base = takeBaseName path
     in ext `elem` ([[osp|.erl|], [osp|.ERL|]] :: [OsPath])
            && ([osp|_SUITE|] `OS.isSuffixOf` base || [osp|_tests|] `OS.isSuffixOf` base)

-- | Match Erlang's `->` arrow operator in both `--strict` (`"->"`)
-- and default (`"-"` adjacent to `">"`) modes.
matchErlangArrow :: [Token] -> Maybe ([Token], [Token])
matchErlangArrow (t1 : ts)
    | isTokenOperator t1 && tToken t1 == "->" = Just ([t1], ts)
matchErlangArrow (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "-"
    , isTokenOperator t2 && tToken t2 == ">"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], ts)
matchErlangArrow _ = Nothing

-- | Find the `->` of a function clause (allowing optional `when ...` guard expressions),
-- aborting if `.` or `;` appears at top level before `->`.
findErlangClauseArrow :: Int -> [Token] -> Maybe ([Token], [Token])
findErlangClauseArrow = go 0 []
  where
    go :: Int -> [Token] -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ _ 0 _ = Nothing
    go _ _ _ [] = Nothing
    go !depth acc !n tokens@(t : ts)
        | depth == 0
        , Just (arrowToks, afterArrow) <- matchErlangArrow tokens =
            Just (reverse arrowToks ++ reverse acc, afterArrow)
        | isTokenBracket t && (tToken t `elem` (["(", "[", "{"] :: [Text])) =
            go (depth + 1) (t : acc) (n - 1) ts
        | isTokenBracket t && (tToken t `elem` ([")", "]", "}"] :: [Text])) =
            go (max 0 (depth - 1)) (t : acc) (n - 1) ts
        | depth == 0 && isTokenOperator t && (tToken t == "." || tToken t == ";") =
            Nothing
        | otherwise =
            go depth (t : acc) (n - 1) ts

-- | Collect tokens of an Erlang function definition until its terminating `.`,
-- ignoring `.` used in record field access (`#record.field`), character literal `$.`,
-- and inside `(...)` / `[...]` / `{...}`.
collectUntilErlangEnd :: Maybe Token -> [Token] -> ([Token], [Token])
collectUntilErlangEnd _ [] = ([], [])
-- Record field access `#rec.field`: `.` here is NOT the end of the function!
collectUntilErlangEnd _ (t1 : t2 : t3 : t4 : ts)
    | isTokenOperator t1 && tToken t1 == "#"
    , isTokenIdentifier t2
    , isTokenOperator t3 && tToken t3 == "."
    , isTokenIdentifier t4 =
        let (collected, remaining) = collectUntilErlangEnd (Just t4) ts
         in (t1 : t2 : t3 : t4 : collected, remaining)
collectUntilErlangEnd prev (t : ts)
    | isTokenBracket t && (tToken t `elem` (["(", "[", "{"] :: [Text])) =
        let closeB = case tToken t of
                "(" -> ")"
                "[" -> "]"
                _ -> "}"
            (bracketTokens, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts
            lastTok = case reverse (t : bracketTokens) of
                (lt : _) -> Just lt
                [] -> Just t
            (restTokens, remaining) = collectUntilErlangEnd lastTok afterBracket
         in (t : bracketTokens ++ restTokens, remaining)
    | isTokenOperator t && tToken t == "."
    , not (isDollarDotLiteral prev t) =
        ([t], ts)
    | otherwise =
        let (collected, remaining) = collectUntilErlangEnd (Just t) ts
         in (t : collected, remaining)
  where
    isDollarDotLiteral :: Maybe Token -> Token -> Bool
    isDollarDotLiteral (Just p) dotTok =
        isTokenOperator p && tToken p == "$" && areAdjacentTokens p dotTok
    isDollarDotLiteral Nothing _ = False

-- | Collect tokens inside `-ifdef(TEST).` / `-ifdef(EUNIT).`, balancing nested
-- `-ifdef(...)` / `-ifndef(...)` up to matching `-endif.`.
-- If a top-level (`depth == 1`) `-else.` branch is reached, stops at `-else.` so
-- the non-test fallback branch remains production code.
collectErlangIfdefTest :: Int -> [Token] -> ([Token], [Token])
collectErlangIfdefTest _ [] = ([], [])
collectErlangIfdefTest !depth (tDash : tDir : tOpen : ts)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenIdentifier tDir && (tToken tDir == "ifdef" || tToken tDir == "ifndef")
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (insideParens, afterParens) = processInsideBrackets "(" ")" 1 ts
            (dotToks, afterDot) = consumeOptionalDot afterParens
            (more, remaining) = collectErlangIfdefTest (depth + 1) afterDot
         in (tDash : tDir : tOpen : insideParens ++ dotToks ++ more, remaining)
collectErlangIfdefTest !depth (tDash : tDir : tDot : ts)
    | isTokenOperator tDash && tToken tDash == "-"
    , (isTokenIdentifier tDir || isTokenKeyword tDir) && tToken tDir == "else"
    , isTokenOperator tDot && tToken tDot == "."
    , depth == 1 =
        ([tDash, tDir, tDot], ts)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenIdentifier tDir && tToken tDir == "endif"
    , isTokenOperator tDot && tToken tDot == "." =
        if depth <= 1
            then ([tDash, tDir, tDot], ts)
            else
                let (more, remaining) = collectErlangIfdefTest (depth - 1) ts
                 in (tDash : tDir : tDot : more, remaining)
collectErlangIfdefTest !depth (t : ts) =
    let (more, remaining) = collectErlangIfdefTest depth ts
     in (t : more, remaining)

consumeOptionalDot :: [Token] -> ([Token], [Token])
consumeOptionalDot (t : ts)
    | isTokenOperator t && tToken t == "." = ([t], ts)
consumeOptionalDot ts = ([], ts)

-- | Match `-ifdef(TEST).` / `-ifdef(EUNIT).` / `-ifdef(PROPER).`.
matchErlangIfdefTest :: [Token] -> Maybe ([Token], [Token])
matchErlangIfdefTest (tDash : tIfdef : tOpen : tMacro : tClose : tDot : ts)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenIdentifier tIfdef && tToken tIfdef == "ifdef"
    , isTokenBracket tOpen && tToken tOpen == "("
    , isTokenIdentifier tMacro
    , tToken tMacro `elem` (["TEST", "EUNIT", "PROPER", "CT_TEST"] :: [Text])
    , isTokenBracket tClose && tToken tClose == ")"
    , isTokenOperator tDot && tToken tDot == "." =
        let (bodyToks, remaining) = collectErlangIfdefTest 1 ts
         in Just ([tDash, tIfdef, tOpen, tMacro, tClose, tDot] ++ bodyToks, remaining)
matchErlangIfdefTest _ = Nothing

-- | Match `-include_lib(".../eunit.hrl").`, `ct.hrl`, or `proper.hrl`.
matchErlangTestInclude :: [Token] -> Maybe ([Token], [Token])
matchErlangTestInclude (tDash : tInc : tOpen : tStr : tClose : tDot : ts)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenIdentifier tInc && (tToken tInc == "include_lib" || tToken tInc == "include")
    , isTokenBracket tOpen && tToken tOpen == "("
    , isTokenString tStr
    , isTestHeaderString (tToken tStr)
    , isTokenBracket tClose && tToken tClose == ")"
    , isTokenOperator tDot && tToken tDot == "." =
        Just ([tDash, tInc, tOpen, tStr, tClose, tDot], ts)
  where
    isTestHeaderString s =
        "eunit.hrl\"" `T.isSuffixOf` s
            || "ct.hrl\"" `T.isSuffixOf` s
            || "proper.hrl\"" `T.isSuffixOf` s
matchErlangTestInclude _ = Nothing

isZeroArityErlangTestName :: Text -> Bool
isZeroArityErlangTestName name =
    "_test" `T.isSuffixOf` name
        || "_test_" `T.isSuffixOf` name
        || ("prop_" `T.isPrefixOf` name && T.length name > 5)

isCommonTestCallback :: Text -> Bool
isCommonTestCallback name =
    name
        `elem` ([ "init_per_suite"
                , "end_per_suite"
                , "init_per_group"
                , "end_per_group"
                , "init_per_testcase"
                , "end_per_testcase"
                ] :: [Text])

-- | Match 0-arity EUnit/PropEr definitions (`*_test()`, `*_test_()`, `prop_*()`)
-- and Common Test lifecycle callbacks (`init_per_suite(...)`, etc.).
matchErlangTestFunc :: [Token] -> Maybe ([Token], [Token])
matchErlangTestFunc (t1 : t2 : t3 : ts)
    | isTokenIdentifier t1 && isZeroArityErlangTestName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenBracket t3 && tToken t3 == ")"
    , Just (guardAndArrow, afterArrow) <- findErlangClauseArrow 60 ts =
        let (testTokens, remaining) = collectUntilErlangEnd Nothing afterArrow
         in Just (t1 : t2 : t3 : guardAndArrow ++ testTokens, remaining)
matchErlangTestFunc (t1 : t2 : ts)
    | isTokenIdentifier t1 && isCommonTestCallback (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
         in case findErlangClauseArrow 60 afterParams of
                Just (guardAndArrow, afterArrow) ->
                    let (testTokens, remaining) = collectUntilErlangEnd Nothing afterArrow
                     in Just (t1 : t2 : params ++ guardAndArrow ++ testTokens, remaining)
                Nothing -> Nothing
matchErlangTestFunc _ = Nothing

-- | Processes Erlang tokens outside test functions and `-ifdef(TEST).` blocks.
-- `atFormStart` is `True` at module start and right after top-level `.` terminators,
-- preventing remote calls like `Mod:my_test()` from starting a test block.
processOutsideErlang :: Bool -> Bool -> [Token] -> [Token]
processOutsideErlang _ _ [] = []
processOutsideErlang keepTests atFormStart tokens@(t : ts) =
    let matched
            | atFormStart =
                matchErlangIfdefTest tokens
                    <|> matchErlangTestInclude tokens
                    <|> matchErlangTestFunc tokens
            | otherwise =
                matchErlangIfdefTest tokens
     in case matched of
            Just (testToks, remaining) ->
                let restOut = processOutsideErlang keepTests True remaining
                 in if keepTests
                        then testToks ++ restOut
                        else restOut
            Nothing ->
                let nextFormStart = isTokenOperator t && tToken t == "."
                 in if keepTests
                        then processOutsideErlang keepTests nextFormStart ts
                        else t : processOutsideErlang keepTests nextFormStart ts
