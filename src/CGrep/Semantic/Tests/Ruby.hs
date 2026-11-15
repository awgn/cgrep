{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Ruby () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Ruby where
    isTestFile = isRubyTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideRuby keepTests False initialLineState tokens

-- | Recognizes RSpec and Minitest test files (`*_spec.rb`, `*_test.rb`, `test_*.rb`).
isRubyTestFilePath :: OsPath -> Bool
isRubyTestFilePath path =
    takeExtension path == [osp|.rb|]
        && ( [osp|_spec|] `OS.isSuffixOf` base
                || [osp|_test|] `OS.isSuffixOf` base
                || [osp|test_|] `OS.isPrefixOf` base
           )
  where
    base = takeBaseName path

-- | RSpec / Minitest / ActiveSupport::TestCase DSL block starters.
isRubyTestBlockName :: Text -> Bool
isRubyTestBlockName name =
    name
        `elem` ([ "describe"
                , "fdescribe"
                , "xdescribe"
                , "context"
                , "fcontext"
                , "xcontext"
                , "it"
                , "fit"
                , "xit"
                , "its"
                , "specify"
                , "fspecify"
                , "xspecify"
                , "example"
                , "fexample"
                , "xexample"
                , "scenario"
                , "feature"
                , "before"
                , "after"
                , "around"
                , "let"
                , "subject"
                , "test"
                , "shared_examples"
                , "shared_examples_for"
                , "shared_context"
                , "it_behaves_like"
                , "it_should_behave_like"
                , "include_examples"
                , "include_context"
                ] :: [Text])

-- | RSpec shared-example inclusion helpers that are valid even without a `do ... end` / `{ ... }` block.
isRubyBracelessRSpecCall :: Text -> Bool
isRubyBracelessRSpecCall name =
    name
        `elem` ([ "it_behaves_like"
                , "it_should_behave_like"
                , "include_examples"
                , "include_context"
                ] :: [Text])

data RubyBlockKind
    = RubyDoBlock [Token] LineState [Token]
    | RubyBraceBlock [Token] LineState [Token]

-- | Look ahead up to `n` tokens for the opening `do` or `{` of an RSpec/Minitest block,
-- tracking `(...)` and `[...]` depth and stopping across uncontinued line breaks at depth 0.
findRubyBlockStart :: Int -> LineState -> [Token] -> Maybe RubyBlockKind
findRubyBlockStart n0 ls0 = go 0 [] False n0 ls0
  where
    go :: Int -> [Token] -> Bool -> Int -> LineState -> [Token] -> Maybe RubyBlockKind
    go _ _ _ 0 _ _ = Nothing
    go _ _ _ _ _ [] = Nothing
    go !depth acc !prevComma !n !ls (t : ts) =
        let (isFirst, _, lsAfterT) = stepLineState ls t
         in if depth == 0 && isFirst && not (null acc) && not prevComma
                then Nothing
                else
                    if isTokenBracket t && (tToken t == "(" || tToken t == "[")
                        then go (depth + 1) (t : acc) False (n - 1) lsAfterT ts
                        else
                            if isTokenBracket t && (tToken t == ")" || tToken t == "]")
                                then go (max 0 (depth - 1)) (t : acc) False (n - 1) lsAfterT ts
                                else
                                    if depth == 0 && (isTokenKeyword t || isTokenIdentifier t) && tToken t == "do"
                                        then Just (RubyDoBlock (reverse (t : acc)) lsAfterT ts)
                                        else
                                            if depth == 0 && isTokenBracket t && tToken t == "{"
                                                then Just (RubyBraceBlock (reverse (t : acc)) lsAfterT ts)
                                                else
                                                    if depth == 0
                                                        && ( tToken t == "="
                                                                || tToken t == ";"
                                                                || ( (isTokenKeyword t || isTokenIdentifier t)
                                                                        && tToken t `elem` (["def", "class", "module", "end"] :: [Text])
                                                                   )
                                                           )
                                                        then Nothing
                                                        else go depth (t : acc) (tToken t == ",") (n - 1) lsAfterT ts

-- | Checks whether a `def` declaration is a Ruby 3.0+ endless method (`def foo(x) = expr`),
-- which has no closing `end` keyword.
isRubyEndlessDef :: LineState -> [Token] -> Bool
isRubyEndlessDef lsAfterDef ts0 =
    let (lsAfterName, afterName) = skipDefName lsAfterDef ts0
        (lsAfterParams, afterParams) = case afterName of
            (tOpen : rest)
                | isTokenBracket tOpen && tToken tOpen == "(" ->
                    let (insideP, remP) = processInsideBrackets "(" ")" 1 rest
                     in (advanceLineState lsAfterName (tOpen : insideP), remP)
            _ -> (lsAfterName, afterName)
     in case afterParams of
            (tEq : restAfterEq) ->
                let (eqFirst, _, _) = stepLineState lsAfterParams tEq
                    nextIsAdjOp = case restAfterEq of
                        (t2 : _) -> isTokenOperator t2 && areAdjacentTokens tEq t2
                        [] -> False
                 in not eqFirst && isTokenOperator tEq && tToken tEq == "=" && not nextIsAdjOp
            [] -> False
  where
    skipDefName !ls (t : ts) =
        let (isFirst, _, ls') = stepLineState ls t
         in if isFirst
                then (ls, t : ts)
                else
                    if isTokenIdentifier t
                        || isTokenKeyword t
                        || (isTokenOperator t && tToken t `elem` ([".", "::", ":", "!", "?"] :: [Text]))
                        then skipDefName ls' ts
                        else (ls, t : ts)
    skipDefName !ls [] = (ls, [])

-- | Collects tokens of a single-line Ruby statement (balancing `()`, `[]`, `{}` across lines),
-- stopping before `;` or the first unbracketed token on a subsequent line.
collectRubySingleLineStmt :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
collectRubySingleLineStmt _ !ls [] = ([], ls, [])
collectRubySingleLineStmt !bDepth !ls allToks@(t : ts) =
    let (isFirst, _, lsAfterT) = stepLineState ls t
     in if bDepth == 0 && (isFirst || tToken t == ";")
            then ([], ls, allToks)
            else
                let !nextDepth = updateRubyBrackets bDepth t
                    (rest, lsEnd, remaining) = collectRubySingleLineStmt nextDepth lsAfterT ts
                 in (t : rest, lsEnd, remaining)
  where
    updateRubyBrackets !d tok
        | isTokenBracket tok =
            case tToken tok of
                "(" -> d + 1
                "[" -> d + 1
                "{" -> d + 1
                ")" -> max 0 (d - 1)
                "]" -> max 0 (d - 1)
                "}" -> max 0 (d - 1)
                _ -> d
        | otherwise = d

-- | Check whether a Ruby `class` header is a test class (`TestFoo`, `FooTest`, `FooTestCase`, `FooSpec`,
-- or `< Minitest::Test` / `< ActiveSupport::TestCase`), rejecting domain words like `Testimonial`.
isRubyTestClass :: Token -> [Token] -> Bool
isRubyTestClass classNameTok afterClassName =
    let cname = tToken classNameTok
        prefixOk = case T.stripPrefix "Test" cname of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
        nameMatch =
            prefixOk
                || ("Test" `T.isSuffixOf` cname && T.length cname > 4)
                || "Tests" `T.isSuffixOf` cname
                || "TestCase" `T.isSuffixOf` cname
        superMatch = case afterClassName of
            (tLt : rest)
                | isTokenOperator tLt && tToken tLt == "<" ->
                    let superTokens = takeWhile (\t -> isTokenIdentifier t || (isTokenOperator t && (tToken t == ":" || tToken t == "::"))) rest
                     in any (\t -> isTokenIdentifier t && ("Test" `T.isInfixOf` tToken t || "Spec" `T.isSuffixOf` tToken t)) superTokens
            _ -> False
     in nameMatch || superMatch

-- | Processes Ruby tokens outside a test block.
processOutsideRuby :: Bool -> Bool -> LineState -> [Token] -> [Token]
processOutsideRuby _ _ _ [] = []
-- Pattern 0: `RSpec.describe ...` / `RSpec.shared_examples ...` / `RSpec.shared_context ...`
processOutsideRuby keepTests _ ls (tRspec : tDot : tDesc : ts)
    | isTokenIdentifier tRspec && tToken tRspec == "RSpec"
    , isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tDesc && isRubyTestBlockName (tToken tDesc)
    , let lsAfterDesc = advanceLineState ls [tRspec, tDot, tDesc]
    , Just blockKind <- findRubyBlockStart 15 lsAfterDesc ts =
        case blockKind of
            RubyDoBlock headerAndDo lsAfterDo afterDo ->
                let (bodyAndEnd, lsAfterEnd, remainingTokens) = collectUntilRubyEnd 1 Nothing Nothing lsAfterDo afterDo
                 in if keepTests
                        then tRspec : tDot : tDesc : headerAndDo ++ bodyAndEnd ++ processOutsideRuby keepTests False lsAfterEnd remainingTokens
                        else processOutsideRuby keepTests False lsAfterEnd remainingTokens
            RubyBraceBlock headerAndBrace lsAfterBrace afterBrace ->
                let (bodyAndClose, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
                    lsAfterClose = advanceLineState lsAfterBrace bodyAndClose
                 in if keepTests
                        then tRspec : tDot : tDesc : headerAndBrace ++ bodyAndClose ++ processOutsideRuby keepTests False lsAfterClose remainingTokens
                        else processOutsideRuby keepTests False lsAfterClose remainingTokens
-- Pattern 1: Unqualified RSpec / ActiveSupport::TestCase DSL block or `it_behaves_like` call
processOutsideRuby keepTests False ls (t1 : ts)
    | isTokenIdentifier t1 && isRubyTestBlockName (tToken t1) =
        let (_, _, lsAfterT1) = stepLineState ls t1
         in case findRubyBlockStart 15 lsAfterT1 ts of
                Just (RubyDoBlock headerAndDo lsAfterDo afterDo) ->
                    let (bodyAndEnd, lsAfterEnd, remainingTokens) = collectUntilRubyEnd 1 Nothing Nothing lsAfterDo afterDo
                     in if keepTests
                            then t1 : headerAndDo ++ bodyAndEnd ++ processOutsideRuby keepTests False lsAfterEnd remainingTokens
                            else processOutsideRuby keepTests False lsAfterEnd remainingTokens
                Just (RubyBraceBlock headerAndBrace lsAfterBrace afterBrace) ->
                    let (bodyAndClose, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
                        lsAfterClose = advanceLineState lsAfterBrace bodyAndClose
                     in if keepTests
                            then t1 : headerAndBrace ++ bodyAndClose ++ processOutsideRuby keepTests False lsAfterClose remainingTokens
                            else processOutsideRuby keepTests False lsAfterClose remainingTokens
                Nothing
                    | isRubyBracelessRSpecCall (tToken t1) ->
                        let (stmtToks, lsAfterStmt, remainingTokens) = collectRubySingleLineStmt 0 lsAfterT1 ts
                         in if keepTests
                                then t1 : stmtToks ++ processOutsideRuby keepTests False lsAfterStmt remainingTokens
                                else processOutsideRuby keepTests False lsAfterStmt remainingTokens
                    | otherwise ->
                        if keepTests
                            then processOutsideRuby keepTests False lsAfterT1 ts
                            else t1 : processOutsideRuby keepTests False lsAfterT1 ts
-- Pattern 2: `def test_*`, `def setup`, `def teardown` (Minitest, including Ruby 3 endless `def test_x = ...`)
processOutsideRuby keepTests _ ls (tDef : tName : ts)
    | (isTokenKeyword tDef || isTokenIdentifier tDef) && tToken tDef == "def"
    , isTokenIdentifier tName && ("test_" `T.isPrefixOf` tToken tName || tToken tName == "setup" || tToken tName == "teardown") =
        let (_, _, lsAfterDef) = stepLineState ls tDef
            (_, _, lsAfterName) = stepLineState lsAfterDef tName
         in if isRubyEndlessDef lsAfterDef (tName : ts)
                then
                    let (stmtToks, lsAfterStmt, remainingTokens) = collectRubySingleLineStmt 0 lsAfterName ts
                     in if keepTests
                            then tDef : tName : stmtToks ++ processOutsideRuby keepTests False lsAfterStmt remainingTokens
                            else processOutsideRuby keepTests False lsAfterStmt remainingTokens
                else
                    let (testTokens, lsAfterEnd, remainingTokens) = collectUntilRubyEnd 1 (Just tDef) (Just tName) lsAfterName ts
                     in if keepTests
                            then tDef : tName : testTokens ++ processOutsideRuby keepTests False lsAfterEnd remainingTokens
                            else processOutsideRuby keepTests False lsAfterEnd remainingTokens
-- Pattern 3: `class Test*`, `class *Test`, `class *Spec`, or `class Foo < Minitest::Test`
processOutsideRuby keepTests _ ls (tClass : tName : ts)
    | (isTokenKeyword tClass || isTokenIdentifier tClass) && tToken tClass == "class"
    , isTokenIdentifier tName && isRubyTestClass tName ts =
        let lsAfterName = advanceLineState ls [tClass, tName]
            (testTokens, lsAfterEnd, remainingTokens) = collectUntilRubyEnd 1 (Just tClass) (Just tName) lsAfterName ts
         in if keepTests
                then tClass : tName : testTokens ++ processOutsideRuby keepTests False lsAfterEnd remainingTokens
                else processOutsideRuby keepTests False lsAfterEnd remainingTokens
-- Fallback
processOutsideRuby keepTests _ ls (t : ts) =
    let (_, _, lsAfterT) = stepLineState ls t
        isBlocker =
            (isTokenOperator t && (tToken t == "." || tToken t == "&."))
                || ((isTokenKeyword t || isTokenIdentifier t) && tToken t == "def")
     in if keepTests
            then processOutsideRuby keepTests isBlocker lsAfterT ts
            else t : processOutsideRuby keepTests isBlocker lsAfterT ts

-- | Determines whether a same-line `if`/`unless` or `while`/`until` opens a block with `end`
-- (`x = if cond ... end`, `; if cond ... end`) vs acting as a postfix modifier (`return x if cond`,
-- `save! unless valid?`) that has NO `end`.
opensSameLineRubyCondBlock :: Text -> Maybe Token -> Maybe Token -> Bool
opensSameLineRubyCondBlock _ _ Nothing = True
opensSameLineRubyCondBlock kw mPrevPrev (Just p)
    | kw == "while" || kw == "until" =
        tToken p `elem` ([";", "(", "[", "{", ",", "="] :: [Text])
    | tToken p `elem` ([";", "(", "[", "{", ","] :: [Text]) = True
    | (isTokenKeyword p || isTokenIdentifier p)
        && tToken p
            `elem` ([ "begin"
                    , "do"
                    , "else"
                    , "elsif"
                    , "then"
                    , "when"
                    , "rescue"
                    , "ensure"
                    , "and"
                    , "or"
                    , "not"
                    ] :: [Text]) =
        True
    | isTokenOperator p =
        let isMethodSuffix =
                (tToken p == "!" || tToken p == "?")
                    && maybe False (\pp -> (isTokenIdentifier pp || isTokenKeyword pp) && areAdjacentTokens pp p) mPrevPrev
         in not isMethodSuffix
    | otherwise = False

-- | Collect tokens until the matching `end` keyword in Ruby, accurately distinguishing:
--   * block `if`/`unless`/`while`/`until` vs postfix modifiers
--   * Ruby 3 endless `def foo = ...` inside classes/blocks
--   * `.end` method calls, `:end` symbols, and `end:` hash keys
collectUntilRubyEnd :: Int -> Maybe Token -> Maybe Token -> LineState -> [Token] -> ([Token], LineState, [Token])
collectUntilRubyEnd _ _ _ !ls [] = ([], ls, [])
collectUntilRubyEnd !depth mPrevPrev mPrev !ls (t : ts) =
    let (isFirst, _, lsAfterT) = stepLineState ls t
        prevIsMethodDot = maybe False (\p -> isTokenOperator p && (tToken p == "." || tToken p == "&.")) mPrev
        prevIsSymColon = maybe False (\p -> isTokenOperator p && tToken p == ":" && areAdjacentTokens p t) mPrev
        nextIsHashColon = case ts of
            (c : _) -> isTokenOperator c && tToken c == ":" && areAdjacentTokens t c
            [] -> False
        isGuardedIdent = prevIsMethodDot || prevIsSymColon || nextIsHashColon
        tokTxt = tToken t
     in if not isGuardedIdent && (isTokenKeyword t || isTokenIdentifier t)
            then
                if tokTxt == "def"
                    then
                        let !nextDepth = if isRubyEndlessDef lsAfterT ts then depth else depth + 1
                            (collected, lsEnd, remaining) = collectUntilRubyEnd nextDepth mPrev (Just t) lsAfterT ts
                         in (t : collected, lsEnd, remaining)
                    else
                        if tokTxt `elem` (["do", "class", "module", "for", "case", "begin"] :: [Text])
                            then
                                let (collected, lsEnd, remaining) = collectUntilRubyEnd (depth + 1) mPrev (Just t) lsAfterT ts
                                 in (t : collected, lsEnd, remaining)
                            else
                                if tokTxt `elem` (["if", "unless", "while", "until"] :: [Text])
                                    then
                                        let opensBlock = isFirst || opensSameLineRubyCondBlock tokTxt mPrevPrev mPrev
                                            !nextDepth = if opensBlock then depth + 1 else depth
                                            (collected, lsEnd, remaining) = collectUntilRubyEnd nextDepth mPrev (Just t) lsAfterT ts
                                         in (t : collected, lsEnd, remaining)
                                    else
                                        if tokTxt == "end"
                                            then
                                                if depth <= 1
                                                    then ([t], lsAfterT, ts)
                                                    else
                                                        let (collected, lsEnd, remaining) = collectUntilRubyEnd (depth - 1) mPrev (Just t) lsAfterT ts
                                                         in (t : collected, lsEnd, remaining)
                                            else
                                                let (collected, lsEnd, remaining) = collectUntilRubyEnd depth mPrev (Just t) lsAfterT ts
                                                 in (t : collected, lsEnd, remaining)
            else
                let (collected, lsEnd, remaining) = collectUntilRubyEnd depth mPrev (Just t) lsAfterT ts
                 in (t : collected, lsEnd, remaining)
