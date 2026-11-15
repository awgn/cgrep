{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Elixir () where

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
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Elixir where
    isTestFile = isElixirTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideElixir keepTests False tokens

-- | Mix (`mix test`) discovers ExUnit test files via `*_test.exs`.
isElixirTestFilePath :: OsPath -> Bool
isElixirTestFilePath path =
    takeExtension path == [osp|.exs|]
        && [osp|_test|] `OS.isSuffixOf` takeBaseName path

-- | True if no newline (`\n`) separates `t1` and `t2`.
onSameLine :: Token -> Token -> Bool
onSameLine t1 t2 =
    case (tToken t1, tToken t2) of
        (TI.Text arr off1 len1, TI.Text _ off2 _) ->
            scanNoNewline arr (off1 + len1) off2
  where
    scanNoNewline !arr !p !end
        | p >= end = True
        | A.unsafeIndex arr p == 10 = False
        | otherwise = scanNoNewline arr (p + 1) end

isWord :: Token -> Text -> Bool
isWord t w = (isTokenKeyword t || isTokenIdentifier t) && tToken t == w

-- | True when `t` is preceded by an adjacent `:` (`:end`, `:do`, `:fn` atom)
-- or by `.` (`span.end` field/remote call), so `t` is NOT a block delimiter.
isAtomOrDotAccess :: Maybe Token -> Token -> Bool
isAtomOrDotAccess (Just p) t =
    (isTokenOperator p && tToken p == ":" && areAdjacentTokens p t)
        || (isTokenOperator p && tToken p == ".")
isAtomOrDotAccess Nothing _ = False

-- | True when `t` is immediately followed by an adjacent `:` (`do:`, `end:`, `fn:` keyword key).
isKeywordListKey :: Token -> [Token] -> Bool
isKeywordListKey t (c : _) =
    isTokenOperator c && tToken c == ":" && areAdjacentTokens t c
isKeywordListKey _ [] = False

isElixirBlockOpen :: Maybe Token -> Token -> [Token] -> Bool
isElixirBlockOpen prev t rest =
    (isWord t "do" || isWord t "fn")
        && not (isAtomOrDotAccess prev t)
        && not (isKeywordListKey t rest)

isElixirBlockClose :: Maybe Token -> Token -> [Token] -> Bool
isElixirBlockClose prev t rest =
    isWord t "end"
        && not (isAtomOrDotAccess prev t)
        && not (isKeywordListKey t rest)

-- | Extract a dotted Elixir module alias `Foo.Bar.BazTest` starting at the head of
-- the token stream, returning `(modulePathTokens, lastSegmentName, remainingTokens)`.
extractElixirModulePath :: [Token] -> Maybe ([Token], Text, [Token])
extractElixirModulePath (t1 : ts)
    | isTokenIdentifier t1 = Just (go [t1] (tToken t1) ts)
  where
    go acc _ (tDot : tNext : rest)
        | isTokenOperator tDot && tToken tDot == "." && isTokenIdentifier tNext =
            go (acc ++ [tDot, tNext]) (tToken tNext) rest
    go acc lastSeg rest = (acc, lastSeg, rest)
extractElixirModulePath _ = Nothing

data ElixirDoHeader
    = ElixirDoBlock [Token] [Token] -- (tokens up to and including block `do`, tokens after `do`)
    | ElixirDoInline [Token] [Token] -- (tokens up to and including `do` + `:`, tokens after `:`)

-- | Find the opening `do` block or inline `do:` keyword of an ExUnit macro within
-- `n` tokens, aborting if a new declaration or ExUnit macro starts at top level.
findElixirDo :: Int -> Maybe Token -> [Token] -> Maybe ElixirDoHeader
findElixirDo n0 prev0 = go 0 [] prev0 n0
  where
    go :: Int -> [Token] -> Maybe Token -> Int -> [Token] -> Maybe ElixirDoHeader
    go _ _ _ 0 _ = Nothing
    go _ _ _ _ [] = Nothing
    go !parenDepth acc prev !n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (parenDepth + 1) (t : acc) (Just t) (n - 1) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (parenDepth - 1)) (t : acc) (Just t) (n - 1) ts
        | parenDepth == 0
        , isWord t "do"
        , not (isAtomOrDotAccess prev t) =
            case ts of
                (c : afterColon)
                    | isTokenOperator c && tToken c == ":" && areAdjacentTokens t c ->
                        Just (ElixirDoInline (reverse (c : t : acc)) afterColon)
                _ ->
                    Just (ElixirDoBlock (reverse (t : acc)) ts)
        | parenDepth == 0
        , not (isAtomOrDotAccess prev t)
        , isElixirDeclBoundary t =
            Nothing
        | otherwise =
            go parenDepth (t : acc) (Just t) (n - 1) ts

    isElixirDeclBoundary :: Token -> Bool
    isElixirDeclBoundary t =
        (isTokenKeyword t || isTokenIdentifier t)
            && case tToken t of
                "defmodule" -> True
                "def" -> True
                "defp" -> True
                "defmacro" -> True
                "defmacrop" -> True
                "defdelegate" -> True
                "defguard" -> True
                "test" -> True
                "describe" -> True
                "property" -> True
                "setup" -> True
                "setup_all" -> True
                _ -> False

-- | Check whether parenthesized call arguments `test("desc", do: ...)` contain a
-- top-level `do:` keyword inside `(...)`.
hasTopLevelInlineDo :: [Token] -> Bool
hasTopLevelInlineDo = go 0 Nothing
  where
    go :: Int -> Maybe Token -> [Token] -> Bool
    go _ _ [] = False
    go !depth prev (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (depth + 1) (Just t) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (depth - 1)) (Just t) ts
        | depth == 0
        , isWord t "do"
        , not (isAtomOrDotAccess prev t)
        , isKeywordListKey t ts =
            True
        | otherwise =
            go depth (Just t) ts

-- | Consume the expression of an inline `, do: expr` macro call, balancing nested
-- brackets `()`/`[]`/`{}` and inner `fn ... end` / `do ... end` blocks, and stopping
-- at statement end (newline unless continued by trailing/leading infix operators, or `;`).
collectInlineDoExpr :: [Token] -> ([Token], [Token])
collectInlineDoExpr [] = ([], [])
collectInlineDoExpr (firstTok : rest0) =
    let !b0 = bracketDelta firstTok
        !blk0 = blockDelta Nothing firstTok rest0
        (more, remaining) = go b0 blk0 firstTok rest0
     in (firstTok : more, remaining)
  where
    bracketDelta :: Token -> Int
    bracketDelta t
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") = 1
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") = -1
        | otherwise = 0

    blockDelta :: Maybe Token -> Token -> [Token] -> Int
    blockDelta prev t ts
        | isElixirBlockOpen prev t ts = 1
        | isElixirBlockClose prev t ts = -1
        | otherwise = 0

    go :: Int -> Int -> Token -> [Token] -> ([Token], [Token])
    go _ _ _ [] = ([], [])
    go !bDepth !blkDepth prev tokens@(t : ts)
        | bDepth > 0 || blkDepth > 0 =
            step bDepth blkDepth prev t ts
        | isTokenOperator t && tToken t == ";" =
            ([], tokens)
        | onSameLine prev t =
            step bDepth blkDepth prev t ts
        | isTrailingContinuation prev || startsLeadingContinuation tokens =
            step bDepth blkDepth prev t ts
        | otherwise =
            ([], tokens)

    step :: Int -> Int -> Token -> Token -> [Token] -> ([Token], [Token])
    step bDepth blkDepth prev t ts =
        let !bNext = max 0 (bDepth + bracketDelta t)
            !blkNext = max 0 (blkDepth + blockDelta (Just prev) t ts)
            (more, remaining) = go bNext blkNext t ts
         in (t : more, remaining)

    isTrailingContinuation :: Token -> Bool
    isTrailingContinuation t =
        ( isTokenOperator t
            && tToken t
                `elem` ([ ","
                        , "="
                        , "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "|"
                        , "&"
                        , "\\"
                        , "=="
                        , "!="
                        , "==="
                        , "!=="
                        , "<="
                        , ">="
                        , "&&"
                        , "||"
                        , "|>"
                        , "<>"
                        , "++"
                        , "--"
                        , "->"
                        , "<-"
                        , "\\\\"
                        ] :: [Text])
        )
            || ( (isTokenKeyword t || isTokenIdentifier t)
                    && tToken t `elem` (["and", "or", "in", "when"] :: [Text])
               )

    startsLeadingContinuation :: [Token] -> Bool
    startsLeadingContinuation (t1 : t2 : _)
        | isTokenOperator t1 && isTokenOperator t2 && areAdjacentTokens t1 t2
        , (tToken t1 == "|" && (tToken t2 == ">" || tToken t2 == "|"))
            || (tToken t1 == "&" && tToken t2 == "&")
            || (tToken t1 == "<" && tToken t2 == ">")
            || (tToken t1 == "+" && tToken t2 == "+")
            || (tToken t1 == "-" && tToken t2 == "-") =
            True
    startsLeadingContinuation (t1 : _) =
        (isTokenOperator t1 && tToken t1 `elem` (["|>", "&&", "||", "<>", "++", "--"] :: [Text]))
            || ((isTokenKeyword t1 || isTokenIdentifier t1) && (tToken t1 == "and" || tToken t1 == "or"))
    startsLeadingContinuation [] = False

-- | Consume tokens on the same line as `anchor`, extending across newlines only
-- while `(...)`, `[...]`, or `{...}` brackets remain open.
collectSameLineBalanced :: Token -> [Token] -> ([Token], [Token])
collectSameLineBalanced anchor = go 0 anchor
  where
    go :: Int -> Token -> [Token] -> ([Token], [Token])
    go _ _ [] = ([], [])
    go !bDepth prev tokens@(t : ts)
        | bDepth == 0 && isTokenOperator t && tToken t == ";" = ([], tokens)
        | bDepth > 0 || onSameLine prev t =
            let !bNext
                    | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") = bDepth + 1
                    | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") = max 0 (bDepth - 1)
                    | otherwise = bDepth
                (more, rest) = go bNext t ts
             in (t : more, rest)
        | otherwise = ([], tokens)

-- | Check whether a `defmodule` body directly calls `use ExUnit.Case` or
-- `use ExUnit.CaseTemplate` at its own top level (ignoring nested `defmodule` blocks).
hasTopLevelUseExUnit :: [Token] -> Bool
hasTopLevelUseExUnit = go 1 Nothing
  where
    go :: Int -> Maybe Token -> [Token] -> Bool
    go _ _ [] = False
    go !depth prev (t : ts)
        | isElixirBlockClose prev t ts =
            depth > 1 && go (depth - 1) (Just t) ts
        | isElixirBlockOpen prev t ts =
            go (depth + 1) (Just t) ts
        | depth == 1
        , isTokenIdentifier t && tToken t == "use"
        , not (isAtomOrDotAccess prev t)
        , (tEx : tDot : tCase : _) <- ts
        , isTokenIdentifier tEx && tToken tEx == "ExUnit"
        , isTokenOperator tDot && tToken tDot == "."
        , isTokenIdentifier tCase && (tToken tCase == "Case" || tToken tCase == "CaseTemplate") =
            True
        | otherwise =
            go depth (Just t) ts

-- | Collect tokens until the matching `end` keyword in Elixir, starting at nesting
-- depth `depth` (typically `1` right after opening `do`), ignoring `:end`/`:do`/`:fn`
-- atoms, `do:`/`end:`/`fn:` keyword keys, and `.end` field accesses.
collectUntilElixirEnd :: Int -> Maybe Token -> [Token] -> ([Token], [Token])
collectUntilElixirEnd _ _ [] = ([], [])
collectUntilElixirEnd !depth prev (t : ts)
    | isElixirBlockOpen prev t ts =
        let (collected, remaining) = collectUntilElixirEnd (depth + 1) (Just t) ts
         in (t : collected, remaining)
    | isElixirBlockClose prev t ts =
        if depth <= 1
            then ([t], ts)
            else
                let (collected, remaining) = collectUntilElixirEnd (depth - 1) (Just t) ts
                 in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilElixirEnd depth (Just t) ts
         in (t : collected, remaining)

consumeDoHeader :: ElixirDoHeader -> ([Token], [Token])
consumeDoHeader (ElixirDoBlock headerAndDo afterDo) =
    let (bodyAndEnd, remaining) = collectUntilElixirEnd 1 Nothing afterDo
     in (headerAndDo ++ bodyAndEnd, remaining)
consumeDoHeader (ElixirDoInline headerAndDoColon afterColon) =
    let (exprTokens, remaining) = collectInlineDoExpr afterColon
     in (headerAndDoColon ++ exprTokens, remaining)

-- | Pattern 1: ExUnit `@tag`, `@describetag`, `@moduletag` attributes.
matchExUnitTag :: [Token] -> Maybe ([Token], [Token])
matchExUnitTag (tAt : tAttr : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tAttr
    , areAdjacentTokens tAt tAttr
    , tToken tAttr `elem` (["tag", "describetag", "moduletag"] :: [Text]) =
        let (argToks, remaining) = collectSameLineBalanced tAttr ts
         in Just (tAt : tAttr : argToks, remaining)
matchExUnitTag _ = Nothing

-- | Pattern 2: ExUnit `test`, `describe`, `property` (both unparenthesized and parenthesized,
-- with either `do ... end` or inline `, do: ...`).
matchExUnitTestMacro :: [Token] -> Maybe ([Token], [Token])
matchExUnitTestMacro (t1 : t2 : ts)
    | isTokenString t2 || (isTokenBracket t2 && tToken t2 == "(")
    , isTokenIdentifier t1
    , let n = tToken t1 in n == "test" || n == "describe" || n == "property" =
        if isTokenString t2
            then case findElixirDo 200 (Just t2) ts of
                Just doHdr ->
                    let (bodyToks, remaining) = consumeDoHeader doHdr
                     in Just (t1 : t2 : bodyToks, remaining)
                Nothing -> Nothing
            else
                if isTokenBracket t2 && tToken t2 == "(" && startsWithString ts
                    then
                        let (insideParens, afterParen) = processInsideBrackets "(" ")" 1 ts
                            innerParens = dropCloseParen insideParens
                         in case findElixirDo 50 Nothing afterParen of
                                Just doHdr ->
                                    let (bodyToks, remaining) = consumeDoHeader doHdr
                                     in Just (t1 : t2 : insideParens ++ bodyToks, remaining)
                                Nothing ->
                                    if hasTopLevelInlineDo innerParens
                                        then Just (t1 : t2 : insideParens, afterParen)
                                        else Nothing
                    else Nothing
  where
    startsWithString (x : _) = isTokenString x
    startsWithString [] = False

    dropCloseParen toks = case reverse toks of
        (lastTok : revMid)
            | isTokenBracket lastTok && tToken lastTok == ")" -> reverse revMid
        _ -> toks
matchExUnitTestMacro _ = Nothing

-- | Pattern 3: ExUnit `setup` / `setup_all` (`do ... end`, `, do: ...`, or `:atom` / `[...]` callback).
matchExUnitSetup :: [Token] -> Maybe ([Token], [Token])
matchExUnitSetup (t1 : ts)
    | isTokenIdentifier t1 && (tToken t1 == "setup" || tToken t1 == "setup_all") =
        case findElixirDo 100 (Just t1) ts of
            Just doHdr ->
                let (bodyToks, remaining) = consumeDoHeader doHdr
                 in Just (t1 : bodyToks, remaining)
            Nothing ->
                if isCallbackSetupArg t1 ts
                    then
                        let (argToks, remaining) = collectSameLineBalanced t1 ts
                         in Just (t1 : argToks, remaining)
                    else Nothing
  where
    isCallbackSetupArg anchor (nextTok : _) =
        onSameLine anchor nextTok
            && ( (isTokenOperator nextTok && tToken nextTok == ":")
                    || (isTokenBracket nextTok && (tToken nextTok == "[" || tToken nextTok == "("))
               )
    isCallbackSetupArg _ [] = False
matchExUnitSetup _ = Nothing

-- | Pattern 4: Standalone `doctest MyModule[, ...]`.
matchExUnitDoctest :: [Token] -> Maybe ([Token], [Token])
matchExUnitDoctest (t1 : ts@(t2 : _))
    | isTokenIdentifier t1 && tToken t1 == "doctest"
    , onSameLine t1 t2
    , isTokenIdentifier t2 || (isTokenBracket t2 && tToken t2 == "(") =
        let (argToks, remaining) = collectSameLineBalanced t1 ts
         in Just (t1 : argToks, remaining)
matchExUnitDoctest _ = Nothing

-- | Pattern 5: `defmodule ...Test` / `...Tests` or modules with top-level `use ExUnit.Case` / `CaseTemplate`.
matchExUnitDefmodule :: [Token] -> Maybe ([Token], [Token])
matchExUnitDefmodule (t1 : ts)
    | isWord t1 "defmodule"
    , Just (modPathTokens, lastSeg, afterModPath) <- extractElixirModulePath ts
    , Just (ElixirDoBlock doTokens afterDo) <- findElixirDo 10 Nothing afterModPath
    , "Test" `T.isSuffixOf` lastSeg
        || "Tests" `T.isSuffixOf` lastSeg
        || hasTopLevelUseExUnit afterDo =
        let (moduleTokens, remainingTokens) = collectUntilElixirEnd 1 Nothing afterDo
         in Just (t1 : modPathTokens ++ doTokens ++ moduleTokens, remainingTokens)
matchExUnitDefmodule _ = Nothing

isElixirDefBlockerName :: Text -> Bool
isElixirDefBlockerName = \case
    "def" -> True
    "defp" -> True
    "defmacro" -> True
    "defmacrop" -> True
    "defdelegate" -> True
    "defguard" -> True
    _ -> False

-- | Processes tokens outside test blocks in Elixir.
-- `prevBlocker` is `True` after `.` or `def`/`defp`/`defmacro`/`defmacrop`/`defdelegate`/`defguard`,
-- preventing `def test(x) do ... end` or `Mod.test(x)` from being mistaken for ExUnit blocks.
processOutsideElixir :: Bool -> Bool -> [Token] -> [Token]
processOutsideElixir _ _ [] = []
processOutsideElixir keepTests !prevBlocker tokens@(t : ts) =
    let matched
            | prevBlocker = matchExUnitDefmodule tokens
            | otherwise =
                matchExUnitTag tokens
                    <|> matchExUnitTestMacro tokens
                    <|> matchExUnitSetup tokens
                    <|> matchExUnitDoctest tokens
                    <|> matchExUnitDefmodule tokens
     in case matched of
            Just (testToks, remaining) ->
                let restOut = processOutsideElixir keepTests False remaining
                 in if keepTests
                        then testToks ++ restOut
                        else restOut
            Nothing ->
                let !isBlocker =
                        (isTokenOperator t && tToken t == ".")
                            || ((isTokenKeyword t || isTokenIdentifier t) && isElixirDefBlockerName (tToken t))
                 in if keepTests
                        then processOutsideElixir keepTests isBlocker ts
                        else t : processOutsideElixir keepTests isBlocker ts
