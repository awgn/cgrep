{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Julia () where

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
import qualified Data.Text as T
import System.OsPath (osp, takeFileName)

instance LanguageTestFilter 'Julia where
    isTestFile path = takeFileName path == [osp|runtests.jl|]
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJulia keepTests initialLineState tokens

-- | Check if a macro identifier (after `@`) is a Julia test macro.
isJuliaTestMacro :: Text -> Bool
isJuliaTestMacro name =
    name
        `elem` ([ "test"
                , "testset"
                , "inferred"
                , "testitem"
                , "testmodule"
                , "testsnippet"
                ] :: [Text])
        || "test_" `T.isPrefixOf` name

-- | Match `@test...` or module-qualified `Test.@test...`, returning `(prefixTokens, macroIdent, rest)`.
matchJuliaTestMacro :: [Token] -> Maybe ([Token], Token, [Token])
matchJuliaTestMacro (tAt : tMacro : rest)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tMacro && isJuliaTestMacro (tToken tMacro) =
        Just ([tAt, tMacro], tMacro, rest)
matchJuliaTestMacro (tMod : tDot : rest0)
    | isTokenIdentifier tMod
    , isTokenOperator tDot && tToken tDot == "."
    , Just (morePre, macroTok, afterMacro) <- matchJuliaTestMacro rest0 =
        Just (tMod : tDot : morePre, macroTok, afterMacro)
matchJuliaTestMacro _ = Nothing

processOutsideJulia :: Bool -> LineState -> [Token] -> [Token]
processOutsideJulia _ !_ [] = []
processOutsideJulia keepTests !ls0 toks@(t : ts)
    | Just (macroPrefix, macroTok, afterMacro) <- matchJuliaTestMacro toks =
        let !lsAfterPrefix = advanceLineState ls0 macroPrefix
            (argTokens, !lsFinal, remainingTokens) =
                case afterMacro of
                    (tOpen : afterOpen)
                        | isTokenBracket tOpen
                        , tToken tOpen == "("
                        , areAdjacentTokens macroTok tOpen ->
                            let (parenBody, remAfterParen) = processInsideBrackets "(" ")" 1 afterOpen
                                allArgs = tOpen : parenBody
                             in (allArgs, advanceLineState lsAfterPrefix allArgs, remAfterParen)
                    _ ->
                        collectJuliaMacroArgs lsAfterPrefix False False afterMacro
         in if keepTests
                then macroPrefix ++ argTokens ++ processOutsideJulia keepTests lsFinal remainingTokens
                else processOutsideJulia keepTests lsFinal remainingTokens
    | otherwise =
        let !ls1 = nextLineState ls0 t
         in if keepTests
                then processOutsideJulia keepTests ls1 ts
                else t : processOutsideJulia keepTests ls1 ts

isLineContinuationToken :: Token -> Bool
isLineContinuationToken t =
    tToken t == ","
        || ( isTokenOperator t
                && tToken t
                    `elem` ([ "="
                            , "+"
                            , "-"
                            , "*"
                            , "/"
                            , "%"
                            , "^"
                            , "&"
                            , "|"
                            , "&&"
                            , "||"
                            , "=="
                            , "!="
                            , "<"
                            , ">"
                            , "<="
                            , ">="
                            , "==="
                            , "!=="
                            , "->"
                            , "\\"
                            , "?"
                            , ":"
                            ] :: [Text])
           )

-- | Collect arguments of a space-separated Julia test macro (`@test expr`, `@testset "..." begin ... end`,
-- `@testset "..." for ... end`), stopping cleanly at a newline (unless preceded by a continuation operator)
-- or `;`.
collectJuliaMacroArgs :: LineState -> Bool -> Bool -> [Token] -> ([Token], LineState, [Token])
collectJuliaMacroArgs ls _ _ [] = ([], ls, [])
collectJuliaMacroArgs ls !prevCont !prevDotColon (t : ts) =
    let (isNewLine, _, ls1) = stepLineState ls t
     in if (isNewLine && not prevCont) || tToken t == ";"
            then ([], ls, t : ts)
            else
                if not prevDotColon
                    && (isTokenKeyword t || isTokenIdentifier t)
                    && (tToken t `elem` (["begin", "for", "let", "do"] :: [Text]))
                    then
                        let (bodyTokens, remaining) = collectJuliaBlock 1 False ts
                            lsAfter = advanceLineState ls1 bodyTokens
                         in (t : bodyTokens, lsAfter, remaining)
                    else
                        if isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{")
                            then
                                let closeB = case tToken t of
                                        "(" -> ")"
                                        "[" -> "]"
                                        _ -> "}"
                                    (insideToks, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts
                                    lsAfterB = advanceLineState ls1 insideToks
                                    (moreToks, lsEnd, remToks) = collectJuliaMacroArgs lsAfterB False False afterBracket
                                 in (t : insideToks ++ moreToks, lsEnd, remToks)
                            else
                                let nextCont = isLineContinuationToken t
                                    nextDotColon = isTokenOperator t && (tToken t == "." || tToken t == ":")
                                    (moreToks, lsEnd, remToks) = collectJuliaMacroArgs ls1 nextCont nextDotColon ts
                                 in (t : moreToks, lsEnd, remToks)

-- | Collect tokens inside a Julia `begin ... end` / `for ... end` block:
--   * Skips `(...)` and `[...]` via `processInsideBrackets` so array slice indexing
--     `arr[begin:end]` and generator expressions `(x for x in 1:5 if x > 0)` never
--     corrupt `end` depth.
--   * Recognizes two-keyword type blocks (`abstract type ... end`, `primitive type ... end`,
--     `mutable struct ... end`).
--   * Ignores `:end` / `:begin` symbols and `obj.end` property accesses.
collectJuliaBlock :: Int -> Bool -> [Token] -> ([Token], [Token])
collectJuliaBlock 0 _ ts = ([], ts)
collectJuliaBlock _ _ [] = ([], [])
collectJuliaBlock !depth !prevDotColon (t1 : t2 : ts)
    | not prevDotColon
    , (isTokenKeyword t1 || isTokenIdentifier t1)
    , (isTokenKeyword t2 || isTokenIdentifier t2)
    , (tToken t1 == "abstract" && tToken t2 == "type")
        || (tToken t1 == "primitive" && tToken t2 == "type")
        || (tToken t1 == "mutable" && tToken t2 == "struct") =
        let (collected, remaining) = collectJuliaBlock (depth + 1) False ts
         in (t1 : t2 : collected, remaining)
collectJuliaBlock !depth !prevDotColon (t : ts)
    | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
        let closeB = if tToken t == "(" then ")" else "]"
            (bracketTokens, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts
            (collected, remaining) = collectJuliaBlock depth False afterBracket
         in (t : bracketTokens ++ collected, remaining)
    | not prevDotColon
    , (isTokenKeyword t || isTokenIdentifier t)
    , tToken t
        `elem` ([ "begin"
                , "function"
                , "struct"
                , "macro"
                , "let"
                , "quote"
                , "do"
                , "if"
                , "for"
                , "while"
                , "try"
                , "module"
                , "baremodule"
                ] :: [Text]) =
        let (collected, remaining) = collectJuliaBlock (depth + 1) False ts
         in (t : collected, remaining)
    | not prevDotColon
    , (isTokenKeyword t || isTokenIdentifier t)
    , tToken t == "end" =
        if depth == 1
            then ([t], ts)
            else
                let (collected, remaining) = collectJuliaBlock (depth - 1) False ts
                 in (t : collected, remaining)
    | otherwise =
        let nextDotColon = isTokenOperator t && (tToken t == "." || tToken t == ":")
            (collected, remaining) = collectJuliaBlock depth nextDotColon ts
         in (t : collected, remaining)
