{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Clojure () where

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
import qualified Data.Text as T
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Clojure where
    isTestFile = isClojureTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideClojure keepTests tokens

-- | Clojure test namespaces follow the `*_test.clj` / `*_test.cljs` / `*_test.cljc` convention.
isClojureTestFilePath :: OsPath -> Bool
isClojureTestFilePath path =
    let ext = takeExtension path
        base = takeBaseName path
     in ext `elem` ([[osp|.clj|], [osp|.cljs|], [osp|.cljc|]] :: [OsPath])
            && [osp|_test|] `OS.isSuffixOf` base

isClojureWord :: Token -> Bool
isClojureWord t = isTokenIdentifier t || isTokenKeyword t

-- | Strip optional Clojure namespace prefix (`t/deftest` -> `deftest`, `clojure.test/testing` -> `testing`).
unqualifyClojureSymbol :: Text -> Text
unqualifyClojureSymbol sym =
    case T.breakOnEnd "/" sym of
        ("", name) -> name
        (_, name) -> name

-- | Balances Clojure delimiters `( ... )` or `{ ... }` while treating character literals
-- (`\(` `\)` `\{` `\}` `\\`, i.e. `\` immediately adjacent to the following token) as atomic.
processClojureDelim :: Text -> Text -> Int -> [Token] -> ([Token], [Token])
processClojureDelim _ _ 0 ts = ([], ts)
processClojureDelim _ _ _ [] = ([], [])
processClojureDelim open close depth (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "\\" && areAdjacentTokens t1 t2 =
        let (inside, remToks) = processClojureDelim open close depth ts
         in (t1 : t2 : inside, remToks)
processClojureDelim open close depth (t : ts)
    | isTokenBracket t && tToken t == open =
        let (inside, remToks) = processClojureDelim open close (depth + 1) ts
         in (t : inside, remToks)
    | isTokenBracket t && tToken t == close =
        if depth == 1
            then ([t], ts)
            else
                let (inside, remToks) = processClojureDelim open close (depth - 1) ts
                 in (t : inside, remToks)
    | otherwise =
        let (inside, remToks) = processClojureDelim open close depth ts
         in (t : inside, remToks)

-- | Matches a Clojure keyword `:kw` or `::kw` (`:` is tokenized as `TokenOperator`).
matchClojureColonKeyword :: [Token] -> Maybe ([Token], [Token])
matchClojureColonKeyword (t1 : t2 : t3 : ts)
    | isTokenOperator t1 && tToken t1 == ":"
    , isTokenOperator t2 && tToken t2 == ":"
    , areAdjacentTokens t1 t2
    , areAdjacentTokens t2 t3
    , isClojureWord t3 =
        Just ([t1, t2, t3], ts)
matchClojureColonKeyword (t1 : t2 : ts)
    | isTokenOperator t1 && (tToken t1 == ":" || tToken t1 == "::")
    , areAdjacentTokens t1 t2
    , isClojureWord t2 =
        Just ([t1, t2], ts)
matchClojureColonKeyword _ = Nothing

-- | Matches a single Clojure reader metadata attachment (`^:kw`, `^{...}`, `^"tag"`, `^Sym`)
-- or `#_` reader discard prefix.
matchSingleClojureMeta :: [Token] -> Maybe ([Token], [Token])
matchSingleClojureMeta (tHat : rest)
    | tToken tHat == "^" =
        case matchClojureColonKeyword rest of
            Just (kwToks, afterKw) -> Just (tHat : kwToks, afterKw)
            Nothing ->
                case rest of
                    (tBrace : afterBrace)
                        | isTokenBracket tBrace && tToken tBrace == "{" ->
                            let (mapBody, afterMap) = processClojureDelim "{" "}" 1 afterBrace
                             in Just (tHat : tBrace : mapBody, afterMap)
                    (tStr : afterStr)
                        | isTokenString tStr ->
                            Just ([tHat, tStr], afterStr)
                    (tSym : afterSym)
                        | isClojureWord tSym ->
                            Just ([tHat, tSym], afterSym)
                    _ -> Nothing
    | isClojureWord tHat
    , "^" `T.isPrefixOf` tToken tHat
    , T.length (tToken tHat) > 1 =
        Just ([tHat], rest)
matchSingleClojureMeta (tSharp : tUnder : rest)
    | isTokenOperator tSharp && tToken tSharp == "#"
    , isTokenIdentifier tUnder && tToken tUnder == "_"
    , areAdjacentTokens tSharp tUnder =
        Just ([tSharp, tUnder], rest)
matchSingleClojureMeta _ = Nothing

consumeClojureMetadata :: [Token] -> ([Token], [Token])
consumeClojureMetadata ts =
    case matchSingleClojureMeta ts of
        Just (mToks, rest) ->
            let (more, finalRest) = consumeClojureMetadata rest
             in (mToks ++ more, finalRest)
        Nothing -> ([], ts)

skipMetaAndKeywords :: [Token] -> [Token]
skipMetaAndKeywords ts =
    let (_, afterMeta) = consumeClojureMetadata ts
     in case matchClojureColonKeyword afterMeta of
            Just (_, afterKw) -> skipMetaAndKeywords afterKw
            Nothing -> afterMeta

hasMidjeArrow :: [Token] -> Bool
hasMidjeArrow [] = False
hasMidjeArrow (t1 : t2 : ts)
    | tToken t1 == "=" && tToken t2 == ">" && areAdjacentTokens t1 t2 = True
    | otherwise = isMidjeArrowToken t1 || hasMidjeArrow (t2 : ts)
hasMidjeArrow [t1] = isMidjeArrowToken t1

isMidjeArrowToken :: Token -> Bool
isMidjeArrowToken t =
    tToken t
        `elem` ([ "=>"
                , "=not=>"
                , "=throws=>"
                , "=contains=>"
                , "=expands-to=>"
                , "=future=>"
                ] :: [Text])

-- | Matches `( <head> ... )` if it is a recognized Clojure test form:
--   * `clojure.test` / `cljs.test` / `test.check` / `expectations`:
--     `deftest`, `deftest-`, `defspec`, `defexpect`, `use-fixtures`, `testing`, `are`
--   * `midje.sweet`: `facts`, `fact` (requiring a string description or `=>` arrow
--     to avoid false positives on production `(fact n)` factorial calls).
matchClojureTestList :: [Token] -> Maybe ([Token], [Token])
matchClojureTestList (tOpen : tHead : ts)
    | isTokenBracket tOpen && tToken tOpen == "("
    , isClojureWord tHead =
        let baseName = unqualifyClojureSymbol (tToken tHead)
            (_, afterMeta) = consumeClojureMetadata ts
         in if
                | baseName `elem` (["deftest", "deftest-", "defspec", "defexpect"] :: [Text]) ->
                    case afterMeta of
                        (arg1 : _) | isClojureWord arg1 -> captureList
                        _ -> Nothing
                | baseName == "use-fixtures" ->
                    case matchClojureColonKeyword afterMeta of
                        Just _ -> captureList
                        Nothing ->
                            case afterMeta of
                                (arg1 : _) | isClojureWord arg1 -> captureList
                                _ -> Nothing
                | baseName == "testing" ->
                    case afterMeta of
                        (arg1 : _) | isTokenString arg1 -> captureList
                        _ -> Nothing
                | baseName == "are" ->
                    case afterMeta of
                        (arg1 : _) | isTokenBracket arg1 && tToken arg1 == "[" -> captureList
                        _ -> Nothing
                | baseName == "facts" || baseName == "fact" ->
                    let (bodyTokens, remainingTokens) = processClojureDelim "(" ")" 1 ts
                        innerTokens = case bodyTokens of
                            [] -> []
                            xs -> init xs
                        startsWithDesc = case skipMetaAndKeywords ts of
                            (arg1 : _) -> isTokenString arg1
                            [] -> False
                     in if startsWithDesc || hasMidjeArrow innerTokens
                            then Just (tOpen : tHead : bodyTokens, remainingTokens)
                            else Nothing
                | otherwise -> Nothing
  where
    captureList =
        let (bodyTokens, remainingTokens) = processClojureDelim "(" ")" 1 ts
         in Just (tOpen : tHead : bodyTokens, remainingTokens)
matchClojureTestList _ = Nothing

-- | (Clojure) Helper: Processes tokens *outside* a test block.
processOutsideClojure :: Bool -> [Token] -> [Token]
processOutsideClojure _ [] = []
-- Skip atomic Clojure character literals (`\(`, `\)`, `\\`, etc.) outside lists
processOutsideClojure keepTests (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "\\" && areAdjacentTokens t1 t2 =
        if keepTests
            then processOutsideClojure keepTests ts
            else t1 : t2 : processOutsideClojure keepTests ts
-- Leading reader metadata / `#_` discard possibly attached to a test form
processOutsideClojure keepTests ts
    | (metaToks@(_ : _), afterMeta) <- consumeClojureMetadata ts =
        case matchClojureTestList afterMeta of
            Just (testListToks, remaining) ->
                if keepTests
                    then metaToks ++ testListToks ++ processOutsideClojure keepTests remaining
                    else processOutsideClojure keepTests remaining
            Nothing ->
                if keepTests
                    then processOutsideClojure keepTests afterMeta
                    else metaToks ++ processOutsideClojure keepTests afterMeta
-- Direct test list `(deftest ...)`, `(use-fixtures :each ...)`, `(testing "..." ...)`, etc.
processOutsideClojure keepTests ts
    | Just (testListToks, remaining) <- matchClojureTestList ts =
        if keepTests
            then testListToks ++ processOutsideClojure keepTests remaining
            else processOutsideClojure keepTests remaining
-- Fallback
processOutsideClojure keepTests (t : ts) =
    if keepTests
        then processOutsideClojure keepTests ts
        else t : processOutsideClojure keepTests ts
