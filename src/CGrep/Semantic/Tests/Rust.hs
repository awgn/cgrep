{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Rust () where

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
import System.OsPath (OsPath, osp, splitDirectories, takeBaseName, takeFileName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Rust where
    isTestFile = isRustTestModulePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideRust keepTests False tokens

-- | Checks if a Rust file path is an entire test/bench module according to Cargo
-- and Rust module conventions:
--   * files under a `tests/` or `benches/` directory
--   * module files named `tests.rs` or ending with `_test.rs` / `_tests.rs` / `-test.rs` / `-tests.rs`
isRustTestModulePath :: OsPath -> Bool
isRustTestModulePath path =
    let parts = splitDirectories path
        dirs = case reverse parts of
            (_ : ds) -> ds
            [] -> []
        base = takeBaseName path
        inCargoTestDir = any (\d -> let n = takeFileName d in n == [osp|tests|] || n == [osp|benches|]) dirs
        isTestModFile =
            base == [osp|tests|]
                || ([osp|_test|] `OS.isSuffixOf` base)
                || ([osp|_tests|] `OS.isSuffixOf` base)
                || ([osp|-test|] `OS.isSuffixOf` base)
                || ([osp|-tests|] `OS.isSuffixOf` base)
     in inCargoTestDir || isTestModFile

-- | Extracts a single outer Rust attribute, e.g.: #[cfg(test)] or #[tokio::test]
extractRustAttribute :: [Token] -> Maybe ([Token], [Token], [Token])
extractRustAttribute (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "#"
    , isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
         in Just (t1 : t2 : insideTokens, dropLastBracket "]" insideTokens, remaining)
extractRustAttribute _ = Nothing

-- | Extracts a single inner Rust attribute, e.g.: #![cfg(test)]
-- Handles both strict mode (`#!` as single operator token) and non-strict mode (`#` + `!`).
extractRustInnerAttribute :: [Token] -> Maybe ([Token], [Token], [Token])
extractRustInnerAttribute (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "#!"
    , isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
         in Just (t1 : t2 : insideTokens, dropLastBracket "]" insideTokens, remaining)
extractRustInnerAttribute (t1 : t2 : t3 : ts)
    | isTokenOperator t1 && tToken t1 == "#"
    , isTokenOperator t2 && tToken t2 == "!"
    , isTokenBracket t3 && tToken t3 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
         in Just (t1 : t2 : t3 : insideTokens, dropLastBracket "]" insideTokens, remaining)
extractRustInnerAttribute _ = Nothing

dropLastBracket :: Text -> [Token] -> [Token]
dropLastBracket _ [] = []
dropLastBracket closeTok [t]
    | isTokenBracket t && tToken t == closeTok = []
dropLastBracket closeTok (t : ts) = t : dropLastBracket closeTok ts

-- | Match `::` in both strict (`"::"`) and non-strict (`":"` + `":"`) lexer modes.
skipDoubleColon :: [Token] -> Maybe ([Token], [Token])
skipDoubleColon (t : ts)
    | isTokenOperator t && tToken t == "::" = Just ([t], ts)
skipDoubleColon (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == ":"
    , isTokenOperator t2 && tToken t2 == ":"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], ts)
skipDoubleColon _ = Nothing

isRustPathIdent :: Token -> Bool
isRustPathIdent t =
    isTokenIdentifier t
        || (isTokenKeyword t && (tToken t == "crate" || tToken t == "self" || tToken t == "super"))

-- | Parse a Rust `a::b::c` path at the start of a token slice, returning
-- `(rawPathTokens, segmentNames, remainingTokens)`.
collectRustPath :: [Token] -> Maybe ([Token], [Text], [Token])
collectRustPath tokens0 =
    let (leadColons, afterLead) = case skipDoubleColon tokens0 of
            Just (cs, r) -> (cs, r)
            Nothing -> ([], tokens0)
     in case afterLead of
            (t1 : ts1)
                | isRustPathIdent t1 ->
                    let (moreToks, moreSegs, rest) = goMore ts1
                     in Just (leadColons ++ (t1 : moreToks), tToken t1 : moreSegs, rest)
            _ -> Nothing
  where
    goMore ts =
        case skipDoubleColon ts of
            Just (cs, tNext : rest)
                | isRustPathIdent tNext ->
                    let (moreToks, moreSegs, finalRest) = goMore rest
                     in (cs ++ (tNext : moreToks), tToken tNext : moreSegs, finalRest)
            _ -> ([], [], ts)

-- | Split tokens inside `(...)` at depth-0 commas.
splitTopLevelCommas :: [Token] -> [[Token]]
splitTopLevelCommas = finish . go (0 :: Int) [] []
  where
    finish (curr, acc) =
        let allParts = reverse (reverse curr : acc)
         in filter (not . null) allParts

    go _ curr acc [] = (curr, acc)
    go depth curr acc (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (depth + 1) (t : curr) acc ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (depth - 1)) (t : curr) acc ts
        | depth == 0 && tToken t == "," =
            go 0 [] (reverse curr : acc) ts
        | otherwise =
            go depth (t : curr) acc ts

-- | Evaluate whether a Rust `cfg(...)` predicate compiles ONLY in test/doctest builds:
--   * `test` / `doctest` -> True
--   * `not(...)` -> False (e.g. `#[cfg(not(test))]` is production-only)
--   * `all(p1, p2, ...)` -> True iff AT LEAST ONE `p_i` is test-only
--   * `any(p1, p2, ...)` -> True iff ALL `p_i` are test-only (so `#[cfg(any(unix, test))]` stays prod)
isTestCfgPred :: [Token] -> Bool
isTestCfgPred [t]
    | isTokenIdentifier t && (tToken t == "test" || tToken t == "doctest") = True
isTestCfgPred (t1 : t2 : ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (insideParens, afterParens) = processInsideBrackets "(" ")" 1 ts
            inner = dropLastBracket ")" insideParens
            args = splitTopLevelCommas inner
         in null afterParens
                && case tToken t1 of
                    "not" -> False
                    "all" -> any isTestCfgPred args
                    "any" -> not (null args) && all isTestCfgPred args
                    _ -> False
isTestCfgPred _ = False

isRustTestAttrName :: Text -> Bool
isRustTestAttrName name =
    name
        `elem` ( [ "test"
                 , "bench"
                 , "rstest"
                 , "test_case"
                 , "test_matrix"
                 , "quickcheck"
                 , "proptest"
                 , "wasm_bindgen_test"
                 , "traced_test"
                 ] ::
                    [Text]
               )

-- | Inspect the meta-item inside `#[ ... ]` or `#![ ... ]`:
--   * `cfg(pred)` -> `isTestCfgPred pred`
--   * `cfg_attr(pred, attr1, ...)` -> True iff any gated `attr_i` is itself a test attribute
--     (so `#[cfg_attr(test, derive(Debug))]` on a prod struct stays prod!)
--   * `<path>[...]` -> True iff the final segment of `<path>` (before `(`, `=`, etc.) is a test attribute
--     (so `#![doc(test(...))]` and `#[instrument(fields(test = true))]` stay prod!)
isMetaItemTestAttr :: [Token] -> Bool
isMetaItemTestAttr metaTokens =
    case collectRustPath metaTokens of
        Nothing -> False
        Just (_, segs, afterPath) ->
            case reverse segs of
                [] -> False
                (lastSeg : _)
                    | lastSeg == "cfg" ->
                        case afterPath of
                            (tOpen : ts)
                                | isTokenBracket tOpen && tToken tOpen == "(" ->
                                    let (insideParens, afterParens) = processInsideBrackets "(" ")" 1 ts
                                     in null afterParens && isTestCfgPred (dropLastBracket ")" insideParens)
                            _ -> False
                    | lastSeg == "cfg_attr" ->
                        case afterPath of
                            (tOpen : ts)
                                | isTokenBracket tOpen && tToken tOpen == "(" ->
                                    let (insideParens, afterParens) = processInsideBrackets "(" ")" 1 ts
                                        args = splitTopLevelCommas (dropLastBracket ")" insideParens)
                                     in null afterParens
                                            && case args of
                                                (_ : gatedAttrs) -> any isMetaItemTestAttr gatedAttrs
                                                [] -> False
                            _ -> False
                    | otherwise ->
                        isRustTestAttrName lastSeg

-- | Collects all contiguous outer attributes before a declaration.
collectRustAttributes :: [Token] -> ([Token], Bool, [Token])
collectRustAttributes = go [] False
  where
    go acc isTest remaining =
        case extractRustAttribute remaining of
            Just (attrTokens, innerMeta, rest) ->
                let testAttr = isMetaItemTestAttr innerMeta
                 in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)

-- | Collects all contiguous inner attributes (#![...]) at the beginning of a module/block.
collectRustInnerAttributes :: [Token] -> ([Token], Bool, [Token])
collectRustInnerAttributes = go [] False
  where
    go acc isTest remaining =
        case extractRustInnerAttribute remaining of
            Just (attrTokens, innerMeta, rest) ->
                let testAttr = isMetaItemTestAttr innerMeta
                 in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)

-- | Match Rust property-test / test-harness macro invocations such as:
--   `proptest! { ... }`, `proptest::proptest! { ... }`,
--   `quickcheck! { ... }`, `wasm_bindgen_test_configure!(run_in_browser);`
matchRustTestMacro :: [Token] -> Maybe ([Token], [Token])
matchRustTestMacro tokens =
    case collectRustPath tokens of
        Just (pathToks, segs, tBang : tOpen : ts)
            | Just lastSeg <- lastMaybe segs
            , lastSeg `elem` (["proptest", "quickcheck", "wasm_bindgen_test_configure"] :: [Text])
            , isTokenOperator tBang && tToken tBang == "!"
            , isTokenBracket tOpen ->
                case tToken tOpen of
                    "{" ->
                        let (bodyToks, rest) = processInsideBrackets "{" "}" 1 ts
                         in Just (pathToks ++ [tBang, tOpen] ++ bodyToks, rest)
                    "(" ->
                        let (bodyToks, rest) = processInsideBrackets "(" ")" 1 ts
                            (semiToks, rest') = consumeOptionalSemi rest
                         in Just (pathToks ++ [tBang, tOpen] ++ bodyToks ++ semiToks, rest')
                    "[" ->
                        let (bodyToks, rest) = processInsideBrackets "[" "]" 1 ts
                            (semiToks, rest') = consumeOptionalSemi rest
                         in Just (pathToks ++ [tBang, tOpen] ++ bodyToks ++ semiToks, rest')
                    _ -> Nothing
        _ -> Nothing
  where
    lastMaybe [] = Nothing
    lastMaybe [x] = Just x
    lastMaybe (_ : xs) = lastMaybe xs

    consumeOptionalSemi (s : rs) | tToken s == ";" = ([s], rs)
    consumeOptionalSemi rs = ([], rs)

data RustItemTarget
    = RustBraced [Token] [Token]
    | RustSemicolon [Token] [Token]
    | RustNoTarget

-- | Finds the target of a Rust attribute: either a braced block `{ ... }` or a
-- semicolon-terminated declaration `... ;` (e.g., `#[cfg(test)] mod tests;`),
-- without crossing top-level semicolons when searching for `{`.
findRustItemTarget :: Int -> [Token] -> RustItemTarget
findRustItemTarget = go 0
  where
    go :: Int -> Int -> [Token] -> RustItemTarget
    go _ 0 _ = RustNoTarget
    go _ _ [] = RustNoTarget
    go depth n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            RustBraced [t] ts
        | depth == 0 && tToken t == ";" =
            RustSemicolon [t] ts
        | otherwise =
            step depth (n - 1) t ts

    step :: Int -> Int -> Token -> [Token] -> RustItemTarget
    step d m tok rest =
        case go d m rest of
            RustBraced pre post -> RustBraced (tok : pre) post
            RustSemicolon pre post -> RustSemicolon (tok : pre) post
            RustNoTarget -> RustNoTarget

-- | (Rust) Helper: Processes tokens *outside* a test block.
processOutsideRust :: Bool -> Bool -> [Token] -> [Token]
processOutsideRust _ _ [] = [] -- End of stream
processOutsideRust keepTests _ tokens
    | let (innerAttrs, isInnerTest, restAfterInner) = collectRustInnerAttributes tokens
    , not (null innerAttrs) =
        if isInnerTest
            then
                -- Inner test attribute (e.g. #![cfg(test)]): the entire enclosing module/file is test code
                if keepTests
                    then tokens
                    else []
            else
                if keepTests
                    then processOutsideRust keepTests False restAfterInner
                    else innerAttrs ++ processOutsideRust keepTests False restAfterInner
processOutsideRust keepTests prevDot tokens =
    let (attrTokens, isTest, restAfterAttrs) = collectRustAttributes tokens
     in if not (null attrTokens)
            then
                if isTest
                    then case findRustItemTarget 250 restAfterAttrs of
                        RustNoTarget ->
                            if keepTests
                                then processOutsideRust keepTests False restAfterAttrs
                                else attrTokens ++ processOutsideRust keepTests False restAfterAttrs
                        RustSemicolon declTokens finalRest ->
                            if keepTests
                                then attrTokens ++ declTokens ++ processOutsideRust keepTests False finalRest
                                else processOutsideRust keepTests False finalRest
                        RustBraced sigTokens restAfterBrace ->
                            let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                             in if keepTests
                                    then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideRust keepTests False finalRest
                                    else processOutsideRust keepTests False finalRest
                    else case findRustModInnerTest restAfterAttrs of
                        Just (modTokens, finalRest) ->
                            if keepTests
                                then attrTokens ++ modTokens ++ processOutsideRust keepTests False finalRest
                                else processOutsideRust keepTests False finalRest
                        Nothing -> case matchRustTestMacro restAfterAttrs of
                            Just (macTokens, finalRest) ->
                                if keepTests
                                    then attrTokens ++ macTokens ++ processOutsideRust keepTests False finalRest
                                    else processOutsideRust keepTests False finalRest
                            Nothing ->
                                if keepTests
                                    then processOutsideRust keepTests False restAfterAttrs
                                    else attrTokens ++ processOutsideRust keepTests False restAfterAttrs
            else case findRustModInnerTest tokens of
                Just (modTokens, finalRest) ->
                    if keepTests
                        then modTokens ++ processOutsideRust keepTests False finalRest
                        else processOutsideRust keepTests False finalRest
                Nothing
                    | not prevDot
                    , Just (macTokens, finalRest) <- matchRustTestMacro tokens ->
                        if keepTests
                            then macTokens ++ processOutsideRust keepTests False finalRest
                            else processOutsideRust keepTests False finalRest
                Nothing ->
                    let (t : ts) = tokens
                        nextPrevDot = isTokenOperator t && tToken t == "."
                     in if keepTests
                            then processOutsideRust keepTests nextPrevDot ts
                            else t : processOutsideRust keepTests nextPrevDot ts

-- | Checks if the token stream starts with `[pub [(...)]] mod <ident> { #![cfg(test)] ... }`
-- and if so returns the entire module tokens and the remaining tokens after `}`.
findRustModInnerTest :: [Token] -> Maybe ([Token], [Token])
findRustModInnerTest tokens =
    case skipModHeader tokens of
        Just (hdrTokens, afterBrace) ->
            let (_, isInnerTest, _) = collectRustInnerAttributes afterBrace
             in if isInnerTest
                    then
                        let (bodyTokens, finalRest) = processInsideBraces 1 afterBrace
                         in Just (hdrTokens ++ bodyTokens, finalRest)
                    else Nothing
        Nothing -> Nothing
  where
    skipModHeader :: [Token] -> Maybe ([Token], [Token])
    skipModHeader (t1 : t2 : ts)
        | isTokenKeyword t1 && tToken t1 == "pub" && isTokenBracket t2 && tToken t2 == "(" =
            let (visTokens, afterVis) = processInsideBrackets "(" ")" 1 ts
             in case skipModDecl afterVis of
                    Just (modHdr, rest) -> Just (t1 : t2 : visTokens ++ modHdr, rest)
                    Nothing -> Nothing
    skipModHeader (t1 : ts)
        | isTokenKeyword t1 && tToken t1 == "pub" =
            case skipModDecl ts of
                Just (modHdr, rest) -> Just (t1 : modHdr, rest)
                Nothing -> Nothing
    skipModHeader ts = skipModDecl ts

    skipModDecl :: [Token] -> Maybe ([Token], [Token])
    skipModDecl (t1 : t2 : t3 : ts)
        | isTokenKeyword t1 && tToken t1 == "mod"
        , isTokenIdentifier t2
        , isTokenBracket t3 && tToken t3 == "{" =
            Just ([t1, t2, t3], ts)
    skipModDecl _ = Nothing
