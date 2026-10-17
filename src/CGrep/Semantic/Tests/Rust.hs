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
import System.OsPath (OsPath, osp, splitDirectories, takeBaseName, takeFileName)
import qualified System.OsString as OS
instance LanguageTestFilter 'Rust where
    isTestFile = isRustTestModulePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideRust keepTests tokens

-- | Checks if a Rust file path is an entire test/bench module according to Cargo
-- and Rust module conventions:
--   * files under a `tests/` or `benches/` directory
--   * module files named `tests.rs` or ending with `_test.rs` / `_tests.rs` / `-test.rs` / `-tests.rs`
isRustTestModulePath :: OsPath -> Bool
isRustTestModulePath path =
    let parts = splitDirectories path
        dirs  = case reverse parts of
                    (_:ds) -> ds
                    []     -> []
        base  = takeBaseName path
        inCargoTestDir = any (\d -> let n = takeFileName d in n == [osp|tests|] || n == [osp|benches|]) dirs
        isTestModFile =
            base == [osp|tests|]
                || ([osp|_test|] `OS.isSuffixOf` base)
                || ([osp|_tests|] `OS.isSuffixOf` base)
                || ([osp|-test|] `OS.isSuffixOf` base)
                || ([osp|-tests|] `OS.isSuffixOf` base)
    in inCargoTestDir || isTestModFile

-- | Extracts a single outer Rust attribute, e.g.: #[cfg(test)] or #[tokio::test]
extractRustAttribute :: [Token] -> Maybe ([Token], [Token])
extractRustAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : t2 : insideTokens, remaining)
extractRustAttribute _ = Nothing

-- | Extracts a single inner Rust attribute, e.g.: #![cfg(test)]
-- Handles both strict mode (`#!` as single operator token) and non-strict mode (`#` + `!`).
extractRustInnerAttribute :: [Token] -> Maybe ([Token], [Token])
extractRustInnerAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "#!" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : t2 : insideTokens, remaining)
extractRustInnerAttribute (t1:t2:t3:ts)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenOperator t2 && tToken t2 == "!" &&
      isTokenBracket t3 && tToken t3 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : t2 : t3 : insideTokens, remaining)
extractRustInnerAttribute _ = Nothing

-- | An attribute is a "test attribute" if the identifier "test" (or similar) exists within it.
isTestAttribute :: [Token] -> Bool
isTestAttribute = any (\t -> isTokenIdentifier t && (tToken t == "test" || tToken t == "rstest" || tToken t == "test_case"))

-- | Collects all contiguous outer attributes before a declaration.
collectRustAttributes :: [Token] -> ([Token], Bool, [Token])
collectRustAttributes = go [] False
  where
    go acc isTest remaining =
        case extractRustAttribute remaining of
            Just (attrTokens, rest) ->
                let testAttr = isTestAttribute attrTokens
                in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)

-- | Collects all contiguous inner attributes (#![...]) at the beginning of a module/block.
collectRustInnerAttributes :: [Token] -> ([Token], Bool, [Token])
collectRustInnerAttributes = go [] False
  where
    go acc isTest remaining =
        case extractRustInnerAttribute remaining of
            Just (attrTokens, rest) ->
                let testAttr = isTestAttribute attrTokens
                in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)

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
    go _ 0 _  = RustNoTarget
    go _ _ [] = RustNoTarget
    go depth n (t:ts)
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
            RustBraced pre post    -> RustBraced (tok : pre) post
            RustSemicolon pre post -> RustSemicolon (tok : pre) post
            RustNoTarget           -> RustNoTarget

-- | (Rust) Helper: Processes tokens *outside* a test block.
processOutsideRust :: Bool -> [Token] -> [Token]
processOutsideRust _ [] = [] -- End of stream
processOutsideRust keepTests tokens
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
            then processOutsideRust keepTests restAfterInner
            else innerAttrs ++ processOutsideRust keepTests restAfterInner
processOutsideRust keepTests tokens =
    let (attrTokens, isTest, restAfterAttrs) = collectRustAttributes tokens
    in if not (null attrTokens)
       then
           if isTest
           then
               case findRustItemTarget 50 restAfterAttrs of
                   RustNoTarget ->
                       if keepTests
                       then processOutsideRust keepTests restAfterAttrs
                       else attrTokens ++ processOutsideRust keepTests restAfterAttrs
                   RustSemicolon declTokens finalRest ->
                       if keepTests
                       then attrTokens ++ declTokens ++ processOutsideRust keepTests finalRest
                       else processOutsideRust keepTests finalRest
                   RustBraced sigTokens restAfterBrace ->
                       let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                       in if keepTests
                          then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideRust keepTests finalRest
                          else processOutsideRust keepTests finalRest
           else
               case findRustModInnerTest restAfterAttrs of
                   Just (modTokens, finalRest) ->
                       if keepTests
                       then attrTokens ++ modTokens ++ processOutsideRust keepTests finalRest
                       else processOutsideRust keepTests finalRest
                   Nothing ->
                       if keepTests
                       then processOutsideRust keepTests restAfterAttrs
                       else attrTokens ++ processOutsideRust keepTests restAfterAttrs
       else
           case findRustModInnerTest tokens of
               Just (modTokens, finalRest) ->
                   if keepTests
                   then modTokens ++ processOutsideRust keepTests finalRest
                   else processOutsideRust keepTests finalRest
               Nothing ->
                   let (t:ts) = tokens
                   in if keepTests
                      then processOutsideRust keepTests ts
                      else t : processOutsideRust keepTests ts

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
    skipModHeader (t1:t2:ts)
        | isTokenKeyword t1 && tToken t1 == "pub" && isTokenBracket t2 && tToken t2 == "(" =
            let (visTokens, afterVis) = processInsideBrackets "(" ")" 1 ts
            in case skipModDecl afterVis of
                Just (modHdr, rest) -> Just (t1 : t2 : visTokens ++ modHdr, rest)
                Nothing -> Nothing
    skipModHeader (t1:ts)
        | isTokenKeyword t1 && tToken t1 == "pub" =
            case skipModDecl ts of
                Just (modHdr, rest) -> Just (t1 : modHdr, rest)
                Nothing -> Nothing
    skipModHeader ts = skipModDecl ts

    skipModDecl :: [Token] -> Maybe ([Token], [Token])
    skipModDecl (t1:t2:t3:ts)
        | isTokenKeyword t1 && tToken t1 == "mod" &&
          isTokenIdentifier t2 &&
          isTokenBracket t3 && tToken t3 == "{" =
            Just ([t1, t2, t3], ts)
    skipModDecl _ = Nothing

-- ------------------------------------------------------------------
