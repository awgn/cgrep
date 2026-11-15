{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Nim () where

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
import System.OsPath (OsPath, osp, splitDirectories, takeBaseName, takeExtension, takeFileName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Nim where
    isTestFile = isNimTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideNim keepTests initialLineState tokens

-- | Recognizes Nimble / Testament test files (`tests/t*.nim`, `test_*.nim`, `*_test.nim`).
isNimTestFilePath :: OsPath -> Bool
isNimTestFilePath path =
    takeExtension path == [osp|.nim|]
        && ( [osp|test_|] `OS.isPrefixOf` base
                || [osp|_test|] `OS.isSuffixOf` base
                || any (\d -> takeFileName d == [osp|tests|]) parentDirs
           )
  where
    base = takeBaseName path
    parentDirs = case reverse (splitDirectories path) of
        (_ : ds) -> ds
        [] -> []

isNimIdentOrKw :: Token -> Bool
isNimIdentOrKw t = isTokenIdentifier t || isTokenKeyword t

isNimTestCondFlag :: Text -> Bool
isNimTestCondFlag name = name `elem` (["test", "testing", "unittest"] :: [Text])

-- | Matches a Nim `std/unittest` or conditional test block header when starting a statement line:
--   1. `suite "name":` / `test "name":`
--   2. `suite("name"):` / `test("name"):`
--   3. `setup:` / `teardown:`
--   4. `when defined(test | testing | unittest):`
matchNimTestHeader :: [Token] -> Maybe ([Token], [Token])
matchNimTestHeader (t1 : t2 : t3 : ts)
    | isNimIdentOrKw t1 && (tToken t1 == "suite" || tToken t1 == "test")
    , isTokenString t2
    , isTokenOperator t3 && tToken t3 == ":" =
        Just ([t1, t2, t3], ts)
matchNimTestHeader (t1 : t2 : t3 : t4 : t5 : ts)
    | isNimIdentOrKw t1 && (tToken t1 == "suite" || tToken t1 == "test")
    , isTokenBracket t2 && tToken t2 == "("
    , isTokenString t3
    , isTokenBracket t4 && tToken t4 == ")"
    , isTokenOperator t5 && tToken t5 == ":" =
        Just ([t1, t2, t3, t4, t5], ts)
matchNimTestHeader (t1 : t2 : ts)
    | isNimIdentOrKw t1 && (tToken t1 == "setup" || tToken t1 == "teardown")
    , isTokenOperator t2 && tToken t2 == ":" =
        Just ([t1, t2], ts)
matchNimTestHeader (t1 : t2 : t3 : t4 : t5 : t6 : ts)
    | isNimIdentOrKw t1 && tToken t1 == "when"
    , isNimIdentOrKw t2 && tToken t2 == "defined"
    , isTokenBracket t3 && tToken t3 == "("
    , isNimIdentOrKw t4 && isNimTestCondFlag (tToken t4)
    , isTokenBracket t5 && tToken t5 == ")"
    , isTokenOperator t6 && tToken t6 == ":" =
        Just ([t1, t2, t3, t4, t5, t6], ts)
matchNimTestHeader _ = Nothing

-- | Processes Nim tokens outside of a test block.
processOutsideNim :: Bool -> LineState -> [Token] -> [Token]
processOutsideNim _ _ [] = []
processOutsideNim keepTests ls allToks@(t : ts) =
    let (isFirst, col, lsAfterT) = stepLineState ls t
     in case if isFirst then matchNimTestHeader allToks else Nothing of
            Just (hdrToks, afterHdr) ->
                let lsAfterHdr = advanceLineState ls hdrToks
                    (bodyToks, lsAfterBody, remaining) = collectNimIndentedBlock col 0 lsAfterHdr afterHdr
                 in if keepTests
                        then hdrToks ++ bodyToks ++ processOutsideNim keepTests lsAfterBody remaining
                        else processOutsideNim keepTests lsAfterBody remaining
            Nothing ->
                if keepTests
                    then processOutsideNim keepTests lsAfterT ts
                    else t : processOutsideNim keepTests lsAfterT ts

-- | Collects all tokens belonging to an indented Nim block opened at column `baseIndent`,
-- stopping as soon as an unbracketed line begins at column `<= baseIndent`.
collectNimIndentedBlock :: Int -> Int -> LineState -> [Token] -> ([Token], LineState, [Token])
collectNimIndentedBlock _ _ !ls [] = ([], ls, [])
collectNimIndentedBlock !baseIndent !bracketDepth !ls allToks@(t : ts) =
    let (isFirst, col, lsAfterT) = stepLineState ls t
     in if bracketDepth == 0 && isFirst && col <= baseIndent
            then ([], ls, allToks)
            else
                let !nextDepth = updateBracketDepth bracketDepth t
                    (rest, lsEnd, remaining) = collectNimIndentedBlock baseIndent nextDepth lsAfterT ts
                 in (t : rest, lsEnd, remaining)
  where
    updateBracketDepth !d tok
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
