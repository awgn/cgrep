{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Python () where

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
import Data.Bits (complement, (.&.))
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Python where
    isTestFile = isPythonTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePython keepTests initialLineState tokens

-- | Recognizes pytest / unittest test files (`test_*.py`, `*_test.py`, `conftest.py`).
isPythonTestFilePath :: OsPath -> Bool
isPythonTestFilePath path =
    takeExtension path == [osp|.py|]
        && ( [osp|test_|] `OS.isPrefixOf` base
                || [osp|_test|] `OS.isSuffixOf` base
                || base == [osp|conftest|]
           )
  where
    base = takeBaseName path

-- | Tracks byte position and leading visual indentation column of the current line
-- across the token stream in O(fileSize) total work.
data LineState = LineState
    { lsPrevEnd :: {-# UNPACK #-} !Int
    , lsLineIndent :: {-# UNPACK #-} !Int
    }

initialLineState :: LineState
initialLineState = LineState (-1) 0

-- | Advance 'LineState' to token @t@, returning @(isFirstTokenOnLine, lineIndentCol, nextState)@.
stepLineState :: LineState -> Token -> (Bool, Int, LineState)
stepLineState (LineState prevEnd prevIndent) t =
    case tToken t of
        TI.Text arr off len
            | off >= 0 ->
                let scanFloor = max 0 prevEnd
                    nlIdx = findNewlineBack arr (off - 1) scanFloor
                    !nextEnd = max prevEnd (off + len)
                 in if prevEnd >= 0 && nlIdx < scanFloor
                        then (False, prevIndent, LineState nextEnd prevIndent)
                        else
                            let !lineStart =
                                    if nlIdx >= 0
                                        then nlIdx + 1
                                        else case findNewlineBack arr (off - 1) 0 of
                                            k | k >= 0 -> k + 1
                                            _ -> 0
                                !col = computeVisualCol arr lineStart off 0
                             in (True, col, LineState nextEnd col)
        _ -> (False, prevIndent, LineState prevEnd prevIndent)
  where
    findNewlineBack :: A.Array -> Int -> Int -> Int
    findNewlineBack !arr !i !low
        | i < low = low - 1
        | A.unsafeIndex arr i == 0x0A = i
        | otherwise = findNewlineBack arr (i - 1) low

    computeVisualCol :: A.Array -> Int -> Int -> Int -> Int
    computeVisualCol !arr !j !limit !col
        | j >= limit = col
        | otherwise =
            let !b = A.unsafeIndex arr j
                !col'
                    | b == 0x09 = (col + 4) .&. complement 3
                    | b == 0x02 || b == 0x03 || b == 0x0D = col
                    | (b .&. 0xC0) == 0x80 = col
                    | otherwise = col + 1
             in computeVisualCol arr (j + 1) limit col'

advanceLineState :: LineState -> [Token] -> LineState
advanceLineState = foldl' (\ls tok -> let (_, _, ls') = stepLineState ls tok in ls')

-- | Check if a Python class name starts with `"Test"` NOT followed by a lowercase ASCII letter
-- (matching `TestMathOperations` while rejecting domain words like `Testimonial`).
isPythonTestClassName :: Text -> Bool
isPythonTestClassName name =
    case T.stripPrefix "Test" name of
        Nothing -> False
        Just rest -> case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAsciiLower c)

-- | Extracts a single Python decorator `@expr` (supporting dotted paths, calls `(...)`, and PEP 614 `[...]`).
extractPythonDecorator :: [Token] -> Maybe ([Token], [Token])
extractPythonDecorator (tAt : tIdent : ts)
    | isTokenOperator tAt && tToken tAt == "@" && isTokenIdentifier tIdent =
        let (tailToks, remaining) = collectDecPostfix ts
         in Just (tAt : tIdent : tailToks, remaining)
  where
    collectDecPostfix (d : i : rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (more, final) = collectDecPostfix rest
             in (d : i : more, final)
    collectDecPostfix (b : rest)
        | isTokenBracket b && (tToken b == "(" || tToken b == "[") =
            let closeB = if tToken b == "(" then ")" else "]"
                (inside, afterB) = processInsideBrackets (tToken b) closeB 1 rest
                (more, final) = collectDecPostfix afterB
             in (b : inside ++ more, final)
    collectDecPostfix rest = ([], rest)
extractPythonDecorator _ = Nothing

-- | Checks if a decorator is a known Python test decorator (examining only the decorator
-- path before `(`/`[`, not arguments).
isPythonTestDecorator :: [Token] -> Bool
isPythonTestDecorator tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && (tToken t == "(" || tToken t == "["))) tokens
        idents = [tToken t | t <- headerTokens, isTokenIdentifier t]
     in case idents of
            (first : _) ->
                first
                    `elem` ([ "pytest"
                            , "pytest_asyncio"
                            , "unittest"
                            , "hypothesis"
                            , "patch"
                            , "mock"
                            , "fixture"
                            , "given"
                            , "parametrize"
                            , "skip"
                            , "skipIf"
                            , "skipUnless"
                            , "expectedFailure"
                            ] :: [Text])
                    || any (\i -> i == "fixture" || i == "parametrize") idents
            [] -> False

-- | Collects a contiguous stack of Python decorators starting at `baseIndent`, requiring
-- each `@...` to begin a new line at `baseIndent` (avoiding matrix-multiplication `A @ B`!).
collectPythonDecorators :: Int -> LineState -> [Token] -> ([[Token]], Bool, LineState, [Token])
collectPythonDecorators !baseIndent = go [] False
  where
    go acc isTest !ls remaining@(t : _) =
        let (isFirst, col, _) = stepLineState ls t
         in if isFirst && col == baseIndent
                then case extractPythonDecorator remaining of
                    Just (dec, rest) ->
                        let !lsAfterDec = advanceLineState ls dec
                            testDec = isPythonTestDecorator dec
                         in go (acc ++ [dec]) (isTest || testDec) lsAfterDec rest
                    Nothing -> (acc, isTest, ls, remaining)
                else (acc, isTest, ls, remaining)
    go acc isTest !ls [] = (acc, isTest, ls, [])

-- | Checks if tokens start with `def`, `async def`, or `class`.
isPythonDefOrClassStart :: [Token] -> Bool
isPythonDefOrClassStart (t1 : t2 : _)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async"
    , isTokenKeyword t2 && tToken t2 == "def" =
        True
isPythonDefOrClassStart (t1 : _)
    | isTokenKeyword t1 && (tToken t1 == "def" || tToken t1 == "class") =
        True
isPythonDefOrClassStart _ = False

-- | Checks if a Python `def` or `class` header is intrinsically a test construct by name/base class.
isPythonTestDefOrClass :: [Token] -> Bool
isPythonTestDefOrClass (t1 : t2 : t3 : _)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async"
    , isTokenKeyword t2 && tToken t2 == "def"
    , isTokenIdentifier t3 && "test_" `T.isPrefixOf` tToken t3 =
        True
isPythonTestDefOrClass (t1 : t2 : _)
    | isTokenKeyword t1 && tToken t1 == "def"
    , isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2 =
        True
isPythonTestDefOrClass (t1 : t2 : ts)
    | isTokenKeyword t1 && tToken t1 == "class"
    , isTokenIdentifier t2 && (isPythonTestClassName (tToken t2) || hasUnittestBase ts) =
        True
isPythonTestDefOrClass _ = False

-- | Checks if a Python class inherits from `*TestCase` (e.g. `(unittest.TestCase)` or `(TestCase)`).
hasUnittestBase :: [Token] -> Bool
hasUnittestBase (t : ts)
    | isTokenBracket t && tToken t == "(" =
        let (baseTokens, _) = processInsideBrackets "(" ")" 1 ts
         in any (\b -> isTokenIdentifier b && ("TestCase" `T.isSuffixOf` tToken b)) baseTokens
hasUnittestBase _ = False

updatePyBracketDepth :: Int -> Token -> Int
updatePyBracketDepth !d tok
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

-- | Consumes a Python `def` / `async def` / `class` header up to its top-level `:`,
-- followed by all tokens in its indented suite (`col > baseIndent`).
consumePythonSuite :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
consumePythonSuite !baseIndent !ls0 toks0 =
    let (hdrToks, lsAfterHdr, afterHdr) = consumeHeader 0 ls0 toks0
        (bodyToks, lsAfterBody, remaining) = collectPythonIndentedBlock baseIndent 0 lsAfterHdr afterHdr
     in (hdrToks ++ bodyToks, lsAfterBody, remaining)
  where
    consumeHeader :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
    consumeHeader _ !ls [] = ([], ls, [])
    consumeHeader !depth !ls (t : ts) =
        let (_, _, lsAfterT) = stepLineState ls t
            !nextDepth = updatePyBracketDepth depth t
         in if depth == 0 && isTokenOperator t && tToken t == ":"
                then ([t], lsAfterT, ts)
                else
                    let (rest, lsEnd, remToks) = consumeHeader nextDepth lsAfterT ts
                     in (t : rest, lsEnd, remToks)

-- | Collects all tokens inside an indented Python suite opened at column `baseIndent`,
-- stopping as soon as an unbracketed line begins at column `<= baseIndent`.
collectPythonIndentedBlock :: Int -> Int -> LineState -> [Token] -> ([Token], LineState, [Token])
collectPythonIndentedBlock _ _ !ls [] = ([], ls, [])
collectPythonIndentedBlock !baseIndent !bracketDepth !ls allToks@(t : ts) =
    let (isFirst, col, lsAfterT) = stepLineState ls t
     in if bracketDepth == 0 && isFirst && col <= baseIndent
            then ([], ls, allToks)
            else
                let !nextDepth = updatePyBracketDepth bracketDepth t
                    (rest, lsEnd, remaining) = collectPythonIndentedBlock baseIndent nextDepth lsAfterT ts
                 in (t : rest, lsEnd, remaining)

-- | Processes Python tokens outside a test suite.
processOutsidePython :: Bool -> LineState -> [Token] -> [Token]
processOutsidePython _ _ [] = []
processOutsidePython keepTests ls allToks@(t : ts) =
    let (isFirst, col, lsAfterT) = stepLineState ls t
     in if isFirst && isTokenOperator t && tToken t == "@"
            then
                let (decs, hasTestDec, lsAfterDecs, afterDecs) = collectPythonDecorators col ls allToks
                    flatDecs = concat decs
                    targetAligned = case afterDecs of
                        (tf : _) ->
                            let (tfFirst, tfCol, _) = stepLineState lsAfterDecs tf
                             in tfFirst && tfCol == col && isPythonDefOrClassStart afterDecs
                        [] -> False
                 in if not (null decs) && targetAligned && (hasTestDec || isPythonTestDefOrClass afterDecs)
                        then
                            let (suiteToks, lsAfterSuite, remaining) = consumePythonSuite col lsAfterDecs afterDecs
                             in if keepTests
                                    then flatDecs ++ suiteToks ++ processOutsidePython keepTests lsAfterSuite remaining
                                    else processOutsidePython keepTests lsAfterSuite remaining
                        else
                            if not (null decs)
                                then
                                    if keepTests
                                        then processOutsidePython keepTests lsAfterDecs afterDecs
                                        else flatDecs ++ processOutsidePython keepTests lsAfterDecs afterDecs
                                else
                                    if keepTests
                                        then processOutsidePython keepTests lsAfterT ts
                                        else t : processOutsidePython keepTests lsAfterT ts
            else
                if isFirst && isPythonTestDefOrClass allToks
                    then
                        let (suiteToks, lsAfterSuite, remaining) = consumePythonSuite col ls allToks
                         in if keepTests
                                then suiteToks ++ processOutsidePython keepTests lsAfterSuite remaining
                                else processOutsidePython keepTests lsAfterSuite remaining
                    else
                        if keepTests
                            then processOutsidePython keepTests lsAfterT ts
                            else t : processOutsidePython keepTests lsAfterT ts
