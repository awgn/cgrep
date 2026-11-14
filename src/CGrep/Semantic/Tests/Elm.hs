{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Elm () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Chunk (pattern ChunkNativeType)
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    isTokenString,
    tToken,
    tTyp,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import System.OsPath (OsPath, osp, splitDirectories, takeExtension, takeFileName)

instance LanguageTestFilter 'Elm where
    isTestFile = isElmTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideElm keepTests False tokens

-- | `elm-test` discovers test modules inside the `tests/` directory (`tests/**/*.elm`).
isElmTestFilePath :: OsPath -> Bool
isElmTestFilePath path =
    takeExtension path == [osp|.elm|]
        && any (\d -> takeFileName d == [osp|tests|]) parentDirs
  where
    parentDirs = case reverse (splitDirectories path) of
        (_ : ds) -> ds
        [] -> []

-- | Checks in O(1) whether a token starts at column 0 (beginning of file or immediately after `\n`).
-- In Elm, all top-level declarations must start at column 0.
isAtColumnZero :: Token -> Bool
isAtColumnZero tok =
    case tToken tok of
        TI.Text arr off _ ->
            off == 0 || A.unsafeIndex arr (off - 1) == 10

-- | `elm-test` functions that take a string description as their first argument.
isElmStringTestBuilder :: Text -> Bool
isElmStringTestBuilder name =
    name `elem` (["describe", "test", "todo"] :: [Text])

-- | `elm-test` fuzz builders (`fuzz`, `fuzz2`, `fuzz3`, `fuzzWith`).
isElmFuzzBuilder :: Text -> Bool
isElmFuzzBuilder name =
    name `elem` (["fuzz", "fuzz2", "fuzz3", "fuzzWith"] :: [Text])

-- | Checks if a token stream starts with a single `:` (and not `::`).
startsWithSingleColon :: [Token] -> Bool
startsWithSingleColon (x1 : x2 : _)
    | isTokenOperator x1 && tToken x1 == ":"
    , isTokenOperator x2 && tToken x2 == ":"
    , areAdjacentTokens x1 x2 =
        False
startsWithSingleColon (x : _)
    | isTokenOperator x && tToken x == ":" =
        True
startsWithSingleColon _ = False

-- | Checks if a token stream starts with a single `=` (and not `==`, `=>`, or `/=`).
startsWithSingleEquals :: [Token] -> Bool
startsWithSingleEquals (x1 : x2 : _)
    | isTokenOperator x1 && tToken x1 == "="
    , isTokenOperator x2 && (tToken x2 == "=" || tToken x2 == ">")
    , areAdjacentTokens x1 x2 =
        False
startsWithSingleEquals (x : _)
    | isTokenOperator x && tToken x == "=" =
        True
startsWithSingleEquals _ = False

-- | Match Elm `->` in both `--strict` (`"->"`) and default (`"-"` adjacent to `">"`) modes.
matchThinArrow :: [Token] -> Maybe [Token]
matchThinArrow (t : ts)
    | isTokenOperator t && tToken t == "->" = Just ts
matchThinArrow (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "-"
    , isTokenOperator t2 && tToken t2 == ">"
    , areAdjacentTokens t1 t2 =
        Just ts
matchThinArrow _ = Nothing

-- | Match Elm `<|` pipe operator (`"<|"` in `--strict`, or `"<"` adjacent to `"|"`).
matchLeftPipe :: [Token] -> Maybe [Token]
matchLeftPipe (t : ts)
    | isTokenOperator t && tToken t == "<|" = Just ts
matchLeftPipe (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "<"
    , isTokenOperator t2 && tToken t2 == "|"
    , areAdjacentTokens t1 t2 =
        Just ts
matchLeftPipe _ = Nothing

unwrapOuterParens :: [Token] -> [Token]
unwrapOuterParens (tOpen : rest)
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (inside, after) = processInsideBrackets "(" ")" 1 rest
         in if null after
                then case reverse inside of
                    (lastTok : revMid)
                        | isTokenBracket lastTok && tToken lastTok == ")" ->
                            unwrapOuterParens (reverse revMid)
                    _ -> tOpen : rest
                else tOpen : rest
unwrapOuterParens toks = toks

-- | True if the return type of an Elm top-level type signature (`: ... -> RetType`)
-- is `Test`, `Test.Test`, `List Test`, or `List Test.Test` (and NOT e.g. `Test -> String`).
isElmTestTypeSignature :: [Token] -> Bool
isElmTestTypeSignature (tColon : rest)
    | startsWithSingleColon (tColon : rest) =
        let sigToks = takeWhile (not . isAtColumnZero) rest
            segments = splitTopLevelArrows 0 [] sigToks
         in case reverse segments of
                (retSeg : _) -> isTestReturnType (unwrapOuterParens retSeg)
                [] -> False
  where
    splitTopLevelArrows :: Int -> [Token] -> [Token] -> [[Token]]
    splitTopLevelArrows _ cur [] = [reverse cur | not (null cur)]
    splitTopLevelArrows !depth cur tokens@(t : ts)
        | depth == 0
        , Just afterArrow <- matchThinArrow tokens =
            reverse cur : splitTopLevelArrows 0 [] afterArrow
        | isTokenBracket t && (tToken t == "(" || tToken t == "{" || tToken t == "[") =
            splitTopLevelArrows (depth + 1) (t : cur) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "}" || tToken t == "]") =
            splitTopLevelArrows (max 0 (depth - 1)) (t : cur) ts
        | otherwise =
            splitTopLevelArrows depth (t : cur) ts

    isSingleTestType :: [Token] -> Bool
    isSingleTestType [t] = isTokenIdentifier t && tToken t == "Test"
    isSingleTestType [t1, dot, t2] =
        isTokenIdentifier t1
            && tToken t1 == "Test"
            && isTokenOperator dot
            && tToken dot == "."
            && isTokenIdentifier t2
            && tToken t2 == "Test"
    isSingleTestType _ = False

    isTestReturnType :: [Token] -> Bool
    isTestReturnType toks
        | isSingleTestType toks = True
    isTestReturnType (tList : elemToks)
        | (tTyp tList == ChunkNativeType || isTokenIdentifier tList)
        , tToken tList == "List" =
            isSingleTestType (unwrapOuterParens elemToks)
    isTestReturnType _ = False
isElmTestTypeSignature _ = False

-- | Checks if a token sequence starts with an `elm-test` builder expression
-- (`describe "..."`, `test "..."`, `todo "..."`, `fuzz ... "..."`, `Test.concat`,
-- `Test.skip`, `Test.only`, etc.).
startsWithElmTestBuilder :: [Token] -> Bool
startsWithElmTestBuilder (tMod : tDot : rest)
    | isTokenIdentifier tMod && tToken tMod == "Test"
    , isTokenOperator tDot && tToken tDot == "." =
        case rest of
            (tFn : afterFn)
                | isTokenIdentifier tFn && tToken tFn == "concat" -> True
                | isTokenIdentifier tFn && (tToken tFn == "skip" || tToken tFn == "only") ->
                    startsAfterWrapper afterFn
            _ -> startsWithUnqualifiedElmBuilder rest
startsWithElmTestBuilder (tFn : rest)
    | isTokenIdentifier tFn && (tToken tFn == "skip" || tToken tFn == "only") =
        startsAfterWrapper rest
    | isTokenIdentifier tFn && tToken tFn == "concat"
    , (tBr : insideList) <- rest
    , isTokenBracket tBr && tToken tBr == "[" =
        startsWithElmTestBuilder insideList
startsWithElmTestBuilder ts = startsWithUnqualifiedElmBuilder ts

startsAfterWrapper :: [Token] -> Bool
startsAfterWrapper tokens
    | Just afterPipe <- matchLeftPipe tokens = startsWithElmTestBuilder afterPipe
startsAfterWrapper (tOpen : rest)
    | isTokenBracket tOpen && tToken tOpen == "(" = startsWithElmTestBuilder rest
startsAfterWrapper _ = False

startsWithUnqualifiedElmBuilder :: [Token] -> Bool
startsWithUnqualifiedElmBuilder (t1 : t2 : _)
    | isTokenIdentifier t1 && isElmStringTestBuilder (tToken t1)
    , isTokenString t2 =
        True
startsWithUnqualifiedElmBuilder (t1 : rest)
    | isTokenIdentifier t1 && isElmFuzzBuilder (tToken t1) =
        any isTokenString (take 30 (takeWhile (not . isAtColumnZero) rest))
startsWithUnqualifiedElmBuilder _ = False

-- | Consume `0..n` Elm parameter patterns (identifiers, `_`, `(...)`, `{...}`) up to
-- and including a single `=`, returning `(paramsAndEquals, tokensAfterEquals)`.
consumeElmBindingParams :: Int -> [Token] -> Maybe ([Token], [Token])
consumeElmBindingParams = go []
  where
    go :: [Token] -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go acc !n ts@(t : ts')
        | startsWithSingleEquals ts =
            Just (reverse (t : acc), ts')
        | not (isAtColumnZero t)
        , isTokenIdentifier t || (isTokenOperator t && tToken t == "_") =
            go (t : acc) (n - 1) ts'
        | not (isAtColumnZero t)
        , isTokenBracket t && (tToken t == "(" || tToken t == "{") =
            let closeB = if tToken t == "(" then ")" else "}"
                (inside, afterBracket) = processInsideBrackets (tToken t) closeB 1 ts'
             in go (reverse inside ++ (t : acc)) (n - 1) afterBracket
        | otherwise = Nothing

isTopLevelElmBinding :: [Token] -> Bool
isTopLevelElmBinding (tName : rest)
    | isTokenIdentifier tName && isAtColumnZero tName =
        case consumeElmBindingParams 12 rest of
            Just _ -> True
            Nothing -> False
isTopLevelElmBinding _ = False

-- | Processes Elm tokens outside of a test block.
processOutsideElm :: Bool -> Bool -> [Token] -> [Token]
processOutsideElm _ _ [] = []
-- Pattern 1: Top-level `<name> : ... -> Test` / `List Test` followed by `<name> <params> = ...`
processOutsideElm keepTests False (tName : ts)
    | isTokenIdentifier tName && isAtColumnZero tName && isElmTestTypeSignature ts =
        let (testTokens, remaining) = collectUntilNextElmDef (Just (tToken tName)) 0 0 False ts
         in if keepTests
                then tName : testTokens ++ processOutsideElm keepTests False remaining
                else processOutsideElm keepTests False remaining
-- Pattern 2: Top-level unannotated `<name> <params> = <elmTestBuilder>`
processOutsideElm keepTests False (tName : rest)
    | isTokenIdentifier tName && isAtColumnZero tName
    , Just (paramsAndEq, afterEq) <- consumeElmBindingParams 12 rest
    , startsWithElmTestBuilder afterEq =
        let (testTokens, remaining) = collectUntilNextElmDef Nothing 0 0 True afterEq
         in if keepTests
                then (tName : paramsAndEq ++ testTokens) ++ processOutsideElm keepTests False remaining
                else processOutsideElm keepTests False remaining
-- Pattern 3: Standalone `describe "..."` / `test "..."` / `fuzz ...`
processOutsideElm keepTests False ts@(t1 : rest)
    | startsWithElmTestBuilder ts =
        let (testTokens, remaining) = collectUntilNextElmDef Nothing 0 0 True rest
         in if keepTests
                then t1 : testTokens ++ processOutsideElm keepTests False remaining
                else processOutsideElm keepTests False remaining
-- Fallback
processOutsideElm keepTests _ (t : ts) =
    let isDot = isTokenOperator t && tToken t == "."
     in if keepTests
            then processOutsideElm keepTests isDot ts
            else t : processOutsideElm keepTests isDot ts

-- | Collects tokens of an Elm top-level test definition until the next top-level declaration.
-- Tracks bracket depth (`()`, `[]`, `{}`), `let ... in` nesting depth, and column-0 boundaries.
collectUntilNextElmDef :: Maybe Text -> Int -> Int -> Bool -> [Token] -> ([Token], [Token])
collectUntilNextElmDef _ _ _ _ [] = ([], [])
collectUntilNextElmDef mName !bracketDepth !letDepth !passedBodyEq (x : xs)
    -- Track brackets
    | isTokenBracket x && (tToken x == "(" || tToken x == "[" || tToken x == "{") =
        let (col, remTokens) = collectUntilNextElmDef mName (bracketDepth + 1) letDepth passedBodyEq xs
         in (x : col, remTokens)
    | isTokenBracket x && (tToken x == ")" || tToken x == "]" || tToken x == "}") =
        let newBracketDepth = max 0 (bracketDepth - 1)
            (col, remTokens) = collectUntilNextElmDef mName newBracketDepth letDepth passedBodyEq xs
         in (x : col, remTokens)
    -- Track `let ... in`
    | isTokenKeyword x && tToken x == "let" =
        let (col, remTokens) = collectUntilNextElmDef mName bracketDepth (letDepth + 1) passedBodyEq xs
         in (x : col, remTokens)
    | isTokenKeyword x && tToken x == "in" =
        let newLetDepth = max 0 (letDepth - 1)
            (col, remTokens) = collectUntilNextElmDef mName bracketDepth newLetDepth passedBodyEq xs
         in (x : col, remTokens)
    -- At top level (bracketDepth == 0 && letDepth == 0 && column 0), stop at top-level keywords
    -- (`effect` is lexed as an identifier in `effect module ...`)
    | bracketDepth == 0
        && letDepth == 0
        && isAtColumnZero x
        && (isTokenKeyword x || isTokenIdentifier x)
        && (tToken x `elem` (["type", "port", "module", "import", "infix", "effect"] :: [Text])) =
        ([], x : xs)
    -- At top level (column 0), stop at the next type signature `<otherIdent> : ...`
    | bracketDepth == 0
        && letDepth == 0
        && isAtColumnZero x
        && isTokenIdentifier x
        && startsWithSingleColon xs
        && (passedBodyEq || Just (tToken x) /= mName) =
        ([], x : xs)
    -- At top level (column 0), if we are in `<name> : ... -> Test` and haven't passed
    -- `<name> <params> =` yet, recognize `<name> <params> =` and flip `passedBodyEq = True`
    | bracketDepth == 0
        && letDepth == 0
        && not passedBodyEq
        && isAtColumnZero x
        && isTokenIdentifier x
        && Just (tToken x) == mName
    , Just (paramsAndEq, afterEq) <- consumeElmBindingParams 12 xs =
        let (col, remTokens) = collectUntilNextElmDef mName bracketDepth letDepth True afterEq
         in (x : paramsAndEq ++ col, remTokens)
    -- At top level (column 0), once `passedBodyEq` is True, stop at the next top-level binding `<nextIdent> <params> =`
    | bracketDepth == 0
        && letDepth == 0
        && passedBodyEq
        && isTopLevelElmBinding (x : xs) =
        ([], x : xs)
    | otherwise =
        let (col, remTokens) = collectUntilNextElmDef mName bracketDepth letDepth passedBodyEq xs
         in (x : col, remTokens)
