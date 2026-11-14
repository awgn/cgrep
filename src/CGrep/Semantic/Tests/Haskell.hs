{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Haskell () where

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
import Data.Bits (complement, (.&.))
import Data.Char (isAsciiUpper)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import System.OsPath (OsPath, osp, takeBaseName, takeExtension)
import qualified System.OsString as OS

instance LanguageTestFilter 'Haskell where
    isTestFile = isHaskellTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideHaskell keepTests initialLineState tokens

-- | HSpec discovery convention (`*Spec.hs`, `*Spec.lhs`).
isHaskellTestFilePath :: OsPath -> Bool
isHaskellTestFilePath path =
    let ext = takeExtension path
        base = takeBaseName path
     in ext `elem` ([[osp|.hs|], [osp|.lhs|]] :: [OsPath])
            && [osp|Spec|] `OS.isSuffixOf` base

-- | Tracks byte position and leading visual indentation column of the current line
-- across the token stream in O(fileSize) total work.
data LineState = LineState
    { lsPrevEnd :: {-# UNPACK #-} !Int
    , lsLineIndent :: {-# UNPACK #-} !Int
    }

initialLineState :: LineState
initialLineState = LineState (-1) 0

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

-- | HSpec / Tasty / HUnit / QuickCheck / Hedgehog / Golden test builder functions
-- that take a test/group label (string literal) as their first argument.
isHaskellTestBuilder :: Text -> Bool
isHaskellTestBuilder name =
    name
        `elem` ([ "describe"
                , "xdescribe"
                , "fdescribe"
                , "it"
                , "xit"
                , "fit"
                , "specify"
                , "xspecify"
                , "fspecify"
                , "context"
                , "xcontext"
                , "fcontext"
                , "testCase"
                , "testCaseInfo"
                , "testCaseSteps"
                , "testGroup"
                , "testProperty"
                , "testPropertyNamed"
                , "goldenVsString"
                , "goldenVsFile"
                , "goldenVsStringDiff"
                , "goldenVsFileDiff"
                , "expectFailBecause"
                , "ignoreTestBecause"
                , "TestLabel"
                ] :: [Text])

-- | Naming conventions for QuickCheck, Hedgehog, SmallCheck, and tasty-discover properties/cases.
isHaskellPropName :: Text -> Bool
isHaskellPropName name =
    "prop_" `T.isPrefixOf` name
        || "hprop_" `T.isPrefixOf` name
        || "scprop_" `T.isPrefixOf` name
        || "case_" `T.isPrefixOf` name
        || "unit_" `T.isPrefixOf` name

-- | Test framework return types in Haskell type signatures.
isHaskellTestTypeName :: Text -> Bool
isHaskellTestTypeName name =
    name
        `elem` ([ "Spec"
                , "SpecWith"
                , "TestTree"
                , "Property"
                , "Assertion"
                ] :: [Text])

isUpperIdent :: Token -> Bool
isUpperIdent t =
    isTokenIdentifier t
        && case T.uncons (tToken t) of
            Just (c, _) -> isAsciiUpper c
            Nothing -> False

-- | Match optional `Mod.Sub.` module qualification before an identifier.
matchQualifiedIdent :: [Token] -> Maybe ([Token], Text, [Token])
matchQualifiedIdent (t1 : ts)
    | isTokenIdentifier t1 = Just (go [t1] (tToken t1) t1 ts)
  where
    go acc _ prev (d : nxt : rest)
        | isUpperIdent prev
        , isTokenOperator d
        , tToken d == "."
        , areAdjacentTokens prev d
        , areAdjacentTokens d nxt
        , isTokenIdentifier nxt =
            go (acc ++ [d, nxt]) (tToken nxt) nxt rest
    go acc curName _ rest = (acc, curName, rest)
matchQualifiedIdent _ = Nothing

-- | Match `::` in both `--strict` (`"::"` / `"∷"`) and default (`":"` adjacent to `":"`) modes.
matchDoubleColon :: [Token] -> Maybe ([Token], [Token])
matchDoubleColon (x1 : x2 : rest)
    | isTokenOperator x1
    , tToken x1 == ":"
    , isTokenOperator x2
    , tToken x2 == ":"
    , areAdjacentTokens x1 x2 =
        Just ([x1, x2], rest)
matchDoubleColon (x : rest)
    | isTokenOperator x && (tToken x == "::" || tToken x == "∷") =
        Just ([x], rest)
matchDoubleColon _ = Nothing

-- | Match `->` or `=>` in type signatures across `--strict` and default modes.
matchTypeArrow :: [Token] -> Maybe ([Token], [Token])
matchTypeArrow (x1 : x2 : rest)
    | isTokenOperator x1
    , (tToken x1 == "-" || tToken x1 == "=")
    , isTokenOperator x2
    , tToken x2 == ">"
    , areAdjacentTokens x1 x2 =
        Just ([x1, x2], rest)
matchTypeArrow (x : rest)
    | isTokenOperator x
    , tToken x `elem` (["->", "=>", "→", "⇒"] :: [Text]) =
        Just ([x], rest)
matchTypeArrow _ = Nothing

-- | True if @t@ is a standalone `=` definition operator (not part of `==`, `/=`, `<=`, `>=`, `=>`, `=<<`).
isStandaloneEquals :: Maybe Token -> Token -> [Token] -> Bool
isStandaloneEquals mPrev t rest =
    isTokenOperator t
        && tToken t == "="
        && not prevAdjOp
        && not nextAdjOp
  where
    prevAdjOp = case mPrev of
        Just p -> isTokenOperator p && areAdjacentTokens p t
        Nothing -> False
    nextAdjOp = case rest of
        (n : _) -> isTokenOperator n && areAdjacentTokens t n
        [] -> False

-- | Determine whether the return type (last top-level segment after `->` / `=>`)
-- of a Haskell type signature is a test type (`Spec`, `SpecWith a`, `TestTree`,
-- `Property`, `Assertion`, `[TestTree]`, `IO TestTree`, etc.).
isHaskellTestReturnType :: [Token] -> Bool
isHaskellTestReturnType sigTokens =
    let retTokens = lastTopLevelTypeSegment 0 [] sigTokens
     in checkReturnTypeTokens retTokens
  where
    lastTopLevelTypeSegment :: Int -> [Token] -> [Token] -> [Token]
    lastTopLevelTypeSegment _ acc [] = reverse acc
    lastTopLevelTypeSegment !depth acc ts@(t : rest)
        | depth == 0
        , Just (_, afterArrow) <- matchTypeArrow ts =
            lastTopLevelTypeSegment 0 [] afterArrow
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            lastTopLevelTypeSegment (depth + 1) (t : acc) rest
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            lastTopLevelTypeSegment (max 0 (depth - 1)) (t : acc) rest
        | otherwise =
            lastTopLevelTypeSegment depth (t : acc) rest

    checkReturnTypeTokens :: [Token] -> Bool
    checkReturnTypeTokens [] = False
    checkReturnTypeTokens (b : inner)
        | isTokenBracket b && (tToken b == "(" || tToken b == "[") =
            checkReturnTypeTokens inner
    checkReturnTypeTokens ts =
        case matchQualifiedIdent ts of
            Just (_, tyName, afterTy)
                | isHaskellTestTypeName tyName -> True
                | tyName == "IO" -> checkReturnTypeTokens afterTy
            _ -> False

-- | Check whether tokens following an identifier at @baseIndent@ form a Haskell
-- equation LHS (`[patterns] [| guards] = rhs`) before any dedent to @<= baseIndent@.
findEquationRhs :: Int -> LineState -> Token -> [Token] -> Maybe [Token]
findEquationRhs !baseIndent ls0 firstTok = go 0 ls0 (Just firstTok)
  where
    go :: Int -> LineState -> Maybe Token -> [Token] -> Maybe [Token]
    go _ _ _ [] = Nothing
    go !depth ls mPrev (t : rest) =
        let (isNewLine, lineIndent, ls') = stepLineState ls t
         in if depth == 0 && isNewLine && lineIndent <= baseIndent
                then Nothing
                else
                    if depth == 0 && isStandaloneEquals mPrev t rest
                        then Just rest
                        else
                            if depth == 0 && isDisallowedBeforeEquals t rest
                                then Nothing
                                else
                                    let !depth' = updateBracketDepth depth t
                                     in go depth' ls' (Just t) rest

    isDisallowedBeforeEquals :: Token -> [Token] -> Bool
    isDisallowedBeforeEquals t rest =
        (isTokenKeyword t && tToken t `elem` (["let", "in", "do", "case", "of", "if", "then", "else", "where", "module", "import", "data", "type", "newtype", "class", "instance"] :: [Text]))
            || (isTokenOperator t && tToken t == "$")
            || case matchDoubleColon (t : rest) of
                Just _ -> True
                Nothing -> False

-- | Check whether the start of an equation RHS begins directly with a test builder expression
-- (e.g. `goldenVsString "golden" ...`, `specify "standalone" ...`, `property $ do ...`, `TestList [...]`).
startsWithTestRhs :: [Token] -> Bool
startsWithTestRhs ts =
    case matchQualifiedIdent ts of
        Just (_, name, afterIdent)
            | isHaskellTestBuilder name ->
                case afterIdent of
                    (s : _) -> isTokenString s
                    [] -> False
            | name `elem` (["TestCase", "TestList"] :: [Text]) -> True
            | name == "property" ->
                case afterIdent of
                    (op : _) ->
                        (isTokenOperator op && tToken op == "$")
                            || (isTokenKeyword op && tToken op == "do")
                    [] -> False
        _ -> False

updateBracketDepth :: Int -> Token -> Int
updateBracketDepth !depth t
    | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") = depth + 1
    | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") = max 0 (depth - 1)
    | otherwise = depth

-- | Collect a Haskell declaration starting at its first token (already known to be on the
-- declaration's header line at @baseIndent@), continuing across all lines indented strictly
-- deeper than @baseIndent@ (and across balanced brackets).
collectHaskellIndentedBody :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
collectHaskellIndentedBody _ ls0 [] = ([], ls0, [])
collectHaskellIndentedBody !baseIndent ls0 (firstTok : rest0) =
    let (_, _, ls1) = stepLineState ls0 firstTok
        !d1 = updateBracketDepth 0 firstTok
        (moreToks, lsFinal, remToks) = go d1 ls1 rest0
     in (firstTok : moreToks, lsFinal, remToks)
  where
    go :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
    go _ ls [] = ([], ls, [])
    go !depth ls (t : ts) =
        let (isNewLine, lineIndent, ls') = stepLineState ls t
         in if depth == 0 && isNewLine && lineIndent <= baseIndent
                then ([], ls, t : ts)
                else
                    let !depth' = updateBracketDepth depth t
                        (acc, lsEnd, rest) = go depth' ls' ts
                     in (t : acc, lsEnd, rest)

-- | Collect zero or more consecutive equations at @baseIndent@ for binding @targetName@.
collectMatchingEquations :: Text -> Int -> LineState -> [Token] -> ([Token], LineState, [Token])
collectMatchingEquations _ _ ls [] = ([], ls, [])
collectMatchingEquations targetName !baseIndent ls toks@(t : ts) =
    let (isNewLine, lineIndent, _) = stepLineState ls t
     in if isNewLine
            && lineIndent == baseIndent
            && isTokenIdentifier t
            && tToken t == targetName
            && case matchDoubleColon ts of Just _ -> False; Nothing -> True
            then
                let (eqToks, lsAfterEq, afterEq) = collectHaskellIndentedBody baseIndent ls toks
                    (moreEqs, lsFinal, finalRest) = collectMatchingEquations targetName baseIndent lsAfterEq afterEq
                 in (eqToks ++ moreEqs, lsFinal, finalRest)
            else ([], ls, toks)

-- | Collect an inline test builder call (`describe "..." $ do ...`, `testCase "..." $ ...`)
-- when the enclosing function was not itself a test binding (e.g. inside `main = hspec $ do`).
collectHaskellBuilderExpr :: Int -> LineState -> [Token] -> [Token] -> ([Token], LineState, [Token])
collectHaskellBuilderExpr !baseIndent lsAfterPrefix prefixToks rest0 =
    let (bodyToks, lsFinal, remToks) = go 0 lsAfterPrefix rest0
     in (prefixToks ++ bodyToks, lsFinal, remToks)
  where
    go :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
    go _ ls [] = ([], ls, [])
    go !depth ls (t : ts) =
        let (isNewLine, lineIndent, ls') = stepLineState ls t
            isUnmatchedClose =
                depth == 0 && isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}")
            isOuterComma =
                depth == 0 && isTokenOperator t && tToken t == ","
            isTopDeclKw =
                depth == 0
                    && isNewLine
                    && isTokenKeyword t
                    && tToken t `elem` (["module", "import", "data", "type", "newtype", "class", "instance", "foreign", "infix", "infixl", "infixr"] :: [Text])
         in if isUnmatchedClose || isOuterComma || isTopDeclKw || (depth == 0 && isNewLine && lineIndent <= baseIndent)
                then ([], ls, t : ts)
                else
                    let !depth' = updateBracketDepth depth t
                        (acc, lsEnd, rest) = go depth' ls' ts
                     in (t : acc, lsEnd, rest)

-- | Match a line-leading test declaration (`spec :: Spec`, `prop_foo :: ...`, `prop_foo x = ...`,
-- or untyped `goldenCheck = goldenVsString ...`).
matchLineLeadingTestDecl :: LineState -> [Token] -> Maybe ([Token], LineState, [Token])
matchLineLeadingTestDecl ls0 toks@(t1 : ts) =
    let (isFirstOnLine, baseIndent, ls1) = stepLineState ls0 t1
     in if not isFirstOnLine || not (isTokenIdentifier t1)
            then Nothing
            else
                matchTypedTestDecl baseIndent ls0 t1 ts
                    <|> matchUntypedTestDecl baseIndent ls0 ls1 t1 ts
  where
    matchTypedTestDecl :: Int -> LineState -> Token -> [Token] -> Maybe ([Token], LineState, [Token])
    matchTypedTestDecl !baseIndent lsStart ident afterIdent = do
        (dcToks, _) <- matchDoubleColon afterIdent
        let (sigAllToks, lsAfterSig, afterSig) = collectHaskellIndentedBody baseIndent lsStart toks
            sigBodyToks = drop (1 + length dcToks) sigAllToks
            name = tToken ident
        if isHaskellPropName name || isHaskellTestReturnType sigBodyToks
            then
                let (eqToks, lsFinal, finalRest) = collectMatchingEquations name baseIndent lsAfterSig afterSig
                 in Just (sigAllToks ++ eqToks, lsFinal, finalRest)
            else Nothing

    matchUntypedTestDecl :: Int -> LineState -> LineState -> Token -> [Token] -> Maybe ([Token], LineState, [Token])
    matchUntypedTestDecl !baseIndent lsStart lsAfterIdent ident afterIdent = do
        rhsToks <- findEquationRhs baseIndent lsAfterIdent ident afterIdent
        let name = tToken ident
        if isHaskellPropName name || (name /= "main" && startsWithTestRhs rhsToks)
            then
                let (firstEq, lsAfterFirst, afterFirst) = collectHaskellIndentedBody baseIndent lsStart toks
                    (moreEqs, lsFinal, finalRest) = collectMatchingEquations name baseIndent lsAfterFirst afterFirst
                 in Just (firstEq ++ moreEqs, lsFinal, finalRest)
            else Nothing
matchLineLeadingTestDecl _ [] = Nothing

-- | Match a standalone test builder expression (`describe "..."`, `testCase "..."`, `goldenVsString "..."`).
matchInlineBuilder :: LineState -> [Token] -> Maybe ([Token], LineState, [Token])
matchInlineBuilder _ [] = Nothing
matchInlineBuilder ls0 toks@(firstQ : _) = do
    (qualBuilderToks, name, afterBuilder) <- matchQualifiedIdent toks
    if not (isHaskellTestBuilder name)
        then Nothing
        else case afterBuilder of
            (strTok : rest)
                | isTokenString strTok ->
                    let (_, lineIndent, _) = stepLineState ls0 firstQ
                        prefix = qualBuilderToks ++ [strTok]
                        lsAfterPrefix = foldl' (\ls tok -> let (_, _, ls') = stepLineState ls tok in ls') ls0 prefix
                     in Just (collectHaskellBuilderExpr lineIndent lsAfterPrefix prefix rest)
            _ -> Nothing

processOutsideHaskell :: Bool -> LineState -> [Token] -> [Token]
processOutsideHaskell _ _ [] = []
processOutsideHaskell keepTests ls0 toks@(t : ts) =
    case matchLineLeadingTestDecl ls0 toks <|> matchInlineBuilder ls0 toks of
        Just (testTokens, lsAfter, remaining) ->
            if keepTests
                then testTokens ++ processOutsideHaskell keepTests lsAfter remaining
                else processOutsideHaskell keepTests lsAfter remaining
        Nothing ->
            let (_, _, ls1) = stepLineState ls0 t
             in if keepTests
                    then processOutsideHaskell keepTests ls1 ts
                    else t : processOutsideHaskell keepTests ls1 ts
