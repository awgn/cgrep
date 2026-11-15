{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Kotlin () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, splitDirectories, takeExtension, takeFileName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Kotlin where
    isTestFile = isKotlinTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideKotlin keepTests False initialLineState tokens

-- | Standard Gradle / Maven / Kotlin Multiplatform test source sets
-- (`src/test/...`, `src/commonTest/...`, `src/jvmTest/...`, `src/androidUnitTest/...`).
isKotlinTestFilePath :: OsPath -> Bool
isKotlinTestFilePath path =
    let ext = takeExtension path
        dirs = map takeFileName (dropLastPart (splitDirectories path))
     in ext `elem` ([[osp|.kt|], [osp|.kts|], [osp|.ktm|]] :: [OsPath])
            && hasKotlinTestSourceSet dirs
  where
    dropLastPart [] = []
    dropLastPart [_] = []
    dropLastPart (x : xs) = x : dropLastPart xs

    hasKotlinTestSourceSet (d1 : d2 : rest)
        | d1 == [osp|src|]
            && (d2 == [osp|test|] || [osp|Test|] `OS.isSuffixOf` d2) =
            True
        | otherwise = hasKotlinTestSourceSet (d2 : rest)
    hasKotlinTestSourceSet _ = False

data KotlinAnno = KotlinAnno
    { kaTokens :: [Token]
    , kaUseSite :: !(Maybe Text)
    , kaSimpleName :: !Text
    , kaHasParens :: !Bool
    }

extractKotlinAnno :: [Token] -> Maybe (KotlinAnno, [Token])
extractKotlinAnno (tAt : tFirst : ts)
    | isTokenOperator tAt
    , tToken tAt == "@"
    , isTokenIdentifier tFirst || isTokenKeyword tFirst =
        let (useSite, useSiteToks, firstAnnoTok, afterFirst) =
                case ts of
                    (tCol : tNext : rest)
                        | isTokenOperator tCol
                        , tToken tCol == ":"
                        , isTokenIdentifier tNext ->
                            (Just (tToken tFirst), [tFirst, tCol], tNext, rest)
                    _ -> (Nothing, [], tFirst, ts)
         in if not (isTokenIdentifier firstAnnoTok)
                then Nothing
                else
                    let (dotPath, lastIdent, afterPath) = collectDotPath firstAnnoTok afterFirst
                        (typeArgToks, afterTypeArgs) = consumeOptionalAngleArgs afterPath
                        (parenToks, hasParens, afterParens) = case afterTypeArgs of
                            (tOpen : rest)
                                | isTokenBracket tOpen && tToken tOpen == "(" ->
                                    let (inside, remToks) = processInsideBrackets "(" ")" 1 rest
                                     in (tOpen : inside, True, remToks)
                            _ -> ([], False, afterTypeArgs)
                        allToks = tAt : useSiteToks ++ (firstAnnoTok : dotPath) ++ typeArgToks ++ parenToks
                     in Just (KotlinAnno allToks useSite lastIdent hasParens, afterParens)
  where
    collectDotPath :: Token -> [Token] -> ([Token], Text, [Token])
    collectDotPath _ (d : i : rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (more, finalName, remToks) = collectDotPath i rest
             in (d : i : more, finalName, remToks)
    collectDotPath curLast rest = ([], tToken curLast, rest)

    consumeOptionalAngleArgs :: [Token] -> ([Token], [Token])
    consumeOptionalAngleArgs (t : rest)
        | isTokenOperator t && "<" `T.isPrefixOf` tToken t =
            let d0 = countChar '<' (tToken t) - countChar '>' (tToken t)
             in if d0 <= 0
                    then ([t], rest)
                    else
                        let (more, after) = goAngles d0 rest
                         in (t : more, after)
    consumeOptionalAngleArgs toks = ([], toks)

    goAngles :: Int -> [Token] -> ([Token], [Token])
    goAngles _ [] = ([], [])
    goAngles !d (x : xs)
        | d <= 0 = ([], x : xs)
        | isTokenOperator x =
            let d' = d + countChar '<' (tToken x) - countChar '>' (tToken x)
             in if d' <= 0
                    then ([x], xs)
                    else
                        let (more, after) = goAngles d' xs
                         in (x : more, after)
        | otherwise =
            let (more, after) = goAngles d xs
             in (x : more, after)
extractKotlinAnno _ = Nothing

countChar :: Char -> Text -> Int
countChar c = T.foldl' (\acc ch -> if ch == c then acc + 1 else acc) 0

isKotlinKnownTestAnnoName :: Text -> Bool
isKotlinKnownTestAnnoName = \case
    "Before" -> True
    "After" -> True
    "BeforeEach" -> True
    "AfterEach" -> True
    "BeforeAll" -> True
    "AfterAll" -> True
    "BeforeClass" -> True
    "AfterClass" -> True
    "BeforeTest" -> True
    "AfterTest" -> True
    "BeforeMethod" -> True
    "AfterMethod" -> True
    "BeforeSuite" -> True
    "AfterSuite" -> True
    "Nested" -> True
    "DataProvider" -> True
    "RunWith" -> True
    "ExtendWith" -> True
    "Property" -> True
    "Example" -> True
    "Rule" -> True
    "ClassRule" -> True
    "TempDir" -> True
    "RegisterExtension" -> True
    "Mock" -> True
    "Spy" -> True
    "InjectMocks" -> True
    "Captor" -> True
    "MockBean" -> True
    "SpyBean" -> True
    "MockitoBean" -> True
    _ -> False

isKotlinBddStepAnnoName :: Text -> Bool
isKotlinBddStepAnnoName = \case
    "Given" -> True
    "When" -> True
    "Then" -> True
    "And" -> True
    "But" -> True
    _ -> False

isKotlinTestAnno :: KotlinAnno -> Bool
isKotlinTestAnno (KotlinAnno _ mUseSite n hasParens)
    | mUseSite == Just "file" = False
    | otherwise =
        hasCamelTestWord n
            || isKotlinKnownTestAnnoName n
            || (hasParens && isKotlinBddStepAnnoName n)
  where
    hasCamelTestWord name =
        let prefixOk = case T.stripPrefix "Test" name of
                Nothing -> False
                Just rest -> case T.uncons rest of
                    Nothing -> True
                    Just (c, _) -> not (isAsciiLower c)
         in prefixOk
                || "Test" `T.isSuffixOf` name
                || "Tests" `T.isSuffixOf` name

collectKotlinAnnotations :: [Token] -> ([Token], Bool, [Token])
collectKotlinAnnotations = go [] False
  where
    go acc !anyTest remaining =
        case extractKotlinAnno remaining of
            Just (ka, rest) ->
                go (acc ++ kaTokens ka) (anyTest || isKotlinTestAnno ka) rest
            Nothing -> (acc, anyTest, remaining)

isStandaloneKotlinEq :: Maybe Token -> Token -> [Token] -> Bool
isStandaloneKotlinEq mPrev t rest =
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

-- | Collect the RHS of an expression-bodied Kotlin function or property (`= assertEquals(...)`,
-- `= runTest { ... }`, `= TemporaryFolder(...)`).
collectKotlinExprBody :: LineState -> [Token] -> ([Token], LineState, [Token])
collectKotlinExprBody ls0 = go True ls0
  where
    go :: Bool -> LineState -> [Token] -> ([Token], LineState, [Token])
    go _ ls [] = ([], ls, [])
    go !allowNewline ls (t : ts) =
        let (isNewLine, _, ls1) = stepLineState ls t
            startsWithChainDot = isTokenOperator t && (tToken t == "." || tToken t == "?.")
         in if tToken t == ";" || (isTokenBracket t && tToken t == "}")
                then ([], ls, t : ts)
                else
                    if isNewLine && not allowNewline && not startsWithChainDot
                        then ([], ls, t : ts)
                        else
                            if isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{")
                                then
                                    let closeB = case tToken t of
                                            "(" -> ")"
                                            "[" -> "]"
                                            _ -> "}"
                                        (inside, afterB) = processInsideBrackets (tToken t) closeB 1 ts
                                        lsAfterB = advanceLineState ls1 inside
                                        (more, lsEnd, remToks) = go False lsAfterB afterB
                                     in (t : inside ++ more, lsEnd, remToks)
                                else
                                    let nextCont =
                                            tToken t == ","
                                                || (isTokenOperator t && tToken t `elem` (["+", "-", "*", "/", "%", "&&", "||", "==", "!=", "<", ">", "<=", ">=", "?:", ".", "?."] :: [Text]))
                                        (more, lsEnd, remToks) = go nextCont ls1 ts
                                     in (t : more, lsEnd, remToks)

-- | Consume the target declaration following `@Test` / `@Rule` / etc., handling:
--   * Backtick-escaped function names containing keywords/operators (`` fun `when fun or ;`() ``)
--   * Braced bodies (`fun foo() { ... }`, `class Foo { ... }`)
--   * Expression-bodied functions and properties (`fun foo() = ...`, `val rule = ...`)
--   * Abstract interface methods (`@Test fun abstractMethod()` -> stops before next `fun` or `}`)
consumeAnnotatedKotlinDecl :: LineState -> Int -> [Token] -> Maybe ([Token], LineState, [Token])
consumeAnnotatedKotlinDecl lsStart n0 = go 0 False True Nothing lsStart n0
  where
    go :: Int -> Bool -> Bool -> Maybe Token -> LineState -> Int -> [Token] -> Maybe ([Token], LineState, [Token])
    go _ _ _ _ _ 0 _ = Nothing
    go _ _ _ _ _ _ [] = Nothing
    go !depth !inBt !canSeeDecl mPrev ls !n (t : ts)
        | isTokenOperator t && odd (countChar '`' (tToken t)) =
            let (_, _, ls1) = stepLineState ls t
             in step depth (not inBt) canSeeDecl (Just t) ls1 (n - 1) t ts
        | inBt =
            let (_, _, ls1) = stepLineState ls t
             in step depth True canSeeDecl (Just t) ls1 (n - 1) t ts
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            let (_, _, ls1) = stepLineState ls t
             in step (depth + 1) False canSeeDecl (Just t) ls1 (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            let (_, _, ls1) = stepLineState ls t
             in step (max 0 (depth - 1)) False canSeeDecl (Just t) ls1 (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            let (_, _, ls1) = stepLineState ls t
                (bodyTokens, finalRest) = processInsideBraces 1 ts
                lsEnd = advanceLineState ls1 bodyTokens
             in Just (t : bodyTokens, lsEnd, finalRest)
        | depth == 0 && (tToken t == ";" || (isTokenBracket t && tToken t == "}")) =
            Nothing
        | depth == 0 && not canSeeDecl && isStandaloneKotlinEq mPrev t ts =
            let (_, _, ls1) = stepLineState ls t
                (exprToks, lsEnd, finalRest) = collectKotlinExprBody ls1 ts
             in Just (t : exprToks, lsEnd, finalRest)
        | depth == 0
            && isTokenKeyword t
            && tToken t `elem` (["fun", "class", "interface", "object", "val", "var"] :: [Text]) =
            if canSeeDecl
                then
                    let (_, _, ls1) = stepLineState ls t
                     in step 0 False False (Just t) ls1 (n - 1) t ts
                else Nothing
        | otherwise =
            let (_, _, ls1) = stepLineState ls t
             in step depth False canSeeDecl (Just t) ls1 (n - 1) t ts

    step !d !bt !csd mp lsNext !m tok rest =
        case go d bt csd mp lsNext m rest of
            Nothing -> Nothing
            Just (pre, lsEnd, post) -> Just (tok : pre, lsEnd, post)

-- | Find opening `{` after a Kotest call header (allowing `.config(...)` before `{`).
findKotestOpeningBrace :: Int -> [Token] -> Maybe ([Token], [Token])
findKotestOpeningBrace = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go !depth !n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0
            && ( tToken t == ";"
                    || (isTokenBracket t && tToken t == "}")
                    || (isTokenKeyword t && tToken t `elem` (["fun", "class", "interface", "object", "val", "var"] :: [Text]))
               ) =
            Nothing
        | otherwise =
            step depth (n - 1) t ts

    step !d !m tok rest =
        case go d m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

isKotestBlockName :: Text -> Bool
isKotestBlockName = \case
    "test" -> True
    "xtest" -> True
    "describe" -> True
    "xdescribe" -> True
    "Describe" -> True
    "xDescribe" -> True
    "it" -> True
    "xit" -> True
    "It" -> True
    "xIt" -> True
    "context" -> True
    "xcontext" -> True
    "Context" -> True
    "xContext" -> True
    "should" -> True
    "xshould" -> True
    "given" -> True
    "xgiven" -> True
    "Given" -> True
    "xGiven" -> True
    "when" -> True
    "xwhen" -> True
    "When" -> True
    "xWhen" -> True
    "then" -> True
    "xthen" -> True
    "Then" -> True
    "xThen" -> True
    "and" -> True
    "xand" -> True
    "And" -> True
    "xAnd" -> True
    "feature" -> True
    "xfeature" -> True
    "Feature" -> True
    "xFeature" -> True
    "scenario" -> True
    "xscenario" -> True
    "Scenario" -> True
    "xScenario" -> True
    "expect" -> True
    "xexpect" -> True
    "Expect" -> True
    "xExpect" -> True
    _ -> False

matchKotestStringBlock :: [Token] -> Maybe ([Token], [Token])
matchKotestStringBlock (b : rest)
    | isTokenBracket b && tToken b == "{" = Just ([b], rest)
matchKotestStringBlock (op : b : rest)
    | tToken op == "-" && isTokenBracket b && tToken b == "{" = Just ([op, b], rest)
    | isTokenIdentifier op
    , tToken op `elem` (["should", "xshould", "config"] :: [Text])
    , isTokenBracket b && tToken b == "{" =
        Just ([op, b], rest)
matchKotestStringBlock (tDot : tCfg : tOpen : rest)
    | isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tCfg && tToken tCfg == "config"
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (cfgArgs, afterCfg) = processInsideBrackets "(" ")" 1 rest
         in case matchKotestStringBlock afterCfg of
                Just (moreHdr, afterBrace) ->
                    Just (tDot : tCfg : tOpen : cfgArgs ++ moreHdr, afterBrace)
                Nothing -> Nothing
matchKotestStringBlock _ = Nothing

processOutsideKotlin :: Bool -> Bool -> LineState -> [Token] -> [Token]
processOutsideKotlin _ _ !_ [] = []
-- Pattern 1: Annotated method, property, or class (@Test, @get:Rule, @BeforeEach, @Nested, etc.)
processOutsideKotlin keepTests !_ !ls0 tokens@(tFirst : _)
    | isTokenOperator tFirst && tToken tFirst == "@"
    , let (attrTokens, isTest, restAfterAttrs) = collectKotlinAnnotations tokens
    , not (null attrTokens) =
        let !lsAfterAttrs = advanceLineState ls0 attrTokens
         in if isTest
                then case consumeAnnotatedKotlinDecl lsAfterAttrs 120 restAfterAttrs of
                    Nothing ->
                        if keepTests
                            then processOutsideKotlin keepTests False lsAfterAttrs restAfterAttrs
                            else attrTokens ++ processOutsideKotlin keepTests False lsAfterAttrs restAfterAttrs
                    Just (declTokens, !lsFinal, finalRest) ->
                        if keepTests
                            then attrTokens ++ declTokens ++ processOutsideKotlin keepTests False lsFinal finalRest
                            else processOutsideKotlin keepTests False lsFinal finalRest
                else
                    if keepTests
                        then processOutsideKotlin keepTests False lsAfterAttrs restAfterAttrs
                        else attrTokens ++ processOutsideKotlin keepTests False lsAfterAttrs restAfterAttrs
-- Pattern 2: Kotest / Spek DSL blocks (`test("...") { ... }`, `Given("...") { ... }`, `when("...") { ... }`)
processOutsideKotlin keepTests False !ls0 (t1 : t2 : ts@(firstArg : _))
    | isTokenBracket t2 && tToken t2 == "("
    , isTokenString firstArg
    , isTokenIdentifier t1 || isTokenKeyword t1
    , isKotestBlockName (tToken t1) =
        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 ts
         in case findKotestOpeningBrace 25 afterArgs of
                Nothing ->
                    let !ls1 = nextLineState ls0 t1
                     in if keepTests
                            then processOutsideKotlin keepTests False ls1 (t2 : ts)
                            else t1 : processOutsideKotlin keepTests False ls1 (t2 : ts)
                Just (sigTokens, restAfterBrace) ->
                    let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                        allTestToks = t1 : t2 : argTokens ++ sigTokens ++ bodyTokens
                        !lsFinal = advanceLineState ls0 allTestToks
                     in if keepTests
                            then allTestToks ++ processOutsideKotlin keepTests False lsFinal finalRest
                            else processOutsideKotlin keepTests False lsFinal finalRest
-- Pattern 3: Kotest StringSpec / FreeSpec / WordSpec DSL blocks (`"..." { ... }`, `"...".config(...) { ... }`)
processOutsideKotlin keepTests False !ls0 (t1 : ts)
    | isTokenString t1 =
        case matchKotestStringBlock ts of
            Just (hdrTokens, restAfterBrace) ->
                let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                    allTestToks = t1 : hdrTokens ++ bodyTokens
                    !lsFinal = advanceLineState ls0 allTestToks
                 in if keepTests
                        then allTestToks ++ processOutsideKotlin keepTests False lsFinal finalRest
                        else processOutsideKotlin keepTests False lsFinal finalRest
            Nothing ->
                let !ls1 = nextLineState ls0 t1
                 in if keepTests
                        then processOutsideKotlin keepTests False ls1 ts
                        else t1 : processOutsideKotlin keepTests False ls1 ts
processOutsideKotlin keepTests !_ !ls0 (t : ts) =
    let !ls1 = nextLineState ls0 t
        !isBlocker =
            (isTokenOperator t && (tToken t == "." || tToken t == "?." || tToken t == "::"))
                || (isTokenKeyword t && (tToken t == "fun" || tToken t == "val" || tToken t == "var"))
     in if keepTests
            then processOutsideKotlin keepTests isBlocker ls1 ts
            else t : processOutsideKotlin keepTests isBlocker ls1 ts
