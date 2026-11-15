{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Scala () where

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
import System.OsPath (OsPath, osp, splitDirectories, takeExtension, takeFileName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Scala where
    isTestFile = isScalaTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideScala keepTests False initialLineState tokens

-- | Standard SBT / Mill / Maven / Gradle / Scala.js / Scala Native test source directories
-- (`src/test/...`, `src/it/...`, `src/jvmTest/...`, `src/jsTest/...`, `src/nativeTest/...`).
isScalaTestFilePath :: OsPath -> Bool
isScalaTestFilePath path =
    let ext = takeExtension path
        dirs = map takeFileName (dropLastPart (splitDirectories path))
     in (ext == [osp|.scala|] || ext == [osp|.sc|])
            && hasScalaTestSourceSet dirs
  where
    dropLastPart [] = []
    dropLastPart [_] = []
    dropLastPart (x : xs) = x : dropLastPart xs

    hasScalaTestSourceSet (d1 : d2 : rest)
        | d1 == [osp|src|]
            && (d2 == [osp|test|] || d2 == [osp|it|] || [osp|Test|] `OS.isSuffixOf` d2) =
            True
        | otherwise = hasScalaTestSourceSet (d2 : rest)
    hasScalaTestSourceSet _ = False

-- | Precise JUnit / MUnit / TestNG annotation names in Scala.
-- Rejects MapStruct `@BeforeMapping`, Spring `@AfterReturning`, and generic `@Validator[Test]`.
isScalaTestAnnotation :: Text -> Bool
isScalaTestAnnotation name =
    name
        `elem` ( [ "Test"
                 , "ParameterizedTest"
                 , "RepeatedTest"
                 , "TestFactory"
                 , "TestTemplate"
                 , "Before"
                 , "After"
                 , "BeforeEach"
                 , "AfterEach"
                 , "BeforeAll"
                 , "AfterAll"
                 , "BeforeClass"
                 , "AfterClass"
                 ] ::
                    [Text]
               )

-- | Extract a single Scala annotation `@pkg.Name[T](args...)`, returning
-- `(rawTokens, simpleAnnotationName, remainingTokens)`.
extractScalaAnnotation :: [Token] -> Maybe ([Token], Text, [Token])
extractScalaAnnotation (tAt : tFirst : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tFirst =
        let (pathToks, lastIdent, afterPath) = collectDotIdents (tToken tFirst) ts
            (typeArgToks, afterTypeArgs) = consumeTypeArgs afterPath
            (argToks, afterArgs) = consumeParenLists afterTypeArgs
         in Just (tAt : tFirst : pathToks ++ typeArgToks ++ argToks, lastIdent, afterArgs)
  where
    collectDotIdents _ (tDot : tId : rest)
        | isTokenOperator tDot && tToken tDot == "." && isTokenIdentifier tId =
            let (more, finLast, finRest) = collectDotIdents (tToken tId) rest
             in (tDot : tId : more, finLast, finRest)
    collectDotIdents curLast rest = ([], curLast, rest)

    consumeTypeArgs (tOpen : rest)
        | isTokenBracket tOpen && tToken tOpen == "[" =
            let (inside, remToks) = processInsideBrackets "[" "]" 1 rest
             in (tOpen : inside, remToks)
    consumeTypeArgs rest = ([], rest)

    consumeParenLists (tOpen : rest)
        | isTokenBracket tOpen && tToken tOpen == "(" =
            let (inside, remToks) = processInsideBrackets "(" ")" 1 rest
                (more, finRest) = consumeParenLists remToks
             in (tOpen : inside ++ more, finRest)
    consumeParenLists rest = ([], rest)
extractScalaAnnotation _ = Nothing

collectScalaAnnotations :: [Token] -> ([Token], Bool, [Token])
collectScalaAnnotations = go [] False
  where
    go acc isTest remaining =
        case extractScalaAnnotation remaining of
            Just (toks, simpleName, rest) ->
                go (acc ++ toks) (isTest || isScalaTestAnnotation simpleName) rest
            Nothing -> (acc, isTest, remaining)

-- | Consume a Scala expression body after `=` (for braceless `@Test def foo() = ...`
-- and ScalaCheck `property("...") = ...`), stopping at a top-level `;` or when a
-- new line starts at `col <= baseIndent` at bracket depth 0.
consumeScalaExprBody :: Int -> LineState -> [Token] -> ([Token], LineState, [Token])
consumeScalaExprBody _ ls0 (tBrace : ts0)
    | isTokenBracket tBrace && tToken tBrace == "{" =
        let (bodyToks, rest) = processInsideBrackets "{" "}" 1 ts0
            lsFinal = advanceLineState ls0 (tBrace : bodyToks)
         in (tBrace : bodyToks, lsFinal, rest)
consumeScalaExprBody baseIndent ls0 ts0 = go 0 [] ls0 ts0
  where
    go :: Int -> [Token] -> LineState -> [Token] -> ([Token], LineState, [Token])
    go _ acc ls [] = (reverse acc, ls, [])
    go depth acc ls (t : ts) =
        let (isFirst, col, ls') = stepLineState ls t
         in if depth == 0 && tToken t == ";"
                then (reverse (t : acc), ls', ts)
                else
                    if depth == 0 && not (null acc) && isFirst && col <= baseIndent
                        then (reverse acc, ls, t : ts)
                        else
                            let !depth'
                                    | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") = depth + 1
                                    | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") = max 0 (depth - 1)
                                    | otherwise = depth
                             in go depth' (t : acc) ls' ts

-- | Consume an annotated Scala method/class/val target: supports both braced `{ ... }`
-- and braceless expression-bodied `= expr` definitions.
consumeScalaAnnotatedTarget :: Int -> LineState -> Int -> [Token] -> Maybe ([Token], LineState, [Token])
consumeScalaAnnotatedTarget baseIndent = go 0 True []
  where
    go :: Int -> Bool -> [Token] -> LineState -> Int -> [Token] -> Maybe ([Token], LineState, [Token])
    go _ _ _ _ 0 _ = Nothing
    go _ _ _ _ _ [] = Nothing
    go depth canSeeDecl acc ls n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            let (_, _, ls') = stepLineState ls t
             in go (depth + 1) canSeeDecl (t : acc) ls' (n - 1) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            let (_, _, ls') = stepLineState ls t
             in go (max 0 (depth - 1)) canSeeDecl (t : acc) ls' (n - 1) ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            let (bodyToks, rest) = processInsideBraces 1 ts
                allToks = reverse (t : acc) ++ bodyToks
                lsFinal = advanceLineState ls (t : bodyToks)
             in Just (allToks, lsFinal, rest)
        | depth == 0 && isTokenOperator t && tToken t == "=" =
            let (_, _, lsAfterEq) = stepLineState ls t
                (exprToks, lsFinal, rest) = consumeScalaExprBody baseIndent lsAfterEq ts
             in Just (reverse (t : acc) ++ exprToks, lsFinal, rest)
        | depth == 0 && tToken t == ";" =
            Nothing
        | depth == 0 && isScalaDeclKeyword t =
            if canSeeDecl
                then
                    let (_, _, ls') = stepLineState ls t
                     in go depth False (t : acc) ls' (n - 1) ts
                else Nothing
        | otherwise =
            let (_, _, ls') = stepLineState ls t
             in go depth canSeeDecl (t : acc) ls' (n - 1) ts

isScalaDeclKeyword :: Token -> Bool
isScalaDeclKeyword t =
    isTokenKeyword t
        && ( tToken t
                `elem` (["def", "val", "var", "class", "object", "trait"] :: [Text])
           )

isScalaModifier :: Token -> Bool
isScalaModifier t =
    (isTokenKeyword t || isTokenIdentifier t)
        && ( tToken t
                `elem` ( [ "final"
                         , "abstract"
                         , "sealed"
                         , "case"
                         , "implicit"
                         , "lazy"
                         , "private"
                         , "protected"
                         , "override"
                         , "open"
                         ] ::
                            [Text]
                       )
           )

isScalaSuiteBaseName :: Text -> Bool
isScalaSuiteBaseName name =
    ("Suite" `T.isSuffixOf` name)
        || ("Spec" `T.isSuffixOf` name)
        || ("SpecLike" `T.isSuffixOf` name)
        || ("SpecDefault" `T.isSuffixOf` name)
        || name
            `elem` ( [ "Specification"
                     , "SpecificationWithJUnit"
                     , "Properties"
                     , "TestCase"
                     ] ::
                        [Text]
                   )

-- | Match a Scala test suite `class` / `object` / `trait` that `extends` a known
-- test suite base type (`AnyFunSuite`, `FunSuite`, `AnyFlatSpec`, `ZIOSpecDefault`,
-- `Specification`, `Properties`, etc.), capturing leading modifiers and the full
-- suite body (including helper `val`s and `def`s inside the suite!).
matchScalaSuiteDecl :: Int -> LineState -> [Token] -> Maybe ([Token], LineState, [Token])
matchScalaSuiteDecl baseIndent ls0 tokens0 =
    let (modToks, afterMods) = collectMods tokens0
     in case afterMods of
            (tKw : tName : rest)
                | isTokenKeyword tKw && (tToken tKw == "class" || tToken tKw == "object" || tToken tKw == "trait")
                , isTokenIdentifier tName ->
                    case scanSuiteHeader rest of
                        Just (hdrToks, isTestSuite, SuiteBrace afterBrace)
                            | isTestSuite ->
                                let (bodyToks, remaining) = processInsideBraces 1 afterBrace
                                    full = modToks ++ [tKw, tName] ++ hdrToks ++ bodyToks
                                    lsFinal = advanceLineState ls0 full
                                 in Just (full, lsFinal, remaining)
                        Just (hdrToks, isTestSuite, SuiteColon afterColon)
                            | isTestSuite ->
                                let lsAfterHdr = advanceLineState ls0 (modToks ++ [tKw, tName] ++ hdrToks)
                                    (bodyToks, lsFinal, remaining) = consumeIndentedSuiteBody baseIndent lsAfterHdr afterColon
                                 in if null bodyToks
                                        then Nothing
                                        else Just (modToks ++ [tKw, tName] ++ hdrToks ++ bodyToks, lsFinal, remaining)
                        _ -> Nothing
            _ -> Nothing
  where
    collectMods (m : tOpen : rest)
        | isScalaModifier m && isTokenBracket tOpen && tToken tOpen == "[" =
            let (scopeToks, afterScope) = processInsideBrackets "[" "]" 1 rest
                (more, fin) = collectMods afterScope
             in (m : tOpen : scopeToks ++ more, fin)
    collectMods (m : rest)
        | isScalaModifier m =
            let (more, fin) = collectMods rest
             in (m : more, fin)
    collectMods rest = ([], rest)

    consumeIndentedSuiteBody minIndent ls ts = go (0 :: Int) [] ls ts
      where
        go _ acc curLs [] = (reverse acc, curLs, [])
        go depth acc curLs (t : rest) =
            let (isFirst, col, nextLs) = stepLineState curLs t
             in if depth == 0 && isFirst && col <= minIndent
                    then (reverse acc, curLs, t : rest)
                    else
                        let !depth'
                                | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") = depth + 1
                                | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") = max 0 (depth - 1)
                                | otherwise = depth
                         in go depth' (t : acc) nextLs rest

data SuiteBodyStart
    = SuiteBrace [Token]
    | SuiteColon [Token]

scanSuiteHeader :: [Token] -> Maybe ([Token], Bool, SuiteBodyStart)
scanSuiteHeader = go (0 :: Int) False False [] Nothing
  where
    -- `seenExtends`: True once `extends` was seen at depth 0
    -- `expectParent`: True when reading the dotted type path of an `extends` / `with` / `,` parent
    -- `curParent`: last identifier seen in the current parent type path
    go _ _ _ _ _ [] = Nothing
    go depth seenExtends isSuite acc curParent (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            let !isSuite' = checkParent isSuite curParent
             in go (depth + 1) seenExtends isSuite' (t : acc) Nothing ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            go (max 0 (depth - 1)) seenExtends isSuite (t : acc) Nothing ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            let !isSuite' = checkParent isSuite curParent
             in Just (reverse (t : acc), isSuite', SuiteBrace ts)
        | depth == 0 && seenExtends && isTokenOperator t && tToken t == ":" =
            let !isSuite' = checkParent isSuite curParent
             in Just (reverse (t : acc), isSuite', SuiteColon ts)
        | depth == 0 && (tToken t == ";" || isScalaDeclKeyword t) =
            Nothing
        | depth == 0 && (isTokenKeyword t || isTokenIdentifier t) && tToken t == "extends" =
            go 0 True isSuite (t : acc) Nothing ts
        | depth == 0 && seenExtends && ((isTokenKeyword t && tToken t == "with") || tToken t == ",") =
            let !isSuite' = checkParent isSuite curParent
             in go 0 True isSuite' (t : acc) Nothing ts
        | depth == 0 && seenExtends && isTokenIdentifier t =
            go 0 True isSuite (t : acc) (Just (tToken t)) ts
        | depth == 0 && seenExtends && isTokenOperator t && tToken t == "." =
            go 0 True isSuite (t : acc) curParent ts
        | otherwise =
            go depth seenExtends (checkParent isSuite curParent) (t : acc) Nothing ts

    checkParent True _ = True
    checkParent False Nothing = False
    checkParent False (Just p) = isScalaSuiteBaseName p

-- | ScalaTest / MUnit / Specs2 / ZIO Test / ScalaCheck block names
isScalaTestBlockName :: Text -> Bool
isScalaTestBlockName name =
    name
        `elem` ( [ "test"
                 , "ignore"
                 , "it"
                 , "they"
                 , "describe"
                 , "scenario"
                 , "feature"
                 , "Scenario"
                 , "Feature"
                 , "property"
                 , "suite"
                 , "suiteAll"
                 , "testM"
                 ] ::
                    [Text]
               )

-- | FlatSpec / WordSpec / FreeSpec / ShouldMatchers infix verbs
isScalaSpecVerb :: Text -> Bool
isScalaSpecVerb name =
    name
        `elem` ( [ "should"
                 , "must"
                 , "can"
                 , "in"
                 , "ignore"
                 , "of"
                 , "when"
                 , "which"
                 , "that"
                 ] ::
                    [Text]
               )

-- | Match FlatSpec / WordSpec / FreeSpec header tokens after a string literal or `it`/`they`
-- followed by a spec verb (e.g. `"A Calc" should "add" in { ... }` or `"test" - { ... }`),
-- stopping if `;`, `def`, `val`, `var`, `class`, `object`, or `trait` appears at top level
-- before `{`.
findFlatSpecBrace :: Int -> [Token] -> Maybe ([Token], [Token])
findFlatSpecBrace = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go depth n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0 && (tToken t == ";" || isScalaDeclKeyword t) =
            Nothing
        | otherwise =
            step depth (n - 1) t ts

    step :: Int -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d m tok rest =
        case go d m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | After `test(...)` / `it(...)` / `describe(...)`, consume any chained `.tag(...)` or
-- curried `(tag1, tag2)` argument lists, and require `{` for standard blocks OR allow a
-- second `(...)` body list for ZIO Test `suite("...")(...)` / `test("...")(...)`.
matchAfterScalaTestParens :: Bool -> [Token] -> Maybe ([Token], [Token])
matchAfterScalaTestParens _ (tBrace : ts)
    | isTokenBracket tBrace && tToken tBrace == "{" =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 ts
            (trailMods, finalRemaining) = consumeTrailingMethodCalls remainingTokens
         in Just (tBrace : bodyTokens ++ trailMods, finalRemaining)
matchAfterScalaTestParens allowParenBody (tOpen : ts)
    | isTokenBracket tOpen && tToken tOpen == "(" =
        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 ts
         in case matchAfterScalaTestParens allowParenBody afterArgs of
                Just (moreTokens, remaining) ->
                    Just (tOpen : argTokens ++ moreTokens, remaining)
                Nothing
                    | allowParenBody ->
                        let (trailMods, finalRemaining) = consumeTrailingMethodCalls afterArgs
                         in Just (tOpen : argTokens ++ trailMods, finalRemaining)
                    | otherwise -> Nothing
matchAfterScalaTestParens allowParenBody (tDot : tMod : tOpen : ts)
    | isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tMod
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 ts
         in case matchAfterScalaTestParens allowParenBody afterArgs of
                Just (moreTokens, remaining) ->
                    Just (tDot : tMod : tOpen : argTokens ++ moreTokens, remaining)
                Nothing -> Nothing
matchAfterScalaTestParens _ _ = Nothing

consumeTrailingMethodCalls :: [Token] -> ([Token], [Token])
consumeTrailingMethodCalls (tDot : tMod : tOpen : ts)
    | isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tMod
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (argToks, afterArgs) = processInsideBrackets "(" ")" 1 ts
            (more, fin) = consumeTrailingMethodCalls afterArgs
         in (tDot : tMod : tOpen : argToks ++ more, fin)
consumeTrailingMethodCalls rest = ([], rest)

-- | (Scala) Helper: Processes tokens *outside* a test block.
-- `prevBlock` is `True` when the immediately preceding token was `.` (method call
-- like `calc.test(...)`) or `def`/`val`/`var` (declaration like `def test(...)`).
processOutsideScala :: Bool -> Bool -> LineState -> [Token] -> [Token]
processOutsideScala _ _ _ [] = []

-- Pattern 1: Annotated method or class (@Test, @BeforeEach, etc.)
processOutsideScala keepTests _ ls tokens@(tFirst : _)
    | let (attrTokens, isTest, restAfterAttrs) = collectScalaAnnotations tokens
    , not (null attrTokens) =
        let (_, baseIndent, _) = stepLineState ls tFirst
            lsAfterAttrs = advanceLineState ls attrTokens
         in if isTest
                then case consumeScalaAnnotatedTarget baseIndent lsAfterAttrs 150 restAfterAttrs of
                    Nothing ->
                        case matchScalaSuiteDecl baseIndent lsAfterAttrs restAfterAttrs of
                            Just (suiteToks, lsFinal, remaining) ->
                                if keepTests
                                    then attrTokens ++ suiteToks ++ processOutsideScala keepTests False lsFinal remaining
                                    else processOutsideScala keepTests False lsFinal remaining
                            Nothing ->
                                if keepTests
                                    then processOutsideScala keepTests False lsAfterAttrs restAfterAttrs
                                    else attrTokens ++ processOutsideScala keepTests False lsAfterAttrs restAfterAttrs
                    Just (declAndBody, lsFinal, remainingTokens) ->
                        if keepTests
                            then attrTokens ++ declAndBody ++ processOutsideScala keepTests False lsFinal remainingTokens
                            else processOutsideScala keepTests False lsFinal remainingTokens
                else case matchScalaSuiteDecl baseIndent lsAfterAttrs restAfterAttrs of
                    Just (suiteToks, lsFinal, remaining) ->
                        if keepTests
                            then attrTokens ++ suiteToks ++ processOutsideScala keepTests False lsFinal remaining
                            else processOutsideScala keepTests False lsFinal remaining
                    Nothing ->
                        if keepTests
                            then processOutsideScala keepTests False lsAfterAttrs restAfterAttrs
                            else attrTokens ++ processOutsideScala keepTests False lsAfterAttrs restAfterAttrs

-- Pattern 2: Suite `class` / `object` / `trait` extending a known Scala test suite base
processOutsideScala keepTests _ ls tokens@(tFirst : _)
    | let (_, baseIndent, _) = stepLineState ls tFirst
    , Just (suiteToks, lsFinal, remainingTokens) <- matchScalaSuiteDecl baseIndent ls tokens =
        if keepTests
            then suiteToks ++ processOutsideScala keepTests False lsFinal remainingTokens
            else processOutsideScala keepTests False lsFinal remainingTokens

-- Pattern 3: uTest `Tests { ... }` block
processOutsideScala keepTests False ls (t1 : t2 : ts)
    | isTokenIdentifier t1 && tToken t1 == "Tests"
    , isTokenBracket t2 && tToken t2 == "{" =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 ts
            allToks = t1 : t2 : bodyTokens
            lsFinal = advanceLineState ls allToks
         in if keepTests
                then allToks ++ processOutsideScala keepTests False lsFinal remainingTokens
                else processOutsideScala keepTests False lsFinal remainingTokens

-- Pattern 4: FunSuite / FunSpec / FeatureSpec / MUnit / ZIO Test / ScalaCheck:
--   * `test("...") { ... }`, `describe("...") { ... }`, `it("...") { ... }`
--   * ZIO Test `suite("...")(...)` / `test("...")(...)`
--   * ScalaCheck `property("...") = ...`
processOutsideScala keepTests False ls (t1 : t2 : ts)
    | isTokenIdentifier t1 && isScalaTestBlockName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , let (_, callIndent, _) = stepLineState ls t1
    , let (argTokens, afterParens) = processInsideBrackets "(" ")" 1 ts =
        let startsWithStr = case argTokens of
                (a0 : _) -> isTokenString a0
                [] -> False
            allowZioParenBody =
                startsWithStr
                    && tToken t1 `elem` (["suite", "suiteAll", "test", "testM"] :: [Text])
         in case afterParens of
                (tEq : tsAfterEq)
                    | tToken t1 == "property"
                    , isTokenOperator tEq && tToken tEq == "=" ->
                        let lsAfterEq = advanceLineState ls (t1 : t2 : argTokens ++ [tEq])
                            (exprToks, lsFinal, remainingTokens) = consumeScalaExprBody callIndent lsAfterEq tsAfterEq
                            allToks = t1 : t2 : argTokens ++ (tEq : exprToks)
                         in if keepTests
                                then allToks ++ processOutsideScala keepTests False lsFinal remainingTokens
                                else processOutsideScala keepTests False lsFinal remainingTokens
                _
                    | Just (braceAndBody, remainingTokens) <- matchAfterScalaTestParens allowZioParenBody afterParens ->
                        let allToks = t1 : t2 : argTokens ++ braceAndBody
                            lsFinal = advanceLineState ls allToks
                         in if keepTests
                                then allToks ++ processOutsideScala keepTests False lsFinal remainingTokens
                                else processOutsideScala keepTests False lsFinal remainingTokens
                    | otherwise ->
                        let (_, _, ls') = stepLineState ls t1
                         in if keepTests
                                then processOutsideScala keepTests False ls' (t2 : ts)
                                else t1 : processOutsideScala keepTests False ls' (t2 : ts)

-- Pattern 5: FlatSpec / WordSpec / FreeSpec starting with a String literal or `it`/`they`
-- e.g. `"A Calculator" should "add numbers" in { ... }`
--      `it should "handle zero" in { ... }`
--      `"A FreeSpec" - { ... }`
processOutsideScala keepTests False ls (t1 : t2 : ts)
    | (isTokenString t1 || (isTokenIdentifier t1 && (tToken t1 == "it" || tToken t1 == "they" || tToken t1 == "ignore")))
    , (isTokenIdentifier t2 && isScalaSpecVerb (tToken t2)) || (isTokenOperator t2 && tToken t2 == "-")
    , Just (headerAndBrace, afterBrace) <- findFlatSpecBrace 15 ts =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
            allToks = t1 : t2 : headerAndBrace ++ bodyTokens
            lsFinal = advanceLineState ls allToks
         in if keepTests
                then allToks ++ processOutsideScala keepTests False lsFinal remainingTokens
                else processOutsideScala keepTests False lsFinal remainingTokens

-- Fallback: advance one token and update `prevBlock`
processOutsideScala keepTests _ ls (t : ts) =
    let (_, _, ls') = stepLineState ls t
        isBlocker =
            (isTokenOperator t && tToken t == ".")
                || (isTokenKeyword t && (tToken t == "def" || tToken t == "val" || tToken t == "var"))
     in if keepTests
            then processOutsideScala keepTests isBlocker ls' ts
            else t : processOutsideScala keepTests isBlocker ls' ts
