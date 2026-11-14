{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Java () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, splitDirectories, takeExtension, takeFileName)

instance LanguageTestFilter 'Java where
    isTestFile = isJavaTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideJava keepTests tokens

-- | Standard Maven / Gradle Java test source tree (`src/test/...`).
isJavaTestFilePath :: OsPath -> Bool
isJavaTestFilePath path =
    takeExtension path == [osp|.java|]
        && hasSrcTestDirs (map takeFileName (dropFileNamePart (splitDirectories path)))
  where
    dropFileNamePart [] = []
    dropFileNamePart [_] = []
    dropFileNamePart (x : xs) = x : dropFileNamePart xs

    hasSrcTestDirs (d1 : d2 : rest)
        | d1 == [osp|src|] && d2 == [osp|test|] = True
        | otherwise = hasSrcTestDirs (d2 : rest)
    hasSrcTestDirs _ = False

-- | Parsed Java annotation (`@pkg.Anno<T>(args)`).
data JavaAnno = JavaAnno
    { jaTokens :: [Token]
    , jaSimpleName :: !Text
    , jaHasParens :: !Bool
    }

extractJavaAnno :: [Token] -> Maybe (JavaAnno, [Token])
extractJavaAnno (tAt : tIdent : ts)
    | isTokenOperator tAt
    , tToken tAt == "@"
    , isTokenIdentifier tIdent =
        let (dotPath, lastIdent, afterPath) = collectDotPath tIdent ts
            (typeArgToks, afterTypeArgs) = consumeOptionalAngleArgs afterPath
            (parenToks, hasParens, afterParens) = case afterTypeArgs of
                (tOpen : rest)
                    | isTokenBracket tOpen && tToken tOpen == "(" ->
                        let (inside, remToks) = processInsideBrackets "(" ")" 1 rest
                         in (tOpen : inside, True, remToks)
                _ -> ([], False, afterTypeArgs)
            allToks = tAt : tIdent : dotPath ++ typeArgToks ++ parenToks
         in Just (JavaAnno allToks lastIdent hasParens, afterParens)
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

    countChar :: Char -> Text -> Int
    countChar c = T.foldl' (\acc ch -> if ch == c then acc + 1 else acc) 0
extractJavaAnno _ = Nothing

-- | Precise Java test annotation classification (JUnit 4/5, TestNG, Mockito, Spring Test,
-- jqwik, Cucumber), avoiding false positives on `@BeforeMapping`, `@AfterReturning`, etc.
isJavaTestAnno :: JavaAnno -> Bool
isJavaTestAnno (JavaAnno _ n hasParens) =
    hasCamelTestWord n
        || n
            `elem` ([ "Before"
                    , "After"
                    , "BeforeEach"
                    , "AfterEach"
                    , "BeforeAll"
                    , "AfterAll"
                    , "BeforeClass"
                    , "AfterClass"
                    , "BeforeMethod"
                    , "AfterMethod"
                    , "BeforeSuite"
                    , "AfterSuite"
                    , "BeforeGroups"
                    , "AfterGroups"
                    , "Nested"
                    , "DataProvider"
                    , "RunWith"
                    , "ExtendWith"
                    , "Property"
                    , "Example"
                    ] :: [Text])
        || isJavaTestFieldAnnoName n
        || (hasParens && n `elem` (["Given", "When", "Then", "And", "But"] :: [Text]))
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

isJavaTestFieldAnnoName :: Text -> Bool
isJavaTestFieldAnnoName n =
    n
        `elem` ([ "Rule"
                , "ClassRule"
                , "TempDir"
                , "RegisterExtension"
                , "Mock"
                , "Spy"
                , "InjectMocks"
                , "Captor"
                , "MockBean"
                , "SpyBean"
                , "MockitoBean"
                , "MockitoSpyBean"
                ] :: [Text])

collectPreciseJavaAnnotations :: [Token] -> ([Token], Bool, Bool, [Token])
collectPreciseJavaAnnotations = go [] False False
  where
    go acc !anyTest !anyField remaining =
        case extractJavaAnno remaining of
            Just (ja, rest) ->
                let isT = isJavaTestAnno ja
                    isF = isJavaTestFieldAnnoName (jaSimpleName ja)
                 in go (acc ++ jaTokens ja) (anyTest || isT) (anyField || isF) rest
            Nothing -> (acc, anyTest, anyField, remaining)

-- | Consume the method, class, or field declaration annotated with a test annotation.
-- Supports braced methods/classes, semicolon-terminated fields (`@Mock`, `@TempDir`), and
-- field initializers containing anonymous inner classes (`@RegisterExtension ... = new X() { ... };`),
-- while NOT swallowing abstract interface methods (`@Test void testDecl();`).
consumeAnnotatedJavaDecl :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
consumeAnnotatedJavaDecl isFieldAnno = go 0 False False
  where
    go :: Int -> Bool -> Bool -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ _ _ 0 _ = Nothing
    go _ _ _ _ [] = Nothing
    go !depth !seenParen !seenEq !n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            let seenParen' = seenParen || (depth == 0 && tToken t == "(")
             in step (depth + 1) seenParen' seenEq (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) seenParen seenEq (n - 1) t ts
        | depth == 0 && isTokenOperator t && tToken t == "=" =
            step 0 seenParen True (n - 1) t ts
        | depth == 0 && tToken t == ";" =
            if isFieldAnno || seenEq || not seenParen
                then Just ([t], ts)
                else Nothing
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            let (braceBody, afterBrace) = processInsideBraces 1 ts
             in if seenEq
                    then case consumeFieldTailUntilSemicolon afterBrace of
                        Just (tailToks, afterSemi) -> Just (t : braceBody ++ tailToks, afterSemi)
                        Nothing -> Just (t : braceBody, afterBrace)
                    else Just (t : braceBody, afterBrace)
        | otherwise =
            step depth seenParen seenEq (n - 1) t ts

    step !d !sp !se !m tok rest =
        case go d sp se m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

    consumeFieldTailUntilSemicolon :: [Token] -> Maybe ([Token], [Token])
    consumeFieldTailUntilSemicolon = goTail 0
      where
        goTail :: Int -> [Token] -> Maybe ([Token], [Token])
        goTail _ [] = Nothing
        goTail !d (x : xs)
            | isTokenBracket x && tToken x == "{" =
                let (inner, afterInner) = processInsideBraces 1 xs
                 in case goTail d afterInner of
                        Just (more, remToks) -> Just (x : inner ++ more, remToks)
                        Nothing -> Nothing
            | isTokenBracket x && (tToken x == "(" || tToken x == "[") =
                fmap (\(more, remToks) -> (x : more, remToks)) (goTail (d + 1) xs)
            | isTokenBracket x && (tToken x == ")" || tToken x == "]") =
                fmap (\(more, remToks) -> (x : more, remToks)) (goTail (max 0 (d - 1)) xs)
            | d == 0 && tToken x == ";" = Just ([x], xs)
            | otherwise =
                fmap (\(more, remToks) -> (x : more, remToks)) (goTail d xs)

isJavaClassModifier :: Token -> Bool
isJavaClassModifier t =
    (isTokenKeyword t || isTokenIdentifier t)
        && tToken t
            `elem` ([ "public"
                    , "protected"
                    , "private"
                    , "abstract"
                    , "final"
                    , "static"
                    , "strictfp"
                    , "sealed"
                    ] :: [Text])

collectJavaClassModifiers :: [Token] -> ([Token], [Token])
collectJavaClassModifiers (t : ts)
    | isJavaClassModifier t =
        let (more, rest) = collectJavaClassModifiers ts
         in (t : more, rest)
collectJavaClassModifiers ts = ([], ts)

-- | Check if a Java class header before `{` extends `*TestCase`, supporting both
-- simple (`extends TestCase`) and package-qualified (`extends junit.framework.TestCase`)
-- superclasses as well as generic type parameters (`class Foo<T> extends BaseTestCase<T>`).
extendsJUnitTestCase :: [Token] -> Bool
extendsJUnitTestCase tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "{") && tToken t /= ";") tokens
     in scanForExtends headerTokens
  where
    scanForExtends [] = False
    scanForExtends (t : ts)
        | isTokenKeyword t && tToken t == "extends" =
            case extractSuperSimpleName ts of
                Just superName -> "TestCase" `T.isSuffixOf` superName
                Nothing -> False
        | otherwise = scanForExtends ts

    extractSuperSimpleName (tIdent : rest)
        | isTokenIdentifier tIdent = Just (followDots (tToken tIdent) rest)
    extractSuperSimpleName _ = Nothing

    followDots _ (d : i : rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            followDots (tToken i) rest
    followDots cur _ = cur

matchJUnit3TestCaseClass :: [Token] -> Maybe ([Token], [Token])
matchJUnit3TestCaseClass toks =
    let (modToks, afterMods) = collectJavaClassModifiers toks
     in case afterMods of
            (tClass : tName : ts)
                | isTokenKeyword tClass && tToken tClass == "class"
                , isTokenIdentifier tName
                , extendsJUnitTestCase ts ->
                    case findOpeningBraceC 100 ts of
                        Just (sigTokens, restAfterBrace) ->
                            let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                             in Just (modToks ++ [tClass, tName] ++ sigTokens ++ bodyTokens, finalRest)
                        Nothing -> Nothing
            _ -> Nothing

processOutsideJava :: Bool -> [Token] -> [Token]
processOutsideJava _ [] = []
processOutsideJava keepTests tokens
    | let (attrTokens, isTest, isFieldAnno, restAfterAttrs) = collectPreciseJavaAnnotations tokens
    , not (null attrTokens) =
        if isTest
            then case consumeAnnotatedJavaDecl isFieldAnno 120 restAfterAttrs of
                Nothing ->
                    if keepTests
                        then processOutsideJava keepTests restAfterAttrs
                        else attrTokens ++ processOutsideJava keepTests restAfterAttrs
                Just (declTokens, finalRest) ->
                    if keepTests
                        then attrTokens ++ declTokens ++ processOutsideJava keepTests finalRest
                        else processOutsideJava keepTests finalRest
            else
                if keepTests
                    then processOutsideJava keepTests restAfterAttrs
                    else attrTokens ++ processOutsideJava keepTests restAfterAttrs
processOutsideJava keepTests tokens
    | Just (classTokens, finalRest) <- matchJUnit3TestCaseClass tokens =
        if keepTests
            then classTokens ++ processOutsideJava keepTests finalRest
            else processOutsideJava keepTests finalRest
processOutsideJava keepTests (t : ts) =
    if keepTests
        then processOutsideJava keepTests ts
        else t : processOutsideJava keepTests ts
