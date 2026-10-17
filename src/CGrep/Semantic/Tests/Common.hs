{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module CGrep.Semantic.Tests.Common (
    LanguageTestFilter (..),
    runLangFilter,
    inTestSuiteDir,
    findOpeningBracketBounded,
    findOpeningBraceBounded,
    findOpeningBraceC,
    findOpeningBrace,
    processInsideBrackets,
    processInsideBraces,
    extractJavaAnnotation,
    isJavaTestAnnotation,
    collectJavaAnnotations,
) where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenOperator,
    tToken,
 )
import qualified Data.Text as T
import System.OsPath (OsPath, osp, splitDirectories, takeFileName)

class LanguageTestFilter (lang :: FileType) where
    isTestFile :: OsPath -> Bool
    isTestFile _ = False

    langFilter :: Maybe Bool -> [Token] -> [Token]

inTestSuiteDir :: OsPath -> Bool
inTestSuiteDir path =
    let parts = splitDirectories path
        dirs  = case reverse parts of
                    (_:ds) -> ds
                    []     -> []
    in any (\d -> takeFileName d == [osp|test_suite|]) dirs

runLangFilter :: forall (lang :: FileType). LanguageTestFilter lang => OsPath -> Maybe Bool -> [Token] -> [Token]
runLangFilter _ Nothing tokens = tokens
runLangFilter path flag@(Just keepTests) tokens
    | not (inTestSuiteDir path) && isTestFile @lang path =
        if keepTests then tokens else []
    | otherwise =
        langFilter @lang flag tokens

-- | Searches for an opening token (e.g., "{") within a lookahead limit.
findOpeningBracketBounded :: T.Text -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBracketBounded _ 0 _  = Nothing
findOpeningBracketBounded _ _ [] = Nothing
findOpeningBracketBounded openBracket n (t:ts)
    | isTokenBracket t && tToken t == openBracket = Just ([t], ts)
    | otherwise =
        case findOpeningBracketBounded openBracket (n - 1) ts of
            Nothing -> Nothing
            Just (pre, post) -> Just (t : pre, post)

-- | Specific helper for curly braces '{'
findOpeningBraceBounded :: Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceBounded = findOpeningBracketBounded "{"

-- | Finds the opening `{` of a declaration within `n` tokens, stopping at a
-- top-level `;` (forward declaration / abstract method), while ignoring braces
-- and semicolons nested inside `(...)` or `[...]` (e.g. parameter annotations
-- like `@MyAnno({1, 2})`).
findOpeningBraceC :: Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceC = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _  = Nothing
    go _ _ [] = Nothing
    go depth n (t:ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0 && tToken t == ";" =
            Nothing
        | otherwise =
            step depth (n - 1) t ts

    step :: Int -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d m tok rest =
        case go d m rest of
            Nothing          -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | Helper to find the first opening brace, returning tokens before/at brace, and tokens after.
-- Returns (tokens_including_brace, tokens_after_brace)
findOpeningBrace :: [Token] -> Maybe ([Token], [Token])
findOpeningBrace = go []
  where
    go :: [Token] -> [Token] -> Maybe ([Token], [Token])
    go _ [] = Nothing -- Reached end without finding a brace
    go acc (t:ts)
        | isTokenBracket t && tToken t == "{" = Just (reverse (t:acc), ts)
        | otherwise = go (t:acc) ts

-- | Processes tokens by balancing arbitrary brackets
processInsideBrackets :: T.Text -> T.Text -> Int -> [Token] -> ([Token], [Token])
processInsideBrackets _ _ 0 ts = ([], ts)
processInsideBrackets _ _ _ [] = ([], [])
processInsideBrackets open close nestingLevel (t:ts)
    | isTokenBracket t && tToken t == open =
        let (nestedInside, remaining) = processInsideBrackets open close (nestingLevel + 1) ts
        in (t : nestedInside, remaining)
    | isTokenBracket t && tToken t == close =
        if nestingLevel == 1
        then ([t], ts)
        else
             let (nestedInside, remaining) = processInsideBrackets open close (nestingLevel - 1) ts
             in (t : nestedInside, remaining)
    | otherwise =
        let (nestedInside, remaining) = processInsideBrackets open close nestingLevel ts
        in (t : nestedInside, remaining)

-- | (Generic) Helper: Processes tokens *inside* a braced block, handling nesting.
processInsideBraces :: Int -> [Token] -> ([Token], [Token])
processInsideBraces 0 ts = ([], ts) -- Base case: found matching '}'
processInsideBraces _ [] = ([], []) -- Error case: unexpected EOF
processInsideBraces nestingLevel (t:ts)
    | isTokenBracket t && tToken t == "{" =
        let (nestedInside, remaining) = processInsideBraces (nestingLevel + 1) ts
        in (t : nestedInside, remaining)
    | isTokenBracket t && tToken t == "}" =
        if nestingLevel == 1
        then ([t], ts)
        else
             let (nestedInside, remaining) = processInsideBraces (nestingLevel - 1) ts
             in (t : nestedInside, remaining)
    | otherwise =
        let (nestedInside, remaining) = processInsideBraces nestingLevel ts
        in (t : nestedInside, remaining)

-- | Shared Java/Kotlin annotation extractor (`@Foo.Bar(...)`)
extractJavaAnnotation :: [Token] -> Maybe ([Token], [Token])
extractJavaAnnotation (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" && isTokenIdentifier t2 =
        let (pathTokens, rest) = collectDotPath ts
        in case rest of
            (t3:ts') | isTokenBracket t3 && tToken t3 == "(" ->
                let (insideTokens, remaining) = processInsideBrackets "(" ")" 1 ts'
                in Just (t1 : t2 : pathTokens ++ t3 : insideTokens, remaining)
            _ -> Just (t1 : t2 : pathTokens, rest)
  where
    collectDotPath (d:i:rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (more, final) = collectDotPath rest
            in (d : i : more, final)
    collectDotPath rest = ([], rest)
extractJavaAnnotation _ = Nothing

-- | Checks only the annotation name (excluding arguments inside `(...)`) to avoid
-- false positives when production annotations reference identifiers containing "Test".
isJavaTestAnnotation :: [Token] -> Bool
isJavaTestAnnotation tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "(")) tokens
        identTokens  = filter isTokenIdentifier headerTokens
    in any (isJavaTestAttrName . tToken) identTokens
  where
    isJavaTestAttrName n =
        "Test" `T.isInfixOf` n
            || "Before" `T.isPrefixOf` n
            || "After" `T.isPrefixOf` n
            || n == "Nested"
            || n == "DataProvider"

collectJavaAnnotations :: [Token] -> ([Token], Bool, [Token])
collectJavaAnnotations = go [] False
  where
    go acc isTest remaining =
        case extractJavaAnnotation remaining of
            Just (attrTokens, rest) ->
                let testAttr = isJavaTestAnnotation attrTokens
                in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)
