{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Go () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, takeBaseName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Go where
    isTestFile = isGoTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideGo keepTests False tokens

-- | Checks if a Go file path is a test file (`*_test.go`).
isGoTestFilePath :: OsPath -> Bool
isGoTestFilePath path =
    let base = takeBaseName path
     in [osp|_test|] `OS.isSuffixOf` base

-- | Checks if a Go function name is a test, benchmark, example, or fuzz function:
-- must start with Test, Benchmark, Example, or Fuzz followed by either end-of-string
-- or a non-lowercase character (uppercase, digit, underscore, etc.).
isGoTestFuncName :: T.Text -> Bool
isGoTestFuncName name =
    any checkPrefix (["Test", "Benchmark", "Example", "Fuzz"] :: [T.Text])
  where
    checkPrefix p =
        case T.stripPrefix p name of
            Nothing -> False
            Just rest ->
                case T.uncons rest of
                    Nothing -> True
                    Just (c, _) -> not (isAsciiLower c)

-- | Consume optional Go 1.18+ generic type parameter list `[T any, ...]` before `(`.
consumeOptionalTypeParams :: [Token] -> ([Token], [Token])
consumeOptionalTypeParams (tOpen : rest)
    | isTokenBracket tOpen && tToken tOpen == "[" =
        let (inside, afterClose) = processInsideBrackets "[" "]" 1 rest
         in (tOpen : inside, afterClose)
consumeOptionalTypeParams ts = ([], ts)

-- | Find the opening `{` of a Go function body after its parameter list:
--   1. Skips `struct { ... }` and `interface { ... }` inline types in the return signature.
--   2. Skips anonymous function return types `func(...) ...` (distinguishing them from a new
--      top-level `func Name(...)` or `func (recv) Name(...)` declaration).
--   3. Tracks `(...)` and `[...]` nesting in return types.
--   4. Stops if `;` or a new top-level declaration keyword appears at depth 0.
findOpeningBraceGo :: Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceGo = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go 0 n (t1 : t2 : ts)
        | isTokenKeyword t1 && (tToken t1 == "struct" || tToken t1 == "interface")
        , isTokenBracket t2 && tToken t2 == "{" =
            let (typeBody, afterTypeBody) = processInsideBraces 1 ts
             in case go 0 (n - 1) afterTypeBody of
                    Nothing -> Nothing
                    Just (pre, post) -> Just (t1 : t2 : typeBody ++ pre, post)
        | isTokenKeyword t1 && tToken t1 == "func"
        , isTokenBracket t2 && tToken t2 == "(" =
            let (fnParams, afterFnParams) = processInsideBrackets "(" ")" 1 ts
             in if isMethodDeclAfterReceiver afterFnParams
                    then Nothing
                    else case go 0 (n - 1) afterFnParams of
                        Nothing -> Nothing
                        Just (pre, post) -> Just (t1 : t2 : fnParams ++ pre, post)
    go depth n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0
            && ( tToken t == ";"
                    || ( isTokenKeyword t
                            && (tToken t `elem` (["func", "type", "var", "const", "package", "import"] :: [Text]))
                       )
               ) =
            Nothing
        | otherwise =
            step depth (n - 1) t ts

    isMethodDeclAfterReceiver (ident : nextTok : _) =
        isTokenIdentifier ident
            && isTokenBracket nextTok
            && (tToken nextTok == "(" || tToken nextTok == "[")
    isMethodDeclAfterReceiver _ = False

    step :: Int -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d m tok rest =
        case go d m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | Check whether a receiver method `func (recv) Test...(...) ...` is a real Testify/Go suite
-- test method rather than a domain method like `func (p *Pool) TestConnection(host string) error`.
isValidSuiteTestMethod :: [Token] -> [Token] -> [Token] -> Bool
isValidSuiteTestMethod receiverTokens paramTokens sigTokens =
    hasSuiteLikeReceiver || (isZeroOrTestingParams && hasNoReturnSignature)
  where
    hasSuiteLikeReceiver =
        any
            ( \t ->
                isTokenIdentifier t
                    && ( "Suite" `T.isInfixOf` tToken t
                            || "Test" `T.isInfixOf` tToken t
                            || "Spec" `T.isSuffixOf` tToken t
                       )
            )
            receiverTokens

    paramInner = initOrEmpty paramTokens
    isZeroOrTestingParams =
        null paramInner
            || any (\t -> isTokenIdentifier t && tToken t == "testing") paramInner

    hasNoReturnSignature = length sigTokens <= 1

    initOrEmpty [] = []
    initOrEmpty [_] = []
    initOrEmpty (x : xs) = x : initOrEmpty xs

-- | Match top-level `func TestXxx[T ...](...) { ... }`.
matchGoTestFunc :: [Token] -> Maybe ([Token], [Token])
matchGoTestFunc (tFunc : tName : rest0)
    | isTokenKeyword tFunc && tToken tFunc == "func"
    , isTokenIdentifier tName && isGoTestFuncName (tToken tName) =
        let (typeParams, afterTypeParams) = consumeOptionalTypeParams rest0
         in case afterTypeParams of
                (tOpen : afterOpen)
                    | isTokenBracket tOpen && tToken tOpen == "(" ->
                        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 afterOpen
                         in case findOpeningBraceGo 50 afterParams of
                                Nothing -> Nothing
                                Just (sigTokens, tokensAfterBrace) ->
                                    let (bodyTokens, remaining) = processInsideBraces 1 tokensAfterBrace
                                     in Just
                                            ( tFunc : tName : typeParams ++ tOpen : paramTokens ++ sigTokens ++ bodyTokens
                                            , remaining
                                            )
                _ -> Nothing
matchGoTestFunc _ = Nothing

-- | Match suite receiver method `func (s *MySuite) TestXxx[T ...](...) { ... }`.
matchGoSuiteMethod :: [Token] -> Maybe ([Token], [Token])
matchGoSuiteMethod (tFunc : tRecvOpen : afterRecvOpen)
    | isTokenKeyword tFunc && tToken tFunc == "func"
    , isTokenBracket tRecvOpen && tToken tRecvOpen == "(" =
        let (receiverTokens, afterReceiver) = processInsideBrackets "(" ")" 1 afterRecvOpen
         in case afterReceiver of
                (tMethod : rest0)
                    | isTokenIdentifier tMethod && isGoTestFuncName (tToken tMethod) ->
                        let (typeParams, afterTypeParams) = consumeOptionalTypeParams rest0
                         in case afterTypeParams of
                                (tOpen : afterOpen)
                                    | isTokenBracket tOpen && tToken tOpen == "(" ->
                                        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 afterOpen
                                         in case findOpeningBraceGo 50 afterParams of
                                                Just (sigTokens, tokensAfterBrace)
                                                    | isValidSuiteTestMethod receiverTokens paramTokens sigTokens ->
                                                        let (bodyTokens, remaining) = processInsideBraces 1 tokensAfterBrace
                                                         in Just
                                                                ( tFunc
                                                                    : tRecvOpen
                                                                    : receiverTokens
                                                                        ++ tMethod
                                                                        : typeParams
                                                                        ++ tOpen
                                                                        : paramTokens
                                                                        ++ sigTokens
                                                                        ++ bodyTokens
                                                                , remaining
                                                                )
                                                _ -> Nothing
                                _ -> Nothing
                _ -> Nothing
matchGoSuiteMethod _ = Nothing

-- | Match Ginkgo v1/v2 (`Describe`, `Context`, `When`, `It`, `Specify`, `BeforeEach`, `PIt`, ...)
-- and GoConvey (`Convey`, `SkipConvey`, `FocusConvey`) BDD blocks when not preceded by `.`.
matchGoBddCall :: [Token] -> Maybe ([Token], [Token])
matchGoBddCall (tName : tOpen : afterOpen)
    | isTokenIdentifier tName
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let name = tToken tName
            (insideParens, remaining) = processInsideBrackets "(" ")" 1 afterOpen
            hasFuncLit = any (\t -> isTokenKeyword t && tToken t == "func") insideParens
            hasStr = any isTokenString insideParens
         in if
                | name `elem` ginkgoPendingContainers && hasStr ->
                    Just (tName : tOpen : insideParens, remaining)
                | name `elem` ginkgoActiveContainers && hasStr && hasFuncLit ->
                    Just (tName : tOpen : insideParens, remaining)
                | name `elem` ginkgoLifecycleHooks && hasFuncLit ->
                    Just (tName : tOpen : insideParens, remaining)
                | otherwise -> Nothing
  where
    ginkgoActiveContainers :: [Text]
    ginkgoActiveContainers =
        [ "Describe"
        , "FDescribe"
        , "Context"
        , "FContext"
        , "When"
        , "FWhen"
        , "It"
        , "FIt"
        , "Specify"
        , "FSpecify"
        , "DescribeTable"
        , "FDescribeTable"
        , "Convey"
        , "FocusConvey"
        ]

    ginkgoPendingContainers :: [Text]
    ginkgoPendingContainers =
        [ "PDescribe"
        , "XDescribe"
        , "PContext"
        , "XContext"
        , "PWhen"
        , "XWhen"
        , "PIt"
        , "XIt"
        , "PSpecify"
        , "XSpecify"
        , "PDescribeTable"
        , "XDescribeTable"
        , "SkipConvey"
        ]

    ginkgoLifecycleHooks :: [Text]
    ginkgoLifecycleHooks =
        [ "BeforeEach"
        , "AfterEach"
        , "JustBeforeEach"
        , "JustAfterEach"
        , "BeforeSuite"
        , "AfterSuite"
        , "SynchronizedBeforeSuite"
        , "SynchronizedAfterSuite"
        , "DeferCleanup"
        , "ReportAfterEach"
        , "ReportAfterSuite"
        ]
matchGoBddCall _ = Nothing

-- | (Go) Helper: Processes tokens *outside* a test function.
processOutsideGo :: Bool -> Bool -> [Token] -> [Token]
processOutsideGo _ _ [] = []
processOutsideGo keepTests prevDot tokens@(t : ts) =
    let matched =
            matchGoTestFunc tokens
                <|> matchGoSuiteMethod tokens
                <|> (if prevDot then Nothing else matchGoBddCall tokens)
     in case matched of
            Just (testTokens, remaining) ->
                if keepTests
                    then testTokens ++ processOutsideGo keepTests False remaining
                    else processOutsideGo keepTests False remaining
            Nothing ->
                let isDot = isTokenOperator t && tToken t == "."
                 in if keepTests
                        then processOutsideGo keepTests isDot ts
                        else t : processOutsideGo keepTests isDot ts
