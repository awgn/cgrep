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
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, takeBaseName)
import qualified System.OsString as OS

instance LanguageTestFilter 'Go where
    isTestFile = isGoTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideGo keepTests tokens

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

-- | Find the opening `{` of a Go function body after its parameter list:
--   1. Skips `struct { ... }` and `interface { ... }` inline types in the return signature.
--   2. Tracks `(...)` and `[...]` nesting in return types.
--   3. Stops if `;` or a new top-level declaration keyword (`func`, `type`, `var`, `const`, `package`, `import`)
--      appears at depth 0 (preventing bodyless `func TestFoo() bool` declarations from swallowing the next function).
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

    step :: Int -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d m tok rest =
        case go d m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | (Go) Helper: Processes tokens *outside* a test function.
processOutsideGo :: Bool -> [Token] -> [Token]
processOutsideGo _ [] = []
-- Pattern 1: func Test...
processOutsideGo keepTests (t1 : t2 : t3 : ts)
    | isTokenKeyword t1 && tToken t1 == "func"
    , isTokenIdentifier t2 && isGoTestFuncName (tToken t2)
    , isTokenBracket t3 && tToken t3 == "(" =
        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 ts
         in case findOpeningBraceGo 50 afterParams of
                Nothing ->
                    if keepTests then processOutsideGo keepTests (t2 : t3 : ts) else t1 : processOutsideGo keepTests (t2 : t3 : ts)
                Just (sigTokens, tokensAfterBrace) ->
                    let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                     in if keepTests
                            then t1 : t2 : t3 : paramTokens ++ sigTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
                            else processOutsideGo keepTests remainingTokens
-- Pattern 2: func (receiver) Test...
processOutsideGo keepTests (t1 : t2 : ts)
    | isTokenKeyword t1 && tToken t1 == "func"
    , isTokenBracket t2 && tToken t2 == "(" =
        let (receiverTokens, afterReceiver) = processInsideBrackets "(" ")" 1 ts
         in case afterReceiver of
                (t3 : t4 : ts')
                    | isTokenIdentifier t3
                    , isGoTestFuncName (tToken t3)
                    , isTokenBracket t4 && tToken t4 == "(" ->
                        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 ts'
                         in case findOpeningBraceGo 50 afterParams of
                                Nothing ->
                                    if keepTests then processOutsideGo keepTests (t2 : ts) else t1 : processOutsideGo keepTests (t2 : ts)
                                Just (sigTokens, tokensAfterBrace) ->
                                    let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                                     in if keepTests
                                            then t1 : t2 : receiverTokens ++ t3 : t4 : paramTokens ++ sigTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
                                            else processOutsideGo keepTests remainingTokens
                _ ->
                    if keepTests then processOutsideGo keepTests (t2 : ts) else t1 : processOutsideGo keepTests (t2 : ts)
-- No test function found, process the current token
processOutsideGo keepTests (t : ts) =
    if keepTests
        then processOutsideGo keepTests ts
        else t : processOutsideGo keepTests ts
