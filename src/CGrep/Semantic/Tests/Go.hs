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
            Nothing   -> False
            Just rest ->
                case T.uncons rest of
                    Nothing     -> True
                    Just (c, _) -> not (isAsciiLower c)

-- | (Go) Helper: Processes tokens *outside* a test function.
processOutsideGo :: Bool -> [Token] -> [Token]
processOutsideGo _ [] = [] -- End of stream

-- Pattern 1: func Test...
processOutsideGo keepTests (t1:t2:t3:ts)
    | isTokenKeyword t1 && tToken t1 == "func" &&
      isTokenIdentifier t2 && isGoTestFuncName (tToken t2) &&
      isTokenBracket t3 && tToken t3 == "("
    =
        let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 ts
        in case findOpeningBraceBounded 50 afterParams of
            Nothing ->
                if keepTests then processOutsideGo keepTests (t2:t3:ts) else t1 : processOutsideGo keepTests (t2:t3:ts)
            Just (sigTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : paramTokens ++ sigTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
                   else processOutsideGo keepTests remainingTokens

-- Pattern 2: func (receiver) Test...
processOutsideGo keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "func" &&
      isTokenBracket t2 && tToken t2 == "("
    =
        let (receiverTokens, afterReceiver) = processInsideBrackets "(" ")" 1 ts
        in case afterReceiver of
            (t3:t4:ts') | isTokenIdentifier t3 &&
                          isGoTestFuncName (tToken t3) &&
                          isTokenBracket t4 && tToken t4 == "(" ->
                let (paramTokens, afterParams) = processInsideBrackets "(" ")" 1 ts'
                in case findOpeningBraceBounded 50 afterParams of
                    Nothing ->
                        if keepTests then processOutsideGo keepTests (t2:ts) else t1 : processOutsideGo keepTests (t2:ts)
                    Just (sigTokens, tokensAfterBrace) ->
                        let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                        in if keepTests
                           then t1 : t2 : receiverTokens ++ t3 : t4 : paramTokens ++ sigTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
                           else processOutsideGo keepTests remainingTokens
            _ ->
                if keepTests then processOutsideGo keepTests (t2:ts) else t1 : processOutsideGo keepTests (t2:ts)

-- No test function found, process the current token
processOutsideGo keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideGo keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideGo keepTests ts

-- ------------------------------------------------------------------
