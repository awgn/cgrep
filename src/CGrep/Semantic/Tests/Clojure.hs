{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Clojure () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Clojure where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideClojure keepTests tokens

-- | Strip optional Clojure namespace prefix (`t/deftest` -> `deftest`, `clojure.test/testing` -> `testing`).
unqualifyClojureSymbol :: Text -> Text
unqualifyClojureSymbol sym =
    case T.breakOnEnd "/" sym of
        ("", name) -> name
        (_, name) -> name

-- | Check if `( <head> <arg1> ... )` is a Clojure test form:
--   1. `(deftest name ...)`, `(deftest- name ...)`, `(defspec name ...)`, `(use-fixtures ...)`
--      (optionally namespace-qualified like `t/deftest` or `clojure.test/deftest`)
--   2. `(testing "string description" ...)` (must have a string literal as its first argument!)
isClojureTestForm :: Token -> [Token] -> Bool
isClojureTestForm headTok rest
    | not (isTokenIdentifier headTok || isTokenKeyword headTok) = False
    | baseName `elem` (["deftest", "deftest-", "defspec", "use-fixtures"] :: [Text]) =
        case rest of
            (arg1 : _) -> isTokenIdentifier arg1 || isTokenKeyword arg1
            [] -> False
    | baseName == "testing" =
        case rest of
            (arg1 : _) -> isTokenString arg1
            [] -> False
    | otherwise = False
  where
    baseName = unqualifyClojureSymbol (tToken headTok)

-- | (Clojure) Helper: Processes tokens *outside* a test block.
processOutsideClojure :: Bool -> [Token] -> [Token]
processOutsideClojure _ [] = []
-- Pattern 1: `(deftest ...)`, `(t/deftest ...)`, `(deftest- ...)`, `(defspec ...)`, `(testing "..." ...)`
processOutsideClojure keepTests (t1 : t2 : ts)
    | isTokenBracket t1 && tToken t1 == "("
    , isClojureTestForm t2 ts =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
         in if keepTests
                then t1 : t2 : bodyTokens ++ processOutsideClojure keepTests remainingTokens
                else processOutsideClojure keepTests remainingTokens
-- No test found, process the current token
processOutsideClojure keepTests (t : ts) =
    if keepTests
        then processOutsideClojure keepTests ts
        else t : processOutsideClojure keepTests ts
