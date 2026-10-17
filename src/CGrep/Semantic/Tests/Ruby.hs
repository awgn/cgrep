{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Ruby () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenNumber,
    isTokenOperator,
    isTokenString,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Ruby where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideRuby keepTests False tokens

-- | RSpec / Minitest / ActiveSupport::TestCase DSL block starters
isRubyTestBlockName :: Text -> Bool
isRubyTestBlockName name =
    name
        `elem` ([ "describe"
                , "fdescribe"
                , "xdescribe"
                , "context"
                , "fcontext"
                , "xcontext"
                , "it"
                , "fit"
                , "xit"
                , "specify"
                , "fspecify"
                , "xspecify"
                , "example"
                , "fexample"
                , "xexample"
                , "scenario"
                , "feature"
                , "before"
                , "after"
                , "around"
                , "let"
                , "let!"
                , "subject"
                , "test"
                ] :: [Text])

data RubyBlockKind
    = RubyDoBlock [Token] [Token] -- (header tokens up to and including `do`, tokens after `do`)
    | RubyBraceBlock [Token] [Token] -- (header tokens up to and including `{`, tokens after `{`)

-- | Look ahead up to `n` tokens for the opening `do` or `{` of an RSpec/Minitest block,
-- tracking `(...)` and `[...]` depth. Stops and returns `Nothing` if `=` (assignment like
-- `context = x + 1`), `;`, `def`, `class`, `module`, or `end` is encountered at depth 0.
findRubyBlockStart :: Int -> [Token] -> Maybe RubyBlockKind
findRubyBlockStart = go 0 []
  where
    go :: Int -> [Token] -> Int -> [Token] -> Maybe RubyBlockKind
    go _ _ 0 _ = Nothing
    go _ _ _ [] = Nothing
    go depth acc n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            go (depth + 1) (t : acc) (n - 1) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            go (max 0 (depth - 1)) (t : acc) (n - 1) ts
        | depth == 0 && (isTokenKeyword t || isTokenIdentifier t) && tToken t == "do" =
            Just (RubyDoBlock (reverse (t : acc)) ts)
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just (RubyBraceBlock (reverse (t : acc)) ts)
        | depth == 0
            && ( (isTokenOperator t && (tToken t == "=" || tToken t == ";"))
                    || ( (isTokenKeyword t || isTokenIdentifier t)
                            && (tToken t == "def" || tToken t == "class" || tToken t == "module" || tToken t == "end")
                       )
               ) =
            Nothing
        | otherwise =
            go depth (t : acc) (n - 1) ts

-- | Check whether a Ruby `class` header is a test class:
--   - class name starts with `"Test"` or ends with `"Test"` or `"TestCase"`, OR
--   - inherits (`<`) from a superclass containing `"Test"` (e.g. `< Minitest::Test`, `< ActiveSupport::TestCase`).
isRubyTestClass :: Token -> [Token] -> Bool
isRubyTestClass classNameTok afterClassName =
    let cname = tToken classNameTok
        nameMatch = "Test" `T.isPrefixOf` cname || "Test" `T.isSuffixOf` cname || "TestCase" `T.isSuffixOf` cname
        superMatch = case afterClassName of
            (tLt : rest)
                | isTokenOperator tLt && tToken tLt == "<" ->
                    let superTokens = takeWhile (\t -> isTokenIdentifier t || (isTokenOperator t && tToken t == ":")) rest
                     in any (\t -> isTokenIdentifier t && "Test" `T.isInfixOf` tToken t) superTokens
            _ -> False
     in nameMatch || superMatch

-- | (Ruby) Helper: Processes tokens *outside* a test block.
-- `prevDefOrDot` is `True` when the immediately preceding token was `def` or `.`
-- (e.g. `def self.describe` or `obj.describe`), EXCEPT when the preceding token was `RSpec.`.
processOutsideRuby :: Bool -> Bool -> [Token] -> [Token]
processOutsideRuby _ _ [] = []

-- Pattern 0: `RSpec.describe ...`
processOutsideRuby keepTests _ (tRspec : tDot : tDesc : ts)
    | isTokenIdentifier tRspec && tToken tRspec == "RSpec"
    , isTokenOperator tDot && tToken tDot == "."
    , isTokenIdentifier tDesc && isRubyTestBlockName (tToken tDesc)
    , Just blockKind <- findRubyBlockStart 15 ts =
        case blockKind of
            RubyDoBlock headerAndDo afterDo ->
                let (bodyAndEnd, remainingTokens) = collectUntilRubyEnd 1 Nothing afterDo
                 in if keepTests
                        then tRspec : tDot : tDesc : headerAndDo ++ bodyAndEnd ++ processOutsideRuby keepTests False remainingTokens
                        else processOutsideRuby keepTests False remainingTokens
            RubyBraceBlock headerAndBrace afterBrace ->
                let (bodyAndClose, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
                 in if keepTests
                        then tRspec : tDot : tDesc : headerAndBrace ++ bodyAndClose ++ processOutsideRuby keepTests False remainingTokens
                        else processOutsideRuby keepTests False remainingTokens

-- Pattern 1: RSpec / ActiveSupport::TestCase block (`describe`, `context`, `it`, `before`, `after`, `let`, `test`, etc.)
processOutsideRuby keepTests False (t1 : ts)
    | isTokenIdentifier t1 && isRubyTestBlockName (tToken t1)
    , Just blockKind <- findRubyBlockStart 15 ts =
        case blockKind of
            RubyDoBlock headerAndDo afterDo ->
                let (bodyAndEnd, remainingTokens) = collectUntilRubyEnd 1 Nothing afterDo
                 in if keepTests
                        then t1 : headerAndDo ++ bodyAndEnd ++ processOutsideRuby keepTests False remainingTokens
                        else processOutsideRuby keepTests False remainingTokens
            RubyBraceBlock headerAndBrace afterBrace ->
                let (bodyAndClose, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
                 in if keepTests
                        then t1 : headerAndBrace ++ bodyAndClose ++ processOutsideRuby keepTests False remainingTokens
                        else processOutsideRuby keepTests False remainingTokens

-- Pattern 2: `def test_*`, `def setup`, `def teardown` (Minitest)
processOutsideRuby keepTests _ (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "def"
    , isTokenIdentifier t2 && ("test_" `T.isPrefixOf` tToken t2 || tToken t2 == "setup" || tToken t2 == "teardown") =
        let (testTokens, remainingTokens) = collectUntilRubyEnd 1 (Just t2) ts
         in if keepTests
                then t1 : t2 : testTokens ++ processOutsideRuby keepTests False remainingTokens
                else processOutsideRuby keepTests False remainingTokens

-- Pattern 3: `class Test*`, `class *Test`, or `class Foo < Minitest::Test`
processOutsideRuby keepTests _ (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class"
    , isTokenIdentifier t2 && isRubyTestClass t2 ts =
        let (testTokens, remainingTokens) = collectUntilRubyEnd 1 (Just t2) ts
         in if keepTests
                then t1 : t2 : testTokens ++ processOutsideRuby keepTests False remainingTokens
                else processOutsideRuby keepTests False remainingTokens

-- No test found, process the current token
processOutsideRuby keepTests _ (t : ts) =
    let isBlocker =
            (isTokenOperator t && tToken t == ".")
                || ((isTokenKeyword t || isTokenIdentifier t) && tToken t == "def")
     in if keepTests
            then processOutsideRuby keepTests isBlocker ts
            else t : processOutsideRuby keepTests isBlocker ts

-- | Check whether the token immediately preceding `if`/`unless`/`while`/`until`
-- can be the end of an expression in a Ruby postfix modifier (e.g. `return 1 if n <= 0`
-- or `raise "err" unless valid?`).
-- If `prevTok` is an identifier, literal (number/string), `)`, `]`, `}`, or `return`/`break`/`next`,
-- then `if`/`unless`/`while`/`until` is a postfix modifier (or lacks its own `end`) unless
-- it starts a new statement! Wait: in Ruby, how to distinguish `x = 1\nif cond` from `return 1 if cond`?
-- Let's check `tOffset` or line/statement context!
canPrecedeModifier :: Token -> Bool
canPrecedeModifier prev =
    isTokenNumber prev
        || isTokenString prev
        || (isTokenKeyword prev && (tToken prev == "return" || tToken prev == "break" || tToken prev == "next" || tToken prev == "true" || tToken prev == "false" || tToken prev == "nil"))

-- | Helper: Collect tokens until the matching `end` keyword in Ruby.
collectUntilRubyEnd :: Int -> Maybe Token -> [Token] -> ([Token], [Token])
collectUntilRubyEnd _ _ [] = ([], [])
collectUntilRubyEnd depth mPrev (t : ts)
    -- Unconditional block starters (`do`, `def`, `class`, `module`, `case`, `begin`, `for`)
    | (isTokenKeyword t || isTokenIdentifier t)
        && ( tToken t == "do"
                || tToken t == "def"
                || tToken t == "class"
                || tToken t == "module"
                || tToken t == "for"
                || tToken t == "case"
                || tToken t == "begin"
           ) =
        let (collected, remaining) = collectUntilRubyEnd (depth + 1) (Just t) ts
         in (t : collected, remaining)
    -- Conditional/loop keywords (`if`, `unless`, `while`, `until`):
    -- Only increment depth if NOT used as a postfix modifier right after a literal/return!
    | (isTokenKeyword t || isTokenIdentifier t)
        && (tToken t == "if" || tToken t == "unless" || tToken t == "while" || tToken t == "until") =
        let isModifier = maybe False canPrecedeModifier mPrev
            nextDepth = if isModifier then depth else depth + 1
            (collected, remaining) = collectUntilRubyEnd nextDepth (Just t) ts
         in (t : collected, remaining)
    -- Found `end` - decrease depth or finish
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
        if depth <= 1
            then ([t], ts)
            else
                let (collected, remaining) = collectUntilRubyEnd (depth - 1) (Just t) ts
                 in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilRubyEnd depth (Just t) ts
         in (t : collected, remaining)
