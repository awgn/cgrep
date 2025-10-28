{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module CGrep.Semantic.Tests (
    filterTests,
) where
import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (Token, isTokenOperator, tToken, isTokenBracket, isTokenIdentifier, isTokenKeyword)
import qualified Data.Text as T

class LanguageTestFilter (lang :: FileType) where
    langFilter :: Maybe Bool -> [Token] -> [Token]

-- | Rust-specific instance using the helper functions.
instance LanguageTestFilter 'Rust where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideRust keepTests tokens

instance LanguageTestFilter 'Go where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideGo keepTests tokens

instance LanguageTestFilter 'Java where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideJava keepTests tokens

instance LanguageTestFilter 'Kotlin where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideJava keepTests tokens

instance LanguageTestFilter 'C where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideC keepTests tokens

instance LanguageTestFilter 'Cpp where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideC keepTests tokens

instance LanguageTestFilter 'Python where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsidePython keepTests tokens

instance LanguageTestFilter 'Zig where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideZig keepTests tokens

instance LanguageTestFilter 'Javascript where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideJavascript keepTests tokens

instance LanguageTestFilter 'Scala where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideScala keepTests tokens

instance LanguageTestFilter 'Haskell where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideHaskell keepTests tokens

instance LanguageTestFilter 'Csharp where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideCsharp keepTests tokens

instance LanguageTestFilter 'Fsharp where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideFsharp keepTests tokens

instance LanguageTestFilter 'Dart where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideDart keepTests tokens

instance LanguageTestFilter 'Elixir where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideElixir keepTests tokens

instance LanguageTestFilter 'Ruby where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideRuby keepTests tokens

instance LanguageTestFilter 'PHP where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsidePHP keepTests tokens

instance LanguageTestFilter 'Swift where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens

instance LanguageTestFilter 'ObjectiveC where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens -- Reuse Swift implementation

instance LanguageTestFilter 'R where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideR keepTests tokens

instance LanguageTestFilter 'Julia where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideJulia keepTests tokens

instance LanguageTestFilter 'Perl where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsidePerl keepTests tokens

instance LanguageTestFilter 'OCaml where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideOCaml keepTests tokens

instance LanguageTestFilter 'Erlang where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideErlang keepTests tokens

instance LanguageTestFilter 'Nim where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideNim keepTests tokens

instance LanguageTestFilter 'Clojure where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideClojure keepTests tokens

instance LanguageTestFilter 'D where
        langFilter Nothing tokens = tokens
        langFilter (Just keepTests) tokens = processOutsideD keepTests tokens

-- ------------------------------------------------------------------
-- Main Dispatcher Function (Runtime-to-Type bridge)
-- ------------------------------------------------------------------

filterTests :: FileType -> Maybe Bool -> [Token] -> [Token]
filterTests fileType flag tokens =
    case fileType of
        Rust       -> langFilter @'Rust flag tokens
        Go         -> langFilter @'Go flag tokens
        Java       -> langFilter @'Java flag tokens
        Kotlin     -> langFilter @'Kotlin flag tokens
        C          -> langFilter @'C flag tokens
        Cpp        -> langFilter @'Cpp flag tokens
        Python     -> langFilter @'Python flag tokens
        Zig        -> langFilter @'Zig flag tokens
        Javascript -> langFilter @'Javascript flag tokens
        Scala      -> langFilter @'Scala flag tokens
        Haskell    -> langFilter @'Haskell flag tokens
        Csharp     -> langFilter @'Csharp flag tokens
        Fsharp     -> langFilter @'Fsharp flag tokens
        Dart       -> langFilter @'Dart flag tokens
        Elixir     -> langFilter @'Elixir flag tokens
        Ruby       -> langFilter @'Ruby flag tokens
        PHP        -> langFilter @'PHP flag tokens
        Swift      -> langFilter @'Swift flag tokens
        ObjectiveC -> langFilter @'ObjectiveC flag tokens
        R          -> langFilter @'R flag tokens
        Julia      -> langFilter @'Julia flag tokens
        Perl       -> langFilter @'Perl flag tokens
        OCaml      -> langFilter @'OCaml flag tokens
        Erlang     -> langFilter @'Erlang flag tokens
        Nim        -> langFilter @'Nim flag tokens
        Clojure    -> langFilter @'Clojure flag tokens
        D          -> langFilter @'D flag tokens
        _          -> tokens

-- ------------------------------------------------------------------
-- Rust-Specific Implementation Helpers (Moved from old function)
-- ------------------------------------------------------------------

-- | (Rust) Helper: Processes tokens *outside* a test block.
processOutsideRust :: Bool -> [Token] -> [Token]
processOutsideRust _ [] = [] -- End of stream
processOutsideRust keepTests (t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:ts)
    -- Look for the exact sequence: #[cfg(test)] mod <name> {
    | (isTokenOperator t1 && tToken t1 == "#") &&
      (isTokenBracket t2 && tToken t2 == "[") &&
      (isTokenIdentifier t3 && tToken t3 == "cfg") &&
      (isTokenBracket t4 && tToken t4 == "(") &&
      (isTokenIdentifier t5 && tToken t5 == "test") &&
      (isTokenBracket t6 && tToken t6 == ")") &&
      (isTokenBracket t7 && tToken t7 == "]") &&
      (isTokenKeyword t8 && tToken t8 == "mod") &&
      isTokenIdentifier t9 && -- Module name (any identifier)
      (isTokenBracket t10 && tToken t10 == "{")
    =
        -- Found the start of a test block.
        -- Find the matching closing brace.
        let (insideTokens, remainingTokens) = processInsideBraces 1 ts
        in if keepTests
           then -- We want test tokens, so keep the *entire* block
                t1:t2:t3:t4:t5:t6:t7:t8:t9:t10:insideTokens ++ processOutsideRust keepTests remainingTokens
           else -- We don't want test tokens, so discard the entire block
                processOutsideRust keepTests remainingTokens

-- No test block marker found, process the current token
processOutsideRust keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideRust keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideRust keepTests ts

-- ------------------------------------------------------------------
-- Go-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Go) Helper: Processes tokens *outside* a test function.
processOutsideGo :: Bool -> [Token] -> [Token]
processOutsideGo _ [] = [] -- End of stream
processOutsideGo keepTests (t1:t2:ts)
    -- Look for "func Test..."
    | isTokenKeyword t1 && tToken t1 == "func" &&
      isTokenIdentifier t2 && "Test" `T.isPrefixOf` (tToken t2)
    =
        -- Found the start of a test function. Now find its opening brace.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideGo keepTests (t2:ts) else t1 : processOutsideGo keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                -- We found the function body's opening brace.
                -- signatureTokens *includes* the opening brace.
                -- tokensAfterBrace starts *after* the opening brace.
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then -- Keep func + name + signature + body
                        t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
                   else -- Discard the whole function
                        processOutsideGo keepTests remainingTokens

-- No test function found, process the current token
processOutsideGo keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideGo keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideGo keepTests ts

-- ------------------------------------------------------------------
-- Java-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Java) Helper: Processes tokens *outside* a test method.
processOutsideJava :: Bool -> [Token] -> [Token]
processOutsideJava _ [] = [] -- End of stream
processOutsideJava keepTests (t1:t2:ts)
    -- Look for "@Test"
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && tToken t2 == "Test"
    =
        -- Found @Test annotation. Now find its opening brace.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideJava keepTests (t2:ts) else t1 : processOutsideJava keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                -- We found the method body's opening brace.
                -- signatureTokens *includes* the opening brace.
                -- tokensAfterBrace starts *after* the opening brace.
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then -- Keep @Test + signature + body
                        t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideJava keepTests remainingTokens
                   else -- Discard the whole method
                        processOutsideJava keepTests remainingTokens

-- No test method found, process the current token
processOutsideJava keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideJava keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideJava keepTests ts

-- ------------------------------------------------------------------
-- C/C++-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (C/C++) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. Functions starting with "test_" (snake_case, common in C)
--   2. Functions starting with "Test" (PascalCase, common in C++)
--   3. TEST(...) macro (Google Test)
--   4. TEST_F(...) macro (Google Test with fixture)
--   5. TEST_CASE(...) macro (Catch2)
processOutsideC :: Bool -> [Token] -> [Token]
processOutsideC _ [] = [] -- End of stream

-- Pattern 1: TEST( macro (Google Test)
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && tToken t1 == "TEST" &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found TEST( macro. Find the opening brace of the test body.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- Pattern 2: TEST_F( macro (Google Test with fixture)
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && tToken t1 == "TEST_F" &&
      isTokenBracket t2 && tToken t2 == "("
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- Pattern 3: TEST_CASE( macro (Catch2)
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && tToken t1 == "TEST_CASE" &&
      isTokenBracket t2 && tToken t2 == "("
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- Pattern 4: Function starting with "test_" or "Test"
processOutsideC keepTests (t1:ts)
    | isTokenIdentifier t1 &&
      (("test_" `T.isPrefixOf` tToken t1) || ("Test" `T.isPrefixOf` tToken t1))
    =
        -- Found a test function. Find the opening brace.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideC keepTests ts else t1 : processOutsideC keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- No test found, process the current token
processOutsideC keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideC keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideC keepTests ts

-- ------------------------------------------------------------------
-- Python-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Python) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. Functions starting with "test_" (pytest, unittest convention)
--   2. Classes starting with "Test" (pytest convention)
--   3. Decorators @pytest, @unittest, @pytest.mark.* (common test decorators)
--
processOutsidePython :: Bool -> [Token] -> [Token]
processOutsidePython _ [] = [] -- End of stream

-- Pattern 1: @pytest or @unittest decorator
processOutsidePython keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && (tToken t2 == "pytest" || tToken t2 == "unittest")
    =
        -- Found a test decorator. Collect decorator line and the function/class.
        let (decoratorTokens, afterDecorator) = collectDecoratorAndFunction ts
        in if keepTests
           then t1 : t2 : decoratorTokens ++ processOutsidePython keepTests afterDecorator
           else processOutsidePython keepTests afterDecorator

-- Pattern 2: @pytest.mark.* decorator
processOutsidePython keepTests (t1:t2:t3:t4:t5:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && tToken t2 == "pytest" &&
      isTokenOperator t3 && tToken t3 == "." &&
      isTokenIdentifier t4 && tToken t4 == "mark" &&
      isTokenOperator t5 && tToken t5 == "."
    =
        -- Found @pytest.mark.* decorator
        let (decoratorTokens, afterDecorator) = collectDecoratorAndFunction ts
        in if keepTests
           then t1 : t2 : t3 : t4 : t5 : decoratorTokens ++ processOutsidePython keepTests afterDecorator
           else processOutsidePython keepTests afterDecorator

-- Pattern 3: def test_*
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "def" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2
    =
        -- Found a test function. Collect tokens until next def/class.
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 4: class Test*
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 && "Test" `T.isPrefixOf` tToken t2
    =
        -- Found a test class. Collect tokens until next class/def at same level.
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- No test found, process the current token
processOutsidePython keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsidePython keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsidePython keepTests ts

-- | Helper: Collect decorator tokens and the following function/class definition.
-- This handles decorators like @pytest.fixture, @unittest.skip, etc.
collectDecoratorAndFunction :: [Token] -> ([Token], [Token])
collectDecoratorAndFunction ts =
    -- First, collect tokens until we hit 'def' or 'class'
    let (beforeDef, fromDef) = collectUntilDefOrClass ts
    in case fromDef of
        [] -> (beforeDef, []) -- No def/class found
        _ ->
            -- Now collect the actual function/class body
            let (bodyTokens, remaining) = collectUntilNextDefOrClass (drop 2 fromDef) -- skip 'def'/'class' and name
            in (beforeDef ++ take 2 fromDef ++ bodyTokens, remaining)

-- | Helper: Collect tokens until we find 'def' or 'class' keyword.
collectUntilDefOrClass :: [Token] -> ([Token], [Token])
collectUntilDefOrClass [] = ([], [])
collectUntilDefOrClass (t:ts)
    | isTokenKeyword t && (tToken t == "def" || tToken t == "class") =
        ([], t:ts) -- Found definition, stop here
    | otherwise =
        let (collected, remaining) = collectUntilDefOrClass ts
        in (t : collected, remaining)

-- | Helper: Collect tokens until we find 'def' or 'class' keyword.
-- This is a simplified heuristic for Python's indentation-based blocks.
collectUntilNextDefOrClass :: [Token] -> ([Token], [Token])
collectUntilNextDefOrClass [] = ([], [])
collectUntilNextDefOrClass (t:ts)
    | isTokenKeyword t && (tToken t == "def" || tToken t == "class") =
        ([], t:ts) -- Found next definition, stop here
    | otherwise =
        let (collected, remaining) = collectUntilNextDefOrClass ts
        in (t : collected, remaining)

-- ------------------------------------------------------------------
-- Zig-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Zig) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test "description" { ... } - Zig's built-in test syntax
-- 
-- Zig has a very clean built-in test syntax that's easy to recognize.
processOutsideZig :: Bool -> [Token] -> [Token]
processOutsideZig _ [] = [] -- End of stream

-- Pattern: test "..." {
processOutsideZig keepTests (t1:ts)
    | isTokenKeyword t1 && tToken t1 == "test"
    =
        -- Found a test block. Find the opening brace.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideZig keepTests ts else t1 : processOutsideZig keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes the test name/description and the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then -- Keep test keyword + description + body
                        t1 : signatureTokens ++ bodyTokens ++ processOutsideZig keepTests remainingTokens
                   else -- Discard the whole test block
                        processOutsideZig keepTests remainingTokens

-- No test found, process the current token
processOutsideZig keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideZig keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideZig keepTests ts

-- ------------------------------------------------------------------
-- Javascript-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Javascript) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. describe('...', function() { ... }) - Mocha/Jasmine/Jest
--   2. describe('...', () => { ... }) - Modern syntax
--   3. it('...', function() { ... }) - Test cases
--   4. it('...', () => { ... }) - Modern syntax
--   5. test('...', function() { ... }) - Jest
--   6. test('...', () => { ... }) - Jest modern syntax
--   7. context('...', ...) - Mocha/Jasmine
-- 
-- All these frameworks use similar patterns with function calls.
processOutsideJavascript :: Bool -> [Token] -> [Token]
processOutsideJavascript _ [] = [] -- End of stream

-- Pattern 1: describe( or it( or test( or context(
processOutsideJavascript keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "describe" || tToken t1 == "it" || tToken t1 == "test" || tToken t1 == "context") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test function call. Find the opening brace of the test body.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideJavascript keepTests (t2:ts) else t1 : processOutsideJavascript keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes everything up to and including the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then -- Keep test function call + signature + body
                        t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideJavascript keepTests remainingTokens
                   else -- Discard the whole test block
                        processOutsideJavascript keepTests remainingTokens

-- No test found, process the current token
processOutsideJavascript keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideJavascript keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideJavascript keepTests ts

-- ------------------------------------------------------------------
-- Scala-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Scala) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test("...") { ... } - ScalaTest FunSuite, MUnit
--   2. it("...") { ... } - ScalaTest FunSpec
--   3. describe("...") { ... } - ScalaTest FunSpec
--   4. scenario("...") { ... } - ScalaTest FeatureSpec
--   5. feature("...") { ... } - ScalaTest FeatureSpec
-- 
-- ScalaTest and MUnit are the most popular testing frameworks for Scala.
processOutsideScala :: Bool -> [Token] -> [Token]
processOutsideScala _ [] = [] -- End of stream

-- Pattern: test( or it( or describe( or scenario( or feature(
processOutsideScala keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "test" || tToken t1 == "it" || tToken t1 == "describe" || 
       tToken t1 == "scenario" || tToken t1 == "feature") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test function call. Find the opening brace of the test body.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideScala keepTests (t2:ts) else t1 : processOutsideScala keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes everything up to and including the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then -- Keep test function call + signature + body
                        t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideScala keepTests remainingTokens
                   else -- Discard the whole test block
                        processOutsideScala keepTests remainingTokens

-- No test found, process the current token
processOutsideScala keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideScala keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideScala keepTests ts

-- ------------------------------------------------------------------
-- Haskell-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Haskell) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. describe "..." - HSpec
--   2. it "..." - HSpec
--   3. context "..." - HSpec (alias for describe)
--   4. testCase "..." - Tasty/HUnit
--   5. testGroup "..." - Tasty
--   6. testProperty "..." - Tasty/QuickCheck
--   7. prop_* functions - QuickCheck convention
-- 
-- Haskell tests often use do-notation or $ operator, so we look for
-- these patterns and collect tokens until the next top-level definition.
processOutsideHaskell :: Bool -> [Token] -> [Token]
processOutsideHaskell _ [] = [] -- End of stream

-- Pattern 1: describe/it/context/testCase/testGroup/testProperty followed by string
processOutsideHaskell keepTests (t1:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "describe" || tToken t1 == "it" || tToken t1 == "context" ||
       tToken t1 == "testCase" || tToken t1 == "testGroup" || tToken t1 == "testProperty")
    =
        -- Found a test block. Try to find opening brace, otherwise collect until next definition.
        case findOpeningBrace ts of
            Just (signatureTokens, tokensAfterBrace) ->
                -- Found braces, use them
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideHaskell keepTests remainingTokens
                   else processOutsideHaskell keepTests remainingTokens
            Nothing ->
                -- No braces, collect until next top-level definition
                let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
                in if keepTests
                   then t1 : testTokens ++ processOutsideHaskell keepTests remainingTokens
                   else processOutsideHaskell keepTests remainingTokens

-- Pattern 2: prop_* function (QuickCheck convention)
processOutsideHaskell keepTests (t1:ts)
    | isTokenIdentifier t1 && "prop_" `T.isPrefixOf` tToken t1
    =
        -- Found a property test function. Collect until next definition.
        let (propTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : propTokens ++ processOutsideHaskell keepTests remainingTokens
           else processOutsideHaskell keepTests remainingTokens

-- No test found, process the current token
processOutsideHaskell keepTests (t:ts) =
    if keepTests
    then -- We want test tokens, so discard this "outside" token
         processOutsideHaskell keepTests ts
    else -- We don't want test tokens, so keep this "outside" token
         t : processOutsideHaskell keepTests ts

-- | Helper: Collect tokens until we find the next top-level Haskell definition.
-- This looks for common patterns that indicate a new definition:
-- - describe, it, context, testCase, testGroup, testProperty (test keywords)
-- - main (main function)
-- - Keywords that start definitions at module level
collectUntilNextHaskellDef :: [Token] -> ([Token], [Token])
collectUntilNextHaskellDef [] = ([], [])
collectUntilNextHaskellDef (t:ts)
    -- Found another test keyword or common top-level definition
    | isTokenIdentifier t && 
      (tToken t == "describe" || tToken t == "it" || tToken t == "context" ||
       tToken t == "testCase" || tToken t == "testGroup" || tToken t == "testProperty" ||
       tToken t == "main" || "prop_" `T.isPrefixOf` tToken t) =
        ([], t:ts) -- Found next definition, stop here
    -- Found a type signature (identifier followed by ::)
    | isTokenIdentifier t =
        case ts of
            (t2:_) | isTokenOperator t2 && tToken t2 == "::" ->
                ([], t:ts) -- Likely a new function definition
            _ ->
                let (collected, remaining) = collectUntilNextHaskellDef ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilNextHaskellDef ts
        in (t : collected, remaining)

-- ------------------------------------------------------------------
-- C#-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (C#) Helper: Processes tokens *outside* a test method.
-- Recognizes:
--   1. [Test] - NUnit
--   2. [TestFixture] - NUnit
--   3. [Fact] - xUnit
--   4. [Theory] - xUnit
--   5. [TestMethod] - MSTest
--   6. [TestClass] - MSTest
-- 
-- C# uses attributes (annotations) similar to Java.
processOutsideCsharp :: Bool -> [Token] -> [Token]
processOutsideCsharp _ [] = [] -- End of stream

-- Pattern: [Test] or [Fact] or [Theory] or [TestMethod] etc.
processOutsideCsharp keepTests (t1:t2:t3:ts)
    | isTokenBracket t1 && tToken t1 == "[" &&
      isTokenIdentifier t2 && 
      (tToken t2 == "Test" || tToken t2 == "TestFixture" || tToken t2 == "Fact" || 
       tToken t2 == "Theory" || tToken t2 == "TestMethod" || tToken t2 == "TestClass") &&
      isTokenBracket t3 && tToken t3 == "]"
    =
        -- Found a test attribute. Find the opening brace of the method/class.
        case findOpeningBrace ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideCsharp keepTests (t3:ts) else t1 : processOutsideCsharp keepTests (t2:t3:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : signatureTokens ++ bodyTokens ++ processOutsideCsharp keepTests remainingTokens
                   else processOutsideCsharp keepTests remainingTokens

-- No test found, process the current token
processOutsideCsharp keepTests (t:ts) =
    if keepTests
    then processOutsideCsharp keepTests ts
    else t : processOutsideCsharp keepTests ts

-- ------------------------------------------------------------------
-- F#-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (F#) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. [<Test>] - NUnit/xUnit (F# attribute syntax)
--   2. [<Fact>] - xUnit
--   3. [<Theory>] - xUnit
--   4. testCase "..." - Expecto
--   5. testList "..." - Expecto
--   6. test "..." - Expecto
-- 
-- F# uses [< >] syntax for attributes and Expecto functions.
processOutsideFsharp :: Bool -> [Token] -> [Token]
processOutsideFsharp _ [] = [] -- End of stream

-- Pattern 1: [<Test>] or [<Fact>] or [<Theory>]
processOutsideFsharp keepTests (t1:t2:t3:t4:t5:ts)
    | isTokenBracket t1 && tToken t1 == "[" &&
      isTokenOperator t2 && tToken t2 == "<" &&
      isTokenIdentifier t3 && 
      (tToken t3 == "Test" || tToken t3 == "Fact" || tToken t3 == "Theory") &&
      isTokenOperator t4 && tToken t4 == ">" &&
      isTokenBracket t5 && tToken t5 == "]"
    =
        -- Found F# test attribute. Find the function body.
        case findOpeningBrace ts of
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : t5 : signatureTokens ++ bodyTokens ++ processOutsideFsharp keepTests remainingTokens
                   else processOutsideFsharp keepTests remainingTokens
            Nothing ->
                -- No braces, collect until next definition
                let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
                in if keepTests
                   then t1 : t2 : t3 : t4 : t5 : testTokens ++ processOutsideFsharp keepTests remainingTokens
                   else processOutsideFsharp keepTests remainingTokens

-- Pattern 2: testCase or testList or test (Expecto)
processOutsideFsharp keepTests (t1:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "testCase" || tToken t1 == "testList" || tToken t1 == "test")
    =
        case findOpeningBrace ts of
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideFsharp keepTests remainingTokens
                   else processOutsideFsharp keepTests remainingTokens
            Nothing ->
                let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
                in if keepTests
                   then t1 : testTokens ++ processOutsideFsharp keepTests remainingTokens
                   else processOutsideFsharp keepTests remainingTokens

-- No test found, process the current token
processOutsideFsharp keepTests (t:ts) =
    if keepTests
    then processOutsideFsharp keepTests ts
    else t : processOutsideFsharp keepTests ts

-- ------------------------------------------------------------------
-- Dart-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Dart) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test('...') or test("...") - Dart test package
--   2. group('...') or group("...") - Test group
--   3. testWidgets('...') - Flutter widget tests
-- 
-- Similar to JavaScript but with Dart-specific functions.
processOutsideDart :: Bool -> [Token] -> [Token]
processOutsideDart _ [] = [] -- End of stream

-- Pattern: test( or group( or testWidgets(
processOutsideDart keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "test" || tToken t1 == "group" || tToken t1 == "testWidgets") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideDart keepTests (t2:ts) else t1 : processOutsideDart keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideDart keepTests remainingTokens
                   else processOutsideDart keepTests remainingTokens

-- No test found, process the current token
processOutsideDart keepTests (t:ts) =
    if keepTests
    then processOutsideDart keepTests ts
    else t : processOutsideDart keepTests ts

-- ------------------------------------------------------------------
-- Elixir-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Elixir) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test "..." - ExUnit test
--   2. describe "..." - ExUnit describe block
--   3. defmodule *Test - Test module
-- 
-- Elixir uses ExUnit with do...end blocks.
processOutsideElixir :: Bool -> [Token] -> [Token]
processOutsideElixir _ [] = [] -- End of stream

-- Pattern 1: test or describe followed by string
processOutsideElixir keepTests (t1:ts)
    | isTokenIdentifier t1 && (tToken t1 == "test" || tToken t1 == "describe")
    =
        -- Collect until we find 'end' keyword or next test/describe
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : testTokens ++ processOutsideElixir keepTests remainingTokens
           else processOutsideElixir keepTests remainingTokens

-- Pattern 2: defmodule *Test
processOutsideElixir keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "defmodule" &&
      isTokenIdentifier t2 && "Test" `T.isSuffixOf` tToken t2
    =
        -- Collect test module until 'end'
        let (moduleTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : t2 : moduleTokens ++ processOutsideElixir keepTests remainingTokens
           else processOutsideElixir keepTests remainingTokens

-- No test found, process the current token
processOutsideElixir keepTests (t:ts) =
    if keepTests
    then processOutsideElixir keepTests ts
    else t : processOutsideElixir keepTests ts

-- | Helper: Collect tokens until we find 'end' keyword in Elixir.
-- This handles do...end blocks.
collectUntilElixirEnd :: [Token] -> ([Token], [Token])
collectUntilElixirEnd = go 0
  where
    go :: Int -> [Token] -> ([Token], [Token])
    go _ [] = ([], [])
    go depth (t:ts)
        -- Found 'do' - increase depth
        | isTokenKeyword t && tToken t == "do" =
            let (collected, remaining) = go (depth + 1) ts
            in (t : collected, remaining)
        -- Found 'end' - decrease depth or finish
        | isTokenKeyword t && tToken t == "end" =
            if depth <= 1
            then ([t], ts) -- Include final 'end' and finish
            else let (collected, remaining) = go (depth - 1) ts
                 in (t : collected, remaining)
        -- Other tokens
        | otherwise =
            let (collected, remaining) = go depth ts
            in (t : collected, remaining)

-- ------------------------------------------------------------------
-- Ruby-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Ruby) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. describe "..." - RSpec
--   2. context "..." - RSpec (alias for describe)
--   3. it "..." - RSpec
--   4. def test_* - Minitest
--   5. class Test* - Minitest
-- 
-- Ruby uses RSpec (BDD) and Minitest frameworks.
processOutsideRuby :: Bool -> [Token] -> [Token]
processOutsideRuby _ [] = [] -- End of stream

-- Pattern 1: describe/context/it (RSpec)
processOutsideRuby keepTests (t1:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "describe" || tToken t1 == "context" || tToken t1 == "it")
    =
        -- Collect until 'end' keyword
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts -- Reuse Elixir helper
        in if keepTests
           then t1 : testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- Pattern 2: def test_* (Minitest)
processOutsideRuby keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "def" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- Pattern 3: class Test* (Minitest)
processOutsideRuby keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 && "Test" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- No test found, process the current token
processOutsideRuby keepTests (t:ts) =
    if keepTests
    then processOutsideRuby keepTests ts
    else t : processOutsideRuby keepTests ts

-- ------------------------------------------------------------------
-- PHP-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (PHP) Helper: Processes tokens *outside* a test method.
-- Recognizes:
--   1. @test annotation in docblock
--   2. test* method naming
--   3. class *Test
-- 
-- PHP uses PHPUnit framework.
processOutsidePHP :: Bool -> [Token] -> [Token]
processOutsidePHP _ [] = [] -- End of stream

-- Pattern 1: @test annotation (in comment/docblock)
processOutsidePHP keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && tToken t2 == "test"
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 2: function/method starting with test
processOutsidePHP keepTests (t1:t2:ts)
    | isTokenKeyword t1 && (tToken t1 == "function" || tToken t1 == "public" || tToken t1 == "protected") &&
      isTokenIdentifier t2 && "test" `T.isPrefixOf` tToken t2
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 3: class *Test
processOutsidePHP keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 && "Test" `T.isSuffixOf` tToken t2
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- No test found, process the current token
processOutsidePHP keepTests (t:ts) =
    if keepTests
    then processOutsidePHP keepTests ts
    else t : processOutsidePHP keepTests ts

-- ------------------------------------------------------------------
-- Swift/Objective-C-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Swift/Objective-C) Helper: Processes tokens *outside* a test method.
-- Recognizes:
--   1. class *Test: XCTestCase
--   2. func test*()
-- 
-- Swift and Objective-C use XCTest framework.
processOutsideSwift :: Bool -> [Token] -> [Token]
processOutsideSwift _ [] = [] -- End of stream

-- Pattern 1: func test*
processOutsideSwift keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "func" &&
      isTokenIdentifier t2 && "test" `T.isPrefixOf` tToken t2
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:ts) else t1 : processOutsideSwift keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 2: class *Test (could also check for XCTestCase inheritance)
processOutsideSwift keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 && "Test" `T.isSuffixOf` tToken t2
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:ts) else t1 : processOutsideSwift keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- No test found, process the current token
processOutsideSwift keepTests (t:ts) =
    if keepTests
    then processOutsideSwift keepTests ts
    else t : processOutsideSwift keepTests ts

-- ------------------------------------------------------------------
-- R-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (R) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. test_that("...", { ... })
--   2. describe("...", { ... })
--   3. context("...", { ... })
-- 
-- R uses testthat framework.
processOutsideR :: Bool -> [Token] -> [Token]
processOutsideR _ [] = [] -- End of stream

-- Pattern: test_that( or describe( or context(
processOutsideR keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && 
      (tToken t1 == "test_that" || tToken t1 == "describe" || tToken t1 == "context") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideR keepTests (t2:ts) else t1 : processOutsideR keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                   else processOutsideR keepTests remainingTokens

-- No test found, process the current token
processOutsideR keepTests (t:ts) =
    if keepTests
    then processOutsideR keepTests ts
    else t : processOutsideR keepTests ts

-- ------------------------------------------------------------------
-- Julia-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Julia) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. @testset "..." begin ... end
--   2. @test expression
-- 
-- Julia uses Test standard library.
processOutsideJulia :: Bool -> [Token] -> [Token]
processOutsideJulia _ [] = [] -- End of stream

-- Pattern 1: @testset
processOutsideJulia keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && tToken t2 == "testset"
    =
        -- Collect until 'end' keyword
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideJulia keepTests remainingTokens
           else processOutsideJulia keepTests remainingTokens

-- Pattern 2: @test
processOutsideJulia keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && tToken t2 == "test"
    =
        -- Single line test, collect until newline or next statement
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideJulia keepTests remainingTokens
           else processOutsideJulia keepTests remainingTokens

-- No test found, process the current token
processOutsideJulia keepTests (t:ts) =
    if keepTests
    then processOutsideJulia keepTests ts
    else t : processOutsideJulia keepTests ts

-- ------------------------------------------------------------------
-- Perl-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Perl) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. Test files (*.t)
--   2. subtest blocks
-- 
-- Perl uses Test::More, Test::Simple frameworks.
-- Note: Perl tests are less structured, so this is a simplified approach.
processOutsidePerl :: Bool -> [Token] -> [Token]
processOutsidePerl _ [] = [] -- End of stream

-- Pattern: subtest
processOutsidePerl keepTests (t1:ts)
    | isTokenIdentifier t1 && tToken t1 == "subtest"
    =
        case findOpeningBrace ts of
            Nothing ->
                -- Collect until next top-level statement
                let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
                in if keepTests
                   then t1 : testTokens ++ processOutsidePerl keepTests remainingTokens
                   else processOutsidePerl keepTests remainingTokens
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsidePerl keepTests remainingTokens
                   else processOutsidePerl keepTests remainingTokens

-- No test found, process the current token
processOutsidePerl keepTests (t:ts) =
    if keepTests
    then processOutsidePerl keepTests ts
    else t : processOutsidePerl keepTests ts

-- ------------------------------------------------------------------
-- OCaml-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (OCaml) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. let test_* = ... - OUnit convention
--   2. test_case - Alcotest
-- 
-- OCaml uses OUnit and Alcotest frameworks.
processOutsideOCaml :: Bool -> [Token] -> [Token]
processOutsideOCaml _ [] = [] -- End of stream

-- Pattern 1: let test_*
processOutsideOCaml keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "let" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2
    =
        -- Collect until next let or end of block
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideOCaml keepTests remainingTokens
           else processOutsideOCaml keepTests remainingTokens

-- Pattern 2: test_case
processOutsideOCaml keepTests (t1:ts)
    | isTokenIdentifier t1 && tToken t1 == "test_case"
    =
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : testTokens ++ processOutsideOCaml keepTests remainingTokens
           else processOutsideOCaml keepTests remainingTokens

-- No test found, process the current token
processOutsideOCaml keepTests (t:ts) =
    if keepTests
    then processOutsideOCaml keepTests ts
    else t : processOutsideOCaml keepTests ts

-- ------------------------------------------------------------------
-- Erlang-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Erlang) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. *_test() - EUnit convention
--   2. *_test_() - EUnit generator convention
-- 
-- Erlang uses EUnit framework.
processOutsideErlang :: Bool -> [Token] -> [Token]
processOutsideErlang _ [] = [] -- End of stream

-- Pattern: function ending with _test or _test_
processOutsideErlang keepTests (t1:t2:ts)
    | isTokenIdentifier t1 && 
      ("_test" `T.isSuffixOf` tToken t1 || "_test_" `T.isSuffixOf` tToken t1) &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Find the function body (might use -> or after parameters)
        case findOpeningBrace ts of
            Nothing ->
                -- Collect until next function definition
                let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
                in if keepTests
                   then t1 : t2 : testTokens ++ processOutsideErlang keepTests remainingTokens
                   else processOutsideErlang keepTests remainingTokens
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideErlang keepTests remainingTokens
                   else processOutsideErlang keepTests remainingTokens

-- No test found, process the current token
processOutsideErlang keepTests (t:ts) =
    if keepTests
    then processOutsideErlang keepTests ts
    else t : processOutsideErlang keepTests ts

-- ------------------------------------------------------------------
-- Nim-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Nim) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. suite "..."
--   2. test "..."
-- 
-- Nim uses unittest module.
processOutsideNim :: Bool -> [Token] -> [Token]
processOutsideNim _ [] = [] -- End of stream

-- Pattern: suite or test
processOutsideNim keepTests (t1:ts)
    | isTokenIdentifier t1 && (tToken t1 == "suite" || tToken t1 == "test")
    =
        -- Collect until next suite/test or end
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : testTokens ++ processOutsideNim keepTests remainingTokens
           else processOutsideNim keepTests remainingTokens

-- No test found, process the current token
processOutsideNim keepTests (t:ts) =
    if keepTests
    then processOutsideNim keepTests ts
    else t : processOutsideNim keepTests ts

-- ------------------------------------------------------------------
-- Clojure-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Clojure) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. (deftest ...)
--   2. (testing ...)
-- 
-- Clojure uses clojure.test.
processOutsideClojure :: Bool -> [Token] -> [Token]
processOutsideClojure _ [] = [] -- End of stream

-- Pattern 1: (deftest
processOutsideClojure keepTests (t1:t2:ts)
    | isTokenBracket t1 && tToken t1 == "(" &&
      isTokenIdentifier t2 && tToken t2 == "deftest"
    =
        -- Collect until matching closing paren
        let (testTokens, remainingTokens) = collectUntilMatchingParen 1 ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideClojure keepTests remainingTokens
           else processOutsideClojure keepTests remainingTokens

-- Pattern 2: (testing
processOutsideClojure keepTests (t1:t2:ts)
    | isTokenBracket t1 && tToken t1 == "(" &&
      isTokenIdentifier t2 && tToken t2 == "testing"
    =
        let (testTokens, remainingTokens) = collectUntilMatchingParen 1 ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideClojure keepTests remainingTokens
           else processOutsideClojure keepTests remainingTokens

-- No test found, process the current token
processOutsideClojure keepTests (t:ts) =
    if keepTests
    then processOutsideClojure keepTests ts
    else t : processOutsideClojure keepTests ts

-- | Helper: Collect tokens until matching closing parenthesis
collectUntilMatchingParen :: Int -> [Token] -> ([Token], [Token])
collectUntilMatchingParen 0 ts = ([], ts)
collectUntilMatchingParen _ [] = ([], [])
collectUntilMatchingParen depth (t:ts)
    | isTokenBracket t && tToken t == "(" =
        let (collected, remaining) = collectUntilMatchingParen (depth + 1) ts
        in (t : collected, remaining)
    | isTokenBracket t && tToken t == ")" =
        if depth == 1
        then ([t], ts)
        else let (collected, remaining) = collectUntilMatchingParen (depth - 1) ts
             in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilMatchingParen depth ts
        in (t : collected, remaining)

-- ------------------------------------------------------------------
-- D-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (D) Helper: Processes tokens *outside* a test block.
-- Recognizes:
--   1. unittest { ... } - D's built-in test blocks
-- 
-- D has built-in unittest blocks.
processOutsideD :: Bool -> [Token] -> [Token]
processOutsideD _ [] = [] -- End of stream

-- Pattern: unittest {
processOutsideD keepTests (t1:ts)
    | isTokenKeyword t1 && tToken t1 == "unittest"
    =
        case findOpeningBrace ts of
            Nothing ->
                if keepTests then processOutsideD keepTests ts else t1 : processOutsideD keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideD keepTests remainingTokens
                   else processOutsideD keepTests remainingTokens

-- No test found, process the current token
processOutsideD keepTests (t:ts) =
    if keepTests
    then processOutsideD keepTests ts
    else t : processOutsideD keepTests ts

-- ------------------------------------------------------------------
-- Generic Helper Functions
-- ------------------------------------------------------------------

-- | (Generic) Helper: Processes tokens *inside* a braced block, handling nesting.
-- Renamed from processInsideRust
processInsideBraces :: Int -> [Token] -> ([Token], [Token])
processInsideBraces 0 ts = ([], ts) -- Base case: found matching '}'
processInsideBraces _ [] = ([], []) -- Error case: unexpected EOF
processInsideBraces nestingLevel (t:ts)
    -- Found a nested opening brace
    | isTokenBracket t && tToken t == "{" =
        let (nestedInside, remaining) = processInsideBraces (nestingLevel + 1) ts
        in (t : nestedInside, remaining) -- Keep '{' as part of inside tokens

    -- Found a closing brace
    | isTokenBracket t && tToken t == "}" =
        if nestingLevel == 1
        then -- This is the final '}' we were looking for
             ([t], ts) -- Include the final '}' and return the rest
        else -- This is a nested '}'
             let (nestedInside, remaining) = processInsideBraces (nestingLevel - 1) ts
             in (t : nestedInside, remaining) -- Keep '}'

    -- Any other token inside the block
    | otherwise =
        let (nestedInside, remaining) = processInsideBraces nestingLevel ts
        in (t : nestedInside, remaining) -- Keep the token

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
