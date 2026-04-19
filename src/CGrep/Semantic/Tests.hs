{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
        langFilter (Just keepTests) tokens = processOutsideKotlin keepTests tokens

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

-- | Extracts a single Rust attribute, e.g.: #[cfg(test)] or #[tokio::test]
extractRustAttribute :: [Token] -> Maybe ([Token], [Token])
extractRustAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : t2 : insideTokens, remaining)
extractRustAttribute _ = Nothing

-- | An attribute is a "test attribute" if the identifier "test" (or similar) exists within it.
isTestAttribute :: [Token] -> Bool
isTestAttribute = any (\t -> isTokenIdentifier t && (tToken t == "test" || tToken t == "rstest" || tToken t == "test_case"))

-- | Collects all contiguous attributes before a declaration.
collectRustAttributes :: [Token] -> ([Token], Bool, [Token])
collectRustAttributes = go [] False
  where
    go acc isTest remaining =
        case extractRustAttribute remaining of
            Just (attrTokens, rest) ->
                let testAttr = isTestAttribute attrTokens
                in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)

-- | (Rust) Helper: Processes tokens *outside* a test block.
processOutsideRust :: Bool -> [Token] -> [Token]
processOutsideRust _ [] = [] -- End of stream
processOutsideRust keepTests tokens =
    let (attrTokens, isTest, restAfterAttrs) = collectRustAttributes tokens
    in if not (null attrTokens)
       then
           if isTest
           then
               case findOpeningBraceBounded 50 restAfterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsideRust keepTests restAfterAttrs
                       else attrTokens ++ processOutsideRust keepTests restAfterAttrs
                   Just (sigTokens, restAfterBrace) ->
                       let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                       in if keepTests
                          then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideRust keepTests finalRest
                          else processOutsideRust keepTests finalRest
           else
               if keepTests
               then processOutsideRust keepTests restAfterAttrs
               else attrTokens ++ processOutsideRust keepTests restAfterAttrs
       else
           let (t:ts) = tokens
           in if keepTests
              then processOutsideRust keepTests ts
              else t : processOutsideRust keepTests ts

-- ------------------------------------------------------------------
-- Go-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Go) Helper: Processes tokens *outside* a test function.
processOutsideGo :: Bool -> [Token] -> [Token]
processOutsideGo _ [] = [] -- End of stream

-- Pattern 1: func Test...
processOutsideGo keepTests (t1:t2:t3:ts)
    | isTokenKeyword t1 && tToken t1 == "func" &&
      isTokenIdentifier t2 && ("Test" `T.isPrefixOf` tToken t2 || "Benchmark" `T.isPrefixOf` tToken t2 || "Example" `T.isPrefixOf` tToken t2 || "Fuzz" `T.isPrefixOf` tToken t2) &&
      isTokenBracket t3 && tToken t3 == "("
    =
        case findOpeningBraceBounded 50 ts of
            Nothing ->
                if keepTests then processOutsideGo keepTests (t2:t3:ts) else t1 : processOutsideGo keepTests (t2:t3:ts)
            Just (sigTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : sigTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
                   else processOutsideGo keepTests remainingTokens

-- Pattern 2: func (receiver) Test...
processOutsideGo keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "func" &&
      isTokenBracket t2 && tToken t2 == "("
    =
        let (receiverTokens, afterReceiver) = processInsideBrackets "(" ")" 1 ts
        in case afterReceiver of
            (t3:t4:ts') | isTokenIdentifier t3 &&
                          ("Test" `T.isPrefixOf` tToken t3 || "Benchmark" `T.isPrefixOf` tToken t3 || "Example" `T.isPrefixOf` tToken t3 || "Fuzz" `T.isPrefixOf` tToken t3) &&
                          isTokenBracket t4 && tToken t4 == "(" ->
                case findOpeningBraceBounded 50 ts' of
                    Nothing ->
                        if keepTests then processOutsideGo keepTests (t2:ts) else t1 : processOutsideGo keepTests (t2:ts)
                    Just (sigTokens, tokensAfterBrace) ->
                        let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                        in if keepTests
                           then t1 : t2 : receiverTokens ++ t3 : t4 : sigTokens ++ bodyTokens ++ processOutsideGo keepTests remainingTokens
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
-- Java-Specific Implementation Helpers
-- ------------------------------------------------------------------

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

isJavaTestAnnotation :: [Token] -> Bool
isJavaTestAnnotation = any (\t -> isTokenIdentifier t &&
    ("Test" `T.isInfixOf` tToken t || "Before" `T.isPrefixOf` tToken t || "After" `T.isPrefixOf` tToken t))

collectJavaAnnotations :: [Token] -> ([Token], Bool, [Token])
collectJavaAnnotations = go [] False
  where
    go acc isTest remaining =
        case extractJavaAnnotation remaining of
            Just (attrTokens, rest) ->
                let testAttr = isJavaTestAnnotation attrTokens
                in go (acc ++ attrTokens) (isTest || testAttr) rest
            Nothing -> (acc, isTest, remaining)

-- | (Java) Helper: Processes tokens *outside* a test method.
processOutsideJava :: Bool -> [Token] -> [Token]
processOutsideJava _ [] = [] -- End of stream
processOutsideJava keepTests tokens =
    let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens
    in if not (null attrTokens)
       then
           if isTest
           then
               case findOpeningBraceC 100 restAfterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsideJava keepTests restAfterAttrs
                       else attrTokens ++ processOutsideJava keepTests restAfterAttrs
                   Just (sigTokens, restAfterBrace) ->
                       let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                       in if keepTests
                          then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideJava keepTests finalRest
                          else processOutsideJava keepTests finalRest
           else
               if keepTests
               then processOutsideJava keepTests restAfterAttrs
               else attrTokens ++ processOutsideJava keepTests restAfterAttrs
       else
           let (t:ts) = tokens
           in if keepTests
              then processOutsideJava keepTests ts
              else t : processOutsideJava keepTests ts

-- ------------------------------------------------------------------
-- Kotlin-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | Helper for Kotlin to avoid swallowing braces of subsequent functions after abstract methods
findOpeningBraceKotlin :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceKotlin _ 0 _  = Nothing
findOpeningBraceKotlin _ _ [] = Nothing
findOpeningBraceKotlin canSeeDecl n (t:ts)
    | isTokenBracket t && tToken t == "{" = Just ([t], ts)
    | isTokenKeyword t && (tToken t == "fun" || tToken t == "class" || tToken t == "interface" || tToken t == "val" || tToken t == "var") =
        if canSeeDecl
        then case findOpeningBraceKotlin False (n - 1) ts of
                 Nothing -> Nothing
                 Just (pre, post) -> Just (t : pre, post)
        else Nothing
    | otherwise =
        case findOpeningBraceKotlin canSeeDecl (n - 1) ts of
            Nothing -> Nothing
            Just (pre, post) -> Just (t : pre, post)

processOutsideKotlin :: Bool -> [Token] -> [Token]
processOutsideKotlin _ [] = [] -- End of stream
processOutsideKotlin keepTests tokens
    | let (attrTokens, isTest, restAfterAttrs) = collectJavaAnnotations tokens, not (null attrTokens)
    =
           if isTest
           then
               case findOpeningBraceKotlin True 100 restAfterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsideKotlin keepTests restAfterAttrs
                       else attrTokens ++ processOutsideKotlin keepTests restAfterAttrs
                   Just (sigTokens, restAfterBrace) ->
                       let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                       in if keepTests
                          then attrTokens ++ sigTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                          else processOutsideKotlin keepTests finalRest
           else
               if keepTests
               then processOutsideKotlin keepTests restAfterAttrs
               else attrTokens ++ processOutsideKotlin keepTests restAfterAttrs

processOutsideKotlin keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test" || tToken t1 == "xtest" || tToken t1 == "describe" || tToken t1 == "xdescribe" || tToken t1 == "it" || tToken t1 == "xit" || tToken t1 == "context" || tToken t1 == "xcontext") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsideKotlin keepTests (t2:ts) else t1 : processOutsideKotlin keepTests (t2:ts)
            Just (sigTokens, restAfterBrace) ->
                let (bodyTokens, finalRest) = processInsideBraces 1 restAfterBrace
                in if keepTests
                   then t1 : t2 : sigTokens ++ bodyTokens ++ processOutsideKotlin keepTests finalRest
                   else processOutsideKotlin keepTests finalRest

processOutsideKotlin keepTests (t:ts) =
    if keepTests
    then processOutsideKotlin keepTests ts
    else t : processOutsideKotlin keepTests ts

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
findOpeningBraceC :: Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceC 0 _  = Nothing
findOpeningBraceC _ [] = Nothing
findOpeningBraceC n (t:ts)
    | isTokenBracket t && tToken t == "{" = Just ([t], ts)
    | tToken t == ";" = Nothing
    | otherwise =
        case findOpeningBraceC (n - 1) ts of
            Nothing -> Nothing
            Just (pre, post) -> Just (t : pre, post)

processOutsideC :: Bool -> [Token] -> [Token]
processOutsideC _ [] = [] -- End of stream

-- Pattern 1: C/C++ Test Macros (Google Test, Catch2, etc.)
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "TEST" || tToken t1 == "TEST_F" || tToken t1 == "TEST_CASE" ||
       tToken t1 == "TYPED_TEST" || tToken t1 == "SECTION" || tToken t1 == "SUITE") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test macro. Look ahead for the opening brace of the test body (limit 100 tokens).
        case findOpeningBraceC 100 ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
                   else processOutsideC keepTests remainingTokens

-- Pattern 2: Functions starting with "test_" or "Test"
processOutsideC keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (("test_" `T.isPrefixOf` tToken t1) || ("Test" `T.isPrefixOf` tToken t1)) &&
      isTokenBracket t2 && tToken t2 == "("
    =
        -- Found a test function. Require the next token to be '(' to avoid matching variables.
        -- Find the opening brace within a bounded lookahead to avoid swallowing forward declarations.
        case findOpeningBraceC 50 ts of
            Nothing -> -- Malformed or forward declaration. Treat as non-test code.
                if keepTests then processOutsideC keepTests (t2:ts) else t1 : processOutsideC keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBraces 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideC keepTests remainingTokens
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

-- Pattern 1: async def test_*
processOutsidePython keepTests (t1:t2:t3:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" &&
      isTokenKeyword t2 && tToken t2 == "def" &&
      isTokenIdentifier t3 && "test_" `T.isPrefixOf` tToken t3
    =
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : t3 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 2: @pytest or @unittest decorator
processOutsidePython keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && (tToken t2 == "pytest" || tToken t2 == "unittest" || tToken t2 == "patch" || tToken t2 == "mock")
    =
        let (decoratorTokens, afterDecorator) = collectDecoratorAndFunction ts
        in if keepTests
           then t1 : t2 : decoratorTokens ++ processOutsidePython keepTests afterDecorator
           else processOutsidePython keepTests afterDecorator

-- Pattern 3: def test_*
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "def" &&
      isTokenIdentifier t2 && "test_" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilNextDefOrClass ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens

-- Pattern 4: class Test* or class *(unittest.TestCase)
processOutsidePython keepTests (t1:t2:ts)
    | isTokenKeyword t1 && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      ("Test" `T.isPrefixOf` tToken t2 || hasUnittestBase ts)
    =
        let (testTokens, remainingTokens) = collectTestClassBody ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsidePython keepTests remainingTokens
           else processOutsidePython keepTests remainingTokens
  where
    hasUnittestBase (t3:t4:t5:t6:t7:_) =
        isTokenBracket t3 && tToken t3 == "(" &&
        isTokenIdentifier t4 && tToken t4 == "unittest" &&
        isTokenOperator t5 && tToken t5 == "." &&
        isTokenIdentifier t6 && tToken t6 == "TestCase" &&
        isTokenBracket t7 && tToken t7 == ")"
    hasUnittestBase _ = False

-- No test found, process the current token
processOutsidePython keepTests (t:ts) =
    if keepTests
    then processOutsidePython keepTests ts
    else t : processOutsidePython keepTests ts

-- | Helper: Collect decorator tokens and the following function/class definition.
collectDecoratorAndFunction :: [Token] -> ([Token], [Token])
collectDecoratorAndFunction ts =
    let (beforeDef, fromDef) = collectUntilDefOrClass ts
    in case fromDef of
        [] -> (beforeDef, [])
        (t1:t2:rest) | (isTokenIdentifier t1 || isTokenKeyword t1) && tToken t1 == "async" && isTokenKeyword t2 && tToken t2 == "def" ->
            let (bodyTokens, remaining) = collectUntilNextDefOrClass rest
            in (beforeDef ++ [t1, t2] ++ bodyTokens, remaining)
        (t:rest) ->
            let (bodyTokens, remaining) = collectUntilNextDefOrClass rest
            in (beforeDef ++ [t] ++ bodyTokens, remaining)

-- | Helper: Collect tokens until we find 'def' or 'class' keyword.
collectUntilDefOrClass :: [Token] -> ([Token], [Token])
collectUntilDefOrClass [] = ([], [])
collectUntilDefOrClass (t:ts)
    | isTokenKeyword t && (tToken t == "def" || tToken t == "class") =
        ([], t:ts)
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:_) | isTokenKeyword t2 && tToken t2 == "def" -> ([], t:ts)
            _ ->
                let (collected, remaining) = collectUntilDefOrClass ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilDefOrClass ts
        in (t : collected, remaining)

-- | Helper: Collect tokens until we find 'def' or 'class' keyword.
collectUntilNextDefOrClass :: [Token] -> ([Token], [Token])
collectUntilNextDefOrClass [] = ([], [])
collectUntilNextDefOrClass (t:ts)
    | isTokenKeyword t && (tToken t == "def" || tToken t == "class") =
        ([], t:ts)
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:_) | isTokenKeyword t2 && tToken t2 == "def" -> ([], t:ts)
            _ ->
                let (collected, remaining) = collectUntilNextDefOrClass ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilNextDefOrClass ts
        in (t : collected, remaining)

-- | Helper: Collect test class body based on method signatures (self/cls)
collectTestClassBody :: [Token] -> ([Token], [Token])
collectTestClassBody [] = ([], [])
collectTestClassBody (t:ts)
    | isTokenKeyword t && tToken t == "class" = ([], t:ts)
    | isTokenKeyword t && tToken t == "def" =
        case ts of
            (name:openParen:firstArg:_)
                | isTokenIdentifier name &&
                  isTokenBracket openParen && tToken openParen == "(" &&
                  isTokenIdentifier firstArg && (tToken firstArg == "self" || tToken firstArg == "cls") ->
                    let (methodTokens, remaining) = collectUntilNextDefOrClass ts
                        (restClass, finalRest) = collectTestClassBody remaining
                    in (t : methodTokens ++ restClass, finalRest)
            _ -> ([], t:ts) -- Not a method with self/cls, so it's a new top-level def
    | (isTokenIdentifier t || isTokenKeyword t) && tToken t == "async" =
        case ts of
            (t2:ts') | isTokenKeyword t2 && tToken t2 == "def" ->
                case ts' of
                    (name:openParen:firstArg:_)
                        | isTokenIdentifier name &&
                          isTokenBracket openParen && tToken openParen == "(" &&
                          isTokenIdentifier firstArg && (tToken firstArg == "self" || tToken firstArg == "cls") ->
                            let (methodTokens, remaining) = collectUntilNextDefOrClass ts
                                (restClass, finalRest) = collectTestClassBody remaining
                            in (t : methodTokens ++ restClass, finalRest)
                    _ -> ([], t:ts)
            _ ->
                let (collected, remaining) = collectTestClassBody ts
                in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectTestClassBody ts
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
        case findOpeningBracketBounded "{" 20 ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideZig keepTests ts else t1 : processOutsideZig keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes the test name/description and the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
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
        -- Found a test function call. Capture everything inside the parentheses.
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then -- Keep test function call + signature + body
                t1 : t2 : bodyTokens ++ processOutsideJavascript keepTests remainingTokens
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
        case findOpeningBracketBounded "{" 20 ts of
            Nothing -> -- Malformed, no '{' found. Treat as non-test code.
                if keepTests then processOutsideScala keepTests (t2:ts) else t1 : processOutsideScala keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                -- signatureTokens includes everything up to and including the opening brace
                -- tokensAfterBrace starts after the opening brace
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
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
        let (testTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : testTokens ++ processOutsideHaskell keepTests remainingTokens
           else processOutsideHaskell keepTests remainingTokens

-- Pattern 2: prop_* function (QuickCheck convention)
processOutsideHaskell keepTests (t1:ts)
    | isTokenIdentifier t1 && "prop_" `T.isPrefixOf` tToken t1
    =
        let (propTokens, remainingTokens) = collectUntilNextHaskellDef ts
        in if keepTests
           then t1 : propTokens ++ processOutsideHaskell keepTests remainingTokens
           else processOutsideHaskell keepTests remainingTokens

-- No test found, process the current token
processOutsideHaskell keepTests (t:ts) =
    if keepTests
    then processOutsideHaskell keepTests ts
    else t : processOutsideHaskell keepTests ts

-- | Helper: Collect tokens until we find the next top-level Haskell definition.
collectUntilNextHaskellDef :: [Token] -> ([Token], [Token])
collectUntilNextHaskellDef [] = ([], [])
-- The Haskell tokenizer splits the `::` operator into two adjacent `:` tokens.
-- Detect that sequence here to recognize the start of a new top-level type signature.
collectUntilNextHaskellDef (x1:x2:xs)
    | isTokenOperator x1 && tToken x1 == ":" &&
      isTokenOperator x2 && tToken x2 == ":" =
        ([], x1:x2:xs)
collectUntilNextHaskellDef (x:xs)
    | isTokenIdentifier x &&
      (tToken x == "describe" || tToken x == "it" || tToken x == "context" ||
       tToken x == "testCase" || tToken x == "testGroup" || tToken x == "testProperty" ||
       tToken x == "main" || "prop_" `T.isPrefixOf` tToken x) =
        ([], x:xs)
    | isTokenOperator x && tToken x == "::" =
        ([], x:xs)
    | isTokenKeyword x && (tToken x == "data" || tToken x == "type" || tToken x == "class" || tToken x == "instance" || tToken x == "newtype" || tToken x == "module") =
        ([], x:xs)
    | isTokenIdentifier x && (tToken x == "let" || tToken x == "in") =
        let (collected, remaining) = collectUntilNextHaskellDef xs
        in (x : collected, remaining)
    | isTokenOperator x && tToken x == "=" =
        let (collected, remaining) = collectUntilNextHaskellDef xs
        in (x : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectUntilNextHaskellDef xs
        in (x : collected, remaining)
-- ------------------------------------------------------------------
-- C#-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (C#) Helper: Processes tokens *outside* a test method.
-- Recognizes C# attributes from multiple test frameworks:
--   NUnit:  [Test], [TestFixture], [TestCase(...)], [TestCaseSource(...)],
--           [SetUp], [TearDown], [OneTimeSetUp], [OneTimeTearDown]
--   xUnit:  [Fact], [Theory], [InlineData(...)], [MemberData(...)], [ClassData(...)]
--   MSTest: [TestClass], [TestMethod], [DataTestMethod], [DataRow(...)],
--           [TestInitialize], [TestCleanup], [ClassInitialize], [ClassCleanup],
--           [AssemblyInitialize], [AssemblyCleanup]
--
-- Handles:
--   - Parameterized attributes, e.g. [Fact(Skip = "reason")], [TestMethod("Name")]
--   - Fully-qualified attributes, e.g. [NUnit.Framework.Test]
--   - Stacked attributes, e.g. [Theory] [InlineData(1,2)] [InlineData(3,4)]

-- | Extracts a single C# attribute block `[...]`, including nested brackets/parens.
extractCsharpAttribute :: [Token] -> Maybe ([Token], [Token])
extractCsharpAttribute (t1:ts)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : insideTokens, remaining)
extractCsharpAttribute _ = Nothing

-- | True if any identifier inside the attribute is a known C# test-framework attribute.
isCsharpTestAttribute :: [Token] -> Bool
isCsharpTestAttribute = any (\t -> isTokenIdentifier t && isCsharpTestAttrName (tToken t))
  where
    isCsharpTestAttrName n =
        -- NUnit
        n == "Test" || n == "TestFixture" ||
        n == "TestCase" || n == "TestCaseSource" ||
        n == "SetUp" || n == "TearDown" ||
        n == "OneTimeSetUp" || n == "OneTimeTearDown" ||
        -- xUnit
        n == "Fact" || n == "Theory" ||
        n == "InlineData" || n == "MemberData" || n == "ClassData" ||
        -- MSTest
        n == "TestClass" || n == "TestMethod" || n == "DataTestMethod" ||
        n == "DataRow" ||
        n == "TestInitialize" || n == "TestCleanup" ||
        n == "ClassInitialize" || n == "ClassCleanup" ||
        n == "AssemblyInitialize" || n == "AssemblyCleanup"

-- | Collects a contiguous run of C# attributes at the head of the stream.
collectCsharpAttributes :: [Token] -> ([[Token]], [Token])
collectCsharpAttributes ts = case extractCsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectCsharpAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

processOutsideCsharp :: Bool -> [Token] -> [Token]
processOutsideCsharp _ [] = [] -- End of stream

-- Pattern: one or more contiguous attributes, at least one of which is a test attribute.
processOutsideCsharp keepTests tokens@(t1:_)
    | isTokenBracket t1 && tToken t1 == "[" =
        let (attrs, afterAttrs) = collectCsharpAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isCsharpTestAttribute attrs
           then
               -- Found at least one test attribute in the stack.
               -- Skip any surrounding modifiers/signature and consume the following braced body.
               case findOpeningBraceBounded 200 afterAttrs of
                   Nothing ->
                       -- No brace found within lookahead: drop attributes gracefully.
                       if keepTests
                       then processOutsideCsharp keepTests afterAttrs
                       else flatAttrs ++ processOutsideCsharp keepTests afterAttrs
                   Just (signatureTokens, tokensAfterBrace) ->
                       let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                       in if keepTests
                          then flatAttrs ++ signatureTokens ++ bodyTokens ++ processOutsideCsharp keepTests remainingTokens
                          else processOutsideCsharp keepTests remainingTokens
           else
               -- Attributes are present but none are test-related: emit them as-is and continue.
               if keepTests
               then processOutsideCsharp keepTests afterAttrs
               else flatAttrs ++ processOutsideCsharp keepTests afterAttrs

-- No attribute / no test: pass through
processOutsideCsharp keepTests (t:ts) =
    if keepTests
    then processOutsideCsharp keepTests ts
    else t : processOutsideCsharp keepTests ts

-- ------------------------------------------------------------------
-- F#-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (F#) Helper: Processes tokens *outside* a test function.
-- Recognizes F# attributes from multiple test frameworks:
--   NUnit:   [<Test>], [<TestFixture>], [<TestCase(...)>], [<TestCaseSource(...)>],
--            [<SetUp>], [<TearDown>], [<OneTimeSetUp>], [<OneTimeTearDown>], [<Category(...)>]
--   xUnit:   [<Fact>], [<Theory>], [<InlineData(...)>], [<MemberData(...)>], [<ClassData(...)>]
--   FsCheck: [<Property>]
--   Expecto: testCase, testCaseAsync, testList, test, ftest, ptest, ftestCase, ptestCase, testProperty
--
-- Notes on F# tokenization:
--   In --strict mode, [< and >] are single operator tokens.
--   In non-strict mode, [< splits into `[` (bracket) + `<` (op), and >] splits into `>` (op) + `]` (bracket).
-- Handles parameterized, fully-qualified, and stacked attributes.
--
-- F# has no braces for function bodies: we consume tokens until the next
-- top-level 'let', 'module', 'type', 'namespace', 'open', or another attribute start.

-- | True if the stream starts with an F# attribute opener: `[<` (single op token) or `[` + `<` (split).
isFsharpAttrOpen :: [Token] -> Bool
isFsharpAttrOpen (t1:t2:_)
    | isTokenOperator t1 && tToken t1 == "[<" = True
    | isTokenBracket  t1 && tToken t1 == "["
      && isTokenOperator t2 && tToken t2 == "<" = True
isFsharpAttrOpen (t1:_)
    | isTokenOperator t1 && tToken t1 == "[<" = True
isFsharpAttrOpen _ = False

-- | True if the given 2 tokens form an F# attribute closer: `>]` or `>` + `]`.
isFsharpAttrClose2 :: Token -> Token -> Bool
isFsharpAttrClose2 a b =
    isTokenOperator a && tToken a == ">" &&
    isTokenBracket  b && tToken b == "]"

isFsharpAttrClose1 :: Token -> Bool
isFsharpAttrClose1 t = isTokenOperator t && tToken t == ">]"

-- | Extracts a single F# attribute block `[< ... >]`, including nested parens.
-- Returns (attributeTokens, tokensAfterAttribute).
extractFsharpAttribute :: [Token] -> Maybe ([Token], [Token])
-- Case A: opener is single token "[<"
extractFsharpAttribute (t1:ts)
    | isTokenOperator t1 && tToken t1 == "[<" =
        let (inside, rest) = consumeFsharpAttrBody ts
        in case rest of
             (c:rs) | isFsharpAttrClose1 c -> Just (t1 : inside ++ [c], rs)
             (c1:c2:rs) | isFsharpAttrClose2 c1 c2 -> Just (t1 : inside ++ [c1, c2], rs)
             _ -> Nothing
-- Case B: opener is two tokens "[" + "<"
extractFsharpAttribute (t1:t2:ts)
    | isTokenBracket t1 && tToken t1 == "[" &&
      isTokenOperator t2 && tToken t2 == "<" =
        let (inside, rest) = consumeFsharpAttrBody ts
        in case rest of
             (c:rs) | isFsharpAttrClose1 c -> Just (t1 : t2 : inside ++ [c], rs)
             (c1:c2:rs) | isFsharpAttrClose2 c1 c2 -> Just (t1 : t2 : inside ++ [c1, c2], rs)
             _ -> Nothing
extractFsharpAttribute _ = Nothing

-- | Consumes the body of an F# attribute, balancing parentheses,
-- stopping just before the `>]` (or `>` + `]`) closer.
consumeFsharpAttrBody :: [Token] -> ([Token], [Token])
consumeFsharpAttrBody [] = ([], [])
consumeFsharpAttrBody (t:rest)
    | isFsharpAttrClose1 t = ([], t:rest)
consumeFsharpAttrBody (t1:t2:rest)
    | isFsharpAttrClose2 t1 t2 = ([], t1:t2:rest)
consumeFsharpAttrBody (t:rest)
    | isTokenBracket t && tToken t == "(" =
        let (paren, afterParen) = processInsideBrackets "(" ")" 1 rest
            (more, final) = consumeFsharpAttrBody afterParen
        in (t : paren ++ more, final)
    | otherwise =
        let (more, final) = consumeFsharpAttrBody rest
        in (t : more, final)

-- | True if any identifier inside the attribute is a known F# test-framework attribute.
isFsharpTestAttribute :: [Token] -> Bool
isFsharpTestAttribute = any (\t -> isTokenIdentifier t && isFsharpTestAttrName (tToken t))
  where
    isFsharpTestAttrName n =
        -- NUnit
        n == "Test" || n == "TestFixture" ||
        n == "TestCase" || n == "TestCaseSource" ||
        n == "SetUp" || n == "TearDown" ||
        n == "OneTimeSetUp" || n == "OneTimeTearDown" ||
        n == "Category" ||
        -- xUnit
        n == "Fact" || n == "Theory" ||
        n == "InlineData" || n == "MemberData" || n == "ClassData" ||
        -- FsCheck
        n == "Property"

-- | Collect a contiguous run of F# attributes.
collectFsharpAttributes :: [Token] -> ([[Token]], [Token])
collectFsharpAttributes ts = case extractFsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectFsharpAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

-- | Collect tokens that are part of a single F# binding (after a `let` / `member`
-- / `type` header). Stops when the next top-level construct, a new attribute
-- stack, or a new Expecto test function is encountered.
collectFsharpBindingBody :: [Token] -> ([Token], [Token])
collectFsharpBindingBody [] = ([], [])
collectFsharpBindingBody (first:rest) =
    let (body, remaining) = go rest
    in (first : body, remaining)
  where
    go [] = ([], [])
    go toks@(t:ts)
        -- Stop at the start of a new attribute.
        | isFsharpAttrOpen toks = ([], toks)
        -- Stop at a new top-level keyword (excluding 'let' since it's used for local bindings).
        | isTokenKeyword t &&
          (tToken t == "module" || tToken t == "namespace" ||
           tToken t == "type" ||
           tToken t == "member" || tToken t == "static" || tToken t == "abstract" ||
           tToken t == "override" || tToken t == "interface") = ([], toks)
        -- Stop at a new top-level Expecto test starter.
        | isTokenIdentifier t && isFsharpExpectoStarter (tToken t) = ([], toks)
        | otherwise =
            let (more, r) = go ts
            in (t : more, r)

-- | Names of Expecto test combinators that start a test block.
isFsharpExpectoStarter :: T.Text -> Bool
isFsharpExpectoStarter n =
    n == "testCase" || n == "testCaseAsync" ||
    n == "testList" || n == "test" ||
    n == "ftest" || n == "ptest" ||
    n == "ftestCase" || n == "ptestCase" ||
    n == "testProperty" || n == "testPropertyWithConfig" ||
    n == "testAsync" || n == "testSequenced"

-- | Consume a single Expecto test block, which may look like:
--     testCase "name" <| fun () -> ...
--     test "name" { ... }
--     testList "name" [ ... ]
--   We skip the string literal (if present), then:
--     - if the next token is `{`, consume the balanced block;
--     - if it's `[`, consume the balanced list (which itself may contain nested test combinators);
--     - otherwise, consume a binding body until the next top-level construct.
collectFsharpExpectoBody :: [Token] -> ([Token], [Token])
collectFsharpExpectoBody ts =
    -- skip an optional string literal (the test name is often a bare string token)
    let afterName = case ts of
                      (n:rest) | not (isTokenIdentifier n) && not (isTokenKeyword n)
                                 && not (isTokenOperator n) && not (isTokenBracket n) -> rest
                      _ -> ts
        prefix    = take (length ts - length afterName) ts
    in case afterName of
        -- Block form: test "..." { ... }
        (b:rest) | isTokenBracket b && tToken b == "{" ->
            let (body, final) = processInsideBrackets "{" "}" 1 rest
            in (prefix ++ b : body, final)
        -- List form: testList "..." [ ... ]
        (b:rest) | isTokenBracket b && tToken b == "[" ->
            let (body, final) = processInsideBrackets "[" "]" 1 rest
            in (prefix ++ b : body, final)
        -- Fallback: accumulate until next top-level construct.
        _ ->
            let (body, final) = collectFsharpBindingBody afterName
            in (prefix ++ body, final)

processOutsideFsharp :: Bool -> [Token] -> [Token]
processOutsideFsharp _ [] = [] -- End of stream

-- Pattern 1: one or more contiguous F# attributes, at least one of which is a test attribute.
processOutsideFsharp keepTests tokens
    | isFsharpAttrOpen tokens =
        let (attrs, afterAttrs) = collectFsharpAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isFsharpTestAttribute attrs
           then
               -- Consume the binding that follows (everything until the next
               -- attribute / top-level keyword / Expecto starter).
               let (body, remaining) = collectFsharpBindingBody afterAttrs
               in if keepTests
                  then flatAttrs ++ body ++ processOutsideFsharp keepTests remaining
                  else processOutsideFsharp keepTests remaining
           else
               -- No test attribute in the stack: emit them as-is and continue.
               if keepTests
               then processOutsideFsharp keepTests afterAttrs
               else flatAttrs ++ processOutsideFsharp keepTests afterAttrs

-- Pattern 2: Expecto test combinators: testCase "..." ..., test "..." { ... }, etc.
processOutsideFsharp keepTests (t1:ts)
    | isTokenIdentifier t1 && isFsharpExpectoStarter (tToken t1) =
        let (body, remaining) = collectFsharpExpectoBody ts
        in if keepTests
           then t1 : body ++ processOutsideFsharp keepTests remaining
           else processOutsideFsharp keepTests remaining

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
--   2. group('...') or group("...") - Dart test package
--   3. testWidgets('...') - Flutter test package
--   4. setUp(...), tearDown(...), setUpAll(...), tearDownAll(...)
--
-- Similar to JavaScript but with Dart-specific functions.
processOutsideDart :: Bool -> [Token] -> [Token]
processOutsideDart _ [] = [] -- End of stream

-- Pattern: test( or group( or testWidgets( or setUp( etc.
processOutsideDart keepTests (t1:t2:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test" || tToken t1 == "group" || tToken t1 == "testWidgets" ||
       tToken t1 == "setUp" || tToken t1 == "tearDown" ||
       tToken t1 == "setUpAll" || tToken t1 == "tearDownAll") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then t1 : t2 : bodyTokens ++ processOutsideDart keepTests remainingTokens
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

-- Pattern 1: test, describe, setup, setup_all followed by string/block
processOutsideElixir keepTests (t1:ts)
    | isTokenIdentifier t1 && (tToken t1 == "test" || tToken t1 == "describe" || tToken t1 == "setup" || tToken t1 == "setup_all")
    =
        -- Collect until we find 'end' keyword
        let (testTokens, remainingTokens) = collectUntilElixirEnd ts
        in if keepTests
           then t1 : testTokens ++ processOutsideElixir keepTests remainingTokens
           else processOutsideElixir keepTests remainingTokens

-- Pattern 2: defmodule ...Test
processOutsideElixir keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "defmodule" &&
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
        -- Found 'do' or 'fn' - increase depth
        | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "do" || tToken t == "fn") =
            let (collected, remaining) = go (depth + 1) ts
            in (t : collected, remaining)
        -- Found 'end' - decrease depth or finish
        | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
            if depth <= 1
            then ([t], ts) -- Include final 'end' and finish
            else
                let (collected, remaining) = go (depth - 1) ts
                in (t : collected, remaining)
        -- Found another definition that doesn't use 'do' but might need tracking
        -- Note: this is simplified, Elixir macros can be complex
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
processOutsideRuby keepTests tokens@(t1:_)
    | isTokenIdentifier t1 &&
      (tToken t1 == "describe" || tToken t1 == "context" || tToken t1 == "it")
    =
        -- Collect until 'end' keyword
        let (testTokens, remainingTokens) = collectUntilRubyEnd tokens
        in if keepTests
           then testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- Pattern 2: def test_*, setup, teardown (Minitest)
processOutsideRuby keepTests tokens@(t1:t2:_)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "def" &&
      isTokenIdentifier t2 && ("test_" `T.isPrefixOf` tToken t2 || tToken t2 == "setup" || tToken t2 == "teardown")
    =
        let (testTokens, remainingTokens) = collectUntilRubyEnd tokens
        in if keepTests
           then testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- Pattern 3: class Test* (Minitest)
processOutsideRuby keepTests tokens@(t1:t2:_)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 && "Test" `T.isPrefixOf` tToken t2
    =
        let (testTokens, remainingTokens) = collectUntilRubyEnd tokens
        in if keepTests
           then testTokens ++ processOutsideRuby keepTests remainingTokens
           else processOutsideRuby keepTests remainingTokens

-- No test found, process the current token
processOutsideRuby keepTests (t:ts) =
    if keepTests
    then processOutsideRuby keepTests ts
    else t : processOutsideRuby keepTests ts

-- | Helper: Collect tokens until we find 'end' keyword in Ruby.
-- This handles do...end, def...end, class...end blocks.
collectUntilRubyEnd :: [Token] -> ([Token], [Token])
collectUntilRubyEnd = go 0
  where
    go :: Int -> [Token] -> ([Token], [Token])
    go _ [] = ([], [])
    go depth (t:ts)
        -- Found block starter - increase depth
        | (isTokenKeyword t || isTokenIdentifier t) &&
          (tToken t == "do" || tToken t == "def" || tToken t == "class" || tToken t == "module" ||
           tToken t == "if" || tToken t == "unless" || tToken t == "while" || tToken t == "until" ||
           tToken t == "for" || tToken t == "case" || tToken t == "begin") =
            let (collected, remaining) = go (depth + 1) ts
            in (t : collected, remaining)
        -- Found 'end' - decrease depth or finish
        | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
            if depth <= 1
            then ([t], ts) -- Include final 'end' and finish
            else let (collected, remaining) = go (depth - 1) ts
                 in (t : collected, remaining)
        -- Other tokens
        | otherwise =
            let (collected, remaining) = go depth ts
            in (t : collected, remaining)

-- ------------------------------------------------------------------
-- PHP-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | Extracts a PHP attribute block `#[...]`
extractPhpAttribute :: [Token] -> Maybe ([Token], [Token])
extractPhpAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (insideTokens, remaining) = processInsideBrackets "[" "]" 1 ts
        in Just (t1 : t2 : insideTokens, remaining)
extractPhpAttribute _ = Nothing

-- | Checks if an attribute is test-related
isPhpTestAttribute :: [Token] -> Bool
isPhpTestAttribute = any (\t -> isTokenIdentifier t &&
    (tToken t == "Test" || tToken t == "DataProvider" || tToken t == "Depends" ||
     tToken t == "Before" || tToken t == "After" || tToken t == "BeforeClass" || tToken t == "AfterClass"))

-- | Collects all PHP attributes before a declaration
collectPhpAttributes :: [Token] -> ([[Token]], [Token])
collectPhpAttributes ts = case extractPhpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectPhpAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

-- | (PHP) Helper: Processes tokens *outside* a test method.
--
-- PHP uses PHPUnit and Pest frameworks.
processOutsidePHP :: Bool -> [Token] -> [Token]
processOutsidePHP _ [] = [] -- End of stream

-- Pattern 1: Pest functions `test(`, `it(`, `describe(`
processOutsidePHP keepTests (t1:t2:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && (tToken t1 == "test" || tToken t1 == "it" || tToken t1 == "describe") &&
      isTokenBracket t2 && tToken t2 == "("
    =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then t1 : t2 : bodyTokens ++ processOutsidePHP keepTests remainingTokens
           else processOutsidePHP keepTests remainingTokens

-- Pattern 2: PHP Attributes #[Test] etc.
processOutsidePHP keepTests tokens@(t1:t2:_)
    | isTokenOperator t1 && tToken t1 == "#" &&
      isTokenBracket t2 && tToken t2 == "[" =
        let (attrs, afterAttrs) = collectPhpAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isPhpTestAttribute attrs
           then
               case findOpeningBraceBounded 100 afterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsidePHP keepTests afterAttrs
                       else flatAttrs ++ processOutsidePHP keepTests afterAttrs
                   Just (sigTokens, tokensAfterBrace) ->
                       let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                       in if keepTests
                          then flatAttrs ++ sigTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                          else processOutsidePHP keepTests remainingTokens
           else
               if keepTests
               then processOutsidePHP keepTests afterAttrs
               else flatAttrs ++ processOutsidePHP keepTests afterAttrs

-- Pattern 3: @test annotation (in comment/docblock)
processOutsidePHP keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && (tToken t2 == "test" || tToken t2 == "dataProvider" || tToken t2 == "depends" || tToken t2 == "before" || tToken t2 == "after")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 4: function test* or lifecycle methods
processOutsidePHP keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "function" &&
      isTokenIdentifier t2 &&
      ("test" `T.isPrefixOf` tToken t2 || tToken t2 == "setUp" || tToken t2 == "tearDown" || tToken t2 == "setUpBeforeClass" || tToken t2 == "tearDownAfterClass")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 5: class *Test*
processOutsidePHP keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 && ("Test" `T.isInfixOf` tToken t2)
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:ts) else t1 : processOutsidePHP keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
                   else processOutsidePHP keepTests remainingTokens

-- Pattern 6: class ... extends TestCase
processOutsidePHP keepTests (t1:t2:t3:t4:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      (isTokenKeyword t3 || isTokenIdentifier t3) && tToken t3 == "extends" &&
      isTokenIdentifier t4 && ("TestCase" `T.isSuffixOf` tToken t4)
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsidePHP keepTests (t2:t3:t4:ts) else t1 : processOutsidePHP keepTests (t2:t3:t4:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : signatureTokens ++ bodyTokens ++ processOutsidePHP keepTests remainingTokens
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
-- | Extracts a Swift attribute `@...`
extractSwiftAttribute :: [Token] -> Maybe ([Token], [Token])
extractSwiftAttribute (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" && isTokenIdentifier t2 =
        case ts of
            (t3:ts') | isTokenBracket t3 && tToken t3 == "(" ->
                let (insideTokens, remaining) = processInsideBrackets "(" ")" 1 ts'
                in Just (t1 : t2 : t3 : insideTokens, remaining)
            _ -> Just (t1 : t2 : [], ts)
extractSwiftAttribute _ = Nothing

-- | Checks if a Swift attribute is test-related
isSwiftTestAttribute :: [Token] -> Bool
isSwiftTestAttribute = any (\t -> isTokenIdentifier t && (tToken t == "Test" || tToken t == "Suite"))

-- | Collects contiguous Swift attributes
collectSwiftAttributes :: [Token] -> ([[Token]], [Token])
collectSwiftAttributes ts = case extractSwiftAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectSwiftAttributes rest
        in (attr : more, final)
    Nothing -> ([], ts)

-- | (Swift) Helper: Processes tokens *outside* a test function.
--
-- Swift and Objective-C use XCTest framework and Swift Testing framework.
processOutsideSwift :: Bool -> [Token] -> [Token]
processOutsideSwift _ [] = [] -- End of stream

-- Pattern 1: Swift Testing Attributes @Test, @Suite
processOutsideSwift keepTests tokens@(t1:ts)
    | isTokenOperator t1 && tToken t1 == "@" =
        let (attrs, afterAttrs) = collectSwiftAttributes tokens
            flatAttrs = concat attrs
        in if not (null attrs) && any isSwiftTestAttribute attrs
           then
               case findOpeningBraceBounded 200 afterAttrs of
                   Nothing ->
                       if keepTests
                       then processOutsideSwift keepTests afterAttrs
                       else flatAttrs ++ processOutsideSwift keepTests afterAttrs
                   Just (sigTokens, tokensAfterBrace) ->
                       let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                       in if keepTests
                          then flatAttrs ++ sigTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                          else processOutsideSwift keepTests remainingTokens
           else if null attrs then
               if keepTests
               then processOutsideSwift keepTests ts
               else t1 : processOutsideSwift keepTests ts
           else
               if keepTests
               then processOutsideSwift keepTests afterAttrs
               else flatAttrs ++ processOutsideSwift keepTests afterAttrs

-- Pattern 2: func test*, setUp, tearDown
processOutsideSwift keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "func" &&
      isTokenIdentifier t2 &&
      ("test" `T.isPrefixOf` tToken t2 || tToken t2 == "setUp" || tToken t2 == "tearDown" || tToken t2 == "setUpWithError" || tToken t2 == "tearDownWithError")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:ts) else t1 : processOutsideSwift keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 3: class *Test* or struct *Test*
processOutsideSwift keepTests (t1:t2:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && (tToken t1 == "class" || tToken t1 == "struct" || tToken t1 == "extension") &&
      isTokenIdentifier t2 && "Test" `T.isInfixOf` tToken t2
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:ts) else t1 : processOutsideSwift keepTests (t2:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 4: class ... : XCTestCase
processOutsideSwift keepTests (t1:t2:t3:t4:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class" &&
      isTokenIdentifier t2 &&
      isTokenOperator t3 && tToken t3 == ":" &&
      isTokenIdentifier t4 && tToken t4 == "XCTestCase"
    =
        case findOpeningBraceBounded 200 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:t3:t4:ts) else t1 : processOutsideSwift keepTests (t2:t3:t4:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                   else processOutsideSwift keepTests remainingTokens

-- Pattern 5: Objective-C - (void)testSomething
processOutsideSwift keepTests (t1:t2:t3:t4:t5:ts)
    | isTokenOperator t1 && (tToken t1 == "-" || tToken t1 == "+") &&
      isTokenBracket t2 && tToken t2 == "(" &&
      isTokenBracket t4 && tToken t4 == ")" &&
      isTokenIdentifier t5 && ("test" `T.isPrefixOf` tToken t5 || tToken t5 == "setUp" || tToken t5 == "tearDown")
    =
        case findOpeningBraceBounded 100 ts of
            Nothing ->
                if keepTests then processOutsideSwift keepTests (t2:t3:t4:t5:ts) else t1 : processOutsideSwift keepTests (t2:t3:t4:t5:ts)
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : t4 : t5 : signatureTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
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
--   1. test_that("...") - testthat
--   2. describe("...") - testthat
--   3. context("...") - testthat
--   4. it("...") - testthat
--   5. test_dir/test_file
--
-- R uses testthat framework.
processOutsideR :: Bool -> [Token] -> [Token]
processOutsideR _ [] = [] -- End of stream

-- Pattern 1: test_that( or describe( or context( or it( etc.
processOutsideR keepTests (t1:ts)
    | isTokenIdentifier t1 &&
      (tToken t1 == "test_that" || tToken t1 == "describe" || tToken t1 == "context" ||
       tToken t1 == "it" || tToken t1 == "test_dir" || tToken t1 == "test_file")
    =
        case findOpeningBracketBounded "(" 20 ts of
            Nothing ->
                if keepTests then t1 : processOutsideR keepTests ts else t1 : processOutsideR keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterBrace
                in if keepTests
                   then t1 : signatureTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
                   else processOutsideR keepTests remainingTokens

-- Pattern 2: Fallback for when test_that is tokenized as test, _, that
processOutsideR keepTests (t1:t2:t3:ts)
    | isTokenIdentifier t1 && tToken t1 == "test" &&
      (isTokenOperator t2 || isTokenIdentifier t2) && tToken t2 == "_" &&
      isTokenIdentifier t3 && (tToken t3 == "that" || tToken t3 == "dir" || tToken t3 == "file")
    =
        case findOpeningBracketBounded "(" 20 ts of
            Nothing ->
                if keepTests then t1 : t2 : t3 : processOutsideR keepTests ts else t1 : t2 : t3 : processOutsideR keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 tokensAfterBrace
                in if keepTests
                   then t1 : t2 : t3 : signatureTokens ++ bodyTokens ++ processOutsideR keepTests remainingTokens
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

-- Pattern 1: @test, @testset, @test_throws, etc.
processOutsideJulia keepTests (t1:t2:ts)
    | isTokenOperator t1 && tToken t1 == "@" &&
      isTokenIdentifier t2 && ("test" `T.isPrefixOf` tToken t2)
    =
        let (testTokens, remainingTokens) = collectJuliaTest ts
        in if keepTests
           then t1 : t2 : testTokens ++ processOutsideJulia keepTests remainingTokens
           else processOutsideJulia keepTests remainingTokens

-- No test found, process the current token
processOutsideJulia keepTests (t:ts) =
    if keepTests
    then processOutsideJulia keepTests ts
    else t : processOutsideJulia keepTests ts

collectJuliaTest :: [Token] -> ([Token], [Token])
collectJuliaTest [] = ([], [])
collectJuliaTest (t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "begin" || tToken t == "for") =
        let (bodyTokens, remaining) = collectJuliaBlock 1 ts
        in (t : bodyTokens, remaining)
    | isTokenBracket t && tToken t == "(" =
        let (bodyTokens, remaining) = processInsideBrackets "(" ")" 1 ts
        in (t : bodyTokens, remaining)
    | isTokenOperator t && tToken t == "@" = ([], t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "function" || tToken t == "struct" || tToken t == "mutable" || tToken t == "macro" || tToken t == "module" || tToken t == "end") = ([], t:ts)
    | otherwise =
        let (collected, remaining) = collectJuliaTest ts
        in (t : collected, remaining)

collectJuliaBlock :: Int -> [Token] -> ([Token], [Token])
collectJuliaBlock 0 ts = ([], ts)
collectJuliaBlock _ [] = ([], [])
collectJuliaBlock depth (t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "begin" || tToken t == "function" || tToken t == "struct" || tToken t == "macro" || tToken t == "let" || tToken t == "quote" || tToken t == "do" || tToken t == "if" || tToken t == "for" || tToken t == "while" || tToken t == "try") =
        let (collected, remaining) = collectJuliaBlock (depth + 1) ts
        in (t : collected, remaining)
    | (isTokenKeyword t || isTokenIdentifier t) && tToken t == "end" =
        if depth == 1
        then ([t], ts)
        else
            let (collected, remaining) = collectJuliaBlock (depth - 1) ts
            in (t : collected, remaining)
    | otherwise =
        let (collected, remaining) = collectJuliaBlock depth ts
        in (t : collected, remaining)

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

-- Pattern 1: subtest
processOutsidePerl keepTests (t1:ts)
    | isTokenIdentifier t1 && (tToken t1 == "subtest" || tToken t1 == "test")
    =
        case findOpeningBracketBounded "{" 20 ts of
            Nothing ->
                if keepTests
                then t1 : processOutsidePerl keepTests ts
                else t1 : processOutsidePerl keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
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

-- Pattern 1: let%test or let%expect_test
processOutsideOCaml keepTests (t1:t2:t3:t4:ts)
    | isTokenKeyword t1 && tToken t1 == "module" &&
      isTokenIdentifier t2 && (tToken t2 == "Test" || "Test" `T.isPrefixOf` tToken t2) &&
      isTokenOperator t3 && tToken t3 == "=" &&
      isTokenKeyword t4 && tToken t4 == "struct"
    =
        let (testTokens, remainingTokens) = collectUntilOCamlEnd ts
        in if keepTests
           then t1 : t2 : t3 : t4 : testTokens ++ processOutsideOCaml keepTests remainingTokens
           else processOutsideOCaml keepTests remainingTokens

processOutsideOCaml keepTests (t1:t2:t3:ts)
    | isTokenKeyword t1 && tToken t1 == "let" &&
      isTokenOperator t2 && tToken t2 == "%" &&
      isTokenIdentifier t3 && (tToken t3 == "test" || "test_" `T.isPrefixOf` tToken t3 || "expect_test" `T.isPrefixOf` tToken t3)
    =
        let (testTokens, remainingTokens) = collectUntilOCamlLet ts
        in if keepTests
           then t1 : t2 : t3 : testTokens ++ processOutsideOCaml keepTests remainingTokens
           else processOutsideOCaml keepTests remainingTokens

-- No test found, process the current token
processOutsideOCaml keepTests (t:ts) =
    if keepTests
    then processOutsideOCaml keepTests ts
    else t : processOutsideOCaml keepTests ts

-- ------------------------------------------------------------------

collectUntilOCamlEnd :: [Token] -> ([Token], [Token])
collectUntilOCamlEnd [] = ([], [])
collectUntilOCamlEnd (t:ts)
    | isTokenKeyword t && tToken t == "end" =
        ([t], ts)
    | otherwise =
        let (collected, remaining) = collectUntilOCamlEnd ts
        in (t : collected, remaining)

-- Erlang-Specific Implementation Helpers
-- ------------------------------------------------------------------

-- | (Erlang) Helper: Processes tokens *outside* a test function.
-- Recognizes:
--   1. *_test() - EUnit convention
--   2. *_test_() - EUnit generator convention
--
-- Erlang uses EUnit framework.

collectUntilErlangEnd :: [Token] -> ([Token], [Token])
collectUntilErlangEnd [] = ([], [])
collectUntilErlangEnd (t:ts)
    | isTokenOperator t && tToken t == "." =
        ([t], ts)
    | otherwise =
        let (collected, remaining) = collectUntilErlangEnd ts
        in (t : collected, remaining)

processOutsideErlang :: Bool -> [Token] -> [Token]
processOutsideErlang _ [] = [] -- End of stream

-- Pattern 1: *_test() or *_test_()
processOutsideErlang keepTests (t1:ts)
    | isTokenIdentifier t1 && ("_test" `T.isSuffixOf` tToken t1 || "_test_" `T.isSuffixOf` tToken t1)
    =
        let (testTokens, remainingTokens) = collectUntilErlangEnd ts
        in if keepTests
           then t1 : testTokens ++ processOutsideErlang keepTests remainingTokens
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

-- Pattern 1: suite "name": or test "name":
processOutsideNim keepTests (t1:ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && (tToken t1 == "suite" || tToken t1 == "test")
    =
        let (testTokens, remainingTokens) = collectUntilNimDef ts
        in if keepTests
           then t1 : testTokens ++ processOutsideNim keepTests remainingTokens
           else processOutsideNim keepTests remainingTokens

-- No test found, process the current token
processOutsideNim keepTests (t:ts) =
    if keepTests
    then processOutsideNim keepTests ts
    else t : processOutsideNim keepTests ts

-- ------------------------------------------------------------------

-- | (Nim) Helper: Collects tokens until the next top-level def.
collectUntilNimDef :: [Token] -> ([Token], [Token])
collectUntilNimDef [] = ([], [])
collectUntilNimDef (t:ts)
    | (isTokenKeyword t || isTokenIdentifier t) && (tToken t == "proc" || tToken t == "func" || tToken t == "iterator" || tToken t == "macro" || tToken t == "template" || tToken t == "method" || tToken t == "suite") =
        ([], t:ts)
    | otherwise =
        let (collected, remaining) = collectUntilNimDef ts
        in (t : collected, remaining)

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

-- Pattern 1: (deftest ...) or (testing ...)
processOutsideClojure keepTests (t1:t2:ts)
    | isTokenBracket t1 && tToken t1 == "(" &&
      isTokenIdentifier t2 && (tToken t2 == "deftest" || tToken t2 == "testing")
    =
        let (bodyTokens, remainingTokens) = processInsideBrackets "(" ")" 1 ts
        in if keepTests
           then t1 : t2 : bodyTokens ++ processOutsideClojure keepTests remainingTokens
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

-- Pattern 1: unittest { ... }
processOutsideD keepTests (t1:ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "unittest"
    =
        case findOpeningBracketBounded "{" 20 ts of
            Nothing ->
                if keepTests
                then t1 : processOutsideD keepTests ts
                else t1 : processOutsideD keepTests ts
            Just (signatureTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
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


-- | (OCaml) Helper: Collects tokens until the next top-level let.
collectUntilOCamlLet :: [Token] -> ([Token], [Token])
collectUntilOCamlLet [] = ([], [])
collectUntilOCamlLet (t:ts)
    | isTokenKeyword t && tToken t == "let" =
        ([], t:ts)
    | otherwise =
        let (collected, remaining) = collectUntilOCamlLet ts
        in (t : collected, remaining)
