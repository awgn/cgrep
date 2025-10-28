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
