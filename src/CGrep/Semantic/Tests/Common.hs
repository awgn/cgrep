{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuasiQuotes #-}

module CGrep.Semantic.Tests.Common (
    LanguageTestFilter (..),
    runLangFilter,
    inTestSuiteDir,
    areAdjacentTokens,
    LineState (..),
    initialLineState,
    stepLineState,
    nextLineState,
    advanceLineState,
    findOpeningBracketBounded,
    findOpeningBraceBounded,
    findOpeningBraceC,
    findOpeningBrace,
    processInsideBrackets,
    processInsideBraces,
) where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    tToken,
 )
import Data.Bits (complement, (.&.))
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI
import System.OsPath (OsPath, osp, splitDirectories, takeFileName)

class LanguageTestFilter (lang :: FileType) where
    isTestFile :: OsPath -> Bool
    isTestFile _ = False

    langFilter :: Maybe Bool -> [Token] -> [Token]

inTestSuiteDir :: OsPath -> Bool
inTestSuiteDir path =
    let parts = splitDirectories path
        dirs = case reverse parts of
            (_ : ds) -> ds
            [] -> []
     in any (\d -> takeFileName d == [osp|test_suite|]) dirs

runLangFilter :: forall (lang :: FileType). (LanguageTestFilter lang) => OsPath -> Maybe Bool -> [Token] -> [Token]
runLangFilter _ Nothing tokens = tokens
runLangFilter path flag@(Just keepTests) tokens
    | not (inTestSuiteDir path) && isTestFile @lang path =
        if keepTests then tokens else []
    | otherwise =
        langFilter @lang flag tokens

-- | Checks in O(1) whether `t2` immediately follows `t1` with zero intervening bytes.
areAdjacentTokens :: Token -> Token -> Bool
areAdjacentTokens t1 t2 =
    case (tToken t1, tToken t2) of
        (TI.Text _ off1 len1, TI.Text _ off2 _) -> off1 + len1 == off2
{-# INLINE areAdjacentTokens #-}

-- | Tracks byte position and leading visual indentation column of the current line
-- across the token stream in O(fileSize) total work with zero allocation.
data LineState = LineState
    { lsPrevEnd :: {-# UNPACK #-} !Int
    , lsLineIndent :: {-# UNPACK #-} !Int
    }

initialLineState :: LineState
initialLineState = LineState (-1) 0
{-# INLINE initialLineState #-}

-- | Advance 'LineState' to token @t@, returning @(isFirstTokenOnLine, lineIndentCol, nextState)@.
stepLineState :: LineState -> Token -> (Bool, Int, LineState)
stepLineState (LineState prevEnd prevIndent) t =
    case tToken t of
        TI.Text arr off len
            | off >= 0 ->
                let scanFloor = max 0 prevEnd
                    nlIdx = findNewlineBack arr (off - 1) scanFloor
                    !nextEnd = max prevEnd (off + len)
                 in if prevEnd >= 0 && nlIdx < scanFloor
                        then (False, prevIndent, LineState nextEnd prevIndent)
                        else
                            let !lineStart =
                                    if nlIdx >= 0
                                        then nlIdx + 1
                                        else case findNewlineBack arr (off - 1) 0 of
                                            k | k >= 0 -> k + 1
                                            _ -> 0
                                !col = computeVisualCol arr lineStart off 0
                             in (True, col, LineState nextEnd col)
        _ -> (False, prevIndent, LineState prevEnd prevIndent)
  where
    findNewlineBack :: A.Array -> Int -> Int -> Int
    findNewlineBack !arr !i !low
        | i < low = low - 1
        | A.unsafeIndex arr i == 0x0A = i
        | otherwise = findNewlineBack arr (i - 1) low

    computeVisualCol :: A.Array -> Int -> Int -> Int -> Int
    computeVisualCol !arr !j !limit !col
        | j >= limit = col
        | otherwise =
            let !b = A.unsafeIndex arr j
                !col'
                    | b == 0x09 = (col + 4) .&. complement 3
                    | b == 0x02 || b == 0x03 || b == 0x0D = col
                    | (b .&. 0xC0) == 0x80 = col
                    | otherwise = col + 1
             in computeVisualCol arr (j + 1) limit col'
{-# INLINE stepLineState #-}

nextLineState :: LineState -> Token -> LineState
nextLineState !ls t = let !(_, _, !ls') = stepLineState ls t in ls'
{-# INLINE nextLineState #-}

advanceLineState :: LineState -> [Token] -> LineState
advanceLineState = foldl' nextLineState
{-# INLINE advanceLineState #-}

-- | Searches for an opening token (e.g., "{") within a lookahead limit.
findOpeningBracketBounded :: T.Text -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBracketBounded _ 0 _ = Nothing
findOpeningBracketBounded _ _ [] = Nothing
findOpeningBracketBounded openBracket n (t : ts)
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
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go depth n (t : ts)
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
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | Helper to find the first opening brace, returning tokens before/at brace, and tokens after.
-- Returns (tokens_including_brace, tokens_after_brace)
findOpeningBrace :: [Token] -> Maybe ([Token], [Token])
findOpeningBrace = go []
  where
    go :: [Token] -> [Token] -> Maybe ([Token], [Token])
    go _ [] = Nothing -- Reached end without finding a brace
    go acc (t : ts)
        | isTokenBracket t && tToken t == "{" = Just (reverse (t : acc), ts)
        | otherwise = go (t : acc) ts

-- | Processes tokens by balancing arbitrary brackets
processInsideBrackets :: T.Text -> T.Text -> Int -> [Token] -> ([Token], [Token])
processInsideBrackets _ _ 0 ts = ([], ts)
processInsideBrackets _ _ _ [] = ([], [])
processInsideBrackets open close nestingLevel (t : ts)
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
processInsideBraces nestingLevel (t : ts)
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
