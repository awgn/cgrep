{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Fsharp () where

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
import Data.Text (Text)
import qualified Data.Text as T

instance LanguageTestFilter 'Fsharp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideFsharp keepTests initialLineState tokens

-- | Parsed F# attribute `[< ... >]` storing both its full token slice (including `[<` and `>]`)
-- and its inner body tokens (between `[<` and `>]`).
data FsharpAttr = FsharpAttr
    { faAllTokens :: [Token]
    , faInnerTokens :: [Token]
    }

-- | True if the stream starts with an F# attribute opener `[<` (and NOT a quotation list `[<@ ... @>]`).
isFsharpAttrOpen :: [Token] -> Bool
isFsharpAttrOpen (t1 : _)
    | isTokenOperator t1 && tToken t1 == "[<" = True
isFsharpAttrOpen (t1 : t2 : rest)
    | isTokenBracket t1
    , tToken t1 == "["
    , isTokenOperator t2
    , areAdjacentTokens t1 t2 =
        let op = tToken t2
         in (op == "<" && not (startsAdjacentAt t2 rest))
                || ("<" `T.isPrefixOf` op && not ("<@" `T.isPrefixOf` op) && op /= "<>")
  where
    startsAdjacentAt prev (n : _) =
        isTokenOperator n && "@" `T.isPrefixOf` tToken n && areAdjacentTokens prev n
    startsAdjacentAt _ [] = False
isFsharpAttrOpen _ = False

-- | True if @a@ and @b@ close an F# attribute (`>]`, or `>>]` in `--strict` mode when closing a generic attribute).
isFsharpAttrClose2 :: Token -> Token -> Bool
isFsharpAttrClose2 a b =
    isTokenOperator a
        && ">" `T.isSuffixOf` tToken a
        && isTokenBracket b
        && tToken b == "]"
        && areAdjacentTokens a b

isFsharpAttrClose1 :: Token -> Bool
isFsharpAttrClose1 t = isTokenOperator t && tToken t == ">]"

-- | Extracts a single F# attribute `[< ... >]`, balancing nested parentheses/brackets/braces.
extractFsharpAttribute :: [Token] -> Maybe (FsharpAttr, [Token])
extractFsharpAttribute toks@(t1 : ts)
    | isTokenOperator t1 && tToken t1 == "[<" =
        finishAttr [t1] ts
    | isFsharpAttrOpen toks
    , (b1 : b2 : restOpen) <- toks =
        finishAttr [b1, b2] restOpen
  where
    finishAttr openToks afterOpen =
        let (inside, rest) = consumeFsharpAttrBody afterOpen
         in case rest of
                (c : rs)
                    | isFsharpAttrClose1 c ->
                        Just (FsharpAttr (openToks ++ inside ++ [c]) inside, rs)
                (c1 : c2 : rs)
                    | isFsharpAttrClose2 c1 c2 ->
                        Just (FsharpAttr (openToks ++ inside ++ [c1, c2]) inside, rs)
                _ -> Nothing
extractFsharpAttribute _ = Nothing

consumeFsharpAttrBody :: [Token] -> ([Token], [Token])
consumeFsharpAttrBody [] = ([], [])
consumeFsharpAttrBody (t : rest)
    | isFsharpAttrClose1 t = ([], t : rest)
consumeFsharpAttrBody (t1 : t2 : rest)
    | isFsharpAttrClose2 t1 t2 = ([], t1 : t2 : rest)
consumeFsharpAttrBody (t : rest)
    | isTokenBracket t && tToken t == "(" =
        let (paren, afterParen) = processInsideBrackets "(" ")" 1 rest
            (more, final) = consumeFsharpAttrBody afterParen
         in (t : paren ++ more, final)
    | isTokenBracket t && tToken t == "[" =
        let (brack, afterBrack) = processInsideBrackets "[" "]" 1 rest
            (more, final) = consumeFsharpAttrBody afterBrack
         in (t : brack ++ more, final)
    | isTokenBracket t && tToken t == "{" =
        let (brace, afterBrace) = processInsideBrackets "{" "}" 1 rest
            (more, final) = consumeFsharpAttrBody afterBrace
         in (t : brace ++ more, final)
    | otherwise =
        let (more, final) = consumeFsharpAttrBody rest
         in (t : more, final)

-- | Classification of a single `;`-separated attribute entry inside `[< ... >]`.
data AttrEntryClass
    = NotTestAttr
    | DeclTestAttr
    | AssemblyOrModuleTestAttr
    deriving stock (Eq)

-- | Analyze all `;`-separated entries inside `[< ... >]`, ignoring `( ... )` arguments
-- and `< ... >` generic type arguments (so `[<Validator<Test>>]` and `[<Obsolete(nameof(Test))>]`
-- never false-positive as test attributes, and `[<assembly: SetUpFixture>]` does not swallow
-- the next top-level `let`/`type`).
classifyFsharpAttr :: FsharpAttr -> [AttrEntryClass]
classifyFsharpAttr (FsharpAttr _ inner) =
    map classifyEntry (splitSemicolon inner)
  where
    splitSemicolon :: [Token] -> [[Token]]
    splitSemicolon [] = []
    splitSemicolon ts =
        let (seg, rest) = breakAtTopSemicolon 0 ts
         in seg : case rest of
                (_ : afterSemi) -> splitSemicolon afterSemi
                [] -> []

    breakAtTopSemicolon :: Int -> [Token] -> ([Token], [Token])
    breakAtTopSemicolon _ [] = ([], [])
    breakAtTopSemicolon d (x : xs)
        | isTokenBracket x && (tToken x == "(" || tToken x == "[" || tToken x == "{") =
            let (more, r) = breakAtTopSemicolon (d + 1) xs in (x : more, r)
        | isTokenBracket x && (tToken x == ")" || tToken x == "]" || tToken x == "}") =
            let (more, r) = breakAtTopSemicolon (max 0 (d - 1)) xs in (x : more, r)
        | d == 0 && isTokenOperator x && tToken x == ";" = ([], x : xs)
        | otherwise =
            let (more, r) = breakAtTopSemicolon d xs in (x : more, r)

    classifyEntry :: [Token] -> AttrEntryClass
    classifyEntry toks =
        let (isAsmOrMod, afterTarget) = stripTargetSpecifier toks
            headerIdents = extractAttrNameIdents afterTarget
         in if any (isFsharpTestAttrName . tToken) headerIdents
                then if isAsmOrMod then AssemblyOrModuleTestAttr else DeclTestAttr
                else NotTestAttr

    stripTargetSpecifier :: [Token] -> (Bool, [Token])
    stripTargetSpecifier (tTarget : tColon : rest)
        | (isTokenIdentifier tTarget || isTokenKeyword tTarget)
        , isTokenOperator tColon && tToken tColon == ":" =
            let tgt = tToken tTarget
             in (tgt == "assembly" || tgt == "module", rest)
    stripTargetSpecifier ts = (False, ts)

    -- Keep only attribute name/namespace identifiers, skipping `(...)`, `[...]`, `{...}`,
    -- and `<...>` generic type argument lists (`Validator<Test>`).
    extractAttrNameIdents :: [Token] -> [Token]
    extractAttrNameIdents [] = []
    extractAttrNameIdents (t : ts)
        | isTokenBracket t && tToken t == "(" =
            let (_, remToks) = processInsideBrackets "(" ")" 1 ts
             in extractAttrNameIdents remToks
        | isTokenBracket t && tToken t == "[" =
            let (_, remToks) = processInsideBrackets "[" "]" 1 ts
             in extractAttrNameIdents remToks
        | isTokenBracket t && tToken t == "{" =
            let (_, remToks) = processInsideBrackets "{" "}" 1 ts
             in extractAttrNameIdents remToks
        | isTokenOperator t && "<" `T.isPrefixOf` tToken t =
            let openAngles = T.length (T.takeWhile (== '<') (tToken t))
             in extractAttrNameIdents (skipGenericAngles openAngles ts)
        | isTokenIdentifier t = t : extractAttrNameIdents ts
        | otherwise = extractAttrNameIdents ts

    skipGenericAngles :: Int -> [Token] -> [Token]
    skipGenericAngles !d ts | d <= 0 = ts
    skipGenericAngles _ [] = []
    skipGenericAngles !d (x : xs)
        | isTokenOperator x =
            let txt = tToken x
                opens = T.length (T.filter (== '<') txt)
                closes = T.length (T.filter (== '>') txt)
             in skipGenericAngles (d + opens - closes) xs
        | otherwise = skipGenericAngles d xs

isFsharpTestAttrName :: Text -> Bool
isFsharpTestAttrName rawName =
    let base = case T.stripSuffix "Attribute" rawName of
            Just stripped | not (T.null stripped) -> stripped
            _ -> rawName
     in base
            `elem` ([ -- NUnit
                      "Test"
                    , "TestFixture"
                    , "TestFixtureSource"
                    , "TestCase"
                    , "TestCaseSource"
                    , "SetUp"
                    , "TearDown"
                    , "OneTimeSetUp"
                    , "OneTimeTearDown"
                    , "SetUpFixture"
                    , -- xUnit
                      "Fact"
                    , "Theory"
                    , "SkippableFact"
                    , "SkippableTheory"
                    , "InlineData"
                    , "MemberData"
                    , "ClassData"
                    , -- FsCheck
                      "Property"
                    , "Properties"
                    , -- Expecto
                      "Tests"
                    , "FTests"
                    , "PTests"
                    , -- MSTest
                      "TestClass"
                    , "TestMethod"
                    , "DataTestMethod"
                    , "TestInitialize"
                    , "TestCleanup"
                    , "ClassInitialize"
                    , "ClassCleanup"
                    , "AssemblyInitialize"
                    , "AssemblyCleanup"
                    ] :: [Text])

collectFsharpAttributes :: [Token] -> ([FsharpAttr], [Token])
collectFsharpAttributes ts = case extractFsharpAttribute ts of
    Just (attr, rest) ->
        let (more, final) = collectFsharpAttributes rest
         in (attr : more, final)
    Nothing -> ([], ts)

-- | Check if stream starts with F# double-backtick identifier delimiter `` `` ``.
matchDoubleBacktick :: [Token] -> Maybe ([Token], [Token])
matchDoubleBacktick (t1 : rest)
    | isTokenOperator t1 && tToken t1 == "``" = Just ([t1], rest)
matchDoubleBacktick (t1 : t2 : rest)
    | isTokenOperator t1
    , tToken t1 == "`"
    , isTokenOperator t2
    , tToken t2 == "`"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], rest)
matchDoubleBacktick _ = Nothing

-- | Consume the rest of a double-backtick quoted identifier `` ``...`` `` on the same line
-- so parentheses/brackets inside `` ``should handle (parens)`` `` do not affect bracket depth.
consumeDoubleBacktickBody :: LineState -> [Token] -> ([Token], LineState, [Token])
consumeDoubleBacktickBody ls [] = ([], ls, [])
consumeDoubleBacktickBody ls toks@(t : ts) =
    let (isNewLine, _, ls1) = stepLineState ls t
     in if isNewLine
            then ([], ls, toks)
            else case matchDoubleBacktick toks of
                Just (closeTicks, afterClose) ->
                    let lsClose = advanceLineState ls closeTicks
                     in (closeTicks, lsClose, afterClose)
                Nothing ->
                    let (more, lsEnd, rest) = consumeDoubleBacktickBody ls1 ts
                     in (t : more, lsEnd, rest)

-- | Collect an F# declaration or Expecto expression body using F#'s offside rule:
--   * Continues across all lines whose indentation column is strictly greater than @baseIndent@
--     (preserving nested local functions `let f x = ...` and unattributed helper members inside
--     `[<TestFixture>] type T() = ...`).
--   * Balances `(`, `[`, `{` (ignoring brackets inside `` ``...`` `` identifiers).
--   * Stops before an unmatched closing `)`, `]`, `}` at depth 0, after top-level `;;`,
--     or at the first token of a new line at depth 0 with `lineIndent <= baseIndent`
--     (consuming a closing `end` keyword at `lineIndent == baseIndent` if present).
collectFsharpIndentedBody :: Int -> Bool -> LineState -> [Token] -> ([Token], LineState, [Token])
collectFsharpIndentedBody _ _ ls [] = ([], ls, [])
collectFsharpIndentedBody _ True ls0 toks0@(firstTok : _)
    | isTokenBracket firstTok && (tToken firstTok == ")" || tToken firstTok == "]" || tToken firstTok == "}") =
        ([], ls0, toks0)
collectFsharpIndentedBody !baseIndent True ls0 toks0@(firstTok : restToks) =
    case matchDoubleBacktick toks0 of
        Just (openTicks, afterOpen) ->
            let lsOpen = advanceLineState ls0 openTicks
                (btBody, lsBt, afterBt) = consumeDoubleBacktickBody lsOpen afterOpen
                (more, lsFinal, final) = go 0 True lsBt afterBt
             in (openTicks ++ btBody ++ more, lsFinal, final)
        Nothing ->
            let (_, _, ls1) = stepLineState ls0 firstTok
                !initDepth
                    | isTokenBracket firstTok && (tToken firstTok == "(" || tToken firstTok == "[" || tToken firstTok == "{") = 1
                    | otherwise = 0
                (more, lsFinal, final) = go initDepth True ls1 restToks
             in (firstTok : more, lsFinal, final)
  where
    go = goBody baseIndent
collectFsharpIndentedBody !baseIndent False ls0 toks0 =
    goBody baseIndent 0 False ls0 toks0

goBody :: Int -> Int -> Bool -> LineState -> [Token] -> ([Token], LineState, [Token])
goBody _ _ _ ls [] = ([], ls, [])
goBody !baseIndent !depth !onHeader ls toks@(t : ts) =
    let (isNewLine, lineIndent, lsAfterT) = stepLineState ls t
        stillHeader = onHeader && not isNewLine
     in if depth == 0 && not stillHeader && isNewLine && lineIndent <= baseIndent
            then
                if isTokenKeyword t && tToken t == "end" && lineIndent == baseIndent
                    then ([t], lsAfterT, ts)
                    else ([], ls, toks)
            else
                if depth == 0 && isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}")
                    then ([], ls, toks)
                    else
                        if depth == 0 && isDoubleSemicolon toks
                            then consumeDoubleSemicolon ls toks
                            else case matchDoubleBacktick toks of
                                Just (openTicks, afterOpen) ->
                                    let lsOpen = advanceLineState ls openTicks
                                        (btBody, lsBt, afterBt) = consumeDoubleBacktickBody lsOpen afterOpen
                                        (more, lsFinal, final) = goBody baseIndent depth stillHeader lsBt afterBt
                                     in (openTicks ++ btBody ++ more, lsFinal, final)
                                Nothing ->
                                    let !nextDepth
                                            | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") = depth + 1
                                            | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") = max 0 (depth - 1)
                                            | otherwise = depth
                                        (more, lsFinal, final) = goBody baseIndent nextDepth stillHeader lsAfterT ts
                                     in (t : more, lsFinal, final)
  where
    isDoubleSemicolon (s1 : _)
        | isTokenOperator s1 && tToken s1 == ";;" = True
    isDoubleSemicolon (s1 : s2 : _)
        | isTokenOperator s1
        , tToken s1 == ";"
        , isTokenOperator s2
        , tToken s2 == ";"
        , areAdjacentTokens s1 s2 =
            True
    isDoubleSemicolon _ = False

    consumeDoubleSemicolon ls1 (s1 : rest)
        | isTokenOperator s1 && tToken s1 == ";;" =
            ([s1], advanceLineState ls1 [s1], rest)
    consumeDoubleSemicolon ls1 (s1 : s2 : rest) =
        ([s1, s2], advanceLineState ls1 [s1, s2], rest)
    consumeDoubleSemicolon ls1 rest = ([], ls1, rest)

-- | Expecto combinator classification.
data ExpectoKind
    = ExpectoAmbiguous -- `test`, `ftest`, `ptest` (must be followed directly by string or `{`)
    | ExpectoNamed -- `testCase`, `testList`, `testTask`, `testProperty`, ... (string or `("..."`)
    | ExpectoConfigFirst -- `testPropertyWithConfig`, `ftestPropertyWithConfig`, `ptestPropertyWithConfig`
    | ExpectoWrapper -- `testSequenced`, `testSequencedGroup`, `testParam`

classifyExpectoName :: Text -> Maybe ExpectoKind
classifyExpectoName n
    | n `elem` (["test", "ftest", "ptest"] :: [Text]) =
        Just ExpectoAmbiguous
    | n
        `elem` ([ "testCase"
                , "ftestCase"
                , "ptestCase"
                , "testCaseAsync"
                , "ftestCaseAsync"
                , "ptestCaseAsync"
                , "testList"
                , "ftestList"
                , "ptestList"
                , "testAsync"
                , "ftestAsync"
                , "ptestAsync"
                , "testTask"
                , "ftestTask"
                , "ptestTask"
                , "testProperty"
                , "ftestProperty"
                , "ptestProperty"
                , "etestProperty"
                , "testTheory"
                , "ftestTheory"
                , "ptestTheory"
                , "testTheoryAsync"
                , "testTheoryTask"
                ] :: [Text]) =
        Just ExpectoNamed
    | n `elem` (["testPropertyWithConfig", "ftestPropertyWithConfig", "ptestPropertyWithConfig"] :: [Text]) =
        Just ExpectoConfigFirst
    | n `elem` (["testSequenced", "testSequencedGroup", "testParam"] :: [Text]) =
        Just ExpectoWrapper
    | otherwise = Nothing

-- | Match an Expecto test starter (optionally qualified with `Expecto.` / `Tests.`),
-- returning @(starterPrefixTokens, remainingTokensAfterStarter)@.
matchExpectoStarter :: [Token] -> Maybe ([Token], [Token])
matchExpectoStarter toks = do
    (qualToks, identTok, afterIdent) <- splitQualifiedExpecto toks
    kind <- classifyExpectoName (tToken identTok)
    if validExpectoArgs kind afterIdent
        then Just (qualToks ++ [identTok], afterIdent)
        else Nothing
  where
    splitQualifiedExpecto :: [Token] -> Maybe ([Token], Token, [Token])
    splitQualifiedExpecto (q1 : d1 : q2 : d2 : fn : rest)
        | isTokenIdentifier q1 && tToken q1 == "Expecto"
        , isTokenOperator d1 && tToken d1 == "."
        , isTokenIdentifier q2 && tToken q2 == "Tests"
        , isTokenOperator d2 && tToken d2 == "."
        , isTokenIdentifier fn =
            Just ([q1, d1, q2, d2], fn, rest)
    splitQualifiedExpecto (q1 : d1 : fn : rest)
        | isTokenIdentifier q1 && (tToken q1 == "Expecto" || tToken q1 == "Tests")
        , isTokenOperator d1 && tToken d1 == "."
        , isTokenIdentifier fn =
            Just ([q1, d1], fn, rest)
    splitQualifiedExpecto (fn : rest)
        | isTokenIdentifier fn = Just ([], fn, rest)
    splitQualifiedExpecto _ = Nothing

    validExpectoArgs :: ExpectoKind -> [Token] -> Bool
    validExpectoArgs ExpectoAmbiguous (t2 : _) =
        isTokenString t2 || (isTokenBracket t2 && tToken t2 == "{")
    validExpectoArgs ExpectoAmbiguous [] = False
    validExpectoArgs ExpectoNamed (t2 : t3 : _)
        | isTokenBracket t2 && tToken t2 == "(" && isTokenString t3 = True
    validExpectoArgs ExpectoNamed (t2 : _) =
        isTokenString t2 || (isTokenBracket t2 && tToken t2 == "{")
    validExpectoArgs ExpectoNamed [] = False
    validExpectoArgs ExpectoConfigFirst afterIdent@(t2 : _)
        | isTokenOperator t2 && (tToken t2 == "=" || tToken t2 == ":") = False
        | otherwise = any isTokenString (take 12 afterIdent)
    validExpectoArgs ExpectoConfigFirst [] = False
    validExpectoArgs ExpectoWrapper (t2 : rest)
        | isTokenOperator t2 && (tToken t2 == "=" || tToken t2 == ":") = False
        | isTokenString t2 || (isTokenBracket t2 && (tToken t2 == "(" || tToken t2 == "[")) = True
        | isTokenOperator t2 && (tToken t2 == "<|" || tToken t2 == "<") = True
        | Just _ <- matchExpectoStarter (t2 : rest) = True
    validExpectoArgs ExpectoWrapper _ = False

-- | Match `let [rec|inline|private|internal|public] <name> = <ExpectoStarter> ...`
-- so unattributed Expecto suite bindings (`let expectoTests = testList "..." [...]`)
-- include their `let name =` header cleanly.
matchLetExpectoBinding :: LineState -> [Token] -> Maybe ([Token], LineState, [Token])
matchLetExpectoBinding ls0 toks@(tLet : rest0)
    | isTokenKeyword tLet && tToken tLet == "let" =
        let (_, baseIndent, _) = stepLineState ls0 tLet
            afterMods = dropWhile isLetModifier rest0
         in case consumeBindingName afterMods of
                Just (afterName)
                    | (tEq : afterEq) <- skipOptionalReturnType afterName
                    , isTokenOperator tEq && tToken tEq == "="
                    , Just _ <- matchExpectoStarter afterEq ->
                        let (body, lsEnd, remaining) = collectFsharpIndentedBody baseIndent True ls0 toks
                         in Just (body, lsEnd, remaining)
                _ -> Nothing
  where
    isLetModifier t =
        isTokenKeyword t
            && (tToken t `elem` (["rec", "inline", "private", "internal", "public"] :: [Text]))

    consumeBindingName (n : rs)
        | isTokenIdentifier n = Just rs
    consumeBindingName ts = do
        (_, afterOpen) <- matchDoubleBacktick ts
        let (_, _, afterBt) = consumeDoubleBacktickBody initialLineState afterOpen
        pure afterBt

    skipOptionalReturnType (c : rs)
        | isTokenOperator c && tToken c == ":" =
            dropWhile (\x -> not (isTokenOperator x && tToken x == "=")) rs
    skipOptionalReturnType rs = rs
matchLetExpectoBinding _ _ = Nothing

-- | Match an F# attribute stack `[< ... >]+` and, if any entry is a test attribute,
-- consume its target declaration using offside indentation.
matchAttributedConstruct :: LineState -> [Token] -> Maybe (Bool, [Token], LineState, [Token])
matchAttributedConstruct ls0 tokens@(firstAttrTok : _)
    | isFsharpAttrOpen tokens
    , (attrs@(_ : _), afterAttrs) <- collectFsharpAttributes tokens =
        let (_, attrIndent, _) = stepLineState ls0 firstAttrTok
            flatAttrs = concatMap faAllTokens attrs
            lsAfterAttrs = advanceLineState ls0 flatAttrs
            classes = concatMap classifyFsharpAttr attrs
            hasDeclTest = DeclTestAttr `elem` classes
            hasAsmTest = AssemblyOrModuleTestAttr `elem` classes
         in if hasDeclTest
                then case afterAttrs of
                    (firstDecl : _) ->
                        let (_, declIndent, _) = stepLineState lsAfterAttrs firstDecl
                            baseIndent = min attrIndent declIndent
                            (body, lsEnd, remaining) = collectFsharpIndentedBody baseIndent True lsAfterAttrs afterAttrs
                         in Just (True, flatAttrs ++ body, lsEnd, remaining)
                    [] -> Just (True, flatAttrs, lsAfterAttrs, [])
                else
                    if hasAsmTest
                        then case afterAttrs of
                            (tDo : _)
                                | isTokenKeyword tDo && tToken tDo == "do" ->
                                    let (_, doIndent, _) = stepLineState lsAfterAttrs tDo
                                        (doBody, lsEnd, remaining) = collectFsharpIndentedBody (min attrIndent doIndent) True lsAfterAttrs afterAttrs
                                     in Just (True, flatAttrs ++ doBody, lsEnd, remaining)
                            _ -> Just (True, flatAttrs, lsAfterAttrs, afterAttrs)
                        else Just (False, flatAttrs, lsAfterAttrs, afterAttrs)
matchAttributedConstruct _ _ = Nothing

-- | Match a standalone Expecto combinator call.
matchStandaloneExpecto :: LineState -> [Token] -> Maybe ([Token], LineState, [Token])
matchStandaloneExpecto ls0 tokens@(firstTok : _) = do
    (starterToks, afterStarter) <- matchExpectoStarter tokens
    let (_, baseIndent, _) = stepLineState ls0 firstTok
        lsAfterStarter = advanceLineState ls0 starterToks
        (namePrefix, lsAfterName, afterName) = case afterStarter of
            (s : rs) | isTokenString s -> ([s], advanceLineState lsAfterStarter [s], rs)
            _ -> ([], lsAfterStarter, afterStarter)
    case afterName of
        (b : rs)
            | isTokenBracket b && tToken b == "{" ->
                let (braceBody, final) = processInsideBrackets "{" "}" 1 rs
                    consumed = b : braceBody
                 in Just (starterToks ++ namePrefix ++ consumed, advanceLineState lsAfterName consumed, final)
        (b : rs)
            | isTokenBracket b && tToken b == "[" ->
                let (listBody, final) = processInsideBrackets "[" "]" 1 rs
                    consumed = b : listBody
                 in Just (starterToks ++ namePrefix ++ consumed, advanceLineState lsAfterName consumed, final)
        _ ->
            let (body, lsEnd, final) = collectFsharpIndentedBody baseIndent True lsAfterName afterName
             in Just (starterToks ++ namePrefix ++ body, lsEnd, final)
matchStandaloneExpecto _ [] = Nothing

processOutsideFsharp :: Bool -> LineState -> [Token] -> [Token]
processOutsideFsharp _ _ [] = []
processOutsideFsharp keepTests ls0 tokens@(t : ts) =
    case matchAttributed tokens of
        Just (isTest, blockToks, lsNext, remaining) ->
            if keepTests
                then
                    if isTest
                        then blockToks ++ processOutsideFsharp keepTests lsNext remaining
                        else processOutsideFsharp keepTests lsNext remaining
                else
                    if isTest
                        then processOutsideFsharp keepTests lsNext remaining
                        else blockToks ++ processOutsideFsharp keepTests lsNext remaining
        Nothing ->
            case matchLetExpectoBinding ls0 tokens <|> matchStandaloneExpecto ls0 tokens of
                Just (testToks, lsNext, remaining) ->
                    if keepTests
                        then testToks ++ processOutsideFsharp keepTests lsNext remaining
                        else processOutsideFsharp keepTests lsNext remaining
                Nothing ->
                    let (_, _, ls1) = stepLineState ls0 t
                     in if keepTests
                            then processOutsideFsharp keepTests ls1 ts
                            else t : processOutsideFsharp keepTests ls1 ts
  where
    matchAttributed = matchAttributedConstruct ls0
