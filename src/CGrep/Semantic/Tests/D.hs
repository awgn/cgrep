{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.D () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    tToken,
 )
import CGrep.Semantic.Tests.Common
import Data.Text (Text)

instance LanguageTestFilter 'D where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideD keepTests True [] tokens

-- | Known standalone test UDAs from `d-unit` and `unit-threaded`.
-- Note: plain string UDAs like `@("desc")` are NOT matched here so production
-- serialization UDAs (`@("json_key")`) are never misclassified; `@("desc")`
-- attached to `unittest { ... }` is captured via the declaration prefix buffer.
isDTestUdaName :: Text -> Bool
isDTestUdaName n =
    n
        `elem` ([ -- d-unit
                  "Test"
                , "Before"
                , "After"
                , "BeforeEach"
                , "AfterEach"
                , "BeforeAll"
                , "AfterAll"
                , -- unit-threaded
                  "UnitTest"
                , "ShouldFail"
                , "ShouldFailWith"
                ] :: [Text])

-- | Split tokens at top-level commas (outside `(...)`, `[...]`, `{...}`).
splitTopLevelCommas :: [Token] -> [[Token]]
splitTopLevelCommas = go 0 []
  where
    go :: Int -> [Token] -> [Token] -> [[Token]]
    go _ cur [] = [reverse cur | not (null cur)]
    go !depth cur (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
            go (depth + 1) (t : cur) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
            go (max 0 (depth - 1)) (t : cur) ts
        | depth == 0 && isTokenOperator t && tToken t == "," =
            let restItems = go 0 [] ts
             in if null cur then restItems else reverse cur : restItems
        | otherwise =
            go depth (t : cur) ts

-- | Strip template instantiation arguments `!(...)` / `!Ident` and call arguments `(...)`
-- from a UDA item so arguments like `@Validator!(Test)` or `@MyAttr(Test)` are never
-- mistaken for the UDA's own name.
stripDUdaArgs :: [Token] -> [Token]
stripDUdaArgs [] = []
stripDUdaArgs (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "!"
    , isTokenBracket t2 && tToken t2 == "(" =
        let (_, remaining) = processInsideBrackets "(" ")" 1 ts
         in stripDUdaArgs remaining
    | isTokenOperator t1 && tToken t1 == "!"
    , isTokenIdentifier t2 || isTokenKeyword t2 =
        stripDUdaArgs ts
stripDUdaArgs (t : ts)
    | isTokenBracket t && tToken t == "(" =
        let (_, remaining) = processInsideBrackets "(" ")" 1 ts
         in stripDUdaArgs remaining
    | otherwise = t : stripDUdaArgs ts

-- | Extract a D User-Defined Attribute (`@(...)` or `@Ident[.Ident]*[!(...)][(...)]`),
-- returning `(udaTokens, isStandaloneTestUda, remainingTokens)`.
extractDUda :: [Token] -> Maybe ([Token], Bool, [Token])
extractDUda (tAt : tOpen : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
            inner = case reverse insideParens of
                (lastTok : revMid)
                    | isTokenBracket lastTok && tToken lastTok == ")" -> reverse revMid
                _ -> insideParens
            isTupleTestItem itemToks = case itemToks of
                (firstTok : _)
                    | isTokenIdentifier firstTok ->
                        let idents = filter isTokenIdentifier (stripDUdaArgs itemToks)
                         in case reverse idents of
                                (lastId : _) -> isDTestUdaName (tToken lastId)
                                [] -> False
                _ -> False
            udaIsTest = any isTupleTestItem (splitTopLevelCommas inner)
         in Just (tAt : tOpen : insideParens, udaIsTest, remaining)
extractDUda (tAt : tIdent : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , isTokenIdentifier tIdent =
        let (dotToks, lastIdent, afterDots) = collectDotChain (tToken tIdent) ts
            (tmplToks, afterTmpl) = collectTemplateArgs afterDots
            (callToks, afterCall) = collectCallArgs afterTmpl
            udaIsTest = isDTestUdaName lastIdent
         in Just (tAt : tIdent : dotToks ++ tmplToks ++ callToks, udaIsTest, afterCall)
  where
    collectDotChain :: Text -> [Token] -> ([Token], Text, [Token])
    collectDotChain _ (d : i : rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (moreToks, finalLast, finalRest) = collectDotChain (tToken i) rest
             in (d : i : moreToks, finalLast, finalRest)
    collectDotChain curLast rest = ([], curLast, rest)

    collectTemplateArgs :: [Token] -> ([Token], [Token])
    collectTemplateArgs (b : openP : rest)
        | isTokenOperator b && tToken b == "!"
        , isTokenBracket openP && tToken openP == "(" =
            let (inside, after) = processInsideBrackets "(" ")" 1 rest
             in (b : openP : inside, after)
    collectTemplateArgs (b : singleArg : rest)
        | isTokenOperator b && tToken b == "!"
        , isTokenIdentifier singleArg || isTokenKeyword singleArg =
            ([b, singleArg], rest)
    collectTemplateArgs rest = ([], rest)

    collectCallArgs :: [Token] -> ([Token], [Token])
    collectCallArgs (openP : rest)
        | isTokenBracket openP && tToken openP == "(" =
            let (inside, after) = processInsideBrackets "(" ")" 1 rest
             in (openP : inside, after)
    collectCallArgs rest = ([], rest)
extractDUda _ = Nothing

-- | Match D attributes/modifiers that can precede `unittest { ... }` or `@Test`
-- declarations (`public`, `static`, `pure`, `nothrow`, `deprecated("...")`, `extern(D)`, etc.).
-- Note: `const(...)`, `immutable(...)`, `inout(...)`, `shared(...)` with parentheses are
-- D type constructors (return types), NOT declaration modifiers, so they are excluded.
matchDPrefixModifier :: [Token] -> Maybe ([Token], [Token])
matchDPrefixModifier (t : tOpen : ts)
    | (isTokenKeyword t || isTokenIdentifier t)
    , tToken t `elem` (["package", "deprecated", "extern", "align", "pragma", "debug"] :: [Text])
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (inside, remaining) = processInsideBrackets "(" ")" 1 ts
         in Just (t : tOpen : inside, remaining)
matchDPrefixModifier (t : ts)
    | isTokenKeyword t || isTokenIdentifier t
    , tToken t `elem` (["const", "immutable", "inout", "shared"] :: [Text]) =
        case ts of
            (nextTok : _) | isTokenBracket nextTok && tToken nextTok == "(" -> Nothing
            _ -> Just ([t], ts)
    | isTokenKeyword t || isTokenIdentifier t
    , tToken t
        `elem` ([ "public"
                , "private"
                , "protected"
                , "package"
                , "export"
                , "static"
                , "final"
                , "abstract"
                , "override"
                , "pure"
                , "nothrow"
                , "__gshared"
                , "scope"
                , "synchronized"
                , "deprecated"
                , "extern"
                , "align"
                , "debug"
                ] :: [Text]) =
        Just ([t], ts)
matchDPrefixModifier _ = Nothing

-- | Consume all tokens until the end of the current braced scope (stopping BEFORE an
-- unmatched closing `}`) or module EOF `[]`, for `version(unittest):` and `@Test:`.
consumeUntilScopeEnd :: Int -> [Token] -> ([Token], [Token])
consumeUntilScopeEnd _ [] = ([], [])
consumeUntilScopeEnd !depth (t : ts)
    | isTokenBracket t && tToken t == "{" =
        let (more, rest) = consumeUntilScopeEnd (depth + 1) ts
         in (t : more, rest)
    | isTokenBracket t && tToken t == "}" =
        if depth == 0
            then ([], t : ts)
            else
                let (more, rest) = consumeUntilScopeEnd (depth - 1) ts
                 in (t : more, rest)
    | otherwise =
        let (more, rest) = consumeUntilScopeEnd depth ts
         in (t : more, rest)

-- | Consume a single D declaration or statement (for `version(unittest) <decl>` and
-- `@Test <decl>`), balancing `(...)`/`[...]` so semicolons inside expression contracts
-- (`out (r; r > 0)`) do not terminate early, and following classic block contracts
-- (`in { ... } out (r) { ... } do { ... }`) through to the function body `{ ... }`.
consumeSingleDDeclOrStmt :: [Token] -> ([Token], [Token])
consumeSingleDDeclOrStmt = go 0 False []
  where
    go :: Int -> Bool -> [Token] -> [Token] -> ([Token], [Token])
    go _ _ acc [] = (reverse acc, [])
    go !pDepth !inContractKw acc (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            go (pDepth + 1) inContractKw (t : acc) ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            go (max 0 (pDepth - 1)) inContractKw (t : acc) ts
        | pDepth == 0 && isTokenOperator t && tToken t == ";" =
            (reverse (t : acc), ts)
        | pDepth == 0 && isTokenBracket t && tToken t == "{" =
            let (bodyToks, afterBody) = processInsideBrackets "{" "}" 1 ts
                accWithBody = reverse bodyToks ++ (t : acc)
             in case afterBody of
                    (nextTok : _)
                        | inContractKw
                        , (isTokenKeyword nextTok || isTokenIdentifier nextTok)
                        , tToken nextTok `elem` (["in", "out", "do", "body"] :: [Text]) ->
                            go 0 False accWithBody afterBody
                    _ -> (reverse accWithBody, afterBody)
        | pDepth == 0 =
            let isContract =
                    (isTokenKeyword t || isTokenIdentifier t)
                        && (tToken t == "in" || tToken t == "out")
             in go 0 isContract (t : acc) ts
        | otherwise =
            go pDepth inContractKw (t : acc) ts

flushPrefix :: Bool -> [Token] -> [Token] -> [Token]
flushPrefix keepTests revPrefix rest
    | null revPrefix = rest
    | keepTests = rest
    | otherwise = reverse revPrefix ++ rest

-- | Processes D tokens outside test blocks, tracking declaration boundaries (`atDeclStart`)
-- and buffering attached UDAs/modifiers (`revPrefix`).
processOutsideD :: Bool -> Bool -> [Token] -> [Token] -> [Token]
processOutsideD keepTests _ revPrefix [] = flushPrefix keepTests revPrefix []
-- Pattern 1: `version ( unittest )` in all 3 D forms (`{ ... }`, `:`, or `<single-decl>`)
processOutsideD keepTests _ revPrefix (t1 : t2 : t3 : t4 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "version"
    , isTokenBracket t2 && tToken t2 == "("
    , (isTokenKeyword t3 || isTokenIdentifier t3) && tToken t3 == "unittest"
    , isTokenBracket t4 && tToken t4 == ")" =
        let header = reverse revPrefix ++ [t1, t2, t3, t4]
         in case ts of
                (b : afterBrace)
                    | isTokenBracket b && tToken b == "{" ->
                        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 afterBrace
                            restOut = processOutsideD keepTests True [] remainingTokens
                         in if keepTests
                                then header ++ (b : bodyTokens) ++ restOut
                                else restOut
                (c : afterColon)
                    | isTokenOperator c && tToken c == ":" ->
                        let (scopeTokens, remainingTokens) = consumeUntilScopeEnd 0 afterColon
                            restOut = processOutsideD keepTests True [] remainingTokens
                         in if keepTests
                                then header ++ (c : scopeTokens) ++ restOut
                                else restOut
                _ ->
                    let (declTokens, remainingTokens) = consumeSingleDDeclOrStmt ts
                        restOut = processOutsideD keepTests True [] remainingTokens
                     in if keepTests
                            then header ++ declTokens ++ restOut
                            else restOut
-- Pattern 2: `[UDAs/modifiers] unittest { ... }`
processOutsideD keepTests _ revPrefix (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "unittest"
    , isTokenBracket t2 && tToken t2 == "{" =
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 ts
            captured = reverse revPrefix ++ (t1 : t2 : bodyTokens)
            restOut = processOutsideD keepTests True [] remainingTokens
         in if keepTests
                then captured ++ restOut
                else restOut
-- Pattern 3: `mixin UnitTest;` (`d-unit` fixture registration)
processOutsideD keepTests True revPrefix (t1 : t2 : t3 : ts)
    | isTokenKeyword t1 && tToken t1 == "mixin"
    , isTokenIdentifier t2 && tToken t2 == "UnitTest"
    , isTokenOperator t3 && tToken t3 == ";" =
        let captured = reverse revPrefix ++ [t1, t2, t3]
            restOut = processOutsideD keepTests True [] ts
         in if keepTests
                then captured ++ restOut
                else restOut
-- Pattern 4: Declaration-start UDAs (`@Test`, `@("name")`, `@safe`, etc.) & modifiers
processOutsideD keepTests True revPrefix tokens
    | Just (udaToks, udaIsTest, afterUda) <- extractDUda tokens =
        if udaIsTest
            then
                let prefixWithUda = reverse revPrefix ++ udaToks
                 in case afterUda of
                        (c : afterColon)
                            | isTokenOperator c && tToken c == ":" ->
                                let (scopeTokens, remaining) = consumeUntilScopeEnd 0 afterColon
                                    restOut = processOutsideD keepTests True [] remaining
                                 in if keepTests
                                        then prefixWithUda ++ (c : scopeTokens) ++ restOut
                                        else restOut
                        _ ->
                            let (declTokens, remaining) = consumeSingleDDeclOrStmt afterUda
                                restOut = processOutsideD keepTests True [] remaining
                             in if keepTests
                                    then prefixWithUda ++ declTokens ++ restOut
                                    else restOut
            else
                processOutsideD keepTests True (reverse udaToks ++ revPrefix) afterUda
    | Just (modToks, afterMod) <- matchDPrefixModifier tokens =
        processOutsideD keepTests True (reverse modToks ++ revPrefix) afterMod
-- Non-unittest `version(...)` condition keeps `atDeclStart = True` for its body
processOutsideD keepTests True revPrefix (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "version"
    , isTokenBracket t2 && tToken t2 == "(" =
        let (condToks, afterCond) = processInsideBrackets "(" ")" 1 ts
            restOut = processOutsideD keepTests True [] afterCond
         in flushPrefix keepTests revPrefix $
                if keepTests
                    then restOut
                    else (t1 : t2 : condToks) ++ restOut
-- Fallback: `;`, `{`, `}`, and `:` (colon attribute specifier like `@safe:`) reset `atDeclStart = True`
processOutsideD keepTests _ revPrefix (t : ts) =
    let resetsDecl =
            (isTokenOperator t && (tToken t == ";" || tToken t == ":"))
                || (isTokenBracket t && (tToken t == "{" || tToken t == "}"))
        restOut = processOutsideD keepTests resetsDecl [] ts
     in flushPrefix keepTests revPrefix $
            if keepTests
                then restOut
                else t : restOut
