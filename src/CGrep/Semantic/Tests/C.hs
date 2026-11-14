{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.C () where

import CGrep.FileType (FileType (..))
import CGrep.Parser.Chunk (pattern ChunkNativeType)
import CGrep.Parser.Token (
    Token,
    isTokenBracket,
    isTokenIdentifier,
    isTokenKeyword,
    isTokenOperator,
    isTokenString,
    tToken,
    tTyp,
 )
import CGrep.Semantic.Tests.Common
import Data.Char (isAsciiLower)
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as TI

instance LanguageTestFilter 'C where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideC keepTests True [] tokens

instance LanguageTestFilter 'Cpp where
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideC keepTests True [] tokens

-- | Braced C/C++ test macros (`MACRO(...) { ... }`).
isCBracedTestMacro :: T.Text -> Bool
isCBracedTestMacro n =
    n
        `elem` ([ -- GoogleTest
                  "TEST"
                , "TEST_F"
                , "TEST_P"
                , "TYPED_TEST"
                , "TYPED_TEST_P"
                , "GTEST_TEST"
                , "GTEST_TEST_F"
                , "GTEST_TEST_P"
                , -- Catch2 / doctest
                  "TEST_CASE"
                , "TEST_CASE_METHOD"
                , "TEMPLATE_TEST_CASE"
                , "TEMPLATE_TEST_CASE_SIG"
                , "TEMPLATE_PRODUCT_TEST_CASE"
                , "TEMPLATE_LIST_TEST_CASE"
                , "SCENARIO"
                , "SCENARIO_METHOD"
                , "SECTION"
                , "DYNAMIC_SECTION"
                , "SUBCASE"
                , "SUITE"
                , "TEST_SUITE"
                , "CATCH_TEST_CASE"
                , "CATCH_SCENARIO"
                , "CATCH_SECTION"
                , "DOCTEST_TEST_CASE"
                , "DOCTEST_SCENARIO"
                , "DOCTEST_SUBCASE"
                , "DOCTEST_TEST_SUITE"
                , -- Boost.Test (braced)
                  "BOOST_AUTO_TEST_CASE"
                , "BOOST_FIXTURE_TEST_CASE"
                , "BOOST_DATA_TEST_CASE"
                , "BOOST_AUTO_TEST_CASE_TEMPLATE"
                , -- CppUTest / Criterion / Unity
                  "TEST_GROUP"
                , "TEST_C"
                , "Test"
                , "ParameterizedTest"
                ] :: [T.Text])

-- | Call/semicolon-terminated C/C++ test macros (`MACRO(...)[;]`, without `{ ... }`).
isCSemicolonTestMacro :: T.Text -> Bool
isCSemicolonTestMacro n =
    n
        `elem` ([ "FRIEND_TEST"
                , "TYPED_TEST_SUITE"
                , "TYPED_TEST_CASE"
                , "TYPED_TEST_SUITE_P"
                , "TYPED_TEST_CASE_P"
                , "REGISTER_TYPED_TEST_SUITE_P"
                , "REGISTER_TYPED_TEST_CASE_P"
                , "INSTANTIATE_TEST_SUITE_P"
                , "INSTANTIATE_TEST_CASE_P"
                , "INSTANTIATE_TYPED_TEST_SUITE_P"
                , "INSTANTIATE_TYPED_TEST_CASE_P"
                , "METHOD_AS_TEST_CASE"
                , "REGISTER_TEST_CASE"
                ] :: [T.Text])

isBoostSuiteBegin :: T.Text -> Bool
isBoostSuiteBegin n = n == "BOOST_AUTO_TEST_SUITE" || n == "BOOST_FIXTURE_TEST_SUITE"

isBoostSuiteEnd :: T.Text -> Bool
isBoostSuiteEnd n = n == "BOOST_AUTO_TEST_SUITE_END"

isDoctestSuiteBegin :: T.Text -> Bool
isDoctestSuiteBegin n = n == "TEST_SUITE_BEGIN" || n == "DOCTEST_TEST_SUITE_BEGIN"

isDoctestSuiteEnd :: T.Text -> Bool
isDoctestSuiteEnd n = n == "TEST_SUITE_END" || n == "DOCTEST_TEST_SUITE_END"

-- | Checks if a C/C++ function name is a test function (`test_*` or `Test*` where `"Test"`
-- is NOT followed by a lowercase ASCII letter, avoiding domain words like `TestifyAccount`).
isCTestFuncName :: T.Text -> Bool
isCTestFuncName name
    | "test_" `T.isPrefixOf` name = True
    | Just rest <- T.stripPrefix "Test" name =
        case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAsciiLower c)
    | otherwise = False

-- | Checks whether `tok` appears at the beginning of a line (only spaces/tabs since BOF or `\n`).
isFirstTokenOnLine :: Token -> Bool
isFirstTokenOnLine tok =
    case tToken tok of
        TI.Text arr off _ -> checkBack arr (off - 1)
  where
    checkBack !arr !i
        | i < 0 = True
        | otherwise =
            let !b = A.unsafeIndex arr i
             in if b == 10
                    then True
                    else (b == 32 || b == 9 || b == 13) && checkBack arr (i - 1)

-- | Checks whether `t1` and `t2` are on the same logical preprocessor line
-- (no `\n` between them unless escaped by `\\`).
onSameLogicalLine :: Token -> Token -> Bool
onSameLogicalLine t1 t2 =
    case (tToken t1, tToken t2) of
        (TI.Text arr off1 len1, TI.Text _ off2 _) ->
            scanBetween arr (off1 + len1) off2 False
  where
    scanBetween !arr !p !end !escaped
        | p >= end = True
        | otherwise =
            let !b = A.unsafeIndex arr p
             in if b == 10
                    then escaped && scanBetween arr (p + 1) end False
                    else
                        if b == 92
                            then scanBetween arr (p + 1) end True
                            else
                                if b == 32 || b == 9 || b == 13
                                    then scanBetween arr (p + 1) end escaped
                                    else scanBetween arr (p + 1) end False

consumePreprocessorLine :: Token -> [Token] -> ([Token], [Token])
consumePreprocessorLine firstTok = go firstTok
  where
    go prev (t : ts)
        | onSameLogicalLine prev t =
            let (more, rest) = go t ts
             in (t : more, rest)
    go _ ts = ([], ts)

consumeOptionalSemicolon :: [Token] -> ([Token], [Token])
consumeOptionalSemicolon (t : ts)
    | isTokenOperator t && tToken t == ";" = ([t], ts)
consumeOptionalSemicolon ts = ([], ts)

-- | Checks whether tokens start with `::` (either single `"::"` in strict mode or adjacent `":"` `":"`).
matchDoubleColon :: [Token] -> Maybe ([Token], [Token])
matchDoubleColon (t : ts)
    | isTokenOperator t && tToken t == "::" = Just ([t], ts)
matchDoubleColon (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == ":"
    , isTokenOperator t2 && tToken t2 == ":"
    , areAdjacentTokens t1 t2 =
        Just ([t1, t2], ts)
matchDoubleColon _ = Nothing

-- | Consumes tokens inside a paired suite (`BOOST_AUTO_TEST_SUITE`..`BOOST_AUTO_TEST_SUITE_END`,
-- `TEST_SUITE_BEGIN`..`TEST_SUITE_END`), handling nested suites.
collectUntilSuiteEnd :: (T.Text -> Bool) -> (T.Text -> Bool) -> Int -> [Token] -> ([Token], [Token])
collectUntilSuiteEnd _ _ 0 ts = ([], ts)
collectUntilSuiteEnd _ _ _ [] = ([], [])
collectUntilSuiteEnd isBegin isEnd depth (t1 : t2 : ts)
    | isTokenIdentifier t1 && isBegin (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
            (semi, afterSemi) = consumeOptionalSemicolon afterParams
            (restSuite, remaining) = collectUntilSuiteEnd isBegin isEnd (depth + 1) afterSemi
         in (t1 : t2 : params ++ semi ++ restSuite, remaining)
    | isTokenIdentifier t1 && isEnd (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
            (semi, afterSemi) = consumeOptionalSemicolon afterParams
         in if depth == 1
                then (t1 : t2 : params ++ semi, afterSemi)
                else
                    let (restSuite, remaining) = collectUntilSuiteEnd isBegin isEnd (depth - 1) afterSemi
                     in (t1 : t2 : params ++ semi ++ restSuite, remaining)
collectUntilSuiteEnd isBegin isEnd depth (t : ts) =
    let (restSuite, remaining) = collectUntilSuiteEnd isBegin isEnd depth ts
     in (t : restSuite, remaining)

-- | Consumes balanced `< ... >` template parameter/argument lists in declaration headers,
-- aborting if `;`, `{`, or `}` appears before closing `>`.
consumeAngleBrackets :: Int -> [Token] -> Maybe ([Token], [Token])
consumeAngleBrackets 0 ts = Just ([], ts)
consumeAngleBrackets _ [] = Nothing
consumeAngleBrackets depth (t : ts)
    | isTokenBracket t && (tToken t == "{" || tToken t == "}") = Nothing
    | isTokenOperator t && tToken t == ";" = Nothing
    | isTokenOperator t && tToken t == "<" =
        case consumeAngleBrackets (depth + 1) ts of
            Just (more, rest) -> Just (t : more, rest)
            Nothing -> Nothing
    | isTokenOperator t && tToken t == ">" =
        if depth == 1
            then Just ([t], ts)
            else case consumeAngleBrackets (depth - 1) ts of
                Just (more, rest) -> Just (t : more, rest)
                Nothing -> Nothing
    | isTokenOperator t && tToken t == ">>" =
        if depth <= 2
            then Just ([t], ts)
            else case consumeAngleBrackets (depth - 2) ts of
                Just (more, rest) -> Just (t : more, rest)
                Nothing -> Nothing
    | otherwise =
        case consumeAngleBrackets depth ts of
            Just (more, rest) -> Just (t : more, rest)
            Nothing -> Nothing

-- | Matches a C++ test fixture class inheriting from `testing::Test`, `testing::TestWithParam`,
-- `CppUnit::TestFixture`, or `CxxTest::TestSuite`.
matchCppTestFixtureClass :: [Token] -> Maybe ([Token], [Token])
matchCppTestFixtureClass (tColon : ts)
    | isTokenOperator tColon && tToken tColon == ":"
    , Nothing <- matchDoubleColon (tColon : ts)
    , Just (baseHeader, afterBrace) <- collectBaseClause ts
    , hasKnownTestFixtureBase baseHeader =
        let (bodyToks, afterBody) = processInsideBraces 1 afterBrace
            (semiToks, remaining) = consumeOptionalSemicolon afterBody
         in Just (tColon : baseHeader ++ bodyToks ++ semiToks, remaining)
  where
    collectBaseClause :: [Token] -> Maybe ([Token], [Token])
    collectBaseClause [] = Nothing
    collectBaseClause (t : rest)
        | isTokenOperator t && tToken t == ";" = Nothing
        | isTokenBracket t && tToken t == "{" = Just ([t], rest)
        | isTokenOperator t && tToken t == "<"
        , Just (angles, afterAngles) <- consumeAngleBrackets 1 rest =
            case collectBaseClause afterAngles of
                Just (more, finalRest) -> Just (t : angles ++ more, finalRest)
                Nothing -> Nothing
        | otherwise =
            case collectBaseClause rest of
                Just (more, finalRest) -> Just (t : more, finalRest)
                Nothing -> Nothing

    hasKnownTestFixtureBase :: [Token] -> Bool
    hasKnownTestFixtureBase [] = False
    hasKnownTestFixtureBase (tNs : rest)
        | isTokenIdentifier tNs
        , Just (_, tCls : _) <- matchDoubleColon rest
        , isTokenIdentifier tCls
        , isTestFixturePair (tToken tNs) (tToken tCls) =
            True
        | otherwise = hasKnownTestFixtureBase rest

    isTestFixturePair ns cls =
        (ns == "testing" && (cls == "Test" || cls == "TestWithParam"))
            || (ns == "CppUnit" && cls == "TestFixture")
            || (ns == "CxxTest" && cls == "TestSuite")
matchCppTestFixtureClass _ = Nothing

-- | Specifier keywords that can appear in a declaration prefix without themselves being a return type.
isDeclSpecifierOnlyKeyword :: T.Text -> Bool
isDeclSpecifierOnlyKeyword kw =
    kw
        `elem` ([ "static"
                , "inline"
                , "constexpr"
                , "consteval"
                , "constinit"
                , "extern"
                , "virtual"
                , "explicit"
                , "friend"
                , "const"
                , "volatile"
                , "restrict"
                , "_Noreturn"
                , "typename"
                , "struct"
                , "enum"
                , "union"
                ] :: [T.Text])

-- | Keywords/types that constitute a concrete return type on a C/C++ function.
-- Note: `void`, `wchar_t`, `char*_t`, `_Bool`, etc. are classified as `TokenKeyword` (`reserved`)
-- in `FileTypeMapTH` rather than `ChunkNativeType` (`types`).
isConcreteReturnTypeToken :: Token -> Bool
isConcreteReturnTypeToken t =
    tTyp t == ChunkNativeType
        || isTokenIdentifier t
        || ( isTokenKeyword t
                && tToken t
                    `elem` ([ "void"
                            , "auto"
                            , "unsigned"
                            , "signed"
                            , "short"
                            , "long"
                            , "wchar_t"
                            , "char8_t"
                            , "char16_t"
                            , "char32_t"
                            , "_Bool"
                            , "_Complex"
                            , "_Decimal32"
                            , "_Decimal64"
                            , "_Decimal128"
                            ] :: [T.Text])
           )

isDeclPrefixWordToken :: Token -> Bool
isDeclPrefixWordToken t =
    isConcreteReturnTypeToken t
        || (isTokenKeyword t && isDeclSpecifierOnlyKeyword (tToken t))

-- | Strips a trailing scope qualifier chain (`[Ns::]Class[<T>]::`) from the end of a
-- declaration prefix so that out-of-line constructors (`TestRunner::TestRunner()`)
-- leave no return-type tokens behind, while out-of-line methods (`void Suite::test_m()`)
-- preserve `void`!
stripTrailingScopeQualifiers :: [Token] -> [Token]
stripTrailingScopeQualifiers toks = reverse (goRev (reverse toks))
  where
    goRev :: [Token] -> [Token]
    goRev (c1 : c2 : rest)
        | isTokenOperator c1 && tToken c1 == ":"
        , isTokenOperator c2 && tToken c2 == ":"
        , areAdjacentTokens c2 c1 =
            stripOneSegment rest
    goRev (cc : rest)
        | isTokenOperator cc && tToken cc == "::" =
            stripOneSegment rest
    goRev other = other

    stripOneSegment :: [Token] -> [Token]
    stripOneSegment rs =
        let rsAfterAngles = skipRevAngles rs
         in case rsAfterAngles of
                (ident : beforeIdent)
                    | isTokenIdentifier ident -> goRev beforeIdent
                -- Leading global `::`
                _ -> goRev rsAfterAngles

    skipRevAngles :: [Token] -> [Token]
    skipRevAngles (t : rs)
        | isTokenOperator t && tToken t == ">" = dropRevAngle 1 rs
        | isTokenOperator t && tToken t == ">>" = dropRevAngle 2 rs
    skipRevAngles rs = rs

    dropRevAngle :: Int -> [Token] -> [Token]
    dropRevAngle 0 rs = rs
    dropRevAngle _ [] = []
    dropRevAngle d (t : rs)
        | isTokenOperator t && tToken t == ">" = dropRevAngle (d + 1) rs
        | isTokenOperator t && tToken t == ">>" = dropRevAngle (d + 2) rs
        | isTokenOperator t && tToken t == "<" = dropRevAngle (d - 1) rs
        | otherwise = dropRevAngle d rs

-- | Checks whether the buffered declaration prefix before `test_name(...)` contains
-- a genuine return type (rejecting C++ constructors like `explicit TestRunner(...)`
-- and out-of-line constructors like `TestRunner::TestRunner()`).
isValidFuncDeclPrefix :: [Token] -> Bool
isValidFuncDeclPrefix prefixToks =
    let withoutTemplate = dropLeadingTemplates prefixToks
        retToks = stripTrailingScopeQualifiers withoutTemplate
     in any isConcreteReturnTypeToken retToks
  where
    dropLeadingTemplates (tKw : tLt : rest)
        | isTokenKeyword tKw && tToken tKw == "template"
        , isTokenOperator tLt && tToken tLt == "<"
        , Just (_, afterAngles) <- consumeAngleBrackets 1 rest =
            dropLeadingTemplates afterAngles
    dropLeadingTemplates other = other

-- | Finds the opening `{` of a C/C++ function/macro body after its `(...)` parameter list,
-- stopping if `;` or unmatched `)` is encountered at depth 0, or if a constructor member
-- initializer `:` (not `::`) appears at depth 0.
findOpeningBraceCDecl :: Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceCDecl = go 0
  where
    go :: Int -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ 0 _ = Nothing
    go _ _ [] = Nothing
    go depth n (t : ts)
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            if depth == 0
                then Nothing
                else step (depth - 1) (n - 1) t ts
        | depth == 0
        , isTokenOperator t && tToken t == ":"
        , Nothing <- matchDoubleColon (t : ts) =
            Nothing
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

flushDeclBuf :: Bool -> [Token] -> [Token] -> [Token]
flushDeclBuf True _ rest = rest
flushDeclBuf False revBuf rest = reverse revBuf ++ rest

processOutsideC :: Bool -> Bool -> [Token] -> [Token] -> [Token]
processOutsideC keepTests _ revBuf [] = flushDeclBuf keepTests revBuf []
-- Preprocessor directives (`#ifdef`, `#include`, `#define`, `#endif`, `#pragma`)
processOutsideC keepTests _ revBuf (tSharp : ts)
    | isTokenOperator tSharp && tToken tSharp == "#" && isFirstTokenOnLine tSharp =
        let (ppLine, remaining) = consumePreprocessorLine tSharp ts
            ppToks = tSharp : ppLine
            afterRest = processOutsideC keepTests True [] remaining
         in flushDeclBuf keepTests revBuf $
                if keepTests then afterRest else ppToks ++ afterRest
-- Paired Boost.Test suite (`BOOST_AUTO_TEST_SUITE(...) ... BOOST_AUTO_TEST_SUITE_END()`)
processOutsideC keepTests _ revBuf (t1 : t2 : ts)
    | isTokenIdentifier t1 && isBoostSuiteBegin (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
            (semi, afterSemi) = consumeOptionalSemicolon afterParams
            (suiteBody, remaining) = collectUntilSuiteEnd isBoostSuiteBegin isBoostSuiteEnd 1 afterSemi
            captured = t1 : t2 : params ++ semi ++ suiteBody
            afterRest = processOutsideC keepTests True [] remaining
         in flushDeclBuf keepTests revBuf $
                if keepTests then captured ++ afterRest else afterRest
-- Paired doctest suite (`TEST_SUITE_BEGIN(...) ... TEST_SUITE_END()`)
processOutsideC keepTests _ revBuf (t1 : t2 : ts)
    | isTokenIdentifier t1 && isDoctestSuiteBegin (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
            (semi, afterSemi) = consumeOptionalSemicolon afterParams
            (suiteBody, remaining) = collectUntilSuiteEnd isDoctestSuiteBegin isDoctestSuiteEnd 1 afterSemi
            captured = t1 : t2 : params ++ semi ++ suiteBody
            afterRest = processOutsideC keepTests True [] remaining
         in flushDeclBuf keepTests revBuf $
                if keepTests then captured ++ afterRest else afterRest
-- Semicolon / call-only test macros (`FRIEND_TEST`, `INSTANTIATE_TEST_SUITE_P`, etc.)
processOutsideC keepTests _ revBuf (t1 : t2 : ts)
    | isTokenIdentifier t1 && isCSemicolonTestMacro (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
            (semi, remaining) = consumeOptionalSemicolon afterParams
            captured = t1 : t2 : params ++ semi
            afterRest = processOutsideC keepTests True [] remaining
         in flushDeclBuf keepTests revBuf $
                if keepTests then captured ++ afterRest else afterRest
-- Braced C/C++ Test Macros (`TEST`, `TEST_F`, `TEST_CASE`, `SCENARIO`, `SUBCASE`, etc.)
processOutsideC keepTests _ revBuf (t1 : t2 : ts)
    | isTokenIdentifier t1 && isCBracedTestMacro (tToken t1)
    , isTokenBracket t2 && tToken t2 == "(" =
        let (params, afterParams) = processInsideBrackets "(" ")" 1 ts
         in case findOpeningBraceCDecl 50 afterParams of
                Nothing ->
                    let afterRest = processOutsideC keepTests False [] (t2 : ts)
                     in flushDeclBuf keepTests revBuf $
                            if keepTests then afterRest else t1 : afterRest
                Just (sigToks, afterBrace) ->
                    let (bodyToks, afterBody) = processInsideBraces 1 afterBrace
                        (semiToks, remaining) = consumeOptionalSemicolon afterBody
                        captured = t1 : t2 : params ++ sigToks ++ bodyToks ++ semiToks
                        afterRest = processOutsideC keepTests True [] remaining
                     in flushDeclBuf keepTests revBuf $
                            if keepTests then captured ++ afterRest else afterRest
-- GoogleTest / CppUnit / CxxTest fixture classes (`class DbFixture : public ::testing::Test { ... };`)
processOutsideC keepTests True revBuf (tKw : tName : ts)
    | isTokenKeyword tKw && (tToken tKw == "class" || tToken tKw == "struct")
    , isTokenIdentifier tName
    , Just (fixtureRest, remaining) <- matchCppTestFixtureClass ts =
        let captured = reverse revBuf ++ (tKw : tName : fixtureRest)
            afterRest = processOutsideC keepTests True [] remaining
         in if keepTests
                then captured ++ afterRest
                else afterRest
-- C++ access specifiers (`public:`, `protected:`, `private:`) reset declaration start
processOutsideC keepTests _ revBuf (tAcc : tColon : ts)
    | isTokenKeyword tAcc
    , tToken tAcc `elem` (["public", "protected", "private"] :: [T.Text])
    , isTokenOperator tColon && tToken tColon == ":"
    , Nothing <- matchDoubleColon (tColon : ts) =
        let afterRest = processOutsideC keepTests True [] ts
         in flushDeclBuf keepTests revBuf $
                if keepTests then afterRest else tAcc : tColon : afterRest
-- C++ destructor `~TestFoo()` (never a test function)
processOutsideC keepTests _ revBuf (t0 : t1 : t2 : ts)
    | tToken t0 == "~"
    , isTokenIdentifier t1
    , isTokenBracket t2 && tToken t2 == "(" =
        let afterRest = processOutsideC keepTests False [] ts
         in flushDeclBuf keepTests revBuf $
                if keepTests then afterRest else t0 : t1 : t2 : afterRest
-- C/C++ function / out-of-line method candidate `<declPrefix> <funcName>(...) { ... }`
processOutsideC keepTests True revBuf (tName : tOpen : ts)
    | isTokenIdentifier tName
    , isTokenBracket tOpen && tToken tOpen == "(" =
        if isCTestFuncName (tToken tName) && isValidFuncDeclPrefix (reverse revBuf)
            then
                let (paramToks, afterParams) = processInsideBrackets "(" ")" 1 ts
                 in case findOpeningBraceCDecl 35 afterParams of
                        Nothing ->
                            let afterRest = processOutsideC keepTests False [] (tOpen : ts)
                             in flushDeclBuf keepTests revBuf $
                                    if keepTests then afterRest else tName : afterRest
                        Just (sigToks, afterBrace) ->
                            let (bodyToks, remaining) = processInsideBraces 1 afterBrace
                                captured = reverse revBuf ++ (tName : tOpen : paramToks) ++ sigToks ++ bodyToks
                                afterRest = processOutsideC keepTests True [] remaining
                             in if keepTests
                                    then captured ++ afterRest
                                    else afterRest
            else
                -- Non-test function or constructor: flush buffered prefix and function name
                let afterRest = processOutsideC keepTests False [] (tOpen : ts)
                 in flushDeclBuf keepTests revBuf $
                        if keepTests then afterRest else tName : afterRest
-- Declaration prefix accumulation (`template <...>`, `[[attr]]`, `__attribute__((...))`, `extern "C"`, types, `*`/`&`/`::`)
processOutsideC keepTests True revBuf (t1 : t2 : ts)
    | isTokenKeyword t1 && tToken t1 == "template"
    , isTokenOperator t2 && tToken t2 == "<"
    , Just (angles, remaining) <- consumeAngleBrackets 1 ts =
        processOutsideC keepTests True (reverse (t1 : t2 : angles) ++ revBuf) remaining
    | isTokenBracket t1 && tToken t1 == "["
    , isTokenBracket t2 && tToken t2 == "[" =
        let (insideAttr, remaining) = processInsideBrackets "[" "]" 2 ts
         in processOutsideC keepTests True (reverse (t1 : t2 : insideAttr) ++ revBuf) remaining
    | (isTokenIdentifier t1 || isTokenKeyword t1)
    , tToken t1 `elem` (["__attribute__", "__declspec", "alignas", "decltype"] :: [T.Text])
    , isTokenBracket t2 && tToken t2 == "(" =
        let (insideParens, remaining) = processInsideBrackets "(" ")" 1 ts
         in processOutsideC keepTests True (reverse (t1 : t2 : insideParens) ++ revBuf) remaining
    | isTokenKeyword t1 && tToken t1 == "extern"
    , isTokenString t2 =
        -- e.g. `extern "C" void test_foo() { ... }` (if `{` follows, `{` flushes `revBuf` as prod)
        processOutsideC keepTests True (t2 : t1 : revBuf) ts
processOutsideC keepTests True revBuf (t : ts)
    | not (null revBuf)
    , isTokenOperator t && tToken t == "<"
    , Just (angles, remaining) <- consumeAngleBrackets 1 ts =
        processOutsideC keepTests True (reverse (t : angles) ++ revBuf) remaining
    | Just (colons, remaining) <- matchDoubleColon (t : ts) =
        processOutsideC keepTests True (reverse colons ++ revBuf) remaining
    | isDeclPrefixWordToken t =
        processOutsideC keepTests True (t : revBuf) ts
    | isTokenOperator t && tToken t `elem` (["*", "&", "&&"] :: [T.Text]) =
        processOutsideC keepTests True (t : revBuf) ts
-- Statement / block boundaries reset `canStartDecl = True`
processOutsideC keepTests _ revBuf (t : ts) =
    let resetsDecl =
            (isTokenOperator t && tToken t == ";")
                || (isTokenBracket t && (tToken t == "{" || tToken t == "}"))
        afterRest = processOutsideC keepTests resetsDecl [] ts
     in flushDeclBuf keepTests revBuf $
            if keepTests then afterRest else t : afterRest
