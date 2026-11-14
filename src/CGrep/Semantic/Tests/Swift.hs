{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.Swift () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, splitDirectories, takeExtension, takeFileName)

instance LanguageTestFilter 'Swift where
    isTestFile = isSwiftTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideSwift keepTests tokens

instance LanguageTestFilter 'ObjectiveC where
    isTestFile = isObjCTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsideObjC keepTests [] tokens

-- | SwiftPM / Xcode `Tests/` directory `.swift` test target files.
isSwiftTestFilePath :: OsPath -> Bool
isSwiftTestFilePath path =
    let ext = takeExtension path
        dirs = map takeFileName (dropLastPart (splitDirectories path))
     in ext == [osp|.swift|] && any (== [osp|Tests|]) dirs
  where
    dropLastPart [] = []
    dropLastPart [_] = []
    dropLastPart (x : xs) = x : dropLastPart xs

-- | Xcode `Tests/` directory `.m` Objective-C test target files.
isObjCTestFilePath :: OsPath -> Bool
isObjCTestFilePath path =
    let ext = takeExtension path
        dirs = map takeFileName (dropLastPart (splitDirectories path))
     in (ext == [osp|.m|] || ext == [osp|.mi|]) && any (== [osp|Tests|]) dirs
  where
    dropLastPart [] = []
    dropLastPart [_] = []
    dropLastPart (x : xs) = x : dropLastPart xs

-- | Extracts a Swift attribute `@Foo.Bar(...)`
extractSwiftAttribute :: [Token] -> Maybe ([Token], [Token])
extractSwiftAttribute (t1 : t2 : ts)
    | isTokenOperator t1 && tToken t1 == "@" && (isTokenIdentifier t2 || isTokenKeyword t2) =
        let (pathTokens, rest) = collectDotPath ts
         in case rest of
                (t3 : ts')
                    | isTokenBracket t3 && tToken t3 == "(" ->
                        let (insideTokens, remaining) = processInsideBrackets "(" ")" 1 ts'
                         in Just (t1 : t2 : pathTokens ++ t3 : insideTokens, remaining)
                _ -> Just (t1 : t2 : pathTokens, rest)
  where
    collectDotPath (d : i : rest)
        | isTokenOperator d && tToken d == "." && isTokenIdentifier i =
            let (more, final) = collectDotPath rest
             in (d : i : more, final)
    collectDotPath rest = ([], rest)
extractSwiftAttribute _ = Nothing

-- | Checks if a Swift attribute is `@Test` or `@Suite` (or `@Testing.Test` / `@Testing.Suite`),
-- examining ONLY the attribute name before `(` so arguments like `@CustomAttr(mode: Test)`
-- are never mistaken for test attributes!
isSwiftTestAttribute :: [Token] -> Bool
isSwiftTestAttribute tokens =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "(")) tokens
        identTokens = filter isTokenIdentifier headerTokens
     in any (\t -> tToken t == "Test" || tToken t == "Suite") identTokens

-- | Swift declaration modifiers (`public`, `final`, `override`, `static`, `private(set)`, etc.).
-- Note: `class` is ONLY a modifier when followed by `func`, `var`, or `let` (`override class func setUp()`).
extractSwiftModifier :: [Token] -> Maybe ([Token], [Token])
extractSwiftModifier (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1)
    , tToken t1 `elem` (["private", "fileprivate", "internal", "public", "package", "unowned"] :: [Text])
    , isTokenBracket t2 && tToken t2 == "(" =
        let (inside, rest) = processInsideBrackets "(" ")" 1 ts
         in Just (t1 : t2 : inside, rest)
extractSwiftModifier (t1 : rest@(t2 : _))
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "class"
    , (isTokenKeyword t2 || isTokenIdentifier t2) && tToken t2 `elem` (["func", "var", "let"] :: [Text]) =
        Just ([t1], rest)
extractSwiftModifier (t1 : rest)
    | (isTokenKeyword t1 || isTokenIdentifier t1)
    , tToken t1
        `elem` ( [ "public"
                 , "internal"
                 , "private"
                 , "fileprivate"
                 , "open"
                 , "package"
                 , "final"
                 , "override"
                 , "static"
                 , "nonisolated"
                 , "isolated"
                 , "convenience"
                 , "mutating"
                 , "nonmutating"
                 , "dynamic"
                 , "lazy"
                 , "indirect"
                 , "required"
                 ] ::
                    [Text]
               ) =
        Just ([t1], rest)
extractSwiftModifier _ = Nothing

-- | Collect interleaved Swift `@Attributes` and declaration modifiers before a declaration.
collectSwiftPrefix :: [Token] -> ([[Token]], [Token], [Token])
collectSwiftPrefix = go [] []
  where
    go attrsRev flatRev ts
        | Just (attr, rest) <- extractSwiftAttribute ts =
            go (attr : attrsRev) (reverse attr ++ flatRev) rest
        | Just (modToks, rest) <- extractSwiftModifier ts =
            go attrsRev (reverse modToks ++ flatRev) rest
        | otherwise = (reverse attrsRev, reverse flatRev, ts)

-- | Find the opening `{` of a Swift declaration while:
--   1. Ignoring tokens inside backtick-escaped identifiers (e.g. `` @Test func `struct or func; in name`() ``).
--   2. Ignoring `{}` nested inside `(...)` or `[...]` (e.g. default closure parameters `= {}`).
--   3. Stopping if `;` or a second declaration keyword (`func`, `struct`, `class`, `actor`, `enum`,
--      `protocol`, `extension`, `var`, `let`) is encountered at depth 0.
findOpeningBraceSwift :: Bool -> Int -> [Token] -> Maybe ([Token], [Token])
findOpeningBraceSwift canSeeDecl0 n0 = go (0 :: Int) False canSeeDecl0 n0
  where
    go :: Int -> Bool -> Bool -> Int -> [Token] -> Maybe ([Token], [Token])
    go _ _ _ 0 _ = Nothing
    go _ _ _ _ [] = Nothing
    go depth inBackticks canSeeDecl n (t : ts)
        | depth == 0 && isTokenOperator t && tToken t == "`" =
            step depth (not inBackticks) canSeeDecl (n - 1) t ts
        | inBackticks =
            step depth True canSeeDecl (n - 1) t ts
        | isTokenBracket t && (tToken t == "(" || tToken t == "[") =
            step (depth + 1) False canSeeDecl (n - 1) t ts
        | isTokenBracket t && (tToken t == ")" || tToken t == "]") =
            step (max 0 (depth - 1)) False canSeeDecl (n - 1) t ts
        | depth == 0 && isTokenBracket t && tToken t == "{" =
            Just ([t], ts)
        | depth == 0 && tToken t == ";" =
            Nothing
        | depth == 0 && isSwiftDeclKeyword t ts =
            if canSeeDecl
                then step depth False False (n - 1) t ts
                else Nothing
        | otherwise =
            step depth False canSeeDecl (n - 1) t ts

    isSwiftDeclKeyword :: Token -> [Token] -> Bool
    isSwiftDeclKeyword tok rest
        | (isTokenKeyword tok || isTokenIdentifier tok) && tToken tok == "class"
        , (tNext : _) <- rest
        , (isTokenKeyword tNext || isTokenIdentifier tNext)
            && tToken tNext `elem` (["func", "var", "let"] :: [Text]) =
            False
        | otherwise =
            (isTokenKeyword tok || isTokenIdentifier tok)
                && ( tToken tok
                        `elem` ( [ "func"
                                 , "struct"
                                 , "class"
                                 , "actor"
                                 , "enum"
                                 , "protocol"
                                 , "extension"
                                 , "var"
                                 , "let"
                                 ] ::
                                    [Text]
                               )
                   )

    step :: Int -> Bool -> Bool -> Int -> Token -> [Token] -> Maybe ([Token], [Token])
    step d bt c m tok rest =
        case go d bt c m rest of
            Nothing -> Nothing
            Just (pre, post) -> Just (tok : pre, post)

-- | Check if a Swift/ObjC function name is an XCTest method (`test*` where `"test"` is
-- NOT followed by a lowercase ASCII letter, or `setUp` / `tearDown` / `setUpWithError` / `tearDownWithError`).
isSwiftTestMethodName :: Text -> Bool
isSwiftTestMethodName name
    | name `elem` (["setUp", "tearDown", "setUpWithError", "tearDownWithError"] :: [Text]) = True
    | Just rest <- T.stripPrefix "test" name =
        case T.uncons rest of
            Nothing -> True
            Just (c, _) -> not (isAsciiLower c)
    | otherwise = False

isSwiftTestTypeName :: Text -> Bool
isSwiftTestTypeName tname =
    let prefixOk = case T.stripPrefix "Test" tname of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
     in prefixOk
            || "Test" `T.isSuffixOf` tname
            || "Tests" `T.isSuffixOf` tname
            || "TestCase" `T.isSuffixOf` tname
            || "Test_" `T.isInfixOf` tname
            || "_Test" `T.isInfixOf` tname

isSwiftTestBaseName :: Text -> Bool
isSwiftTestBaseName name =
    name
        `elem` ( [ "XCTestCase"
                 , "QuickSpec"
                 , "AsyncSpec"
                 , "SenTestCase"
                 , "KIFTestCase"
                 , "FBSnapshotTestCase"
                 ] ::
                    [Text]
               )

-- | Check if a Swift `class` / `struct` / `actor` / `extension` is a test type:
--   1. Type name matches `isSwiftTestTypeName`, OR
--   2. Type header inherits (`:`) from `XCTestCase` / `QuickSpec` / `AsyncSpec`.
isSwiftTestType :: Token -> [Token] -> Bool
isSwiftTestType typeNameTok afterTypeName =
    let headerTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "{") && tToken t /= ";") afterTypeName
        inheritsTestBase = any (\t -> isTokenIdentifier t && isSwiftTestBaseName (tToken t)) headerTokens
     in isSwiftTestTypeName (tToken typeNameTok) || inheritsTestBase

isSwiftTypeKeyword :: Token -> Bool
isSwiftTypeKeyword t =
    (isTokenKeyword t || isTokenIdentifier t)
        && tToken t `elem` (["class", "struct", "actor", "extension"] :: [Text])

matchUnattributedSwiftTestDecl :: [Token] -> Maybe ([Token], [Token])
matchUnattributedSwiftTestDecl (t1 : t2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "func"
    , isTokenIdentifier t2 && isSwiftTestMethodName (tToken t2) =
        case findOpeningBraceSwift False 150 ts of
            Nothing -> Nothing
            Just (sigTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in Just (t1 : t2 : sigTokens ++ bodyTokens, remainingTokens)
matchUnattributedSwiftTestDecl (t1 : tBt1 : t2 : tBt2 : ts)
    | (isTokenKeyword t1 || isTokenIdentifier t1) && tToken t1 == "func"
    , isTokenOperator tBt1 && tToken tBt1 == "`"
    , isTokenIdentifier t2 && isSwiftTestMethodName (tToken t2)
    , isTokenOperator tBt2 && tToken tBt2 == "`" =
        case findOpeningBraceSwift False 150 ts of
            Nothing -> Nothing
            Just (sigTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in Just ([t1, tBt1, t2, tBt2] ++ sigTokens ++ bodyTokens, remainingTokens)
matchUnattributedSwiftTestDecl (t1 : t2 : ts)
    | isSwiftTypeKeyword t1
    , isTokenIdentifier t2 && isSwiftTestType t2 ts =
        case findOpeningBraceSwift False 200 ts of
            Nothing -> Nothing
            Just (sigTokens, tokensAfterBrace) ->
                let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                 in Just (t1 : t2 : sigTokens ++ bodyTokens, remainingTokens)
matchUnattributedSwiftTestDecl _ = Nothing

-- | (Swift) Helper: Processes tokens *outside* a test function/suite.
processOutsideSwift :: Bool -> [Token] -> [Token]
processOutsideSwift _ [] = []

-- Pattern 1: Leading `@Attributes` and/or declaration modifiers (`@Test`, `@Suite`, `@MainActor final class ...`, `override func setUp()`, etc.)
processOutsideSwift keepTests tokens
    | let (attrs, prefixToks, afterPrefix) = collectSwiftPrefix tokens
    , not (null prefixToks) =
        if any isSwiftTestAttribute attrs
            then case findOpeningBraceSwift True 200 afterPrefix of
                Nothing ->
                    if keepTests
                        then processOutsideSwift keepTests afterPrefix
                        else prefixToks ++ processOutsideSwift keepTests afterPrefix
                Just (sigTokens, tokensAfterBrace) ->
                    let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
                     in if keepTests
                            then prefixToks ++ sigTokens ++ bodyTokens ++ processOutsideSwift keepTests remainingTokens
                            else processOutsideSwift keepTests remainingTokens
            else case matchUnattributedSwiftTestDecl afterPrefix of
                Just (declAndBody, remainingTokens) ->
                    if keepTests
                        then prefixToks ++ declAndBody ++ processOutsideSwift keepTests remainingTokens
                        else processOutsideSwift keepTests remainingTokens
                Nothing ->
                    if keepTests
                        then processOutsideSwift keepTests afterPrefix
                        else prefixToks ++ processOutsideSwift keepTests afterPrefix

-- Pattern 2: Unprefixed `func test*` / `setUp` / `tearDown` or `class/struct/actor/extension *Tests`
processOutsideSwift keepTests tokens
    | Just (declAndBody, remainingTokens) <- matchUnattributedSwiftTestDecl tokens =
        if keepTests
            then declAndBody ++ processOutsideSwift keepTests remainingTokens
            else processOutsideSwift keepTests remainingTokens

-- Fallback: advance one token
processOutsideSwift keepTests (t : ts) =
    if keepTests
        then processOutsideSwift keepTests ts
        else t : processOutsideSwift keepTests ts

-- ---------------------------------------------------------------------------
-- Objective-C semantic test parser (`XCTestCase`, `SenTestCase`, `Specta`, `Kiwi`)
-- ---------------------------------------------------------------------------

-- | Consume tokens up to and including `@end` at brace depth 0.
consumeUntilObjCEnd :: [Token] -> ([Token], [Token])
consumeUntilObjCEnd = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go depth acc (t1 : t2 : ts)
        | depth == 0
        , isTokenOperator t1 && tToken t1 == "@"
        , (isTokenIdentifier t2 || isTokenKeyword t2) && tToken t2 == "end" =
            (reverse (t2 : t1 : acc), ts)
    go depth acc (t : ts) =
        let depth'
                | isTokenBracket t && tToken t == "{" = depth + 1
                | isTokenBracket t && tToken t == "}" = max 0 (depth - 1)
                | otherwise = depth
         in go depth' (t : acc) ts

-- | Consume tokens up to and including a closing macro identifier (e.g. `SpecEnd` / `SPEC_END`).
consumeUntilIdent :: Text -> [Token] -> ([Token], [Token])
consumeUntilIdent endName = go []
  where
    go acc [] = (reverse acc, [])
    go acc (t : ts)
        | isTokenIdentifier t && tToken t == endName = (reverse (t : acc), ts)
        | otherwise = go (t : acc) ts

-- | Match an Objective-C test method inside a non-test `@implementation` or standalone:
--   * 0-arg `- (void)testSomething { ... }`, `- (void)setUp { ... }`, `- (void)tearDown { ... }`
--   * 1-arg `- (BOOL)setUpWithError:(NSError **)error { ... }` / `tearDownWithError:`
matchObjCTestMethod :: [Token] -> Maybe ([Token], [Token])
matchObjCTestMethod (tSign : tOpen : ts)
    | isTokenOperator tSign && (tToken tSign == "-" || tToken tSign == "+")
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let (retToks, afterRet) = processInsideBrackets "(" ")" 1 ts
         in case afterRet of
                -- 1-arg XCTest `setUpWithError:(NSError **)error { ... }` / `tearDownWithError:`
                (tSel : tColon : tPOpen : restParam)
                    | isTokenIdentifier tSel
                    , tToken tSel == "setUpWithError" || tToken tSel == "tearDownWithError"
                    , isTokenOperator tColon && tToken tColon == ":"
                    , isTokenBracket tPOpen && tToken tPOpen == "(" ->
                        let (pTypeToks, afterPType) = processInsideBrackets "(" ")" 1 restParam
                         in case afterPType of
                                (tPName : afterPName)
                                    | isTokenIdentifier tPName
                                    , Just (preBrace, afterBrace) <- findObjCMethodBrace 20 afterPName ->
                                        let (bodyToks, remaining) = processInsideBrackets "{" "}" 1 afterBrace
                                         in Just
                                                ( tSign
                                                    : tOpen
                                                    : retToks
                                                    ++ [tSel, tColon, tPOpen]
                                                    ++ pTypeToks
                                                    ++ (tPName : preBrace)
                                                    ++ bodyToks
                                                , remaining
                                                )
                                _ -> Nothing
                -- 0-arg `- (void)testSomething { ... }` / `- (void)setUp { ... }`
                (tSel : afterSel)
                    | isTokenIdentifier tSel
                    , isSwiftTestMethodName (tToken tSel)
                    , isZeroArgObjCReturn retToks
                    , not (startsWithColon afterSel)
                    , Just (preBrace, afterBrace) <- findObjCMethodBrace 20 afterSel ->
                        let (bodyToks, remaining) = processInsideBrackets "{" "}" 1 afterBrace
                         in Just (tSign : tOpen : retToks ++ (tSel : preBrace) ++ bodyToks, remaining)
                _ -> Nothing
  where
    startsWithColon (c : _) = isTokenOperator c && tToken c == ":"
    startsWithColon [] = False

    isZeroArgObjCReturn toks =
        let inner = filter (\t -> not (isTokenBracket t && tToken t == ")")) toks
         in any (\t -> tToken t == "void" || tToken t == "IBAction") inner

    findObjCMethodBrace :: Int -> [Token] -> Maybe ([Token], [Token])
    findObjCMethodBrace 0 _ = Nothing
    findObjCMethodBrace _ [] = Nothing
    findObjCMethodBrace n (t : rest)
        | isTokenBracket t && tToken t == "{" = Just ([t], rest)
        | tToken t == ";" || (isTokenOperator t && (tToken t == "-" || tToken t == "+" || tToken t == "@")) = Nothing
        | isTokenBracket t && tToken t == "(" =
            let (inParens, afterP) = processInsideBrackets "(" ")" 1 rest
             in case findObjCMethodBrace (n - 1) afterP of
                    Nothing -> Nothing
                    Just (pre, post) -> Just (t : inParens ++ pre, post)
        | otherwise =
            case findObjCMethodBrace (n - 1) rest of
                Nothing -> Nothing
                Just (pre, post) -> Just (t : pre, post)
matchObjCTestMethod _ = Nothing

processOutsideObjC :: Bool -> [Text] -> [Token] -> [Token]
processOutsideObjC _ _ [] = []

-- Pattern 1: `@interface ClassName [: SuperClass] ... @end`
processOutsideObjC keepTests knownTests (tAt : tKw : tName : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , (isTokenIdentifier tKw || isTokenKeyword tKw) && tToken tKw == "interface"
    , isTokenIdentifier tName =
        let cname = tToken tName
            (ifaceRest, remaining) = consumeUntilObjCEnd ts
            hdrTokens =
                takeWhile
                    ( \t ->
                        not (isTokenBracket t && tToken t == "{")
                            && not (isTokenOperator t && (tToken t == "-" || tToken t == "+" || tToken t == "@"))
                    )
                    ifaceRest
            inheritsTest = any (\t -> isTokenIdentifier t && isSwiftTestBaseName (tToken t)) hdrTokens
            isTestCls = cname `elem` knownTests || isSwiftTestTypeName cname || inheritsTest
            knownTests' = if isTestCls && cname `notElem` knownTests then cname : knownTests else knownTests
            fullIface = tAt : tKw : tName : ifaceRest
         in if isTestCls
                then
                    if keepTests
                        then fullIface ++ processOutsideObjC keepTests knownTests' remaining
                        else processOutsideObjC keepTests knownTests' remaining
                else
                    if keepTests
                        then processOutsideObjC keepTests knownTests' remaining
                        else fullIface ++ processOutsideObjC keepTests knownTests' remaining

-- Pattern 2: `@implementation ClassName ... @end`
processOutsideObjC keepTests knownTests (tAt : tKw : tName : ts)
    | isTokenOperator tAt && tToken tAt == "@"
    , (isTokenIdentifier tKw || isTokenKeyword tKw) && tToken tKw == "implementation"
    , isTokenIdentifier tName =
        let cname = tToken tName
            (implRest, remaining) = consumeUntilObjCEnd ts
            isTestCls = cname `elem` knownTests || isSwiftTestTypeName cname
            fullImpl = tAt : tKw : tName : implRest
         in if isTestCls
                then
                    if keepTests
                        then fullImpl ++ processOutsideObjC keepTests knownTests remaining
                        else processOutsideObjC keepTests knownTests remaining
                else
                    let innerFiltered = processOutsideObjC keepTests knownTests implRest
                     in if keepTests
                            then innerFiltered ++ processOutsideObjC keepTests knownTests remaining
                            else [tAt, tKw, tName] ++ innerFiltered ++ processOutsideObjC keepTests knownTests remaining

-- Pattern 3: Specta `SpecBegin(Name) ... SpecEnd` and Kiwi `SPEC_BEGIN(Name) ... SPEC_END`
processOutsideObjC keepTests knownTests (t1 : tOpen : ts)
    | isTokenIdentifier t1 && (tToken t1 == "SpecBegin" || tToken t1 == "SPEC_BEGIN")
    , isTokenBracket tOpen && tToken tOpen == "(" =
        let endMacro = if tToken t1 == "SpecBegin" then "SpecEnd" else "SPEC_END"
            (argToks, afterArgs) = processInsideBrackets "(" ")" 1 ts
            (specBody, remaining) = consumeUntilIdent endMacro afterArgs
            fullSpec = t1 : tOpen : argToks ++ specBody
         in if keepTests
                then fullSpec ++ processOutsideObjC keepTests knownTests remaining
                else processOutsideObjC keepTests knownTests remaining

-- Pattern 4: Standalone Objective-C XCTest method (`- (void)test...`, `- (BOOL)setUpWithError:...`)
processOutsideObjC keepTests knownTests tokens
    | Just (methodToks, remaining) <- matchObjCTestMethod tokens =
        if keepTests
            then methodToks ++ processOutsideObjC keepTests knownTests remaining
            else processOutsideObjC keepTests knownTests remaining

-- Fallback: advance one token
processOutsideObjC keepTests knownTests (t : ts) =
    if keepTests
        then processOutsideObjC keepTests knownTests ts
        else t : processOutsideObjC keepTests knownTests ts
