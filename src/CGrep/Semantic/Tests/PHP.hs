{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module CGrep.Semantic.Tests.PHP () where

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
import Data.Char (isAsciiLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.OsPath (OsPath, osp, splitDirectories, takeExtension, takeFileName)

instance LanguageTestFilter 'PHP where
    isTestFile = isPhpTestFilePath
    langFilter Nothing tokens = tokens
    langFilter (Just keepTests) tokens = processOutsidePHP keepTests False tokens

-- | Recognizes `.phpt` core test files and `.php` files inside a `tests` directory
-- (e.g. `tests/Pest.php`, `tests/Feature/AuthTest.php`), leaving standalone mixed files
-- outside `tests` to token-level filtering.
isPhpTestFilePath :: OsPath -> Bool
isPhpTestFilePath path =
    ext == [osp|.phpt|]
        || (ext == [osp|.php|] && any (\d -> takeFileName d == [osp|tests|]) parentDirs)
  where
    ext = takeExtension path
    parentDirs = case reverse (splitDirectories path) of
        (_ : ds) -> ds
        [] -> []

-- | Pest PHP test, architecture & lifecycle functions that take a closure or fluent `->...` chain.
isPestBlockFunction :: Text -> Bool
isPestBlockFunction name =
    name
        `elem` ( [ "test"
                 , "it"
                 , "describe"
                 , "arch"
                 , "beforeEach"
                 , "afterEach"
                 , "beforeAll"
                 , "afterAll"
                 ] ::
                    [Text]
               )

-- | PHP 8 attribute names for PHPUnit 10/11+ (`#[Test]`, `#[DataProvider('...')]`, `#[Before]`, etc.).
isPhpUnitAttributeName :: Text -> Bool
isPhpUnitAttributeName name =
    name
        `elem` ( [ "Test"
                 , "TestWith"
                 , "TestWithJson"
                 , "DataProvider"
                 , "DataProviderExternal"
                 , "Before"
                 , "BeforeClass"
                 , "After"
                 , "AfterClass"
                 , "PreCondition"
                 , "PostCondition"
                 ] ::
                    [Text]
               )

-- | Extract a single PHP 8 attribute block `#[Attr1(...), \PHPUnit\Framework\Attributes\Test]`,
-- inspecting ONLY each attribute's namespace path before `(` so arguments like `#[Route('/api/test')]`
-- are never misclassified as test attributes.
extractPhpAttribute :: [Token] -> Maybe ([Token], Bool, [Token])
extractPhpAttribute (tHash : tOpen : ts)
    | isTokenOperator tHash && tToken tHash == "#"
    , isTokenBracket tOpen && tToken tOpen == "["
    , areAdjacentTokens tHash tOpen =
        let (inside, remaining) = processInsideBrackets "[" "]" 1 ts
            inner = dropClosingBracket inside
            items = splitAttrCommas inner
            isTest = any isPhpTestAttrItem items
         in Just (tHash : tOpen : inside, isTest, remaining)
  where
    dropClosingBracket [] = []
    dropClosingBracket [t] | isTokenBracket t && tToken t == "]" = []
    dropClosingBracket (t : rest) = t : dropClosingBracket rest

    splitAttrCommas = filter (not . null) . finish . go (0 :: Int) [] []
      where
        finish (cur, acc) = reverse (reverse cur : acc)
        go _ cur acc [] = (cur, acc)
        go depth cur acc (t : rest)
            | isTokenBracket t && (tToken t == "(" || tToken t == "[" || tToken t == "{") =
                go (depth + 1) (t : cur) acc rest
            | isTokenBracket t && (tToken t == ")" || tToken t == "]" || tToken t == "}") =
                go (max 0 (depth - 1)) (t : cur) acc rest
            | depth == 0 && tToken t == "," =
                go 0 [] (reverse cur : acc) rest
            | otherwise =
                go depth (t : cur) acc rest

    isPhpTestAttrItem toks =
        let pathToks = takeWhile (\t -> not (isTokenBracket t && tToken t == "(")) toks
            identToks = filter isTokenIdentifier pathToks
         in case reverse identToks of
                (lastIdent : _) -> isPhpUnitAttributeName (tToken lastIdent)
                [] -> False
extractPhpAttribute _ = Nothing

collectPhpAttributes :: [Token] -> ([Token], Bool, [Token])
collectPhpAttributes = go [] False
  where
    go acc isTest remaining =
        case extractPhpAttribute remaining of
            Just (attrToks, attrIsTest, rest) ->
                go (acc ++ attrToks) (isTest || attrIsTest) rest
            Nothing -> (acc, isTest, remaining)

-- | PHP declaration modifiers that can precede `class` or `function`.
isPhpModifier :: Token -> Bool
isPhpModifier t =
    (isTokenKeyword t || isTokenIdentifier t)
        && tToken t
            `elem` ( [ "public"
                     , "protected"
                     , "private"
                     , "static"
                     , "final"
                     , "readonly"
                     , "abstract"
                     ] ::
                        [Text]
                   )

collectPhpModifiers :: [Token] -> ([Token], [Token])
collectPhpModifiers (t : ts)
    | isPhpModifier t =
        let (mods, rest) = collectPhpModifiers ts
         in (t : mods, rest)
collectPhpModifiers ts = ([], ts)

-- | Check whether the tokens inside a Pest call's parentheses `(...)` contain a closure
-- (`function` or `fn` or `=>`).
hasPestClosure :: [Token] -> Bool
hasPestClosure [] = False
hasPestClosure (t1 : t2 : ts)
    | tToken t1 == "=" && tToken t2 == ">" = True
    | otherwise = isPestClosureSingle t1 || hasPestClosure (t2 : ts)
hasPestClosure [t1] = isPestClosureSingle t1

isPestClosureSingle :: Token -> Bool
isPestClosureSingle t =
    tToken t == "=>"
        || ((isTokenKeyword t || isTokenIdentifier t) && (tToken t == "function" || tToken t == "fn"))

-- | Consume any trailing chained Pest calls (`->throws(...)`, `->and(...)`, `->not->toUse(...)`)
-- allowing PHP keywords like `and`, `or`, `match`, `use` as method/property names after `->`.
consumePestChain :: [Token] -> ([Token], [Token])
consumePestChain (tDash : tGt : tMethod : rest)
    | isTokenOperator tDash && tToken tDash == "-"
    , isTokenOperator tGt && tToken tGt == ">"
    , areAdjacentTokens tDash tGt
    , isTokenIdentifier tMethod || isTokenKeyword tMethod =
        case rest of
            (tOpen : afterOpen)
                | isTokenBracket tOpen && tToken tOpen == "(" ->
                    let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 afterOpen
                        (moreChain, final) = consumePestChain afterArgs
                     in (tDash : tGt : tMethod : tOpen : argTokens ++ moreChain, final)
            _ ->
                let (moreChain, final) = consumePestChain rest
                 in (tDash : tGt : tMethod : moreChain, final)
consumePestChain (tArrow : tMethod : rest)
    | isTokenOperator tArrow && tToken tArrow == "->"
    , isTokenIdentifier tMethod || isTokenKeyword tMethod =
        case rest of
            (tOpen : afterOpen)
                | isTokenBracket tOpen && tToken tOpen == "(" ->
                    let (argTokens, afterArgs) = processInsideBrackets "(" ")" 1 afterOpen
                        (moreChain, final) = consumePestChain afterArgs
                     in (tArrow : tMethod : tOpen : argTokens ++ moreChain, final)
            _ ->
                let (moreChain, final) = consumePestChain rest
                 in (tArrow : tMethod : moreChain, final)
consumePestChain ts = ([], ts)

-- | Check if a PHP function/method is a PHPUnit test or lifecycle method by name.
isPhpTestMethod :: Token -> [Token] -> Bool
isPhpTestMethod nameTok afterName
    | name `elem` (["setUp", "tearDown", "setUpBeforeClass", "tearDownAfterClass"] :: [Text]) = True
    | Just rest <- T.stripPrefix "test" name =
        case T.uncons rest of
            Nothing ->
                case afterName of
                    (tOpen : tClose : _) ->
                        isTokenBracket tOpen
                            && tToken tOpen == "("
                            && isTokenBracket tClose
                            && tToken tClose == ")"
                    _ -> False
            Just (c, _) -> not (isAsciiLower c)
    | otherwise = False
  where
    name = tToken nameTok

-- | Check if a PHP class name or `extends` clause denotes a PHPUnit test class.
isPhpTestClass :: Token -> [Token] -> Bool
isPhpTestClass classNameTok afterClassName =
    let cname = tToken classNameTok
        prefixOk = case T.stripPrefix "Test" cname of
            Nothing -> False
            Just rest -> case T.uncons rest of
                Nothing -> True
                Just (c, _) -> not (isAsciiLower c)
        nameMatch =
            prefixOk
                || "Test" `T.isSuffixOf` cname
                || "Tests" `T.isSuffixOf` cname
                || "TestCase" `T.isSuffixOf` cname
                || "Test_" `T.isInfixOf` cname
                || "_Test" `T.isInfixOf` cname
        extendsMatch = case afterClassName of
            (tExt : rest)
                | (isTokenKeyword tExt || isTokenIdentifier tExt) && tToken tExt == "extends" ->
                    let superTokens = takeWhile (\t -> not (isTokenBracket t && tToken t == "{") && tToken t /= ";") rest
                     in any (\t -> isTokenIdentifier t && ("TestCase" `T.isSuffixOf` tToken t || "Test" `T.isSuffixOf` tToken t)) superTokens
            _ -> False
     in nameMatch || extendsMatch

isPestCallValid :: Text -> [Token] -> [Token] -> Bool
isPestCallValid fnName bodyTokens chainTokens
    | isPestBlockFunction fnName =
        hasPestClosure bodyTokens || not (null chainTokens)
    | fnName == "todo" =
        case bodyTokens of
            (tStr : _) -> isTokenString tStr
            _ -> False
    | fnName == "dataset" =
        case bodyTokens of
            (tStr : tComma : _) -> isTokenString tStr && tToken tComma == ","
            _ -> False
    | fnName == "uses" =
        not (null chainTokens)
            || any
                ( \t ->
                    isTokenIdentifier t
                        && ( "TestCase" `T.isSuffixOf` tToken t
                                || tToken t == "RefreshDatabase"
                                || tToken t == "DatabaseMigrations"
                                || tToken t == "DatabaseTransactions"
                           )
                )
                bodyTokens
    | otherwise = False

isPestCandidateName :: Text -> Bool
isPestCandidateName name =
    isPestBlockFunction name || name `elem` (["todo", "dataset", "uses"] :: [Text])

-- | Matches any braced PHP `function` or `class` (used when `#[Test]` / `#[Before]` / etc. is present!).
matchAnyPhpDecl :: [Token] -> Maybe ([Token], [Token])
matchAnyPhpDecl (tKw : tName : ts)
    | (isTokenKeyword tKw || isTokenIdentifier tKw) && (tToken tKw == "function" || tToken tKw == "class")
    , isTokenIdentifier tName = do
        (signatureTokens, tokensAfterBrace) <- findOpeningBraceC 200 ts
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
        Just (tKw : tName : signatureTokens ++ bodyTokens, remainingTokens)
matchAnyPhpDecl _ = Nothing

-- | Matches a PHP test `class` or `function` by naming/inheritance convention.
matchPhpDeclAfterMods :: [Token] -> Maybe ([Token], [Token])
matchPhpDeclAfterMods (tKw : tName : ts)
    | (isTokenKeyword tKw || isTokenIdentifier tKw) && tToken tKw == "function"
    , isTokenIdentifier tName && isPhpTestMethod tName ts = do
        (signatureTokens, tokensAfterBrace) <- findOpeningBraceC 100 ts
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
        Just (tKw : tName : signatureTokens ++ bodyTokens, remainingTokens)
    | (isTokenKeyword tKw || isTokenIdentifier tKw) && tToken tKw == "class"
    , isTokenIdentifier tName && isPhpTestClass tName ts = do
        (signatureTokens, tokensAfterBrace) <- findOpeningBraceC 200 ts
        let (bodyTokens, remainingTokens) = processInsideBrackets "{" "}" 1 tokensAfterBrace
        Just (tKw : tName : signatureTokens ++ bodyTokens, remainingTokens)
matchPhpDeclAfterMods _ = Nothing

-- | (PHP) Helper: Processes tokens *outside* a test method.
processOutsidePHP :: Bool -> Bool -> [Token] -> [Token]
processOutsidePHP _ _ [] = []
-- Pattern 1: Pest functions (`test`, `it`, `describe`, `arch`, `todo`, `dataset`, `uses`, lifecycle hooks)
processOutsidePHP keepTests False (t1 : t2 : ts)
    | (isTokenIdentifier t1 || isTokenKeyword t1) && isPestCandidateName (tToken t1)
    , isTokenBracket t2 && tToken t2 == "("
    , let (bodyTokens, afterParens) = processInsideBrackets "(" ")" 1 ts
    , let (chainTokens, remainingTokens) = consumePestChain afterParens
    , isPestCallValid (tToken t1) bodyTokens chainTokens =
        if keepTests
            then t1 : t2 : bodyTokens ++ chainTokens ++ processOutsidePHP keepTests False remainingTokens
            else processOutsidePHP keepTests False remainingTokens
-- Pattern 2: Leading PHP 8 attributes (`#[...]`) and/or modifiers (`final`, `readonly`, `public`, `static`, ...)
processOutsidePHP keepTests _ allToks
    | let (attrToks, hasTestAttr, afterAttrs) = collectPhpAttributes allToks
    , let (modToks, afterMods) = collectPhpModifiers afterAttrs
    , not (null attrToks) || not (null modToks) =
        let prefixToks = attrToks ++ modToks
            matchedDecl =
                if hasTestAttr
                    then matchAnyPhpDecl afterMods
                    else matchPhpDeclAfterMods afterMods
         in case matchedDecl of
                Just (declToks, remainingTokens) ->
                    if keepTests
                        then prefixToks ++ declToks ++ processOutsidePHP keepTests False remainingTokens
                        else processOutsidePHP keepTests False remainingTokens
                Nothing ->
                    if keepTests
                        then processOutsidePHP keepTests False afterMods
                        else prefixToks ++ processOutsidePHP keepTests False afterMods
-- Pattern 3: Unmodified `function test*` or `class *Test`
processOutsidePHP keepTests _ allToks
    | Just (declToks, remainingTokens) <- matchPhpDeclAfterMods allToks =
        if keepTests
            then declToks ++ processOutsidePHP keepTests False remainingTokens
            else processOutsidePHP keepTests False remainingTokens
-- Fallback
processOutsidePHP keepTests _ (t : ts) =
    let isBlocker =
            ((isTokenKeyword t || isTokenIdentifier t) && tToken t == "function")
                || (isTokenOperator t && (tToken t == ">" || tToken t == "->" || tToken t == "?->" || tToken t == ":" || tToken t == "::"))
     in if keepTests
            then processOutsidePHP keepTests isBlocker ts
            else t : processOutsidePHP keepTests isBlocker ts
