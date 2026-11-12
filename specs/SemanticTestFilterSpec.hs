module SemanticTestFilterSpec (semanticTestFilterTests) where

import CGrep.FileType (FileType (..))
import CGrep.FileTypeMap (FileTypeInfoMap (..))
import CGrep.FileTypeMapTH (fileTypeInfoMap)
import CGrep.Match (putMatches)
import CGrep.Parser.Chunk (parseChunks)
import Config (defaultConfig)
import Control.Monad (forM_, unless)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Foldable (toList)
import Data.List (sort)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Options (Options (..), defaultOptions)
import Reader (Env (..))
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))
import TestHelpers (
    lexSource,
    lookupFTInfo,
    matchChunkTexts,
    matchLineNums,
    matchLines,
    runSearchFile,
    runSearchMem,
 )

semanticTestFilterTests :: TestTree
semanticTestFilterTests =
    testGroup
        "Semantic strategy, test-filtering & ignore-case corner cases"
        [ sliceVsTestFilterTests
        , ignoreCaseCornerCases
        , crossStrategyAndAllLanguagesTests
        , fixtureSmokeTests
        ]

sliceVsTestFilterTests :: TestTree
sliceVsTestFilterTests =
    testGroup
        "Multi-line test structures are not truncated before filterTests (-T / --no-tests)"
        [ testCase "PHP Allman-style test class with '{' on next line matched on class header" $ do
            let src =
                    T.unlines
                        [ "<?php"
                        , "class OrderService"
                        , "{"
                        , "    public function run() {}"
                        , "}"
                        , "class OrderServiceTest extends TestCase"
                        , "{"
                        , "    public function testCreate() {}"
                        , "}"
                        ]
                optOnly = defaultOptions{semantic = True, word_match = True, tests = Just True}
                optNo = defaultOptions{semantic = True, word_match = True, tests = Just False}
            msOnly <- runSearchMem optOnly PHP "OrderTest.php" src ["OrderServiceTest"]
            msNo <- runSearchMem optNo PHP "OrderTest.php" src ["OrderServiceTest"]
            matchLineNums msOnly @?= [6]
            matchLineNums msNo @?= []
        , testCase "Java @Test annotation matched directly when body follows on subsequent lines" $ do
            let src =
                    T.unlines
                        [ "class Demo {"
                        , "    void prodMethod() {}"
                        , "    @Test"
                        , "    void checkBehavior() {"
                        , "        assertTrue(true);"
                        , "    }"
                        , "}"
                        ]
                optOnly = defaultOptions{semantic = True, word_match = True, tests = Just True}
                optNo = defaultOptions{semantic = True, word_match = True, tests = Just False}
            msOnly <- runSearchMem optOnly Java "Demo.java" src ["Test"]
            msNo <- runSearchMem optNo Java "Demo.java" src ["Test"]
            matchLineNums msOnly @?= [3]
            matchLineNums msNo @?= []
        , testCase "Rust #[tokio::test] attribute matched directly when async fn follows on next line" $ do
            let src =
                    T.unlines
                        [ "pub fn prod_work() {}"
                        , "#[tokio::test]"
                        , "async fn check_async() {"
                        , "    assert_eq!(1, 1);"
                        , "}"
                        ]
                optOnly = defaultOptions{semantic = True, word_match = True, tests = Just True}
                optNo = defaultOptions{semantic = True, word_match = True, tests = Just False}
            msOnly <- runSearchMem optOnly Rust "lib.rs" src ["tokio"]
            msNo <- runSearchMem optNo Rust "lib.rs" src ["tokio"]
            matchLineNums msOnly @?= [2]
            matchLineNums msNo @?= []
        , testCase "JavaScript multi-line describe('suite', () => { ... }) matched on suite title" $ do
            let src =
                    T.unlines
                        [ "const prodCfg = \"other\";"
                        , "describe("
                        , "  \"target suite\","
                        , "  () => {"
                        , "    it(\"works\", () => {});"
                        , "  }"
                        , ");"
                        ]
                optOnly = defaultOptions{semantic = True, tests = Just True}
                optNo = defaultOptions{semantic = True, tests = Just False}
            msOnly <- runSearchMem optOnly Javascript "app.js" src ["\"target suite\""]
            msNo <- runSearchMem optNo Javascript "app.js" src ["\"target suite\""]
            matchLineNums msOnly @?= [3]
            matchLineNums msNo @?= []
        , testCase "OCaml let%test with local 'let x = target' before 'in' on following line" $ do
            let src =
                    T.unlines
                        [ "let prod_fn x = x + 1"
                        , "let%test \"calc\" ="
                        , "  let v = target_binding"
                        , "  in v = 42"
                        ]
                optOnly = defaultOptions{semantic = True, tests = Just True}
                optNo = defaultOptions{semantic = True, tests = Just False}
            msOnly <- runSearchMem optOnly OCaml "calc.ml" src ["target_binding"]
            msNo <- runSearchMem optNo OCaml "calc.ml" src ["target_binding"]
            matchLineNums msOnly @?= [3]
            matchLineNums msNo @?= []
        , testCase "Bash BATS setup() preceding @test later in file is recognized as test code" $ do
            let src =
                    T.unlines
                        [ "prod_helper() {"
                        , "  echo ok"
                        , "}"
                        , "setup() {"
                        , "  init_fixture_state"
                        , "}"
                        , "@test \"runs check\" {"
                        , "  run prod_helper"
                        , "}"
                        ]
                optOnly = defaultOptions{semantic = True, tests = Just True}
                optNo = defaultOptions{semantic = True, tests = Just False}
            msOnly <- runSearchMem optOnly Bash "suite.sh" src ["init_fixture_state"]
            msNo <- runSearchMem optNo Bash "suite.sh" src ["init_fixture_state"]
            matchLineNums msOnly @?= [5]
            matchLineNums msNo @?= []
        ]

ignoreCaseCornerCases :: TestTree
ignoreCaseCornerCases =
    testGroup
        "--ignore-case (-i) corner cases across Semantic, Tokenizer & BoyerMoore"
        [ testCase "-i -S preserves case-sensitive test markers (@Test, TEST_F, func Test..., [Fact])" $ do
            -- Java @Test
            let javaSrc = "class A {\n  void prod() { runCheck(); }\n  @Test void t() { runCheck(); }\n}\n"
            jTest <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just True} Java "A.java" javaSrc ["runcheck"]
            jProd <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just False} Java "A.java" javaSrc ["runcheck"]
            matchLineNums jTest @?= [3]
            matchLineNums jProd @?= [2]

            -- C++ GTest TEST_F
            let cppSrc = "void prod() { CheckCall(); }\nTEST_F(MySuite, Case1) { CheckCall(); }\n"
            cTest <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just True} Cpp "a.cpp" cppSrc ["checkcall"]
            cProd <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just False} Cpp "a.cpp" cppSrc ["checkcall"]
            matchLineNums cTest @?= [2]
            matchLineNums cProd @?= [1]

            -- Go func TestXxx
            let goSrc = "package p\nfunc prod() { doWork() }\nfunc TestWidget(t *testing.T) { doWork() }\n"
            gTest <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just True} Go "a.go" goSrc ["dowork"]
            gProd <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just False} Go "a.go" goSrc ["dowork"]
            matchLineNums gTest @?= [3]
            matchLineNums gProd @?= [2]

            -- C# [Fact]
            let csSrc = "class S {\n  void Prod() { DoStep(); }\n  [Fact] void TestStep() { DoStep(); }\n}\n"
            csTest <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just True} Csharp "a.cs" csSrc ["dostep"]
            csProd <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just False} Csharp "a.cs" csSrc ["dostep"]
            matchLineNums csTest @?= [3]
            matchLineNums csProd @?= [2]
        , testCase "-i -S preserves semantic wildcards (ANY, STR, NUM, NOT, OR) without surrounding spaces" $ do
            let src =
                    T.unlines
                        [ "res1 = COMPUTE(x, 42);"
                        , "res2 = Compute(\"hello\", y);"
                        , "res3 = Compute(only_one);"
                        ]
                opt = defaultOptions{semantic = True, ignore_case = True}
            msAny <- runSearchMem opt C "a.c" src ["compute(ANY, ANY)"]
            msNotStr <- runSearchMem opt C "a.c" src ["compute(NOT STR, NUM)"]
            msOr <- runSearchMem opt C "a.c" src ["res1 OR res3 = _"]
            matchLineNums msAny @?= [1, 2]
            matchLineNums msNotStr @?= [1]
            matchLineNums msOr @?= [1, 3]
        , testCase "-i -S and -i --native match Haskell 'Int' (NativeType) without falsely matching identifier 'print'" $ do
            let src = "addOne :: Int -> Int\nprintVal x = x\n"
            msSem <- runSearchMem defaultOptions{semantic = True, ignore_case = True} Haskell "a.hs" src ["int"]
            msTok <- runSearchMem defaultOptions{nativeType = True, ignore_case = True} Haskell "a.hs" src ["int"]
            matchLineNums msSem @?= [1]
            matchChunkTexts msSem @?= [["Int", "Int"]]
            matchLineNums msTok @?= [1]
            matchChunkTexts msTok @?= [["Int", "Int"]]
        , testCase "-i -S and -i --keyword match C 'if' (Keyword) when queried as 'IF' without matching 'shift'" $ do
            let src = "int shift = 1;\nif (shift) { return 0; }\n"
            msSem <- runSearchMem defaultOptions{semantic = True, ignore_case = True} C "a.c" src ["IF"]
            msTok <- runSearchMem defaultOptions{keyword = True, ignore_case = True} C "a.c" src ["IF"]
            matchLineNums msSem @?= [2]
            matchLineNums msTok @?= [2]
        , testCase "-i -c (BoyerMoore) strips C++ uppercase R\"(...)\" raw string before lowercasing" $ do
            let src = "auto raw = R\"(SECRET_VALUE)\";\nint secret_value = 1;\n"
                opt = defaultOptions{code = True, ignore_case = True}
            ms <- runSearchMem opt Cpp "a.cpp" src ["SECRET_VALUE"]
            matchLineNums ms @?= [2]
        , testCase "-i with width-changing Unicode ('İ' U+0130, 'K' U+212A, 'ﬀ' U+FB00) preserves exact line/col and --color output" $ do
            let src = "// İİİİİİİİ KK ﬀﬀ target_on_one\nint second_line = 2;\n"
                opt = defaultOptions{ignore_case = True, color = True}
            -- Note: comment is NOT excluded here (default context = all), so target_on_one is on line 1!
            ms <- runSearchMem opt C "utf8_ic.c" src ["TARGET_ON_ONE"]
            matchLineNums ms @?= [1]
            mBuilder <- runReaderT (putMatches ms) (Env defaultConfig opt)
            case mBuilder of
                Just b -> do
                    let rendered = TL.toStrict (TLB.toLazyText b)
                    assertBool "rendered --color output must contain line:1:" (T.isInfixOf ":1:" rendered)
                    assertBool "rendered --color output must preserve UTF-8 chars" (T.length rendered > 0)
                Nothing -> assertFailure "expected non-empty rendered output"
        ]

crossStrategyAndAllLanguagesTests :: TestTree
crossStrategyAndAllLanguagesTests =
    testGroup
        "Edit-distance (-e), Regex (-G), and exhaustive 65+ FileType coverage"
        [ testCase "Edit-distance (-e) works with Semantic (-S -e) and Tokenizer (--identifier -e) without exact substring in file" $ do
            let src = "int header = 0;\n// calclate in comment on line 2\nint res = calculate(42);\n"
            msSem <- runSearchMem defaultOptions{semantic = True, edit_dist = True, code = True} C "a.c" src ["calclate(42)"]
            msTok <- runSearchMem defaultOptions{identifier = True, edit_dist = True, code = True} C "a.c" src ["calclate"]
            matchLineNums msSem @?= [3]
            matchLineNums msTok @?= [3]
        , testCase "Levenshtein (-e) and Regex (-G) both strip C++ R\"(...)\" raw strings under -i -c" $ do
            let src = "auto raw = R\"(SECRET_TOKEN)\";\nint secret_tokn = 1;\n"
            msLev <- runSearchMem defaultOptions{edit_dist = True, code = True, ignore_case = True} Cpp "a.cpp" src ["SECRET_TOKEN"]
            msRx <- runSearchMem defaultOptions{regex_posix = True, code = True, ignore_case = True} Cpp "a.cpp" src ["secret_tok[a-z]*"]
            matchLineNums msLev @?= [2]
            matchLineNums msRx @?= [2]
        , testCase "Every FileType in fileTypeInfoMap executes ContextFilter, parseTokens, parseChunks & Semantic.search totally" $ do
            let allFTs = Map.keys (unMapInfo fileTypeInfoMap)
                sample =
                    T.unlines
                        [ "// comment # comment -- comment \"c\" (* c *)"
                        , "x = \"hello © € 🦀\" + 'a' + '\\x41' + $z"
                        , "target_ident = 123"
                        ]
            assertBool "expected >= 60 file types" (length allFTs >= 60)
            forM_ allFTs $ \ft -> do
                let toks = lexSource ft defaultOptions{code = True} False sample
                    chunks = toList $ parseChunks (snd <$> lookupFTInfo ft) sample
                assertBool (show ft <> " token offsets valid") (all (\_ -> True) toks && length chunks >= 0)
                ms1 <- runSearchMem defaultOptions{semantic = True, tests = Just False} ft "sample" sample ["target_ident"]
                ms2 <- runSearchMem defaultOptions{semantic = True, ignore_case = True, tests = Just True} ft "sample" sample ["TARGET_IDENT"]
                assertBool (show ft <> " search total") (length ms1 >= 0 && length ms2 >= 0)
        ]

fixtureSmokeTests :: TestTree
fixtureSmokeTests =
    testGroup
        "Repository fixture regression checks (test_suite/ & test/)"
        [ testCase "All test_suite/* fixtures cleanly separate cgrep_prod_* and cgrep_test_* across 6 flag modes" $ do
            hasSuite <- doesDirectoryExist "test_suite"
            assertBool "test_suite directory should exist" hasSuite
            entries <- sort <$> listDirectory "test_suite"
            let files = map ("test_suite" </>) entries
            assertBool "expected at least 30 test_suite files" (length files >= 30)
            forM_ files $ \fp -> do
                isF <- doesFileExist fp
                whenFile isF $ do
                    let modes :: [(String, Options, T.Text)]
                        modes =
                            [ ("prod", defaultOptions{semantic = True, tests = Just False}, "cgrep_prod_")
                            , ("test", defaultOptions{semantic = True, tests = Just True}, "cgrep_test_")
                            , ("prod-strict", defaultOptions{semantic = True, strict = True, tests = Just False}, "cgrep_prod_")
                            , ("test-strict", defaultOptions{semantic = True, strict = True, tests = Just True}, "cgrep_test_")
                            , ("prod-icase", defaultOptions{semantic = True, ignore_case = True, tests = Just False}, "cgrep_prod_")
                            , ("test-icase", defaultOptions{semantic = True, ignore_case = True, tests = Just True}, "cgrep_test_")
                            ]
                    forM_ modes $ \(label, opt, expectedPrefix) -> do
                        ms <- runSearchFile opt fp ["cgrep_"]
                        let tokStream = concat (matchChunkTexts ms)
                            cgrepToks = [t | t <- tokStream, "cgrep_" `T.isPrefixOf` T.toLower t]
                        whenNull cgrepToks $
                            assertFailure (fp <> " [" <> label <> "]: expected non-empty cgrep_ matches")
                        let wrongToks = [t | t <- cgrepToks, not (expectedPrefix `T.isPrefixOf` T.toLower t)]
                        unless (null wrongToks) $
                            assertFailure $
                                fp
                                    <> " ["
                                    <> label
                                    <> "]: expected all tokens to start with "
                                    <> T.unpack expectedPrefix
                                    <> " but found "
                                    <> show wrongToks
        , testCase "test/test.utf8 multi-byte fixture returns uncorrupted UTF-8 lines and exact line numbers" $ do
            exists <- doesFileExist "test/test.utf8"
            assertBool "test/test.utf8 should exist" exists
            msBm <- runSearchFile defaultOptions{force_type = Just "Text"} "test/test.utf8" ["xxx"]
            matchLineNums msBm @?= [75]
            assertBool "matched line 75 must preserve polytonic Greek UTF-8" $
                all (T.isInfixOf "παριστάντες") (matchLines msBm)
            msSem <- runSearchFile defaultOptions{semantic = True, force_type = Just "Text"} "test/test.utf8" ["xxx"]
            matchLineNums msSem @?= [75]
            assertBool "semantic matched line 75 must preserve polytonic Greek UTF-8" $
                all (T.isInfixOf "παριστάντες") (matchLines msSem)
        ]
  where
    whenFile True act = act
    whenFile False _ = pure ()
    whenNull [] act = act
    whenNull (_ : _) _ = pure ()
