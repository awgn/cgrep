{-# LANGUAGE OverloadedStrings #-}

module LanguageTestParsersSpec (languageTestParsersTests) where

import CGrep.FileType (FileType (..))
import CGrep.FileTypeMapTH (fileTypeLookup)
import qualified Data.Text as T
import Options (Options (..), defaultOptions)
import System.OsPath (unsafeEncodeUtf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import TestHelpers (matchLineNums, runSearchMem)

noTestsOpt :: Options
noTestsOpt = defaultOptions{semantic = True, tests = Just False}

onlyTestsOpt :: Options
onlyTestsOpt = defaultOptions{semantic = True, tests = Just True}

languageTestParsersTests :: TestTree
languageTestParsersTests =
    testGroup
        "Language-specific semantic test parser regressions"
        [ bashParserTests
        , cCppParserTests
        ]

bashParserTests :: TestTree
bashParserTests =
    testGroup
        "Bash semantic test parser"
        [ testCase "Bash: ${#var}, ${v#p}, $#, 'dir\\' single quotes, and $'it\\'s' do not break brace balancing" $ do
            let src =
                    T.unlines
                        [ "@test 'length and prefix expansions' {" -- 1 (test)
                        , "  local len=${#output}" -- 2 (test: must not leave unclosed '{')
                        , "  local base=${path##*/} cgrep_marker=1" -- 3 (test)
                        , "}" -- 4 (test)
                        , "prod_after_bats() {" -- 5 (prod)
                        , "  local dir='C:\\temp\\' quote=$'\\''" -- 6 (prod)
                        , "  if [ $# -eq 0 ]; then echo cgrep_marker; fi" -- 7 (prod)
                        , "}" -- 8 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Bash "script.sh" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Bash "script.sh" src ["cgrep_marker"]
            matchLineNums prodMs @?= [7]
            matchLineNums testMs @?= [3]
        , testCase "Bash: subshell function bodies '( ... )' with inner case patterns and hyphenated/namespaced names" $ do
            let src =
                    T.unlines
                        [ "test_subshell() (" -- 1 (test)
                        , "  case \"$1\" in" -- 2 (test)
                        , "    ok) local cgrep_marker=1 ;;" -- 3 (test: unbalanced ')' in case arm)
                        , "  esac" -- 4 (test)
                        , "  local cgrep_marker=2" -- 5 (test: still inside subshell!)
                        , ")" -- 6 (test)
                        , "function test-for-empty {" -- 7 (test: hyphenated with keyword 'for')
                        , "  local cgrep_marker=3" -- 8 (test)
                        , "}" -- 9 (test)
                        , "auth::test_login() {" -- 10 (test: Google-style :: namespace)
                        , "  local cgrep_marker=4" -- 11 (test)
                        , "}" -- 12 (test)
                        , "non-test_runner() {" -- 13 (prod: suffix 'test_runner' must NOT trigger test)
                        , "  local cgrep_marker=5" -- 14 (prod)
                        , "}" -- 15 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Bash "runner.sh" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Bash "runner.sh" src ["cgrep_marker"]
            matchLineNums prodMs @?= [14]
            matchLineNums testMs @?= [3, 5, 8, 11]
        , testCase "Bash: .bats extension maps to Bash + isTestFile, and BATS setup_suite/setup in helper files" $ do
            let ft = fst <$> fileTypeLookup defaultOptions (unsafeEncodeUtf "deploy.bats")
            ft @?= Just Bash
            let batsSrc =
                    T.unlines
                        [ "mock_curl() { echo cgrep_marker; }" -- 1 (helper inside .bats -> test)
                        , "@test 'deploy' { mock_curl; }" -- 2 (test)
                        ]
            batsProd <- runSearchMem noTestsOpt Bash "deploy.bats" batsSrc ["cgrep_marker"]
            batsTest <- runSearchMem onlyTestsOpt Bash "deploy.bats" batsSrc ["cgrep_marker"]
            matchLineNums batsProd @?= []
            matchLineNums batsTest @?= [1]
            let helperSrc =
                    T.unlines
                        [ "setup_suite() { export cgrep_marker=1; }" -- 1 (test)
                        , "setup() { export cgrep_marker=2; }" -- 2 (test: enabled by setup_suite BATS indicator)
                        , "prod_util() { echo cgrep_marker; }" -- 3 (prod)
                        ]
            hlpProd <- runSearchMem noTestsOpt Bash "test_helper.bash" helperSrc ["cgrep_marker"]
            hlpTest <- runSearchMem onlyTestsOpt Bash "test_helper.bash" helperSrc ["cgrep_marker"]
            matchLineNums hlpProd @?= [3]
            matchLineNums hlpTest @?= [1, 2]
        ]

cCppParserTests :: TestTree
cCppParserTests =
    testGroup
        "C / C++ semantic test parser"
        [ testCase "C/C++: test function return types/specifiers are captured cleanly while explicit/out-of-line constructors stay prod" $ do
            let src =
                    T.unlines
                        [ "#ifdef ENABLE_TESTS" -- 1
                        , "static void test_parse_header(int x) {" -- 2 (test: full header included)
                        , "    int cgrep_marker = x;" -- 3 (test)
                        , "}" -- 4 (test)
                        , "#endif" -- 5
                        , "class TestRunner {" -- 6 (prod)
                        , "public:" -- 7 (prod)
                        , "    explicit TestRunner(int v) {" -- 8 (prod: constructor after 'explicit', NOT a test)
                        , "        int cgrep_marker = v;" -- 9 (prod)
                        , "    }" -- 10 (prod)
                        , "};" -- 11 (prod)
                        , "TestRunner::TestRunner() {" -- 12 (prod: out-of-line constructor)
                        , "    int cgrep_marker = 0;" -- 13 (prod)
                        , "}" -- 14 (prod)
                        , "void MyFixture::test_method() {" -- 15 (test: out-of-line test method with return type)
                        , "    int cgrep_marker = 2;" -- 16 (test)
                        , "}" -- 17 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Cpp "runner.cpp" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Cpp "runner.cpp" src ["cgrep_marker"]
            matchLineNums prodMs @?= [9, 13]
            matchLineNums testMs @?= [3, 16]
            -- Return type + storage specifier must belong to test, not leak into --no-tests
            sigProd <- runSearchMem noTestsOpt Cpp "runner.cpp" src ["static void"]
            sigTest <- runSearchMem onlyTestsOpt Cpp "runner.cpp" src ["static void test_parse_header"]
            matchLineNums sigProd @?= []
            matchLineNums sigTest @?= [2]
        , testCase "C/C++: Boost/doctest braceless suites, GTest ::testing::Test fixtures, FRIEND_TEST, and INSTANTIATE_TEST_SUITE_P" $ do
            let src =
                    T.unlines
                        [ "BOOST_AUTO_TEST_SUITE(MathSuite)" -- 1 (test: braceless Boost suite)
                        , "static int suite_helper() { return cgrep_marker; }" -- 2 (test)
                        , "BOOST_AUTO_TEST_CASE(adds) { int x = cgrep_marker; }" -- 3 (test)
                        , "BOOST_AUTO_TEST_SUITE_END()" -- 4 (test)
                        , "int prod_after_boost() { return cgrep_marker; }" -- 5 (prod)
                        , "TEST_SUITE_BEGIN(\"doctest_suite\");" -- 6 (test: doctest suite begin)
                        , "static int doc_helper() { return cgrep_marker; }" -- 7 (test)
                        , "TEST_SUITE_END();" -- 8 (test)
                        , "class DbFixture : public ::testing::Test {" -- 9 (test: GTest fixture class)
                        , "protected:" -- 10 (test)
                        , "    void SetUp() override { int x = cgrep_marker; }" -- 11 (test)
                        , "};" -- 12 (test)
                        , "class Account {" -- 13 (prod)
                        , "    FRIEND_TEST(AccountTest, Debit);" -- 14 (test: semicolon macro)
                        , "    int debit() { return cgrep_marker; }" -- 15 (prod)
                        , "};" -- 16 (prod)
                        , "INSTANTIATE_TEST_SUITE_P(Grp, PTest, Values(cgrep_marker));" -- 17 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Cpp "suite.cpp" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Cpp "suite.cpp" src ["cgrep_marker"]
            matchLineNums prodMs @?= [5, 15]
            matchLineNums testMs @?= [2, 3, 7, 11, 17]
            friendProd <- runSearchMem noTestsOpt Cpp "suite.cpp" src ["FRIEND_TEST"]
            friendTest <- runSearchMem onlyTestsOpt Cpp "suite.cpp" src ["FRIEND_TEST"]
            matchLineNums friendProd @?= []
            matchLineNums friendTest @?= [14]
        ]
