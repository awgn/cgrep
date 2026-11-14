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
        , clojureParserTests
        , csharpParserTests
        , dParserTests
        , dartParserTests
        , elixirParserTests
        , elmParserTests
        , erlangParserTests
        , fsharpParserTests
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

clojureParserTests :: TestTree
clojureParserTests =
    testGroup
        "Clojure semantic test parser"
        [ testCase "Clojure: (use-fixtures :each ...), ^:meta / ^{...} metadata, and \\( \\) \\\\ char literals inside deftest" $ do
            let src =
                    T.unlines
                        [ "(use-fixtures :each (fn [f] (def cgrep_marker 1) (f)))" -- 1 (test: ':each' starts with ':')
                        , "(deftest ^{:integration true} map-meta-test" -- 2 (test: map metadata before name)
                        , "  (is (= \\( (first \"(a)\")))" -- 3 (test: '\\(' char literal must NOT leave '(' open!)
                        , "  (is (= \\\\ (first \"\\\\\")))" -- 4 (test: '\\\\' char literal right before ')')
                        , "  (def cgrep_marker 2))" -- 5 (test)
                        , "(defn prod-after-char-lit [x]" -- 6 (prod: must NOT be swallowed by '\\(' above!)
                        , "  (def cgrep_marker 3))" -- 7 (prod)
                        , "^:integration" -- 8 (test: outer metadata attached to deftest)
                        , "(deftest outer-meta-test (def cgrep_marker 4))" -- 9 (test)
                        , "(testing ^:focus \"focused\" (def cgrep_marker 5))" -- 10 (test: metadata before string in testing)
                        ]
            prodMs <- runSearchMem noTestsOpt Clojure "core.clj" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Clojure "core.clj" src ["cgrep_marker"]
            matchLineNums prodMs @?= [7]
            matchLineNums testMs @?= [1, 5, 9, 10]
            metaProd <- runSearchMem noTestsOpt Clojure "core.clj" src ["integration"]
            metaTest <- runSearchMem onlyTestsOpt Clojure "core.clj" src ["integration"]
            matchLineNums metaProd @?= []
            matchLineNums metaTest @?= [2, 8]
        , testCase "Clojure: defexpect, Midje facts/fact with '=>', factorial (fact n) guard, and *_test.clj isTestFile" $ do
            let src =
                    T.unlines
                        [ "(defn calc [n] (fact cgrep_marker))" -- 1 (prod: factorial call, NOT Midje!)
                        , "(defexpect calc-expect (expect 2 cgrep_marker))" -- 2 (test: Expectations)
                        , "(facts \"math suite\" (fact (+ 1 1) => cgrep_marker))" -- 3 (test: Midje facts)
                        , "(fact (+ 2 2) => cgrep_marker)" -- 4 (test: Midje arrow fact without string)
                        ]
            prodMs <- runSearchMem noTestsOpt Clojure "math.clj" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Clojure "math.clj" src ["cgrep_marker"]
            matchLineNums prodMs @?= [1]
            matchLineNums testMs @?= [2, 3, 4]
            let testFileSrc = "(defn helper [] cgrep_marker)\n"
            tfProd <- runSearchMem noTestsOpt Clojure "math_test.clj" testFileSrc ["cgrep_marker"]
            tfTest <- runSearchMem onlyTestsOpt Clojure "math_test.clj" testFileSrc ["cgrep_marker"]
            matchLineNums tfProd @?= []
            matchLineNums tfTest @?= [1]
        ]

csharpParserTests :: TestTree
csharpParserTests =
    testGroup
        "C# semantic test parser"
        [ testCase "C#: #region/#nullable/#endif before [Fact]/[SkippableFact] and '=> new Obj { ... }.Check()' expression body" $ do
            let src =
                    T.unlines
                        [ "#nullable enable" -- 1
                        , "[TestFixtureSource(nameof(Args))]" -- 2 (test)
                        , "public class ParamFixtureTests {" -- 3 (test)
                        , "    public void Check() { var x = cgrep_marker; }" -- 4 (test)
                        , "}" -- 5 (test)
                        , "public class OrderTests {" -- 6 (prod)
                        , "    #region FastChecks" -- 7
                        , "    [SkippableFact]" -- 8 (test: after #region identifier!)
                        , "    public void ValidatesOrder() =>" -- 9 (test: expression-bodied with object initializer)
                        , "        new Order { Id = 1 }.Verify(cgrep_marker);" -- 10 (test: after '}' before ';')
                        , "    #endregion" -- 11
                        , "    public int ProdHelper() { return cgrep_marker; }" -- 12 (prod)
                        , "}" -- 13 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Csharp "OrderTests.cs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Csharp "OrderTests.cs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [12]
            matchLineNums testMs @?= [4, 10]
        , testCase "C#: C# 11 generic attribute [Validator<Test>] stays prod, [assembly: ...] stays separate, and SpecFlow [Given]" $ do
            let src =
                    T.unlines
                        [ "[assembly: InternalsVisibleTo(\"MyApp.Tests\")]" -- 1 (prod assembly attr)
                        , "[TestFixture]" -- 2 (test)
                        , "public class SuiteTests { int x = cgrep_marker; }" -- 3 (test)
                        , "public class ProdController {" -- 4 (prod)
                        , "    [Validator<Test>]" -- 5 (prod: generic type arg 'Test' is NOT attr name!)
                        , "    public int Create() { return cgrep_marker; }" -- 6 (prod)
                        , "    [method: Fact]" -- 7 (test: explicit 'method:' target)
                        , "    public void ExplicitTargetTest() { var y = cgrep_marker; }" -- 8 (test)
                        , "    [Given(\"a valid user\")]" -- 9 (test: SpecFlow/Reqnroll BDD step)
                        , "    public void GivenValidUser() { var z = cgrep_marker; }" -- 10 (test)
                        , "}" -- 11 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Csharp "Controller.cs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Csharp "Controller.cs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [6]
            matchLineNums testMs @?= [3, 8, 10]
            asmProd <- runSearchMem noTestsOpt Csharp "Controller.cs" src ["InternalsVisibleTo"]
            asmTest <- runSearchMem onlyTestsOpt Csharp "Controller.cs" src ["InternalsVisibleTo"]
            matchLineNums asmProd @?= [1]
            matchLineNums asmTest @?= []
        ]

dParserTests :: TestTree
dParserTests =
    testGroup
        "D semantic test parser"
        [ testCase "D: version(unittest) function declaration, version(unittest): colon scope, and version(unittest) else branch" $ do
            let src =
                    T.unlines
                        [ "version(unittest) void testHelper() {" -- 1 (test: braceless version(unittest) + braced func!)
                        , "    int a = cgrep_marker;" -- 2 (test)
                        , "}" -- 3 (test)
                        , "version(unittest) {" -- 4 (test)
                        , "    enum mode = cgrep_marker;" -- 5 (test)
                        , "} else {" -- 6 (prod else branch)
                        , "    enum mode = cgrep_marker;" -- 7 (prod)
                        , "}" -- 8 (prod)
                        , "class Service {" -- 9 (prod)
                        , "    int run() { return cgrep_marker; }" -- 10 (prod)
                        , "version(unittest):" -- 11 (test up to closing '}' of Service!)
                        , "    int mockHelper() { return cgrep_marker; }" -- 12 (test)
                        , "}" -- 13 (prod closing brace of Service)
                        , "int afterClassProd() { return cgrep_marker; }" -- 14 (prod: outside Service!)
                        , "version(unittest):" -- 15 (test up to module EOF!)
                        , "int moduleTailMock() { return cgrep_marker; }" -- 16 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt D "service.d" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt D "service.d" src ["cgrep_marker"]
            matchLineNums prodMs @?= [7, 10, 14]
            matchLineNums testMs @?= [2, 5, 12, 16]
        , testCase "D: @(\"name\") UDAs before unittest, @Test/@ShouldFail with contracts, vs @safe: and @Validator!(Test)" $ do
            let src =
                    T.unlines
                        [ "@safe:" -- 1 (prod scope-wide colon attribute)
                        , "@(\"named_suite_tag\")" -- 2 (test UDA attached to unittest)
                        , "@safe pure nothrow unittest {" -- 3 (test)
                        , "    int x = cgrep_marker;" -- 4 (test)
                        , "}" -- 5 (test)
                        , "@(\"json_field\")" -- 6 (prod string UDA on prod function)
                        , "@Validator!(Test)" -- 7 (prod template UDA with 'Test' arg)
                        , "int serialize() { return cgrep_marker; }" -- 8 (prod)
                        , "@dunit.Test" -- 9 (test UDA)
                        , "int checkedAdd(int a) in { assert(a > 0); } do {" -- 10 (test with classic contract)
                        , "    return cgrep_marker;" -- 11 (test)
                        , "}" -- 12 (test)
                        , "@ShouldFail" -- 13 (unit-threaded test UDA)
                        , "int failCase(int a) in (a > 0) out (r; r > 0) {" -- 14 (test with expression contract)
                        , "    return cgrep_marker;" -- 15 (test)
                        , "}" -- 16 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt D "math.d" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt D "math.d" src ["cgrep_marker"]
            matchLineNums prodMs @?= [8]
            matchLineNums testMs @?= [4, 11, 15]
            udaProd <- runSearchMem noTestsOpt D "math.d" src ["\"named_suite_tag\""]
            udaTest <- runSearchMem onlyTestsOpt D "math.d" src ["\"named_suite_tag\""]
            matchLineNums udaProd @?= []
            matchLineNums udaTest @?= [2]
        ]

dartParserTests :: TestTree
dartParserTests =
    testGroup
        "Dart semantic test parser"
        [ testCase "Dart: named-param method bool test({required ...}) & map literal group({...}) stay prod; tear-off setUp & blocTest<B,S> are tests" $ do
            let src =
                    T.unlines
                        [ "class Validator {" -- 1 (prod)
                        , "  bool test({required String cgrep_marker}) {" -- 2 (prod: named params '{...}', NOT callback!)
                        , "    return cgrep_marker.isNotEmpty;" -- 3 (prod)
                        , "  }" -- 4 (prod)
                        , "}" -- 5 (prod)
                        , "void prodCalls(Router router) {" -- 6 (prod)
                        , "  group({'key': cgrep_marker});" -- 7 (prod: map literal, NOT closure!)
                        , "  router.group('/api', (r) { return cgrep_marker; });" -- 8 (prod: method call on receiver)
                        , "}" -- 9 (prod)
                        , "void main() {" -- 10 (prod wrapper in non-_test.dart file)
                        , "  setUp(cgrep_marker);" -- 11 (test: tear-off callback)
                        , "  test('tear-off check', cgrep_marker);" -- 12 (test: string desc + tear-off)
                        , "  blocTest<CounterBloc, int>('emits [1]', build: () => cgrep_marker);" -- 13 (test: generic blocTest)
                        , "}" -- 14
                        ]
            prodMs <- runSearchMem noTestsOpt Dart "app.dart" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Dart "app.dart" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 3, 7, 8]
            matchLineNums testMs @?= [11, 12, 13]
        , testCase "Dart: @isTest/@isTestGroup wrappers, @visibleForTesting stays prod, and *_test.dart isTestFile" $ do
            let src =
                    T.unlines
                        [ "@visibleForTesting" -- 1 (prod: exposes prod helper to tests)
                        , "int internalCalc() => cgrep_marker;" -- 2 (prod)
                        , "@isTest" -- 3 (test: package:meta custom test wrapper)
                        , "void authTest(String desc, dynamic Function() body) {" -- 4 (test)
                        , "  var tag = cgrep_marker;" -- 5 (test)
                        , "  test(desc, body);" -- 6 (test)
                        , "}" -- 7 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Dart "test_helpers.dart" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Dart "test_helpers.dart" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2]
            matchLineNums testMs @?= [5]
            let testFileSrc = "class MockRepo { int x = cgrep_marker; }\n"
            tfProd <- runSearchMem noTestsOpt Dart "login_test.dart" testFileSrc ["cgrep_marker"]
            tfTest <- runSearchMem onlyTestsOpt Dart "login_test.dart" testFileSrc ["cgrep_marker"]
            matchLineNums tfProd @?= []
            matchLineNums tfTest @?= [1]
        ]

elixirParserTests :: TestTree
elixirParserTests =
    testGroup
        "Elixir semantic test parser"
        [ testCase "Elixir: :end/:do/:fn atoms, end:/fn: keyword keys, and span.end do not break do...end balancing" $ do
            let src =
                    T.unlines
                        [ "defmodule ParserTest do" -- 1 (test)
                        , "  use ExUnit.Case" -- 2 (test)
                        , "  test \"atoms and keyword keys\" do" -- 3 (test)
                        , "    opts = [start: 0, end: 10, fn: :do]" -- 4 (test: end: and fn: and :do!)
                        , "    assert seek(:end) == span.end" -- 5 (test: :end and span.end!)
                        , "    res = %{cb: fn x -> x + 1 end, tag: cgrep_marker}" -- 6 (test: real fn...end after 'cb: ')
                        , "    assert res.tag == opts[:start]" -- 7 (test)
                        , "  end" -- 8 (test)
                        , "end" -- 9 (test)
                        , "defmodule ProdAfterSuite do" -- 10 (prod: must NOT be swallowed!)
                        , "  def run(), do: cgrep_marker" -- 11 (prod)
                        , "end" -- 12 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Elixir "parser.ex" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Elixir "parser.ex" src ["cgrep_marker"]
            matchLineNums prodMs @?= [11]
            matchLineNums testMs @?= [6]
        , testCase "Elixir: one-liner 'test ..., do:', parenthesized test(\"...\"), use ExUnit.CaseTemplate, @tag, and *_test.exs" $ do
            let src =
                    T.unlines
                        [ "@tag :cgrep_marker" -- 1 (test: ExUnit @tag attribute)
                        , "test \"one-liner\", do: assert(cgrep_marker == 1)" -- 2 (test: inline do: without end!)
                        , "defmodule ProdService do" -- 3 (prod: must NOT be swallowed by one-liner!)
                        , "  def ok(), do: cgrep_marker" -- 4 (prod)
                        , "end" -- 5 (prod)
                        , "test(\"parenthesized call\", %{a: a}) do" -- 6 (test: parenthesized test(...) do)
                        , "  assert a == cgrep_marker" -- 7 (test)
                        , "end" -- 8 (test)
                        , "defmodule MyApp.ConnCase do" -- 9 (test: CaseTemplate module not ending in 'Test')
                        , "  use ExUnit.CaseTemplate" -- 10 (test)
                        , "  def helper(), do: cgrep_marker" -- 11 (test)
                        , "end" -- 12 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Elixir "suite.exs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Elixir "suite.exs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [4]
            matchLineNums testMs @?= [1, 2, 7, 11]
            let testFileSrc = "defmodule FakeMailer do\n  def send(), do: cgrep_marker\nend\n"
            tfProd <- runSearchMem noTestsOpt Elixir "accounts_test.exs" testFileSrc ["cgrep_marker"]
            tfTest <- runSearchMem onlyTestsOpt Elixir "accounts_test.exs" testFileSrc ["cgrep_marker"]
            matchLineNums tfProd @?= []
            matchLineNums tfTest @?= [2]
        ]

elmParserTests :: TestTree
elmParserTests =
    testGroup
        "Elm semantic test parser"
        [ testCase "Elm: parameterized 'String -> Test' & 'List Test' vs prod 'Test -> String' false positive" $ do
            let src =
                    T.unlines
                        [ "formatRunner : Test -> String" -- 1 (prod: takes Test, returns String!)
                        , "formatRunner t =" -- 2 (prod)
                        , "    cgrep_marker" -- 3 (prod)
                        , "" -- 4
                        , "checkCase : String -> Int -> Test" -- 5 (test: parameterized helper returning Test)
                        , "checkCase label expected =" -- 6 (test: has params before '=')
                        , "    Expect.equal expected cgrep_marker" -- 7 (test)
                        , "" -- 8
                        , "unannotatedProdAfterParamTest x =" -- 9 (prod: must NOT be swallowed!)
                        , "    x + cgrep_marker" -- 10 (prod)
                        , "" -- 11
                        , "unitCases : List Test.Test" -- 12 (test: List Test.Test)
                        , "unitCases =" -- 13 (test)
                        , "    [ checkCase \"one\" cgrep_marker ]" -- 14 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Elm "src/Suite.elm" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Elm "src/Suite.elm" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 10]
            matchLineNums testMs @?= [7, 14]
        , testCase "Elm: Test.concat, Test.skip, Test.only, complex fuzz (>8 tokens), and tests/*.elm isTestFile" $ do
            let src =
                    T.unlines
                        [ "allTests =" -- 1 (test: Test.concat builder)
                        , "    Test.concat [ cgrep_marker ]" -- 2 (test)
                        , "" -- 3
                        , "focused =" -- 4 (test: Test.only + complex fuzz)
                        , "    Test.only <|" -- 5 (test)
                        , "        fuzz (Fuzz.map2 Pair Fuzz.int Fuzz.string) \"pair fuzz\" (\\_ -> cgrep_marker)" -- 6 (test)
                        , "" -- 7
                        , "effect module Ports where { command = MyCmd } exposing (..)" -- 8 (prod: 'effect module' stops test!)
                        , "prodHelper x = cgrep_marker" -- 9 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Elm "src/Helpers.elm" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Elm "src/Helpers.elm" src ["cgrep_marker"]
            matchLineNums prodMs @?= [9]
            matchLineNums testMs @?= [2, 6]
            let testFileSrc = "helperVal = cgrep_marker\n"
            tfProd <- runSearchMem noTestsOpt Elm "tests/Spec.elm" testFileSrc ["cgrep_marker"]
            tfTest <- runSearchMem onlyTestsOpt Elm "tests/Spec.elm" testFileSrc ["cgrep_marker"]
            matchLineNums tfProd @?= []
            matchLineNums tfTest @?= [1]
        ]

erlangParserTests :: TestTree
erlangParserTests =
    testGroup
        "Erlang semantic test parser"
        [ testCase "Erlang: -ifdef(TEST)...-else...-endif, $. char literal inside *_test(), and guarded *_test() when" $ do
            let src =
                    T.unlines
                        [ "-module(codec)." -- 1 (prod)
                        , "encode(X) -> {ok, X, cgrep_marker}." -- 2 (prod)
                        , "-ifdef(TEST)." -- 3 (test)
                        , "fixture_helper(X) -> {mock, X, cgrep_marker}." -- 4 (test: inside -ifdef(TEST))
                        , "-ifdef(EXTRA)." -- 5 (test: nested ifdef)
                        , "extra_helper() -> cgrep_marker." -- 6 (test)
                        , "-endif." -- 7 (test: closes EXTRA, NOT TEST!)
                        , "after_nested_helper() -> cgrep_marker." -- 8 (test: still inside TEST!)
                        , "-else." -- 9 (prod else branch of -ifdef(TEST))
                        , "prod_fallback() -> cgrep_marker." -- 10 (prod)
                        , "-endif." -- 11
                        , "dot_char_test() when true ->" -- 12 (test: guard + $. char literal!)
                        , "    Sep = $.," -- 13 (test: $. must NOT terminate function!)
                        , "    {Sep, cgrep_marker}." -- 14 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Erlang "src/codec.erl" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Erlang "src/codec.erl" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 10]
            matchLineNums testMs @?= [4, 6, 8, 14]
        , testCase "Erlang: PropEr prop_*(), Common Test *_SUITE.erl and Rebar3 *_tests.erl isTestFile" $ do
            let src =
                    T.unlines
                        [ "prop_roundtrip() ->" -- 1 (test: PropEr 0-arity property)
                        , "    ?FORALL(X, int(), encode(X) =:= cgrep_marker)." -- 2 (test)
                        , "prop_helper(Arity1) ->" -- 3 (prod: 1-arity function is NOT a 0-arity PropEr property)
                        , "    {Arity1, cgrep_marker}." -- 4 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Erlang "src/props.erl" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Erlang "src/props.erl" src ["cgrep_marker"]
            matchLineNums prodMs @?= [4]
            matchLineNums testMs @?= [2]
            let suiteSrc = "helper(Cfg) -> {Cfg, cgrep_marker}.\n"
            ctProd <- runSearchMem noTestsOpt Erlang "test/http_SUITE.erl" suiteSrc ["cgrep_marker"]
            ctTest <- runSearchMem onlyTestsOpt Erlang "test/http_SUITE.erl" suiteSrc ["cgrep_marker"]
            matchLineNums ctProd @?= []
            matchLineNums ctTest @?= [1]
            euProd <- runSearchMem noTestsOpt Erlang "test/http_tests.erl" suiteSrc ["cgrep_marker"]
            euTest <- runSearchMem onlyTestsOpt Erlang "test/http_tests.erl" suiteSrc ["cgrep_marker"]
            matchLineNums euProd @?= []
            matchLineNums euTest @?= [1]
        ]

fsharpParserTests :: TestTree
fsharpParserTests =
    testGroup
        "F# semantic test parser"
        [ testCase "F#: nested 'let f x =' inside [<Fact>], backtick names, and unattributed helpers inside [<TestFixture>]" $ do
            let src =
                    T.unlines
                        [ "let prodBefore x = cgrep_marker" -- 1 (prod)
                        , "[<Fact>]" -- 2 (test)
                        , "let ``should handle (parens) in backtick name`` () =" -- 3 (test)
                        , "    let localHelper f x = f x" -- 4 (test: nested function must NOT end test!)
                        , "    let v = localHelper id cgrep_marker" -- 5 (test)
                        , "    Assert.Equal(v, v)" -- 6 (test)
                        , "[<TestFixture>]" -- 7 (test)
                        , "type OrderFixture() =" -- 8 (test)
                        , "    let seedOrder x = x + cgrep_marker" -- 9 (test: class let binding inside TestFixture)
                        , "    member private this.BuildHelper() = cgrep_marker" -- 10 (test: helper member inside TestFixture)
                        , "    [<Test>]" -- 11 (test)
                        , "    member this.CheckOrder() = Assert.Pass()" -- 12 (test)
                        , "type MixedService() =" -- 13 (prod class)
                        , "    [<Test>]" -- 14 (test member)
                        , "    member this.InlineCheck() =" -- 15 (test)
                        , "        let innerFn a = a + cgrep_marker" -- 16 (test)
                        , "        innerFn 1" -- 17 (test)
                        , "    member this.ProdMethod() = cgrep_marker" -- 18 (prod member!)
                        , "let prodAfter x = cgrep_marker" -- 19 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Fsharp "src/Service.fs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Fsharp "src/Service.fs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [1, 18, 19]
            matchLineNums testMs @?= [5, 9, 10, 16]
        , testCase "F#: Expecto [<Tests>], testPropertyWithConfig, testTask, [<assembly: ...>], and generic [<Validator<Test>>]" $ do
            let src =
                    T.unlines
                        [ "[<assembly: NUnit.Framework.SetUpFixture>]" -- 1 (test assembly attr)
                        , "do ()" -- 2
                        , "[<Validator<Test>>]" -- 3 (prod generic attr referencing Test type arg)
                        , "let validatedProd x = cgrep_marker" -- 4 (prod)
                        , "[<Tests>]" -- 5 (test: Expecto [<Tests>] attribute)
                        , "let suiteWithSharedSetup =" -- 6 (test)
                        , "    let sharedCfg = cgrep_marker" -- 7 (test)
                        , "    testList \"suite\" [" -- 8 (test)
                        , "        testPropertyWithConfig sharedCfg \"prop\" <| fun x ->" -- 9 (test)
                        , "            let checkProp y = y = cgrep_marker" -- 10 (test)
                        , "            checkProp x" -- 11 (test)
                        , "        testTask \"async task\" {" -- 12 (test)
                        , "            return cgrep_marker" -- 13 (test)
                        , "        }" -- 14 (test)
                        , "    ]" -- 15 (test)
                        , "let prodEnd x = cgrep_marker" -- 16 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Fsharp "src/Suite.fs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Fsharp "src/Suite.fs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [4, 16]
            matchLineNums testMs @?= [7, 10, 13]
        ]
