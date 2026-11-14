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
        , goParserTests
        , haskellParserTests
        , javaParserTests
        , javascriptParserTests
        , juliaParserTests
        , kotlinParserTests
        , luaParserTests
        , nimParserTests
        , ocamlParserTests
        , phpParserTests
        , perlParserTests
        , pythonParserTests
        , rParserTests
        , rubyParserTests
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

goParserTests :: TestTree
goParserTests =
    testGroup
        "Go semantic test parser"
        [ testCase "Go: generic func TestXxx[T any], func-returning suite method, vs prod (p *Pool) TestConnection(...) error" $ do
            let src =
                    T.unlines
                        [ "package pool" -- 1
                        , "type Pool struct{}" -- 2
                        , "func (p *Pool) TestConnection(host string) error {" -- 3 (prod method on non-suite receiver with params+error!)
                        , "    _ = cgrep_marker" -- 4 (prod)
                        , "    return nil" -- 5
                        , "}" -- 6
                        , "func TestRoundTrip[T comparable](t *testing.T) {" -- 7 (test: Go 1.18+ generic test function)
                        , "    _ = cgrep_marker" -- 8 (test)
                        , "}" -- 9
                        , "type HandlerTestSuite struct{}" -- 10
                        , "func (s *HandlerTestSuite) TestMiddleware() func(int) int {" -- 11 (test: returns func(int) int!)
                        , "    return func(x int) int { return x + cgrep_marker }" -- 12 (test)
                        , "}" -- 13
                        ]
            prodMs <- runSearchMem noTestsOpt Go "pool.go" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Go "pool.go" src ["cgrep_marker"]
            matchLineNums prodMs @?= [4]
            matchLineNums testMs @?= [8, 12]
        , testCase "Go: Ginkgo Describe/When/It/BeforeEach/PIt and GoConvey Convey blocks vs obj.Describe(...)" $ do
            let src =
                    T.unlines
                        [ "package api" -- 1
                        , "func prodDescribe(svc Service) {" -- 2 (prod)
                        , "    svc.Describe(\"resource\", func() { _ = cgrep_marker })" -- 3 (prod: method call .Describe)
                        , "}" -- 4
                        , "var _ = Describe(\"Router\", func() {" -- 5 (test: Ginkgo)
                        , "    BeforeEach(func() {" -- 6 (test)
                        , "        _ = cgrep_marker" -- 7 (test)
                        , "    })" -- 8
                        , "    When(\"authenticated\", func() {" -- 9 (test)
                        , "        It(\"serves requests\", func() {" -- 10 (test)
                        , "            _ = cgrep_marker" -- 11 (test)
                        , "        })" -- 12
                        , "        PIt(\"pending spec without callback\", cgrep_marker)" -- 13 (test)
                        , "    })" -- 14
                        , "})" -- 15
                        , "func prodAfter() { _ = cgrep_marker }" -- 16 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Go "router.go" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Go "router.go" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 16]
            matchLineNums testMs @?= [7, 11, 13]
        ]

haskellParserTests :: TestTree
haskellParserTests =
    testGroup
        "Haskell semantic test parser"
        [ testCase "Haskell: unparenthesized '::' in let/where inside Spec/prop_*, multi-clause prop_*, and untyped prod fn" $ do
            let src =
                    T.unlines
                        [ "prodFirst :: Int -> Int" -- 1 (prod)
                        , "prodFirst x = x + cgrep_marker" -- 2 (prod)
                        , "" -- 3
                        , "spec :: Spec" -- 4 (test: :: Spec return type)
                        , "spec = do" -- 5 (test)
                        , "  let sharedFixture :: Int" -- 6 (test: local :: before describe!)
                        , "      sharedFixture = cgrep_marker" -- 7 (test)
                        , "  describe \"parser\" $ do" -- 8 (test)
                        , "    it \"handles local type sigs\" $ do" -- 9 (test)
                        , "      let y :: Int" -- 10 (test: unparenthesized :: inside it!)
                        , "          y = sharedFixture + cgrep_marker" -- 11 (test)
                        , "      pure ()" -- 12 (test)
                        , "" -- 13
                        , "prop_nonEmpty :: [Int] -> Bool" -- 14 (test)
                        , "prop_nonEmpty [] = True" -- 15 (test: pattern-matched 1st clause)
                        , "prop_nonEmpty (x : xs) = check x || prop_nonEmpty xs" -- 16 (test: 2nd clause!)
                        , "  where" -- 17 (test)
                        , "    check :: Int -> Bool" -- 18 (test: where helper with ::)
                        , "    check n = n == cgrep_marker" -- 19 (test)
                        , "" -- 20
                        , "untypedProdAfter z = z * cgrep_marker" -- 21 (prod: untyped top-level function!)
                        ]
            prodMs <- runSearchMem noTestsOpt Haskell "src/Codec.hs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Haskell "src/Codec.hs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 21]
            matchLineNums testMs @?= [7, 11, 19]
        , testCase "Haskell: Hedgehog hprop_*, goldenVsString, 'Spec -> IO ()' prod false-positive guard, and *Spec.hs isTestFile" $ do
            let src =
                    T.unlines
                        [ "runSuite :: Spec -> IO ()" -- 1 (prod: Spec is input arg, NOT return type!)
                        , "runSuite s = hspec s >> print cgrep_marker" -- 2 (prod)
                        , "" -- 3
                        , "hprop_roundtrip :: Property" -- 4 (test: Hedgehog hprop_*)
                        , "hprop_roundtrip = property $ do" -- 5 (test)
                        , "  let v = cgrep_marker" -- 6 (test)
                        , "  v === v" -- 7 (test)
                        , "" -- 8
                        , "goldenCheck =" -- 9
                        , "  goldenVsString \"golden\" \"out.golden\" (pure cgrep_marker)" -- 10 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Haskell "src/Runner.hs" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Haskell "src/Runner.hs" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2]
            matchLineNums testMs @?= [6, 10]
            let specFileSrc = "helperVal = cgrep_marker\n"
            sfProd <- runSearchMem noTestsOpt Haskell "specs/CodecSpec.hs" specFileSrc ["cgrep_marker"]
            sfTest <- runSearchMem onlyTestsOpt Haskell "specs/CodecSpec.hs" specFileSrc ["cgrep_marker"]
            matchLineNums sfProd @?= []
            matchLineNums sfTest @?= [1]
        ]

javaParserTests :: TestTree
javaParserTests =
    testGroup
        "Java semantic test parser"
        [ testCase "Java: @BeforeMapping/@AfterReturning/@Validator<Test> stay prod; qualified extends junit.framework.TestCase & public modifier" $ do
            let src =
                    T.unlines
                        [ "public class OrderMapper {" -- 1 (prod)
                        , "    @BeforeMapping" -- 2 (prod: MapStruct, NOT JUnit @Before!)
                        , "    public void enrich() { int x = cgrep_marker; }" -- 3 (prod)
                        , "    @AfterReturning(\"execution(* *(..))\")" -- 4 (prod: Spring AOP, NOT JUnit @After!)
                        , "    public void logReturn() { int y = cgrep_marker; }" -- 5 (prod)
                        , "    @Validator<Test>" -- 6 (prod: generic type arg Test)
                        , "    public void check() { int z = cgrep_marker; }" -- 7 (prod)
                        , "}" -- 8 (prod)
                        , "public final class LegacyOrderTest extends junit.framework.TestCase {" -- 9 (test: modifiers + qualified TestCase!)
                        , "    public void testCreate() { int a = cgrep_marker; }" -- 10 (test)
                        , "}" -- 11 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Java "src/main/java/OrderMapper.java" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Java "src/main/java/OrderMapper.java" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 5, 7]
            matchLineNums testMs @?= [10]
            modProd <- runSearchMem noTestsOpt Java "src/main/java/OrderMapper.java" src ["public final class LegacyOrderTest"]
            modTest <- runSearchMem onlyTestsOpt Java "src/main/java/OrderMapper.java" src ["public final class LegacyOrderTest"]
            matchLineNums modProd @?= []
            matchLineNums modTest @?= [9]
        , testCase "Java: @RegisterExtension anonymous-class field, @Mock/@TempDir fields, jqwik @Property, Cucumber @Given, and src/test/java" $ do
            let src =
                    T.unlines
                        [ "class MixedOrderCheck {" -- 1 (prod wrapper)
                        , "    @Mock private OrderRepo cgrep_marker;" -- 2 (test field)
                        , "    @RegisterExtension" -- 3 (test field with anonymous class + trailing tokens before ';')
                        , "    static ServerExt ext = new ServerExt() { int v = cgrep_marker; }.build(cgrep_marker);" -- 4 (test)
                        , "    @Property" -- 5 (test: jqwik property)
                        , "    boolean positive(@ForAll int n) { return n == cgrep_marker; }" -- 6 (test)
                        , "    @Given(\"an existing order\")" -- 7 (test: Cucumber step)
                        , "    public void givenOrder() { int k = cgrep_marker; }" -- 8 (test)
                        , "    int prodCalc() { return cgrep_marker; }" -- 9 (prod)
                        , "}" -- 10
                        ]
            prodMs <- runSearchMem noTestsOpt Java "src/main/java/MixedOrderCheck.java" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Java "src/main/java/MixedOrderCheck.java" src ["cgrep_marker"]
            matchLineNums prodMs @?= [9]
            matchLineNums testMs @?= [2, 4, 6, 8]
            let testDirSrc = "class TestDataFactory { static int build() { return cgrep_marker; } }\n"
            tdProd <- runSearchMem noTestsOpt Java "src/test/java/com/acme/TestDataFactory.java" testDirSrc ["cgrep_marker"]
            tdTest <- runSearchMem onlyTestsOpt Java "src/test/java/com/acme/TestDataFactory.java" testDirSrc ["cgrep_marker"]
            matchLineNums tdProd @?= []
            matchLineNums tdTest @?= [1]
        ]

javascriptParserTests :: TestTree
javascriptParserTests =
    testGroup
        "JavaScript / TypeScript semantic test parser"
        [ testCase "JS/TS: Playwright test.describe/test.beforeEach/test.step, Deno.test, and tear-off callbacks" $ do
            let src =
                    T.unlines
                        [ "interface Runner {" -- 1 (prod TS interface)
                        , "  test(cb: () => cgrep_marker);" -- 2 (prod: TS method signature with param ':', NOT a test call!)
                        , "}" -- 3
                        , "function* test(cb = () => cgrep_marker) { yield 1; }" -- 4 (prod generator function)
                        , "test.describe('Checkout flow', () => {" -- 5 (test: Playwright test.describe!)
                        , "  test.beforeEach(cgrep_marker);" -- 6 (test: Playwright hook with tear-off!)
                        , "  test('completes order', async ({ page }) => {" -- 7 (test)
                        , "    await test.step('fill form', async () => { const a = cgrep_marker; });" -- 8 (test)
                        , "  });" -- 9
                        , "});" -- 10
                        , "Deno.test('deno unit', () => { const b = cgrep_marker; });" -- 11 (test: Deno.test)
                        , "it('supports tear-off ref', cgrep_marker);" -- 12 (test: tear-off callback)
                        , "const prodAfter = cgrep_marker;" -- 13 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Javascript "src/checkout.ts" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Javascript "src/checkout.ts" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 4, 13]
            matchLineNums testMs @?= [6, 8, 11, 12]
        , testCase "JS/TS: QUnit.module/QUnit.test, Ava test.serial, ftest, and *.spec.ts / __tests__ isTestFile" $ do
            let src =
                    T.unlines
                        [ "QUnit.module('math', (hooks) => {" -- 1 (test)
                        , "  QUnit.test('adds', (assert) => { assert.equal(cgrep_marker, 2); });" -- 2 (test)
                        , "});" -- 3
                        , "test.serial('ava serial', (t) => { t.is(cgrep_marker, 1); });" -- 4 (test)
                        , "ftest('focused jest test', () => { expect(cgrep_marker).toBe(1); });" -- 5 (test)
                        , "const prodVal = runner?.test(() => cgrep_marker);" -- 6 (prod: optional chaining ?.test)
                        ]
            prodMs <- runSearchMem noTestsOpt Javascript "src/math.ts" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Javascript "src/math.ts" src ["cgrep_marker"]
            matchLineNums prodMs @?= [6]
            matchLineNums testMs @?= [2, 4, 5]
            let specFileSrc = "export const fixture = cgrep_marker;\n"
            spProd <- runSearchMem noTestsOpt Javascript "src/auth.spec.ts" specFileSrc ["cgrep_marker"]
            spTest <- runSearchMem onlyTestsOpt Javascript "src/auth.spec.ts" specFileSrc ["cgrep_marker"]
            matchLineNums spProd @?= []
            matchLineNums spTest @?= [1]
            duProd <- runSearchMem noTestsOpt Javascript "src/__tests__/helpers.ts" specFileSrc ["cgrep_marker"]
            duTest <- runSearchMem onlyTestsOpt Javascript "src/__tests__/helpers.ts" specFileSrc ["cgrep_marker"]
            matchLineNums duProd @?= []
            matchLineNums duTest @?= [1]
        ]

juliaParserTests :: TestTree
juliaParserTests =
    testGroup
        "Julia semantic test parser"
        [ testCase "Julia: single-line @test does not swallow subsequent prod assignments or prod for-loops" $ do
            let src =
                    T.unlines
                        [ "@test add(2, 3) == cgrep_marker" -- 1 (test: single-line @test)
                        , "PROD_CONST = cgrep_marker" -- 2 (prod: must NOT be swallowed by @test above!)
                        , "@test check(1) == true" -- 3 (test)
                        , "for i in 1:3" -- 4 (prod for-loop on next line: NOT a @testset for-block!)
                        , "    acc = cgrep_marker" -- 5 (prod)
                        , "end" -- 6 (prod)
                        , "@testset \"looped suite $i\" for i in 1:2" -- 7 (test: @testset for-block on SAME line!)
                        , "    @test i == cgrep_marker" -- 8 (test)
                        , "end" -- 9 (test)
                        ]
            prodMs <- runSearchMem noTestsOpt Julia "src/calc.jl" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Julia "src/calc.jl" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 5]
            matchLineNums testMs @?= [1, 8]
        , testCase "Julia: generator (x for x in ... if ...), abstract type...end, :end/obj.end inside @testset, Test.@testset, and runtests.jl" $ do
            let src =
                    T.unlines
                        [ "Test.@testset \"generators and types\" begin" -- 1 (test: module-qualified Test.@testset)
                        , "    abstract type MockNode end" -- 2 (test: abstract type...end must NOT close @testset!)
                        , "    total = sum(x for x in 1:5 if x > 0)" -- 3 (test: generator for/if without end!)
                        , "    sym = (:end, span.end)" -- 4 (test: :end symbol and .end field!)
                        , "    v = cgrep_marker" -- 5 (test: still inside @testset!)
                        , "end" -- 6 (test: actual closing end of @testset!)
                        , "prod_after_suite = cgrep_marker" -- 7 (prod: must NOT be swallowed!)
                        ]
            prodMs <- runSearchMem noTestsOpt Julia "src/suite.jl" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Julia "src/suite.jl" src ["cgrep_marker"]
            matchLineNums prodMs @?= [7]
            matchLineNums testMs @?= [5]
            qualProd <- runSearchMem noTestsOpt Julia "src/suite.jl" src ["Test"]
            matchLineNums qualProd @?= []
            let rtSrc = "helper_fixture() = cgrep_marker\n"
            rtProd <- runSearchMem noTestsOpt Julia "test/runtests.jl" rtSrc ["cgrep_marker"]
            rtTest <- runSearchMem onlyTestsOpt Julia "test/runtests.jl" rtSrc ["cgrep_marker"]
            matchLineNums rtProd @?= []
            matchLineNums rtTest @?= [1]
        ]

kotlinParserTests :: TestTree
kotlinParserTests =
    testGroup
        "Kotlin semantic test parser"
        [ testCase "Kotlin: backtick fun names with keywords, braceless '= assertEquals(...)', @get:Rule, vs @BeforeMapping" $ do
            let src =
                    T.unlines
                        [ "class OrderService {" -- 1 (prod)
                        , "    @BeforeMapping" -- 2 (prod: MapStruct, NOT JUnit!)
                        , "    fun enrich() { val a = cgrep_marker }" -- 3 (prod)
                        , "    @get:Rule" -- 4 (test: Kotlin use-site target @get:Rule)
                        , "    val tempFolder = TemporaryFolder(cgrep_marker)" -- 5 (test)
                        , "    @Test" -- 6 (test: backtick method name containing fun/val/class/;)
                        , "    fun `when fun or val or ; is in name`() {" -- 7 (test)
                        , "        val b = cgrep_marker" -- 8 (test)
                        , "    }" -- 9
                        , "    @Test" -- 10 (test: expression-bodied test without braces!)
                        , "    fun singleLineExprTest() = assertEquals(1, cgrep_marker)" -- 11 (test)
                        , "    fun prodAfterExprTest() = cgrep_marker" -- 12 (prod: must NOT be swallowed!)
                        , "}" -- 13
                        ]
            prodMs <- runSearchMem noTestsOpt Kotlin "src/main/kotlin/OrderService.kt" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Kotlin "src/main/kotlin/OrderService.kt" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 12]
            matchLineNums testMs @?= [5, 8, 11]
        , testCase "Kotlin: Kotest BehaviorSpec Given/When/Then, StringSpec .config(...), and KMP src/commonTest isTestFile" $ do
            let src =
                    T.unlines
                        [ "fun prodRunner(runner: Runner) {" -- 1 (prod)
                        , "    runner.test(\"not kotest\") { val x = cgrep_marker }" -- 2 (prod: method call on receiver)
                        , "}" -- 3
                        , "class AuthSpec : BehaviorSpec({" -- 4
                        , "    Given(\"a user\") {" -- 5 (test: capitalized Kotest Given)
                        , "        When(\"logging in\") {" -- 6 (test: capitalized Kotest When)
                        , "            Then(\"succeeds\") { val y = cgrep_marker }" -- 7 (test: capitalized Kotest Then)
                        , "        }" -- 8
                        , "    }" -- 9
                        , "    \"configured string spec\".config(invocations = 2) {" -- 10 (test: .config(...) before '{')
                        , "        val z = cgrep_marker" -- 11 (test)
                        , "    }" -- 12
                        , "})" -- 13
                        ]
            prodMs <- runSearchMem noTestsOpt Kotlin "src/main/kotlin/Auth.kt" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Kotlin "src/main/kotlin/Auth.kt" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2]
            matchLineNums testMs @?= [7, 11]
            let kmpSrc = "class FakeClock { val now = cgrep_marker }\n"
            kmpProd <- runSearchMem noTestsOpt Kotlin "shared/src/commonTest/kotlin/FakeClock.kt" kmpSrc ["cgrep_marker"]
            kmpTest <- runSearchMem onlyTestsOpt Kotlin "shared/src/commonTest/kotlin/FakeClock.kt" kmpSrc ["cgrep_marker"]
            matchLineNums kmpProd @?= []
            matchLineNums kmpTest @?= [1]
        ]

luaParserTests :: TestTree
luaParserTests =
    testGroup
        "Lua semantic test parser"
        [ testCase "Lua: nested LuaUnit table paths M.AuthTests:setUp/test_*, lust.describe/lust.before, and Busted tear-offs" $ do
            let src =
                    T.unlines
                        [ "local M = {}" -- 1 (prod)
                        , "M.AuthTests = {}" -- 2 (test: nested LuaUnit suite table)
                        , "function M.AuthTests:setUp()" -- 3 (test: nested table lifecycle method)
                        , "    self.token = cgrep_marker" -- 4 (test)
                        , "end" -- 5
                        , "function M.AuthTests:test_login()" -- 6 (test: nested table test method)
                        , "    assert(self.token == cgrep_marker)" -- 7 (test)
                        , "end" -- 8
                        , "function M.AuthService:validate()" -- 9 (prod: non-test table & method)
                        , "    return cgrep_marker" -- 10 (prod)
                        , "end" -- 11
                        , "lust.describe('auth', function()" -- 12 (test: qualified lust.describe)
                        , "    lust.before(function() local x = cgrep_marker end)" -- 13 (test: lust.before)
                        , "    lust.it('checks token', function() assert(cgrep_marker) end)" -- 14 (test: lust.it)
                        , "end)" -- 15
                        , "before_each(cgrep_marker)" -- 16 (test: Busted tear-off hook)
                        , "it('supports tear-off callback', cgrep_marker)" -- 17 (test: Busted tear-off spec)
                        , "setup(cgrep_marker)" -- 18 (prod: plugin setup(opts), NOT Busted setup(function() ...))
                        ]
            prodMs <- runSearchMem noTestsOpt Lua "lua/auth.lua" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Lua "lua/auth.lua" src ["cgrep_marker"]
            matchLineNums prodMs @?= [10, 18]
            matchLineNums testMs @?= [4, 7, 13, 14, 16, 17]
            lustProd <- runSearchMem noTestsOpt Lua "lua/auth.lua" src ["lust"]
            matchLineNums lustProd @?= []
        , testCase "Lua: *_spec.lua and test_*.lua isTestFile conventions" $ do
            let specSrc = "local function helper_fixture() return cgrep_marker end\n"
            spProd <- runSearchMem noTestsOpt Lua "tests/parser_spec.lua" specSrc ["cgrep_marker"]
            spTest <- runSearchMem onlyTestsOpt Lua "tests/parser_spec.lua" specSrc ["cgrep_marker"]
            matchLineNums spProd @?= []
            matchLineNums spTest @?= [1]
            luProd <- runSearchMem noTestsOpt Lua "tests/test_parser.lua" specSrc ["cgrep_marker"]
            luTest <- runSearchMem onlyTestsOpt Lua "tests/test_parser.lua" specSrc ["cgrep_marker"]
            matchLineNums luProd @?= []
            matchLineNums luTest @?= [1]
        ]

nimParserTests :: TestTree
nimParserTests =
    testGroup
        "Nim semantic test parser"
        [ testCase "Nim: indented local proc/type inside suite/test stay test; top-level let/var after test stay prod" $ do
            let src =
                    T.unlines
                        [ "suite \"codec suite\":" -- 1 (test)
                        , "  type MockPacket = object" -- 2 (test: indented local type must NOT end suite!)
                        , "    tag: int" -- 3 (test)
                        , "  proc makePacket(): int =" -- 4 (test: indented local proc must NOT end suite!)
                        , "    return cgrep_marker" -- 5 (test)
                        , "  setup:" -- 6 (test)
                        , "    let seed = cgrep_marker" -- 7 (test)
                        , "  test(\"encodes packet\"):" -- 8 (test: parenthesized test("..."):)
                        , "    let v = makePacket() + seed + cgrep_marker" -- 9 (test)
                        , "" -- 10
                        , "let topLevelProdConst = cgrep_marker" -- 11 (prod: col 0 let after suite!)
                        , "if test(\"prod_predicate\"):" -- 12 (prod: 'test' not at start of line!)
                        , "  var x = cgrep_marker" -- 13 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Nim "src/codec.nim" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Nim "src/codec.nim" src ["cgrep_marker"]
            matchLineNums prodMs @?= [11, 13]
            matchLineNums testMs @?= [5, 7, 9]
        , testCase "Nim: when defined(test): with else branch, top-level setup:, and tests/*.nim isTestFile" $ do
            let src =
                    T.unlines
                        [ "when defined(test):" -- 1 (test)
                        , "  proc mockClock(): int = cgrep_marker" -- 2 (test)
                        , "else:" -- 3 (prod else branch at col 0!)
                        , "  proc realClock(): int = cgrep_marker" -- 4 (prod)
                        , "setup:" -- 5 (test: module-level std/unittest setup:)
                        , "  var globalFixture = cgrep_marker" -- 6 (test)
                        , "var prodAfterSetup = cgrep_marker" -- 7 (prod: col 0 var after setup!)
                        ]
            prodMs <- runSearchMem noTestsOpt Nim "src/clock.nim" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Nim "src/clock.nim" src ["cgrep_marker"]
            matchLineNums prodMs @?= [4, 7]
            matchLineNums testMs @?= [2, 6]
            let tfSrc = "proc helper(): int = cgrep_marker\n"
            tfProd <- runSearchMem noTestsOpt Nim "tests/tcodec.nim" tfSrc ["cgrep_marker"]
            tfTest <- runSearchMem onlyTestsOpt Nim "tests/tcodec.nim" tfSrc ["cgrep_marker"]
            matchLineNums tfProd @?= []
            matchLineNums tfTest @?= [1]
        ]

ocamlParserTests :: TestTree
ocamlParserTests =
    testGroup
        "OCaml semantic test parser"
        [ testCase "OCaml: ';;' terminates let%test before top-level expr, module%test struct...end, and [%%expect_test ...]" $ do
            let src =
                    T.unlines
                        [ "let%test \"stops at double semi\" =" -- 1 (test)
                        , "  let x = cgrep_marker in x = 1;;" -- 2 (test: ends at ;;)
                        , "print_endline cgrep_marker;;" -- 3 (prod: top-level expr after ;; without 'let'!)
                        , "module%test AuthSuite = struct" -- 4 (test: Jane Street module%test)
                        , "  let helper () = cgrep_marker" -- 5 (test)
                        , "end" -- 6 (test)
                        , "module%test _ = struct" -- 7 (test: anonymous module%test _)
                        , "  let anon_helper () = cgrep_marker" -- 8 (test)
                        , "end" -- 9 (test)
                        , "[%%expect_test" -- 10 (test: item extension [%%expect_test ...])
                        , "  let _ = cgrep_marker" -- 11 (test)
                        , "]" -- 12 (test)
                        , "let prod_after x = x + cgrep_marker" -- 13 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt OCaml "lib/auth.ml" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt OCaml "lib/auth.ml" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 13]
            matchLineNums testMs @?= [2, 5, 8, 11]
        , testCase "OCaml: Alcotest.test_case, module TestSuite : sig...end = struct...end, and test_*.ml isTestFile" $ do
            let src =
                    T.unlines
                        [ "let prod_helper () = cgrep_marker" -- 1 (prod)
                        , "let cases = [" -- 2 (prod list wrapper)
                        , "  Alcotest.test_case \"adds\" `Quick (fun () -> ignore cgrep_marker);" -- 3 (test: Alcotest)
                        , "]" -- 4
                        , "module ParserTests : sig end = struct" -- 5 (test: module signature before = struct)
                        , "  let v = cgrep_marker" -- 6 (test)
                        , "end" -- 7
                        ]
            prodMs <- runSearchMem noTestsOpt OCaml "lib/parser.ml" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt OCaml "lib/parser.ml" src ["cgrep_marker"]
            matchLineNums prodMs @?= [1]
            matchLineNums testMs @?= [3, 6]
            alcoProd <- runSearchMem noTestsOpt OCaml "lib/parser.ml" src ["Alcotest"]
            matchLineNums alcoProd @?= []
            let duneTestSrc = "let fixture_val = cgrep_marker\n"
            dtProd <- runSearchMem noTestsOpt OCaml "test/test_parser.ml" duneTestSrc ["cgrep_marker"]
            dtTest <- runSearchMem onlyTestsOpt OCaml "test/test_parser.ml" duneTestSrc ["cgrep_marker"]
            matchLineNums dtProd @?= []
            matchLineNums dtTest @?= [1]
        ]

phpParserTests :: TestTree
phpParserTests =
    testGroup
        "PHP semantic test parser"
        [ testCase "PHP: prod Rule::test($x) stays prod; final readonly class *Test & public function testXxx capture modifiers" $ do
            let src =
                    T.unlines
                        [ "<?php" -- 1
                        , "class RegexRule {" -- 2 (prod)
                        , "    public function test(string $input): bool {" -- 3 (prod: bare test($input) on non-test class!)
                        , "        return $input === $cgrep_marker;" -- 4 (prod)
                        , "    }" -- 5
                        , "}" -- 6
                        , "trait OrderAssertions {" -- 7 (prod/shared trait)
                        , "    public function testCalculatesTax(): void {" -- 8 (test method with 'public' modifier)
                        , "        $x = $cgrep_marker;" -- 9 (test)
                        , "    }" -- 10
                        , "}" -- 11
                        , "final readonly class PaymentServiceTest extends TestCase {" -- 12 (test with final readonly)
                        , "    public function check(): void { $y = $cgrep_marker; }" -- 13 (test)
                        , "}" -- 14
                        ]
            prodMs <- runSearchMem noTestsOpt PHP "app/Rules/RegexRule.php" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt PHP "app/Rules/RegexRule.php" src ["cgrep_marker"]
            matchLineNums prodMs @?= [4]
            matchLineNums testMs @?= [9, 13]
            modProd <- runSearchMem noTestsOpt PHP "app/Rules/RegexRule.php" src ["readonly"]
            modTest <- runSearchMem onlyTestsOpt PHP "app/Rules/RegexRule.php" src ["final readonly"]
            matchLineNums modProd @?= []
            matchLineNums modTest @?= [12]
        , testCase "PHP: Pest higher-order ->and(...) keyword chains, arch/todo/dataset/uses, and tests/*.php isTestFile" $ do
            let src =
                    T.unlines
                        [ "<?php" -- 1
                        , "function prodHelper() { return $cgrep_marker; }" -- 2 (prod)
                        , "it('chains keyword ->and()', fn () => 1)->and($cgrep_marker)->toBeInt();" -- 3 (test: 'and' is PHP keyword!)
                        , "arch('no debug calls')->expect('App')->not->toUse($cgrep_marker);" -- 4 (test: Pest arch)
                        , "todo($cgrep_marker);" -- 5 (prod: non-string arg is NOT Pest todo('...'))
                        , "todo('pending oauth')->group($cgrep_marker);" -- 6 (test: Pest todo('...'))
                        , "dataset('users', [$cgrep_marker]);" -- 7 (test: Pest dataset)
                        , "uses(TestCase::class)->in($cgrep_marker);" -- 8 (test: Pest uses()->in())
                        ]
            prodMs <- runSearchMem noTestsOpt PHP "routes/api.php" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt PHP "routes/api.php" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 5]
            matchLineNums testMs @?= [3, 4, 6, 7, 8]
            let pestFileSrc = "<?php\nfunction customExpectation() { return $cgrep_marker; }\n"
            pfProd <- runSearchMem noTestsOpt PHP "tests/Pest.php" pestFileSrc ["cgrep_marker"]
            pfTest <- runSearchMem onlyTestsOpt PHP "tests/Pest.php" pestFileSrc ["cgrep_marker"]
            matchLineNums pfProd @?= []
            matchLineNums pfTest @?= [2]
        ]

perlParserTests :: TestTree
perlParserTests =
    testGroup
        "Perl semantic test parser"
        [ testCase "Perl: $obj->subtest(...) / $obj->test('...', sub {}) stay prod in non-strict mode; Test::Spec before/after & dies_ok" $ do
            let src =
                    T.unlines
                        [ "sub run_router {" -- 1 (prod)
                        , "    my ($router) = @_;" -- 2 (prod)
                        , "    $router->subtest('prod method', sub { my $x = $cgrep_marker; });" -- 3 (prod: '->' method call!)
                        , "    $router->test('prod route', sub { my $y = $cgrep_marker; });" -- 4 (prod: '->' method call!)
                        , "}" -- 5
                        , "describe 'Auth' => sub {" -- 6 (test)
                        , "    before each => sub { my $seed = $cgrep_marker; };" -- 7 (test: Test::Spec before each)
                        , "    it 'rejects bad token' => sub {" -- 8 (test)
                        , "        dies_ok { verify($cgrep_marker) } 'dies on invalid token';" -- 9 (test: Test::Exception)
                        , "    };" -- 10
                        , "};" -- 11
                        , "throws_ok { parse_token($cgrep_marker) } qr/invalid/, 'top-level throws_ok';" -- 12 (test: top-level Test::Exception)
                        ]
            prodMs <- runSearchMem noTestsOpt Perl "lib/Auth.pm" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Perl "lib/Auth.pm" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 4]
            matchLineNums testMs @?= [7, 9, 12]
        , testCase "Perl: multi-attribute Test::Class ': Tags(fast) : Test(2)' and .t file isTestFile" $ do
            let src =
                    T.unlines
                        [ "package Local::Service;" -- 1 (prod)
                        , "sub compute { return $cgrep_marker; }" -- 2 (prod)
                        , "sub check_auth : Tags(fast, smoke) : Test(startup => 1) {" -- 3 (test: multi-attr Test::Class)
                        , "    my $v = $cgrep_marker;" -- 4 (test)
                        , "}" -- 5
                        ]
            prodMs <- runSearchMem noTestsOpt Perl "lib/Service.pm" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Perl "lib/Service.pm" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2]
            matchLineNums testMs @?= [4]
            let tFileSrc = "sub make_fixture { my $x = $cgrep_marker; return $x; }\nok(make_fixture());\n"
            tProd <- runSearchMem noTestsOpt Perl "t/01-auth.t" tFileSrc ["cgrep_marker"]
            tTest <- runSearchMem onlyTestsOpt Perl "t/01-auth.t" tFileSrc ["cgrep_marker"]
            matchLineNums tProd @?= []
            matchLineNums tTest @?= [1]
        ]

pythonParserTests :: TestTree
pythonParserTests =
    testGroup
        "Python semantic test parser"
        [ testCase "Python: nested def/class/@decorator inside test_foo & TestSuite stay test; col-0 prod assignments & 'A @ B' matmul stay prod" $ do
            let src =
                    T.unlines
                        [ "def test_nested_defs_and_classes():" -- 1 (test)
                        , "    class MockClient:" -- 2 (test: nested class must NOT end test_foo!)
                        , "        pass" -- 3 (test)
                        , "    @staticmethod" -- 4 (test: nested decorator must NOT end test_foo!)
                        , "    def build_client():" -- 5 (test: nested def must NOT end test_foo!)
                        , "        return cgrep_marker" -- 6 (test)
                        , "    assert build_client() == cgrep_marker" -- 7 (test: after nested def!)
                        , "" -- 8
                        , "PROD_GLOBAL_AFTER_TEST = cgrep_marker" -- 9 (prod: col-0 assignment after test_foo!)
                        , "matmul_res = weights @ cgrep_marker" -- 10 (prod: '@' is matrix multiplication, NOT decorator!)
                        , "def test_after_matmul():" -- 11 (test)
                        , "    assert cgrep_marker" -- 12 (test)
                        , "" -- 13
                        , "class TestOrderFlow:" -- 14 (test)
                        , "    SHARED_SEED = cgrep_marker" -- 15 (test: class attribute)
                        , "    def zero_arg_helper():" -- 16 (test: helper without self/cls inside Test*!)
                        , "        return cgrep_marker" -- 17 (test)
                        , "if __name__ == '__main__':" -- 18 (prod: col-0 block after TestOrderFlow!)
                        , "    run_app(cgrep_marker)" -- 19 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Python "app/service.py" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Python "app/service.py" src ["cgrep_marker"]
            matchLineNums prodMs @?= [9, 10, 19]
            matchLineNums testMs @?= [6, 7, 12, 15, 17]
        , testCase "Python: Hypothesis @settings + @given stack, multi-line def test_* signature, and conftest.py / test_*.py isTestFile" $ do
            let src =
                    T.unlines
                        [ "@settings(max_examples=50)" -- 1 (test: stacked above @given)
                        , "@given(x=st.integers())" -- 2 (test: Hypothesis @given)
                        , "def roundtrip_prop(" -- 3 (test: multi-line signature without test_ prefix!)
                        , "    x: int," -- 4 (test)
                        , ") -> None:" -- 5 (test: ')' at col 0 inside parens!)
                        , "    assert decode(encode(x)) == cgrep_marker" -- 6 (test)
                        , "def prod_after_prop(x: int) -> int:" -- 7 (prod)
                        , "    return x + cgrep_marker" -- 8 (prod)
                        ]
            prodMs <- runSearchMem noTestsOpt Python "app/codec.py" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Python "app/codec.py" src ["cgrep_marker"]
            matchLineNums prodMs @?= [8]
            matchLineNums testMs @?= [6]
            setProd <- runSearchMem noTestsOpt Python "app/codec.py" src ["max_examples"]
            matchLineNums setProd @?= []
            let confSrc = "def make_db_session():\n    return cgrep_marker\n"
            cfProd <- runSearchMem noTestsOpt Python "tests/conftest.py" confSrc ["cgrep_marker"]
            cfTest <- runSearchMem onlyTestsOpt Python "tests/conftest.py" confSrc ["cgrep_marker"]
            matchLineNums cfProd @?= []
            matchLineNums cfTest @?= [2]
        ]

rParserTests :: TestTree
rParserTests =
    testGroup
        "R semantic test parser"
        [ testCase "R: testthat::test_that in non-strict mode, Hmisc::describe & s3.describe guards, and standalone expect_*" $ do
            let src =
                    T.unlines
                        [ "summarize_col <- function(x) {" -- 1 (prod)
                        , "  a <- Hmisc::describe(\"age_label\", cgrep_marker)" -- 2 (prod: non-testthat namespace!)
                        , "  b <- custom.describe(\"col_label\", cgrep_marker)" -- 3 (prod: R dot-identifier custom.describe!)
                        , "  return(list(a, b))" -- 4 (prod)
                        , "}" -- 5
                        , "testthat::test_that(\"qualified testthat call\", {" -- 6 (test: non-strict ':' ':'!)
                        , "  v <- cgrep_marker" -- 7 (test)
                        , "})" -- 8
                        , "expect_equal(add(1, 2), cgrep_marker)" -- 9 (test: standalone testthat/tinytest expect_equal)
                        , "tinytest::expect_true(cgrep_marker > 0)" -- 10 (test: qualified tinytest::expect_true)
                        ]
            prodMs <- runSearchMem noTestsOpt R "R/stats.R" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt R "R/stats.R" src ["cgrep_marker"]
            matchLineNums prodMs @?= [2, 3]
            matchLineNums testMs @?= [7, 9, 10]
            nsProd <- runSearchMem noTestsOpt R "R/stats.R" src ["testthat"]
            matchLineNums nsProd @?= []
        , testCase "R: testthat tests/testthat/test-*.R and test_*.r isTestFile conventions" $ do
            let helperSrc = "make_sample_df <- function() data.frame(x = cgrep_marker)\n"
            t1Prod <- runSearchMem noTestsOpt R "tests/testthat/test-stats.R" helperSrc ["cgrep_marker"]
            t1Test <- runSearchMem onlyTestsOpt R "tests/testthat/test-stats.R" helperSrc ["cgrep_marker"]
            matchLineNums t1Prod @?= []
            matchLineNums t1Test @?= [1]
            t2Prod <- runSearchMem noTestsOpt R "inst/tinytest/test_stats.r" helperSrc ["cgrep_marker"]
            t2Test <- runSearchMem onlyTestsOpt R "inst/tinytest/test_stats.r" helperSrc ["cgrep_marker"]
            matchLineNums t2Prod @?= []
            matchLineNums t2Test @?= [1]
        ]

rubyParserTests :: TestTree
rubyParserTests =
    testGroup
        "Ruby semantic test parser"
        [ testCase "Ruby: postfix 'return x if ...' / 'save! unless ...', multi-line '1\\nif...end', range.end/:end/end:, and Testimonial guard" $ do
            let src =
                    T.unlines
                        [ "class Testimonial" -- 1 (prod: Testimonial is NOT a Test* class!)
                        , "  def body" -- 2 (prod)
                        , "    cgrep_marker" -- 3 (prod)
                        , "  end" -- 4
                        , "end" -- 5
                        , "RSpec.describe 'Order' do" -- 6 (test)
                        , "  it 'handles postfix modifiers and .end/:end/end:' do" -- 7 (test)
                        , "    x = 1" -- 8 (test: ends with number 1)
                        , "    if x > 0" -- 9 (test: block 'if' on new line after number 1 MUST expect 'end'!)
                        , "      y = range.end + ({ end: :end }[:end] ? 1 : 0)" -- 10 (test: .end, end:, :end!)
                        , "    end" -- 11 (test: closes 'if x > 0', NOT 'it'!)
                        , "    save! unless valid?(y)" -- 12 (test: postfix 'unless' after save! and valid?(y) has NO 'end'!)
                        , "    return cgrep_marker if y > 0" -- 13 (test: postfix 'if' after identifier has NO 'end'!)
                        , "  end" -- 14 (test: closes 'it')
                        , "end" -- 15 (test: closes 'RSpec.describe')
                        , "class ProdAfterRSpec" -- 16 (prod: must NOT be swallowed!)
                        , "  def run" -- 17 (prod)
                        , "    cgrep_marker" -- 18 (prod)
                        , "  end" -- 19
                        , "end" -- 20
                        ]
            prodMs <- runSearchMem noTestsOpt Ruby "app/models/order.rb" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Ruby "app/models/order.rb" src ["cgrep_marker"]
            matchLineNums prodMs @?= [3, 18]
            matchLineNums testMs @?= [13]
        , testCase "Ruby: shared_examples / it_behaves_like, Ruby 3 endless 'def test_foo = ...', and *_spec.rb / *_test.rb isTestFile" $ do
            let src =
                    T.unlines
                        [ "RSpec.shared_examples 'auditable' do" -- 1 (test: RSpec.shared_examples)
                        , "  it { expect(subject).to eq(cgrep_marker) }" -- 2 (test)
                        , "end" -- 3
                        , "it_behaves_like 'auditable', cgrep_marker" -- 4 (test: braceless it_behaves_like line)
                        , "def test_endless_ruby3 = assert_equal(1, cgrep_marker)" -- 5 (test: Ruby 3 endless method without 'end'!)
                        , "def prod_after_endless" -- 6 (prod: must NOT be swallowed by endless def!)
                        , "  cgrep_marker" -- 7 (prod)
                        , "end" -- 8
                        ]
            prodMs <- runSearchMem noTestsOpt Ruby "lib/audit.rb" src ["cgrep_marker"]
            testMs <- runSearchMem onlyTestsOpt Ruby "lib/audit.rb" src ["cgrep_marker"]
            matchLineNums prodMs @?= [7]
            matchLineNums testMs @?= [2, 4, 5]
            let specFileSrc = "class FakeGateway\n  def charge = cgrep_marker\nend\n"
            spProd <- runSearchMem noTestsOpt Ruby "spec/models/order_spec.rb" specFileSrc ["cgrep_marker"]
            spTest <- runSearchMem onlyTestsOpt Ruby "spec/models/order_spec.rb" specFileSrc ["cgrep_marker"]
            matchLineNums spProd @?= []
            matchLineNums spTest @?= [2]
        ]
