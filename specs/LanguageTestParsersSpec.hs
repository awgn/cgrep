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
