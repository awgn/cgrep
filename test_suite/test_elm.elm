module CalculatorTest exposing (..)

import Expect
import Fuzz exposing (int)
import Test exposing (Test, describe, fuzz, test, todo)


-- Production function with type annotation
calculateProd : Int -> Int -> Int
calculateProd a b =
    let
        cgrep_prod_sum =
            a + b
    in
    if cgrep_prod_sum == 0 then
        1
    else
        cgrep_prod_sum


-- Production function with name starting with "test" ("testify") and record field .test
testify : { a | test : String } -> String
testify record =
    let
        cgrep_prod_testify =
            record.test
    in
    cgrep_prod_testify


-- 1. Annotated top-level Test suite with nested describe, test, fuzz, todo, and local let..in
suite : Test
suite =
    describe "Calculator suite"
        [ test "adds two numbers" <|
            \_ ->
                let
                    cgrep_test_elm_basic : Int
                    cgrep_test_elm_basic =
                        calculateProd 2 3
                in
                Expect.equal 5 cgrep_test_elm_basic
        , fuzz int "fuzzes addition" <|
            \x ->
                let
                    cgrep_test_elm_fuzz =
                        calculateProd x 0
                in
                Expect.equal x cgrep_test_elm_fuzz
        , describe "nested context"
            [ test "handles zero" (\_ -> Expect.equal 1 (let cgrep_test_elm_nested = calculateProd 0 0 in cgrep_test_elm_nested))
            , todo "todo item"
            ]
        ]


-- Production function without type annotation immediately after suite
unannotatedProd x =
    let
        cgrep_prod_after_suite =
            x + 10
    in
    cgrep_prod_after_suite


-- 2. Qualified Test.Test annotation and Test.test builder
singleCheck : Test.Test
singleCheck =
    Test.test "qualified test" <|
        \_ ->
            let
                cgrep_test_elm_qualified =
                    calculateProd 10 20
            in
            Expect.equal 30 cgrep_test_elm_qualified


-- 3. Unannotated top-level test suite using describe directly
unannotatedSuite =
    describe "Unannotated suite"
        [ test "unannotated test" <|
            \_ ->
                let
                    cgrep_test_elm_unannotated =
                        calculateProd 4 5
                in
                Expect.equal 9 cgrep_test_elm_unannotated
        ]


-- Final production definition
finalProd : Int
finalProd =
    let
        cgrep_prod_final =
            unannotatedProd 5
    in
    cgrep_prod_final
