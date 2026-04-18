// F# test example file

module TestExample

open NUnit.Framework
open Xunit
open Expecto

// Regular helper function (not a test)
let add x y = x + y

// Another helper function
let multiply x y = x * y

// Helper function
let factorial n =
    let rec loop acc n =
        if n <= 0 then acc
        else loop (acc * n) (n - 1)
    loop 1 n

// NUnit style test with F# attributes
[<Test>]
let ``add two numbers returns correct sum`` () =
    let result = add 2 3
    Assert.AreEqual(5, result)

[<Test>]
let ``multiply returns correct product`` () =
    let result = multiply 4 5
    Assert.AreEqual(20, result)

// xUnit style tests
[<Fact>]
let ``factorial of 5 is 120`` () =
    let result = factorial 5
    Assert.Equal(120, result)

[<Fact>]
let ``factorial of 0 is 1`` () =
    let result = factorial 0
    Assert.Equal(1, result)

// Regular type (not a test)
type Calculator = {
    Value: int
}

// Helper functions
let makeCalculator initial = { Value = initial }

let addToCalculator n calc = 
    { calc with Value = calc.Value + n }

// xUnit Theory
[<Theory>]
[<InlineData(2, 3, 5)>]
[<InlineData(10, 20, 30)>]
let ``add is correct for various inputs`` x y expected =
    let result = add x y
    Assert.Equal(expected, result)

// Expecto tests
let tests =
    testList "Math operations" [
        testCase "addition works" <| fun () ->
            let result = add 1 1
            Expect.equal result 2 "1 + 1 should equal 2"
        
        testCase "multiplication works" <| fun () ->
            let result = multiply 3 4
            Expect.equal result 12 "3 * 4 should equal 12"
        
        testList "Nested tests" [
            test "factorial test" {
                let result = factorial 3
                Expect.equal result 6 "3! should equal 6"
            }
        ]
    ]

// Helper function at the end
let processData data =
    data
    |> List.filter (fun x -> x > 0)
    |> List.map (fun x -> x * 2)

// More regular code
type Person = {
    Name: string
    Age: int
}

let makePerson name age = { Name = name; Age = age }
// =========================================================================
// --- CGREP SEMANTIC TESTS (appended) ---
// =========================================================================

module CgrepExtended

open NUnit.Framework
open Xunit
open Expecto
open FsCheck

// --- Production code (should survive -T False) ---
let compute_cgrep x =
    let cgrep_prod_1 = 1
    x * 2

// Function whose name contains the substring "test" but is NOT a test (no attribute)
let runTestHarness_cgrep x =
    let cgrep_prod_2 = 2
    x + 1

type CalculatorCgrep = {
    Value: int
}

let makeCalculatorCgrep initial =
    let cgrep_prod_3 = 3
    { Value = initial }

// --- NUnit-style tests ---
[<TestFixture>]
type NUnitFixtureCgrep() =
    [<SetUp>]
    member this.Init () =
        let cgrep_test_1 = 1
        ()

    [<TearDown>]
    member this.Cleanup () =
        let cgrep_test_2 = 2
        ()

    [<OneTimeSetUp>]
    member this.ClassInit () =
        let cgrep_test_3 = 3
        ()

    [<Test>]
    [<Category("unit")>]
    member this.StackedAttributesIsTest () =
        let cgrep_test_4 = 4
        Assert.AreEqual(1, 1)

[<Test>]
let simpleNunitTestCgrep () =
    let cgrep_test_5 = 5
    Assert.AreEqual(2, 1 + 1)

// --- NUnit parametrized with TestCase ---
[<TestCase(1, 2, 3)>]
[<TestCase(4, 5, 9)>]
let parametrizedNunitCgrep a b expected =
    let cgrep_test_6 = 6
    Assert.AreEqual(expected, a + b)

[<TestCaseSource("dataSource")>]
let testCaseSourceCgrep x =
    let cgrep_test_7 = 7
    Assert.IsTrue(x > 0)

// --- xUnit with parameterized Fact and Theory ---
[<Fact>]
let factCgrep () =
    let cgrep_test_8 = 8
    Assert.Equal(1, 1)

[<Fact(Skip = "reason")>]
let factWithParamCgrep () =
    let cgrep_test_9 = 9
    Assert.Equal(1, 1)

[<Theory>]
[<InlineData(1, 2)>]
[<InlineData(3, 4)>]
let theoryInlineCgrep a b =
    let cgrep_test_10 = 10
    Assert.True(a < b)

[<Theory>]
[<MemberData(nameof(dataSource))>]
let theoryMemberCgrep x =
    let cgrep_test_11 = 11
    Assert.True(x >= 0)

// --- Fully-qualified attributes ---
[<NUnit.Framework.Test>]
let fullyQualifiedNunitCgrep () =
    let cgrep_test_12 = 12
    Assert.Pass()

[<Xunit.Fact>]
let fullyQualifiedXunitCgrep () =
    let cgrep_test_13 = 13
    Assert.Equal(1, 1)

// --- FsCheck property ---
[<Property>]
let propReverseCgrep (xs: int list) =
    let cgrep_test_14 = 14
    List.rev (List.rev xs) = xs

// --- Expecto testList / testCase / test / ftest / ptest ---
let expectoTestsCgrep =
    testList "cgrep math" [
        testCase "addition cgrep" <| fun () ->
            let cgrep_test_15 = 15
            Expect.equal 2 2 "trivial"

        test "block form cgrep" {
            let cgrep_test_16 = 16
            Expect.equal 3 3 "trivial"
        }

        ftest "focused test cgrep" {
            let cgrep_test_17 = 17
            Expect.equal 4 4 "trivial"
        }

        ptest "pending test cgrep" {
            let cgrep_test_18 = 18
            Expect.equal 5 5 "trivial"
        }

        testCaseAsync "async case cgrep" <| async {
            let cgrep_test_19 = 19
            return ()
        }
    ]

// --- Production code AFTER all tests (confinement check) ---
let tripled_cgrep x =
    let cgrep_prod_4 = 4
    x * 3

let describe_cgrep n =
    let cgrep_prod_5 = 5
    sprintf "n=%d" n

type PersonCgrep = {
    Name: string
    Age: int
}

let makePersonCgrep name age =
    let cgrep_prod_6 = 6
    { Name = name; Age = age }
