// F# test example file

module TestExample

open NUnit.Framework
open Xunit
open Expecto
open FsCheck

// --- Production code (should survive --no-tests) ---
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

// Production function using a local variable named `test` (NOT an Expecto test block!)
let validateWithLocalVar x =
    let test = x > 0
    let cgrep_prod_4 = 4
    if test then cgrep_prod_4 else 0

// Production function with non-test attribute whose argument references `Test`
[< System.Obsolete(nameof(Test)) >]
let legacyProdFunction Test =
    let cgrep_prod_5 = 5
    Test + cgrep_prod_5

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

// --- FsCheck property followed immediately by an unattributed top-level production `let`! ---
[<Property>]
let propReverseCgrep (xs: int list) =
    let cgrep_test_14 = 14
    List.rev (List.rev xs) = xs

let prodFunctionRightAfterAttributeTest x =
    let cgrep_prod_6 = 6
    x + cgrep_prod_6

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
    let cgrep_prod_7 = 7
    x * 3

let describe_cgrep n =
    let cgrep_prod_8 = 8
    sprintf "n=%d" n

type PersonCgrep = {
    Name: string
    Age: int
}

let makePersonCgrep name age =
    let cgrep_prod_9 = 9
    { Name = name; Age = age }
