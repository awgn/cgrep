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