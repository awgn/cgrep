# Nim test example file

import unittest

# Regular helper function (not a test)
proc add(a, b: int): int =
  return a + b

# Another helper function
proc multiply(x, y: int): int =
  return x * y

# Helper function
proc factorial(n: int): int =
  if n <= 0:
    return 1
  return n * factorial(n - 1)

# Regular type (not a test)
type
  Calculator = ref object
    value: int

# Helper functions for Calculator
proc newCalculator(initial: int = 0): Calculator =
  return Calculator(value: initial)

proc add(calc: Calculator, n: int): Calculator =
  calc.value += n
  return calc

proc getValue(calc: Calculator): int =
  return calc.value

proc reset(calc: Calculator) =
  calc.value = 0

# unittest suite
suite "Math operations":
  test "addition works correctly":
    let result = add(2, 3)
    check result == 5
  
  test "multiplication works":
    let result = multiply(4, 5)
    check result == 20

# Helper function (not a test)
proc processData(arr: seq[int]): seq[int] =
  var filtered: seq[int] = @[]
  for x in arr:
    if x > 0:
      filtered.add(x * 2)
  return filtered

# More unittest tests
suite "Factorial tests":
  test "calculates factorial of 5":
    let result = factorial(5)
    check result == 120
  
  test "handles zero":
    let result = factorial(0)
    check result == 1
  
  test "handles one":
    let result = factorial(1)
    check result == 1

# Calculator tests
suite "Calculator":
  test "starts with zero":
    let calc = newCalculator()
    check getValue(calc) == 0
  
  test "can be initialized with value":
    let calc = newCalculator(10)
    check getValue(calc) == 10
  
  test "adds numbers correctly":
    let calc = newCalculator()
    discard calc.add(5).add(10)
    check getValue(calc) == 15
  
  test "handles negative numbers":
    let calc = newCalculator()
    discard calc.add(-5)
    check getValue(calc) == -5
  
  test "reset works":
    let calc = newCalculator()
    discard calc.add(100)
    reset(calc)
    check getValue(calc) == 0

# Regular type (not a test)
type
  Person = object
    name: string
    age: int

proc newPerson(name: string, age: int): Person =
  return Person(name: name, age: age)

# Helper function
proc formatNumber(n: float): string =
  return n.formatFloat(ffDecimal, 2)

# String helper functions (not tests)
proc reverseString(s: string): string =
  var reversed = ""
  for i in countdown(s.len - 1, 0):
    reversed.add(s[i])
  return reversed

proc capitalizeString(s: string): string =
  return s.toUpperAscii()

# String tests
suite "String operations":
  test "reverses a string":
    let result = reverseString("hello")
    check result == "olleh"
  
  test "capitalizes a string":
    let result = capitalizeString("world")
    check result == "WORLD"
  
  test "handles empty string":
    let result = reverseString("")
    check result == ""