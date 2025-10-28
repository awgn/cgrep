// Swift test example file

import XCTest

// Regular helper class (not a test)
class MathHelper {
    static func add(_ a: Int, _ b: Int) -> Int {
        return a + b
    }
    
    static func multiply(_ x: Int, _ y: Int) -> Int {
        return x * y
    }
    
    static func factorial(_ n: Int) -> Int {
        if n <= 0 {
            return 1
        }
        return n * factorial(n - 1)
    }
}

// Regular class (not a test)
class Calculator {
    private var value: Int = 0
    
    init(initial: Int = 0) {
        self.value = initial
    }
    
    func add(_ n: Int) -> Calculator {
        value += n
        return self
    }
    
    func getValue() -> Int {
        return value
    }
    
    func reset() {
        value = 0
    }
}

// XCTest test class
class MathHelperTests: XCTestCase {
    
    func testAddition() {
        let result = MathHelper.add(2, 3)
        XCTAssertEqual(result, 5)
    }
    
    func testMultiplication() {
        let result = MathHelper.multiply(4, 5)
        XCTAssertEqual(result, 20)
    }
    
    func testFactorialCalculation() {
        let result = MathHelper.factorial(5)
        XCTAssertEqual(result, 120)
    }
    
    func testFactorialOfZero() {
        let result = MathHelper.factorial(0)
        XCTAssertEqual(result, 1)
    }
}

// Helper function (not a test)
func processData(_ array: [Int]) -> [Int] {
    return array.filter { $0 > 0 }.map { $0 * 2 }
}

// XCTest for Calculator
class CalculatorTests: XCTestCase {
    
    func testInitialValueIsZero() {
        let calc = Calculator()
        XCTAssertEqual(calc.getValue(), 0)
    }
    
    func testCanBeInitializedWithValue() {
        let calc = Calculator(initial: 10)
        XCTAssertEqual(calc.getValue(), 10)
    }
    
    func testAddWorksCorrectly() {
        let calc = Calculator()
        calc.add(5).add(10)
        XCTAssertEqual(calc.getValue(), 15)
    }
    
    func testHandlesNegativeNumbers() {
        let calc = Calculator()
        calc.add(-5)
        XCTAssertEqual(calc.getValue(), -5)
    }
    
    func testResetWorks() {
        let calc = Calculator()
        calc.add(100)
        calc.reset()
        XCTAssertEqual(calc.getValue(), 0)
    }
}

// Regular class (not a test)
class StringHelper {
    static func reverse(_ str: String) -> String {
        return String(str.reversed())
    }
    
    static func capitalize(_ str: String) -> String {
        return str.uppercased()
    }
}

// XCTest for StringHelper
class StringHelperTests: XCTestCase {
    
    func testReverseString() {
        let result = StringHelper.reverse("hello")
        XCTAssertEqual(result, "olleh")
    }
    
    func testCapitalizeString() {
        let result = StringHelper.capitalize("world")
        XCTAssertEqual(result, "WORLD")
    }
    
    func testEmptyString() {
        let result = StringHelper.reverse("")
        XCTAssertEqual(result, "")
    }
}

// Helper struct
struct Person {
    let name: String
    let age: Int
    
    func greeting() -> String {
        return "Hello, I'm \(name)"
    }
}

// Helper function
func formatNumber(_ n: Double) -> String {
    return String(format: "%.2f", n)
}

// More helper code
extension Array where Element == Int {
    func doubled() -> [Int] {
        return self.map { $0 * 2 }
    }
}