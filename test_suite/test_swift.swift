// Swift test example file

import XCTest
import Testing

// Regular helper class (not a test)
class MathHelper {
    static func add(_ a: Int, _ b: Int) -> Int {
        let cgrep_prod_1 = 1
        return a + b
    }

    static func multiply(_ x: Int, _ y: Int) -> Int {
        let cgrep_prod_2 = 2
        return x * y
    }
}

// Regular class (not a test)
class Calculator {
    private var value: Int = 0

    init(initial: Int = 0) {
        self.value = initial
    }

    func add(_ n: Int) -> Calculator {
        let cgrep_prod_3 = 3
        value += n
        return self
    }

    func getValue() -> Int {
        let cgrep_prod_4 = 4
        return value
    }
}

// Production struct whose name contains "Test" as part of a domain word (`Testimonial`):
// NOT a test struct!
struct Testimonial {
    let author: String

    func format() -> String {
        let cgrep_prod_5 = 5
        return author
    }
}

// Production function whose name starts with lowercase `test` as part of a word (`testify`):
// NOT an XCTest method!
func testifyAccount() -> Int {
    let cgrep_prod_6 = 6
    return cgrep_prod_6
}

// Production function with non-test attribute whose argument references `Test`:
// NOT a Swift Testing `@Test` attribute!
@CustomAttr(mode: Test)
func legacyProductionFunc() -> Int {
    let cgrep_prod_7 = 7
    return cgrep_prod_7
}

// Protocol with `func testSomething()` requirement (no body `{}`) followed by a production struct:
// Must NOT swallow the subsequent production struct's `{ ... }`!
protocol Verifiable {
    func testConnection() -> Bool
}

struct ProdStructAfterProtocol {
    func keepMe() -> Int {
        let cgrep_prod_8 = 8
        return cgrep_prod_8
    }
}

class ProductionCode_Cgrep {
    func compute(x: Int) -> Int {
        let cgrep_prod_9 = 9
        return x * 2
    }

    func runTestHarness(x: Int) -> Int {
        let cgrep_prod_10 = 10
        return x + 1
    }
}

// Swift Testing (macro based), including default closure parameter `= {}` in signature!
@Test func macroTestCgrep(onComplete: () -> Void = {}) {
    let cgrep_test_1 = 1
    onComplete()
    #expect(1 == 1)
}

@Test("Custom Name")
func parameterizedMacroCgrep() {
    let cgrep_test_2 = 2
}

@Test(arguments: [1, 2, 3])
func argumentsMacroCgrep(value: Int) {
    let cgrep_test_3 = 3
}

@Suite("A descriptive suite name")
struct SuiteCgrep {
    @Test func insideSuiteCgrep() {
        let cgrep_test_4 = 4
    }
}

// Generic XCTestCase subclass `class GenericSpec<T>: XCTestCase`
// (where class name does NOT contain "Test"!)
class GenericSpec<T>: XCTestCase {
    func helperInGenericSpec() -> Int {
        let cgrep_test_5 = 5
        return cgrep_test_5
    }
}

class ClassicXCTest_Cgrep: XCTestCase {
    override func setUp() {
        let cgrep_test_6 = 6
    }

    override func tearDown() {
        let cgrep_test_7 = 7
    }

    override class func setUp() {
        let cgrep_test_8 = 8
    }

    override class func tearDown() {
        let cgrep_test_9 = 9
    }

    func testClassicCgrep() {
        let cgrep_test_10 = 10
    }
}

class FinalProduction_Cgrep {
    func tripled(x: Int) -> Int {
        let cgrep_prod_11 = 11
        return x * 3
    }
}
