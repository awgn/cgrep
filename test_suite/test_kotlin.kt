package com.example.test

import org.junit.jupiter.api.Test
import io.kotest.core.spec.style.FunSpec
import io.kotest.core.spec.style.DescribeSpec

// 1. Normal production function
fun normalProductionFunction() {
    val CGREP_IDENTIFIER = 1
}

// 2. Normal function with "Test" in the name (False positive check)
fun processTestResults() {
    val CGREP_IDENTIFIER = 2
}

// 3. Abstract test function in an interface (No semicolon, no body)
// The parser should NOT swallow the next function's brace!
interface TestContract {
    @Test
    fun abstractTestMethod()
}

fun functionAfterAbstract() {
    val CGREP_IDENTIFIER = 3
}

// 4. Standard JUnit @Test
class JUnitTest {
    @Test
    fun basicTest() {
        val CGREP_IDENTIFIER_TEST = 4
        assert(1 == 1)
    }

    @org.junit.jupiter.api.BeforeEach
    fun setup() {
        val CGREP_IDENTIFIER_TEST = 5
    }
}

// 5. Kotest FunSpec style
class MyFunSpec : FunSpec({
    test("String length should return the length of the string") {
        val CGREP_IDENTIFIER_TEST = 6
    }
    
    xtest("Disabled test") {
        val CGREP_IDENTIFIER_TEST = 7
    }
})

// 6. Kotest DescribeSpec style
class MyDescribeSpec : DescribeSpec({
    describe("A calculator") {
        val CGREP_IDENTIFIER_TEST = 8
        
        it("should add two numbers") {
            val CGREP_IDENTIFIER_TEST = 9
        }
        
        context("when dealing with negative numbers") {
            val CGREP_IDENTIFIER_TEST = 10
            
            it("should work as well") {
                val CGREP_IDENTIFIER_TEST = 11
            }
        }
    }
})