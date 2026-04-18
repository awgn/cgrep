package com.example.test

import org.junit.jupiter.api.Test
import io.kotest.core.spec.style.FunSpec
import io.kotest.core.spec.style.DescribeSpec

// 1. Normal production function
fun normalProductionFunction() {
    val cgrep_prod_1 = 1
}

// 2. Normal function with "Test" in the name (False positive check)
fun processTestResults() {
    val cgrep_prod_2 = 2
}

// 3. Abstract test function in an interface (No semicolon, no body)
// The parser should NOT swallow the next function's brace!
interface TestContract {
    @Test
    fun abstractTestMethod()
}

fun functionAfterAbstract() {
    val cgrep_prod_3 = 3
}

// 4. Standard JUnit @Test
class JUnitTest {
    @Test
    fun basicTest() {
        val cgrep_test_1 = 1
        assert(1 == 1)
    }

    @org.junit.jupiter.api.BeforeEach
    fun setup() {
        val cgrep_test_2 = 2
    }
}

// 5. Kotest FunSpec style
class MyFunSpec : FunSpec({
    test("String length should return the length of the string") {
        val cgrep_test_3 = 3
    }
    
    xtest("Disabled test") {
        val cgrep_test_4 = 4
    }
})

// 6. Kotest DescribeSpec style
class MyDescribeSpec : DescribeSpec({
    describe("A calculator") {
        val cgrep_test_5 = 5
        
        it("should add two numbers") {
            val cgrep_test_6 = 6
        }
        
        context("when dealing with negative numbers") {
            val cgrep_test_7 = 7
            
            it("should work as well") {
                val cgrep_test_8 = 8
            }
        }
    }
})