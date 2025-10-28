// Scala test example file

package com.example.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// Regular helper object (not a test)
object MathHelper {
  def add(a: Int, b: Int): Int = {
    a + b
  }
  
  def multiply(x: Int, y: Int): Int = {
    x * y
  }
}

// Regular class (not a test)
class Calculator {
  private var value: Int = 0
  
  def add(n: Int): Calculator = {
    value += n
    this
  }
  
  def getValue: Int = value
}

// FunSuite style tests (ScalaTest/MUnit)
class MathSuite extends AnyFunSuite {
  test("addition should work correctly") {
    val result = MathHelper.add(2, 3)
    assert(result == 5)
  }
  
  test("multiplication should work") {
    val result = MathHelper.multiply(4, 5)
    assert(result == 20)
  }
}

// Regular function outside tests
def formatNumber(n: Double): String = {
  f"$n%.2f"
}

// FunSpec style tests with describe/it
class CalculatorSpec extends AnyFunSpec {
  describe("Calculator") {
    it("should start with zero") {
      val calc = new Calculator
      assert(calc.getValue == 0)
    }
    
    it("should add numbers correctly") {
      val calc = new Calculator
      calc.add(5).add(10)
      assert(calc.getValue == 15)
    }
    
    describe("when adding negative numbers") {
      it("should handle negatives") {
        val calc = new Calculator
        calc.add(-5)
        assert(calc.getValue == -5)
      }
    }
  }
}

// Helper function
def processData(data: List[Int]): List[Int] = {
  data.filter(_ > 0).map(_ * 2)
}

// Another FunSuite test
class DataProcessingSuite extends AnyFunSuite {
  test("processData filters and transforms") {
    val input = List(1, -2, 3, -4, 5)
    val result = processData(input)
    assert(result == List(2, 6, 10))
  }
}

// Regular trait (not a test)
trait Printable {
  def print(): String
}

// More helper code
object StringUtils {
  def reverse(s: String): String = s.reverse
  
  def capitalize(s: String): String = {
    if (s.isEmpty) s
    else s.head.toUpper + s.tail
  }
}

// FeatureSpec style test
class StringFeatureSpec extends AnyFunSpec {
  feature("String manipulation") {
    scenario("reversing a string") {
      val result = StringUtils.reverse("hello")
      assert(result == "olleh")
    }
    
    scenario("capitalizing a string") {
      val result = StringUtils.capitalize("world")
      assert(result == "World")
    }
  }
}

// Final helper function
def sumList(numbers: List[Int]): Int = {
  numbers.sum
}