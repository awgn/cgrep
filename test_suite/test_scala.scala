// Scala test example file

package com.example.tests

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

// Regular helper object (not a test)
object MathHelper {
  def add(a: Int, b: Int): Int = {
    val cgrep_prod_1 = 1
    a + b
  }

  def multiply(x: Int, y: Int): Int = {
    val cgrep_prod_2 = 2
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

  def getValue: Int = {
    val cgrep_prod_3 = 3
    value
  }

  // Regular method named test() - NOT a test block!
  def test(input: String): Boolean = {
    val cgrep_prod_4 = 4
    input.nonEmpty
  }
}

// Calling validator.test(...) without braces followed by a prod function:
// Must NOT swallow the subsequent prod function's braces!
object ValidatorRunner {
  def run(calc: Calculator): Boolean = {
    val ok = calc.test("hello")
    def helperAfterCall(): Int = {
      val cgrep_prod_5 = 5
      cgrep_prod_5
    }
    ok && helperAfterCall() > 0
  }
}

// FunSuite style tests (ScalaTest/MUnit)
class MathSuite extends AnyFunSuite {
  test("addition should work correctly") {
    val cgrep_test_1 = 1
    val result = MathHelper.add(2, 3)
    assert(result == 5)
  }

  test("multiplication should work") {
    val cgrep_test_2 = 2
    val result = MathHelper.multiply(4, 5)
    assert(result == 20)
  }

  ignore("temporarily ignored test") {
    val cgrep_test_3 = 3
    assert(1 == 1)
  }
}

// Regular function outside tests
def formatNumber(n: Double): String = {
  val cgrep_prod_6 = 6
  f"$n%.2f"
}

// FunSpec style tests with describe/it
class CalculatorSpec extends AnyFunSpec {
  describe("Calculator") {
    it("should start with zero") {
      val cgrep_test_4 = 4
      val calc = new Calculator
      assert(calc.getValue == 0)
    }

    it("should add numbers correctly") {
      val cgrep_test_5 = 5
      val calc = new Calculator
      calc.add(5).add(10)
      assert(calc.getValue == 15)
    }

    describe("when adding negative numbers") {
      it("should handle negatives") {
        val cgrep_test_6 = 6
        val calc = new Calculator
        calc.add(-5)
        assert(calc.getValue == -5)
      }
    }
  }
}

// FlatSpec / WordSpec style tests ("..." should "..." in { ... })
class FlatCalculatorSpec extends AnyFlatSpec with Matchers {
  "A Calculator" should "add numbers" in {
    val cgrep_test_7 = 7
    val calc = new Calculator
    calc.add(3)
    calc.getValue should be (3)
  }

  it should "handle zero" in {
    val cgrep_test_8 = 8
    val calc = new Calculator
    calc.getValue should be (0)
  }
}

// FeatureSpec style test (ScalaTest 3.0 scenario/feature and 3.1+ Scenario/Feature)
class StringFeatureSpec extends AnyFunSpec {
  feature("String manipulation") {
    scenario("reversing a string") {
      val cgrep_test_9 = 9
      assert("hello".reverse == "olleh")
    }
  }

  Feature("Modern ScalaTest 3.2 Feature") {
    Scenario("capitalizing a string") {
      val cgrep_test_10 = 10
      assert("world".capitalize == "World")
    }
  }
}

// JUnit / MUnit @Test annotation in Scala
class AnnotatedSuite {
  @Test
  def testSomething(): Unit = {
    val cgrep_test_11 = 11
    assert(true)
  }
}

// Final helper function
def sumList(numbers: List[Int]): Int = {
  val cgrep_prod_7 = 7
  numbers.sum
}
