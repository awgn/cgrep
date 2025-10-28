<?php
// PHP test example file

require_once 'PHPUnit/Autoload.php';

// Regular helper class (not a test)
class MathHelper
{
    public static function add($a, $b)
    {
        return $a + $b;
    }

    public static function multiply($x, $y)
    {
        return $x * $y;
    }

    public static function factorial($n)
    {
        if ($n <= 0) {
            return 1;
        }
        return $n * self::factorial($n - 1);
    }
}

// Regular helper class
class Calculator
{
    private $value = 0;

    public function __construct($initial = 0)
    {
        $this->value = $initial;
    }

    public function add($n)
    {
        $this->value += $n;
        return $this;
    }

    public function getValue()
    {
        return $this->value;
    }

    public function reset()
    {
        $this->value = 0;
    }
}

// PHPUnit test class
class MathHelperTest extends PHPUnit\Framework\TestCase
{
    public function testAddition()
    {
        $result = MathHelper::add(2, 3);
        $this->assertEquals(5, $result);
    }

    public function testMultiplication()
    {
        $result = MathHelper::multiply(4, 5);
        $this->assertEquals(20, $result);
    }

    /**
     * @test
     */
    public function factorialCalculation()
    {
        $result = MathHelper::factorial(5);
        $this->assertEquals(120, $result);
    }

    /**
     * @test
     */
    public function factorialOfZero()
    {
        $result = MathHelper::factorial(0);
        $this->assertEquals(1, $result);
    }
}

// Helper function (not a test)
function processArray($array)
{
    return array_map(function($x) {
        return $x * 2;
    }, array_filter($array, function($x) {
        return $x > 0;
    }));
}

// PHPUnit test class for Calculator
class CalculatorTest extends PHPUnit\Framework\TestCase
{
    public function testInitialValueIsZero()
    {
        $calc = new Calculator();
        $this->assertEquals(0, $calc->getValue());
    }

    public function testCanBeInitializedWithValue()
    {
        $calc = new Calculator(10);
        $this->assertEquals(10, $calc->getValue());
    }

    public function testAddWorksCorrectly()
    {
        $calc = new Calculator();
        $calc->add(5)->add(10);
        $this->assertEquals(15, $calc->getValue());
    }

    /**
     * @test
     */
    public function handlesNegativeNumbers()
    {
        $calc = new Calculator();
        $calc->add(-5);
        $this->assertEquals(-5, $calc->getValue());
    }

    public function testResetWorks()
    {
        $calc = new Calculator();
        $calc->add(100);
        $calc->reset();
        $this->assertEquals(0, $calc->getValue());
    }
}

// Regular class (not a test)
class StringHelper
{
    public static function reverse($str)
    {
        return strrev($str);
    }

    public static function capitalize($str)
    {
        return strtoupper($str);
    }
}

// PHPUnit test class for StringHelper
class StringHelperTest extends PHPUnit\Framework\TestCase
{
    public function testReverseString()
    {
        $result = StringHelper::reverse("hello");
        $this->assertEquals("olleh", $result);
    }

    /**
     * @test
     */
    public function capitalizeString()
    {
        $result = StringHelper::capitalize("world");
        $this->assertEquals("WORLD", $result);
    }

    public function testEmptyString()
    {
        $result = StringHelper::reverse("");
        $this->assertEquals("", $result);
    }
}

// Helper class
class Person
{
    public $name;
    public $age;

    public function __construct($name, $age)
    {
        $this->name = $name;
        $this->age = $age;
    }

    public function greeting()
    {
        return "Hello, I'm " . $this->name;
    }
}

// Helper function
function formatNumber($n)
{
    return number_format($n, 2);
}

?>