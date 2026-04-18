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
// =========================================================================
// --- CGREP SEMANTIC TESTS (appended) ---
// =========================================================================

class ProductionHelper_Cgrep {
    public function compute($x) {
        $cgrep_prod_1 = 1;
        return $x * 2;
    }
    
    public function runTestHarness($x) {
        $cgrep_prod_2 = 2;
        return $x + 1;
    }
}

// Pest framework support
test('pest simple test cgrep', function () {
    $cgrep_test_1 = 1;
    expect(true)->toBeTrue();
});

it('does something in pest cgrep', function () {
    $cgrep_test_2 = 2;
    expect(1)->toEqual(1);
});

describe('pest group cgrep', function () {
    $cgrep_test_3 = 3;
    
    it('is nested', function () {
        $cgrep_test_4 = 4;
        expect(2)->toEqual(2);
    });
});

class AttributesTest_Cgrep extends TestCase {
    #[Test]
    public function nativeAttributeCgrep() {
        $cgrep_test_5 = 5;
    }
    
    #[DataProvider('foo')]
    public function dataDrivenCgrep() {
        $cgrep_test_6 = 6;
    }

    /** @test */
    public function docblockTestCgrep() {
        $cgrep_test_7 = 7;
    }

    public function testRegularFunctionCgrep() {
        $cgrep_test_8 = 8;
    }

    protected function setUp(): void {
        $cgrep_test_9 = 9;
    }
    
    public function tearDown(): void {
        $cgrep_test_10 = 10;
    }
    
    public static function setUpBeforeClass(): void {
        $cgrep_test_11 = 11;
    }
    
    public static function tearDownAfterClass(): void {
        $cgrep_test_12 = 12;
    }
}

class FinalProduction_Cgrep {
    public function tripled($x) {
        $cgrep_prod_3 = 3;
        return $x * 3;
    }
}
