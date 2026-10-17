<?php
// PHP test example file

require_once 'PHPUnit/Autoload.php';

// Regular helper class (not a test)
class MathHelper
{
    public static function add($a, $b)
    {
        $cgrep_prod_1 = 1;
        return $a + $b;
    }

    public static function multiply($x, $y)
    {
        $cgrep_prod_2 = 2;
        return $x * $y;
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
        $cgrep_prod_3 = 3;
        $this->value += $n;
        return $this;
    }

    public function getValue()
    {
        $cgrep_prod_4 = 4;
        return $this->value;
    }

    // Regular method named `test` or `describe` - NOT a Pest block!
    public function describe($cgrep_prod_5 = 'default')
    {
        $cgrep_prod_6 = 6;
        return $cgrep_prod_5;
    }
}

// Regular production function calling `$obj->test(...)` - NOT a Pest block!
function validateCalculator($calc)
{
    $cgrep_prod_7 = 7;
    return $calc->describe($cgrep_prod_7);
}

// Production class whose name contains "Test" as part of a domain word (Testimonial):
// NOT a test class!
class TestimonialManager
{
    public function getLatest()
    {
        $cgrep_prod_8 = 8;
        return $cgrep_prod_8;
    }
}

// Production function whose name starts with lowercase `test` as part of a word (`testify`):
// NOT a PHPUnit test method!
function testifyUser($user)
{
    $cgrep_prod_9 = 9;
    return $cgrep_prod_9;
}

// Abstract test method in a base class followed by a production class:
// Must NOT swallow the subsequent production class!
abstract class BaseSpec
{
    abstract public function testMustBeImplemented();
}

class ProdServiceAfterAbstractMethod
{
    public function keepMe()
    {
        $cgrep_prod_10 = 10;
        return $cgrep_prod_10;
    }
}

// Pest framework support (including chained expectation methods `->throws(...)` after `test(...)`!)
test('pest simple test cgrep', function () {
    $cgrep_test_1 = 1;
    expect(true)->toBeTrue();
})->throws(Exception::class, $cgrep_test_2 = 'expected error');

it('does something in pest cgrep', function () {
    $cgrep_test_3 = 3;
    expect(1)->toEqual(1);
});

describe('pest group cgrep', function () {
    $cgrep_test_4 = 4;

    it('is nested', function () {
        $cgrep_test_5 = 5;
        expect(2)->toEqual(2);
    });
});

// Pest lifecycle hooks: beforeEach, afterEach, beforeAll, afterAll
beforeEach(function () {
    $cgrep_test_6 = 6;
});

afterEach(function () {
    $cgrep_test_7 = 7;
});

// PHPUnit test class with namespace path `extends \PHPUnit\Framework\TestCase`
// even when class name does NOT contain "Test"!
class CalculatorSpec extends \PHPUnit\Framework\TestCase
{
    public function helperInsideSpec()
    {
        $cgrep_test_8 = 8;
        return $cgrep_test_8;
    }

    public function testInitialValueIsZero()
    {
        $cgrep_test_9 = 9;
        $calc = new Calculator();
        $this->assertEquals(0, $calc->getValue());
    }
}

class AttributesTest_Cgrep extends TestCase
{
    public function testRegularFunctionCgrep()
    {
        $cgrep_test_10 = 10;
    }

    public function test_snake_case_method()
    {
        $cgrep_test_11 = 11;
    }

    protected function setUp(): void
    {
        $cgrep_test_12 = 12;
    }

    public function tearDown(): void
    {
        $cgrep_test_13 = 13;
    }

    public static function setUpBeforeClass(): void
    {
        $cgrep_test_14 = 14;
    }

    public static function tearDownAfterClass(): void
    {
        $cgrep_test_15 = 15;
    }
}

class FinalProduction_Cgrep
{
    public function tripled($x)
    {
        $cgrep_prod_11 = 11;
        return $x * 3;
    }
}
