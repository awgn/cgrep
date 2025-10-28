// D test example file

import std.stdio;
import std.algorithm;
import std.array;
import std.conv;

// Regular helper function (not a test)
int add(int a, int b)
{
    return a + b;
}

// Another helper function
int multiply(int x, int y)
{
    return x * y;
}

// Helper function
int factorial(int n)
{
    if (n <= 0)
        return 1;
    return n * factorial(n - 1);
}

// unittest block - D's built-in test
unittest
{
    assert(add(2, 3) == 5);
    assert(multiply(4, 5) == 20);
}

// Regular class (not a test)
class Calculator
{
    private int value = 0;

    this(int initial = 0)
    {
        value = initial;
    }

    Calculator add(int n)
    {
        value += n;
        return this;
    }

    int getValue()
    {
        return value;
    }

    void reset()
    {
        value = 0;
    }
}

// More unittest blocks
unittest
{
    assert(factorial(5) == 120);
    assert(factorial(0) == 1);
    assert(factorial(1) == 1);
}

// Helper function (not a test)
int[] processData(int[] arr)
{
    return arr.filter!(x => x > 0).map!(x => x * 2).array;
}

// Calculator tests
unittest
{
    auto calc = new Calculator();
    assert(calc.getValue() == 0);
}

unittest
{
    auto calc = new Calculator(10);
    assert(calc.getValue() == 10);
}

unittest
{
    auto calc = new Calculator();
    calc.add(5).add(10);
    assert(calc.getValue() == 15);
}

unittest
{
    auto calc = new Calculator();
    calc.add(-5);
    assert(calc.getValue() == -5);
}

unittest
{
    auto calc = new Calculator();
    calc.add(100);
    calc.reset();
    assert(calc.getValue() == 0);
}

// Helper struct (not a test)
struct Person
{
    string name;
    int age;

    string greeting()
    {
        return "Hello, I'm " ~ name;
    }
}

Person makePerson(string name, int age)
{
    return Person(name, age);
}

// Helper function
string formatNumber(double n)
{
    import std.format;
    return format("%.2f", n);
}

// String operations helper
string reverseString(string s)
{
    import std.range;
    return s.retro.to!string;
}

string capitalizeString(string s)
{
    import std.string;
    return s.toUpper;
}

// String tests
unittest
{
    assert(reverseString("hello") == "olleh");
}

unittest
{
    assert(capitalizeString("world") == "WORLD");
}

unittest
{
    assert(reverseString("") == "");
}

// More helper code
int[] filterPositive(int[] data)
{
    return data.filter!(x => x > 0).array;
}

void main()
{
    writeln("D test example - run with -unittest flag");
}