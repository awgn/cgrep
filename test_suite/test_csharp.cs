// C# test example file

using System;
using NUnit.Framework;
using Xunit;

namespace TestExample
{
    // Regular helper class (not a test)
    public class Calculator
    {
        private int value = 0;

        public int Add(int n)
        {
            value += n;
            return value;
        }

        public int GetValue()
        {
            return value;
        }
    }

    // Helper class
    public static class MathHelper
    {
        public static int Add(int a, int b)
        {
            return a + b;
        }

        public static int Multiply(int x, int y)
        {
            return x * y;
        }
    }

    // NUnit test fixture
    [TestFixture]
    public class MathTests
    {
        [Test]
        public void AddTwoNumbers_ReturnsCorrectSum()
        {
            var result = MathHelper.Add(2, 3);
            Assert.AreEqual(5, result);
        }

        [Test]
        public void Multiply_ReturnsCorrectProduct()
        {
            var result = MathHelper.Multiply(4, 5);
            Assert.AreEqual(20, result);
        }
    }

    // Regular class (not a test)
    public class StringHelper
    {
        public static string Reverse(string s)
        {
            char[] arr = s.ToCharArray();
            Array.Reverse(arr);
            return new string(arr);
        }
    }

    // xUnit test class
    public class CalculatorTests
    {
        [Fact]
        public void Calculator_InitialValue_IsZero()
        {
            var calc = new Calculator();
            Assert.Equal(0, calc.GetValue());
        }

        [Theory]
        [InlineData(5, 10, 15)]
        [InlineData(1, 2, 3)]
        public void Calculator_Add_WorksCorrectly(int first, int second, int expected)
        {
            var calc = new Calculator();
            calc.Add(first);
            calc.Add(second);
            Assert.Equal(expected, calc.GetValue());
        }
    }

    // MSTest style tests
    [TestClass]
    public class StringTests
    {
        [TestMethod]
        public void ReverseString_ReturnsReversed()
        {
            var result = StringHelper.Reverse("hello");
            Assert.AreEqual("olleh", result);
        }

        [TestMethod]
        public void ReverseEmptyString_ReturnsEmpty()
        {
            var result = StringHelper.Reverse("");
            Assert.AreEqual("", result);
        }
    }

    // Helper function
    public static class DataProcessor
    {
        public static int[] FilterPositive(int[] data)
        {
            return Array.FindAll(data, x => x > 0);
        }
    }
}

namespace CgrepExtended
{
    // --- Production code (should survive -T False) ---
    public class ProductionHelper
    {
        public int Compute(int x)
        {
            var CGREP_IDENTIFIER = 1;
            return x * 2;
        }

        // Method whose name contains the word "Test" but is NOT a test (no attribute)
        public int RunTestHarness(int x)
        {
            var CGREP_IDENTIFIER = 2;
            return x + 1;
        }
    }

    // --- NUnit with parametrized TestCase attribute ---
    [TestFixture]
    public class NUnitParametrizedTests
    {
        [TestCase(1, 2, 3)]
        [TestCase(4, 5, 9)]
        public void Add_WithMultipleInputs(int a, int b, int expected)
        {
            var CGREP_IDENTIFIER_TEST = 1;
            Assert.AreEqual(expected, a + b);
        }

        [SetUp]
        public void Init()
        {
            var CGREP_IDENTIFIER_TEST = 2;
        }

        [TearDown]
        public void Cleanup()
        {
            var CGREP_IDENTIFIER_TEST = 3;
        }

        [OneTimeSetUp]
        public void ClassInit()
        {
            var CGREP_IDENTIFIER_TEST = 4;
        }

        [OneTimeTearDown]
        public void ClassCleanup()
        {
            var CGREP_IDENTIFIER_TEST = 5;
        }

        [Test]
        [Category("unit")]
        public void StackedAttributes_IsTest()
        {
            var CGREP_IDENTIFIER_TEST = 6;
        }
    }

    // --- xUnit without any class-level attribute ---
    public class XunitNoClassAttributeTests
    {
        [Fact]
        public void PlainFact()
        {
            var CGREP_IDENTIFIER_TEST = 7;
        }

        [Fact(Skip = "temp")]
        public void FactWithParameter()
        {
            var CGREP_IDENTIFIER_TEST = 8;
        }

        [Theory]
        [InlineData(1, 2)]
        [InlineData(3, 4)]
        public void TheoryWithInlineData(int a, int b)
        {
            var CGREP_IDENTIFIER_TEST = 9;
        }

        [Theory]
        [MemberData(nameof(DataSource))]
        public void TheoryWithMemberData(int value)
        {
            var CGREP_IDENTIFIER_TEST = 10;
        }
    }

    // --- MSTest with DataTestMethod / DataRow ---
    [TestClass]
    public class MSTestExtendedTests
    {
        [TestInitialize]
        public void Initialize()
        {
            var CGREP_IDENTIFIER_TEST = 11;
        }

        [TestCleanup]
        public void Finalize()
        {
            var CGREP_IDENTIFIER_TEST = 12;
        }

        [ClassInitialize]
        public static void ClassInit(TestContext ctx)
        {
            var CGREP_IDENTIFIER_TEST = 13;
        }

        [ClassCleanup]
        public static void ClassDispose()
        {
            var CGREP_IDENTIFIER_TEST = 14;
        }

        [DataTestMethod]
        [DataRow(1, 2, 3)]
        [DataRow(4, 5, 9)]
        public void DataDriven(int a, int b, int expected)
        {
            var CGREP_IDENTIFIER_TEST = 15;
        }

        [TestMethod("Friendly Name")]
        public void TestMethodWithArgument()
        {
            var CGREP_IDENTIFIER_TEST = 16;
        }
    }

    // --- Fully qualified attributes ---
    [NUnit.Framework.TestFixture]
    public class FullyQualifiedAttrTests
    {
        [NUnit.Framework.Test]
        public void FullyQualifiedTest()
        {
            var CGREP_IDENTIFIER_TEST = 17;
        }
    }

    // --- Production code AFTER all tests (confinement check) ---
    public static class FinalProductionUtils
    {
        public static int Tripled(int x)
        {
            var CGREP_IDENTIFIER = 3;
            return x * 3;
        }

        public static string Describe(int n)
        {
            var CGREP_IDENTIFIER = 4;
            return $"n={n}";
        }
    }
}
