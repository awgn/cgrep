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
            var cgrep_prod_1 = 1;
            value += n;
            return value;
        }

        public int GetValue()
        {
            var cgrep_prod_2 = 2;
            return value;
        }
    }

    // Helper class
    public static class MathHelper
    {
        public static int Add(int a, int b)
        {
            var cgrep_prod_3 = 3;
            return a + b;
        }

        public static int Multiply(int x, int y)
        {
            var cgrep_prod_4 = 4;
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
            var cgrep_test_1 = 1;
            var result = MathHelper.Add(2, 3);
            Assert.AreEqual(5, result);
        }

        [Test]
        public void Multiply_ReturnsCorrectProduct()
        {
            var cgrep_test_2 = 2;
            var result = MathHelper.Multiply(4, 5);
            Assert.AreEqual(20, result);
        }
    }

    // Regular class (not a test)
    public class StringHelper
    {
        public static string Reverse(string s)
        {
            var cgrep_prod_5 = 5;
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
            var cgrep_test_3 = 3;
            var calc = new Calculator();
            Assert.Equal(0, calc.GetValue());
        }

        [Theory]
        [InlineData(5, 10, 15)]
        [InlineData(1, 2, 3)]
        public void Calculator_Add_WorksCorrectly(int first, int second, int expected)
        {
            var cgrep_test_4 = 4;
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
            var cgrep_test_5 = 5;
            var result = StringHelper.Reverse("hello");
            Assert.AreEqual("olleh", result);
        }

        [TestMethod]
        public void ReverseEmptyString_ReturnsEmpty()
        {
            var cgrep_test_6 = 6;
            var result = StringHelper.Reverse("");
            Assert.AreEqual("", result);
        }
    }

    // Helper function
    public static class DataProcessor
    {
        public static int[] FilterPositive(int[] data)
        {
            var cgrep_prod_6 = 6;
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
            var cgrep_prod_7 = 7;
            return x * 2;
        }

        // Method whose name contains the word "Test" but is NOT a test (no attribute)
        public int RunTestHarness(int x)
        {
            var cgrep_prod_8 = 8;
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
            var cgrep_test_7 = 7;
            Assert.AreEqual(expected, a + b);
        }

        [SetUp]
        public void Init()
        {
            var cgrep_test_8 = 8;
        }

        [TearDown]
        public void Cleanup()
        {
            var cgrep_test_9 = 9;
        }

        [OneTimeSetUp]
        public void ClassInit()
        {
            var cgrep_test_10 = 10;
        }

        [OneTimeTearDown]
        public void ClassCleanup()
        {
            var cgrep_test_11 = 11;
        }

        [Test]
        [Category("unit")]
        public void StackedAttributes_IsTest()
        {
            var cgrep_test_12 = 12;
        }
    }

    // --- xUnit without any class-level attribute ---
    public class XunitNoClassAttributeTests
    {
        [Fact]
        public void PlainFact()
        {
            var cgrep_test_13 = 13;
        }

        [Fact(Skip = "temp")]
        public void FactWithParameter()
        {
            var cgrep_test_14 = 14;
        }

        [Theory]
        [InlineData(1, 2)]
        [InlineData(3, 4)]
        public void TheoryWithInlineData(int a, int b)
        {
            var cgrep_test_15 = 15;
        }

        [Theory]
        [MemberData(nameof(DataSource))]
        public void TheoryWithMemberData(int value)
        {
            var cgrep_test_16 = 16;
        }
    }

    // --- MSTest with DataTestMethod / DataRow ---
    [TestClass]
    public class MSTestExtendedTests
    {
        [TestInitialize]
        public void Initialize()
        {
            var cgrep_test_17 = 17;
        }

        [TestCleanup]
        public void Finalize()
        {
            var cgrep_test_18 = 18;
        }

        [ClassInitialize]
        public static void ClassInit(TestContext ctx)
        {
            var cgrep_test_19 = 19;
        }

        [ClassCleanup]
        public static void ClassDispose()
        {
            var cgrep_test_20 = 20;
        }

        [DataTestMethod]
        [DataRow(1, 2, 3)]
        [DataRow(4, 5, 9)]
        public void DataDriven(int a, int b, int expected)
        {
            var cgrep_test_21 = 21;
        }

        [TestMethod("Friendly Name")]
        public void TestMethodWithArgument()
        {
            var cgrep_test_22 = 22;
        }
    }

    // --- Fully qualified attributes ---
    [NUnit.Framework.TestFixture]
    public class FullyQualifiedAttrTests
    {
        [NUnit.Framework.Test]
        public void FullyQualifiedTest()
        {
            var cgrep_test_23 = 23;
        }
    }

    // --- Production code AFTER all tests (confinement check) ---
    public static class FinalProductionUtils
    {
        public static int Tripled(int x)
        {
            var cgrep_prod_9 = 9;
            return x * 3;
        }

        public static string Describe(int n)
        {
            var cgrep_prod_10 = 10;
            return $"n={n}";
        }
    }
}
