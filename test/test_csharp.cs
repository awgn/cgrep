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