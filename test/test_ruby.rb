# Ruby test example file

require 'rspec'
require 'minitest/autorun'

# Regular helper module (not a test)
module MathHelper
  def self.add(a, b)
    a + b
  end

  def self.multiply(x, y)
    x * y
  end

  def self.factorial(n)
    return 1 if n <= 0
    n * factorial(n - 1)
  end
end

# Regular helper class (not a test)
class Calculator
  attr_reader :value

  def initialize(initial = 0)
    @value = initial
  end

  def add(n)
    @value += n
    self
  end

  def reset
    @value = 0
  end
end

# RSpec tests
describe 'MathHelper' do
  describe 'basic arithmetic' do
    it 'adds two numbers correctly' do
      result = MathHelper.add(2, 3)
      expect(result).to eq(5)
    end

    it 'multiplies numbers' do
      result = MathHelper.multiply(4, 5)
      expect(result).to eq(20)
    end
  end

  context 'when calculating factorials' do
    it 'calculates factorial of 5' do
      result = MathHelper.factorial(5)
      expect(result).to eq(120)
    end

    it 'handles zero' do
      result = MathHelper.factorial(0)
      expect(result).to eq(1)
    end
  end
end

# Helper function
def process_data(array)
  array.select { |x| x > 0 }.map { |x| x * 2 }
end

# RSpec tests for Calculator
describe Calculator do
  it 'starts with zero by default' do
    calc = Calculator.new
    expect(calc.value).to eq(0)
  end

  it 'can be initialized with a value' do
    calc = Calculator.new(10)
    expect(calc.value).to eq(10)
  end

  describe '#add' do
    it 'adds numbers correctly' do
      calc = Calculator.new
      calc.add(5).add(10)
      expect(calc.value).to eq(15)
    end

    it 'handles negative numbers' do
      calc = Calculator.new
      calc.add(-5)
      expect(calc.value).to eq(-5)
    end
  end

  describe '#reset' do
    it 'resets to zero' do
      calc = Calculator.new
      calc.add(100)
      calc.reset
      expect(calc.value).to eq(0)
    end
  end
end

# Minitest style tests
class TestMathHelper < Minitest::Test
  def test_addition_works
    result = MathHelper.add(1, 1)
    assert_equal 2, result
  end

  def test_multiplication_works
    result = MathHelper.multiply(3, 4)
    assert_equal 12, result
  end

  def test_factorial_calculation
    result = MathHelper.factorial(3)
    assert_equal 6, result
  end
end

# Regular class (not a test)
class StringHelper
  def self.reverse(str)
    str.reverse
  end

  def self.capitalize_all(str)
    str.upcase
  end
end

# Minitest for StringHelper
class TestStringHelper < Minitest::Test
  def test_reverse_string
    result = StringHelper.reverse("hello")
    assert_equal "olleh", result
  end

  def test_capitalize_string
    result = StringHelper.capitalize_all("world")
    assert_equal "WORLD", result
  end

  def test_empty_string
    result = StringHelper.reverse("")
    assert_equal "", result
  end
end

# More helper code
class Person
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  def greeting
    "Hello, I'm #{@name}"
  end
end

# Helper method
def format_number(n)
  "%.2f" % n
end