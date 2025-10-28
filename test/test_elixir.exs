# Elixir test example file

defmodule MathHelper do
  @moduledoc """
  Regular helper module (not a test)
  """

  def add(a, b) do
    a + b
  end

  def multiply(x, y) do
    x * y
  end

  def factorial(0), do: 1
  def factorial(n) when n > 0 do
    n * factorial(n - 1)
  end
end

defmodule Calculator do
  @moduledoc """
  Regular calculator module (not a test)
  """

  defstruct value: 0

  def new(initial \\ 0) do
    %Calculator{value: initial}
  end

  def add(%Calculator{value: v} = calc, n) do
    %{calc | value: v + n}
  end

  def get_value(%Calculator{value: v}), do: v
end

defmodule MathHelperTest do
  use ExUnit.Case
  doctest MathHelper

  describe "basic arithmetic" do
    test "adds two numbers correctly" do
      result = MathHelper.add(2, 3)
      assert result == 5
    end

    test "multiplies numbers" do
      result = MathHelper.multiply(4, 5)
      assert result == 20
    end
  end

  describe "factorial function" do
    test "calculates factorial of 5" do
      result = MathHelper.factorial(5)
      assert result == 120
    end

    test "handles zero" do
      result = MathHelper.factorial(0)
      assert result == 1
    end

    test "handles one" do
      assert MathHelper.factorial(1) == 1
    end
  end
end

# Helper function outside test module
defmodule StringHelper do
  def reverse(str) do
    str
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.join()
  end

  def capitalize(str) do
    String.capitalize(str)
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "calculator starts with zero" do
    calc = Calculator.new()
    assert Calculator.get_value(calc) == 0
  end

  test "calculator can be initialized with value" do
    calc = Calculator.new(10)
    assert Calculator.get_value(calc) == 10
  end

  describe "calculator operations" do
    test "adds numbers correctly" do
      calc = Calculator.new()
      calc = Calculator.add(calc, 5)
      calc = Calculator.add(calc, 10)
      assert Calculator.get_value(calc) == 15
    end

    test "handles negative numbers" do
      calc = Calculator.new()
      calc = Calculator.add(calc, -5)
      assert Calculator.get_value(calc) == -5
    end
  end
end

# Regular module for data processing
defmodule DataProcessor do
  def filter_positive(list) do
    Enum.filter(list, fn x -> x > 0 end)
  end

  def double_values(list) do
    Enum.map(list, fn x -> x * 2 end)
  end
end

defmodule StringHelperTest do
  use ExUnit.Case

  describe "string operations" do
    test "reverses a string" do
      result = StringHelper.reverse("hello")
      assert result == "olleh"
    end

    test "capitalizes a string" do
      result = StringHelper.capitalize("world")
      assert result == "World"
    end

    test "handles empty string" do
      assert StringHelper.reverse("") == ""
    end
  end
end

# More helper code
defmodule Person do
  defstruct name: "", age: 0

  def new(name, age) do
    %Person{name: name, age: age}
  end
end