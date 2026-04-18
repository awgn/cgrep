# Elixir test example file

defmodule MathHelper do
  @moduledoc """
  Regular helper module (not a test)
  """

  def add(a, b) do
    cgrep_prod_1 = 1
    a + b
  end

  def multiply(x, y) do
    cgrep_prod_2 = 2
    x * y
  end

  def factorial(0), do: 1
  def factorial(n) when n > 0 do
    cgrep_prod_3 = 3
    n * factorial(n - 1)
  end
end

defmodule Calculator do
  @moduledoc """
  Regular calculator module (not a test)
  """

  defstruct value: 0

  def new(initial \\ 0) do
    cgrep_prod_4 = 4
    %Calculator{value: initial}
  end

  def add(%Calculator{value: v} = calc, n) do
    cgrep_prod_5 = 5
    %{calc | value: v + n}
  end

  def get_value(%Calculator{value: v}), do: v
end

defmodule MathHelperTest do
  use ExUnit.Case
  doctest MathHelper
    cgrep_test_1 = 1

  describe "basic arithmetic" do
    cgrep_test_2 = 2
    test "adds two numbers correctly" do
    cgrep_test_3 = 3
      result = MathHelper.add(2, 3)
      assert result == 5
    end

    test "multiplies numbers" do
    cgrep_test_4 = 4
      result = MathHelper.multiply(4, 5)
      assert result == 20
    end
  end

  describe "factorial function" do
    cgrep_test_5 = 5
    test "calculates factorial of 5" do
    cgrep_test_6 = 6
      result = MathHelper.factorial(5)
      assert result == 120
    end

    test "handles zero" do
    cgrep_test_7 = 7
      result = MathHelper.factorial(0)
      assert result == 1
    end

    test "handles one" do
    cgrep_test_8 = 8
      assert MathHelper.factorial(1) == 1
    end
  end
end

# Helper function outside test module
defmodule StringHelper do
  def reverse(str) do
    cgrep_prod_6 = 6
    str
    |> String.graphemes()
    |> Enum.reverse()
    |> Enum.join()
  end

  def capitalize(str) do
    cgrep_prod_7 = 7
    String.capitalize(str)
  end
end

defmodule CalculatorTest do
  use ExUnit.Case

  test "calculator starts with zero" do
    cgrep_test_9 = 9
    calc = Calculator.new()
    assert Calculator.get_value(calc) == 0
  end

  test "calculator can be initialized with value" do
    cgrep_test_10 = 10
    calc = Calculator.new(10)
    assert Calculator.get_value(calc) == 10
  end

  describe "calculator operations" do
    cgrep_test_11 = 11
    test "adds numbers correctly" do
    cgrep_test_12 = 12
      calc = Calculator.new()
      calc = Calculator.add(calc, 5)
      calc = Calculator.add(calc, 10)
      assert Calculator.get_value(calc) == 15
    end

    test "handles negative numbers" do
    cgrep_test_13 = 13
      calc = Calculator.new()
      calc = Calculator.add(calc, -5)
      assert Calculator.get_value(calc) == -5
    end
  end
end

# Regular module for data processing
defmodule DataProcessor do
  def filter_positive(list) do
    cgrep_prod_8 = 8
    Enum.filter(list, fn x -> x > 0 end)
  end

  def double_values(list) do
    cgrep_prod_9 = 9
    Enum.map(list, fn x -> x * 2 end)
  end
end

defmodule StringHelperTest do
  use ExUnit.Case

  describe "string operations" do
    cgrep_test_14 = 14
    test "reverses a string" do
    cgrep_test_15 = 15
      result = StringHelper.reverse("hello")
      assert result == "olleh"
    end

    test "capitalizes a string" do
    cgrep_test_16 = 16
      result = StringHelper.capitalize("world")
      assert result == "World"
    end

    test "handles empty string" do
    cgrep_test_17 = 17
      assert StringHelper.reverse("") == ""
    end
  end
end

# More helper code
defmodule Person do
  defstruct name: "", age: 0

  def new(name, age) do
    cgrep_prod_10 = 10
    %Person{name: name, age: age}
  end
end
# =========================================================================
# --- CGREP SEMANTIC TESTS (appended) ---
# =========================================================================

defmodule ProductionModule_Cgrep do
  def run(x) do
    cgrep_prod_11 = 11
    x * 2
  end
  
  def run_test_harness(x) do
    cgrep_prod_12 = 12
    x + 1
  end
end

defmodule MyFeatureTest do
  use ExUnit.Case

  setup do
    cgrep_test_18 = 18
    :ok
  end

  setup_all do
    cgrep_test_19 = 19
    :ok
  end

  test "simple test cgrep" do
    cgrep_test_20 = 20
    assert 1 == 1
  end

  describe "nested group cgrep" do
    cgrep_test_21 = 21
    
    test "inside group" do
    cgrep_test_22 = 22
      assert 3 == 3
    end
  end
end

# An ExUnit test not in a *Test module (valid in scripts, though uncommon)
test "standalone test cgrep" do
    cgrep_test_23 = 23
  assert 2 == 2
end

describe "standalone describe cgrep" do
    cgrep_test_24 = 24
  
  test "standalone inside describe" do
    cgrep_test_25 = 25
    assert 4 == 4
  end
end

defmodule FinalProduction_Cgrep do
  def tripled(x) do
    cgrep_prod_13 = 13
    x * 3
  end
end
