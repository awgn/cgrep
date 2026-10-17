# Ruby test example file

require 'rspec'
require 'minitest/autorun'

# Regular helper module (not a test)
module MathHelper
  def self.add(a, b)
    cgrep_prod_1 = 1
    a + b
  end

  def self.multiply(x, y)
    cgrep_prod_2 = 2
    x * y
  end

  # Regular method named `describe` - NOT an RSpec block!
  def self.describe(item)
    cgrep_prod_3 = 3
    item.to_s
  end
end

# Regular helper class (not a test)
class Calculator
  attr_reader :value

  def initialize(initial = 0)
    @value = initial
  end

  def add(n)
    cgrep_prod_4 = 4
    @value += n
    self
  end

  # Regular method using a local variable named `context` - NOT an RSpec block!
  def evaluate(x)
    context = x + 1
    cgrep_prod_5 = 5
    context * cgrep_prod_5
  end
end

# RSpec tests with `RSpec.describe`, `before`, `after`, `let`, and inline `{ ... }`
RSpec.describe Calculator do
  let(:calc) { cgrep_test_1 = 1; Calculator.new }

  before(:each) do
    cgrep_test_2 = 2
  end

  after(:each) do
    cgrep_test_3 = 3
  end

  it 'starts with zero by default' do
    cgrep_test_4 = 4
    expect(calc.value).to eq(0)
  end

  describe '#add' do
    it 'adds numbers correctly' do
      cgrep_test_5 = 5
      calc.add(5).add(10)
      expect(calc.value).to eq(15)
    end
  end

  context 'when disabled' do
    xit 'is skipped' do
      cgrep_test_6 = 6
    end
  end
end

# Top-level one-liner RSpec block with `{ ... }` instead of `do...end`,
# followed by a production class: must NOT swallow the production class!
it { cgrep_test_7 = 7; is_expected.to be_a(Calculator) }

class StringHelper
  def self.reverse(str)
    cgrep_prod_6 = 6
    str.reverse
  end
end

# Minitest class named `CalculatorTest < Minitest::Test` (suffix `Test`)
class CalculatorTest < Minitest::Test
  def setup
    cgrep_test_8 = 8
  end

  def teardown
    cgrep_test_9 = 9
  end

  # Helper method inside Minitest class
  def helper_in_test_class(n)
    cgrep_test_10 = 10
    n + cgrep_test_10
  end

  def test_addition_works
    cgrep_test_11 = 11
    result = MathHelper.add(1, 1)
    assert_equal 2, result
  end

  # Rails ActiveSupport::TestCase style: `test "..." do ... end`
  test "active support style test" do
    cgrep_test_12 = 12
    assert true
  end
end

# Minitest class with prefix `Test*` AND containing a one-line modifier `return 1 if n <= 0` inside!
# Followed by a production module: must NOT swallow `ProductionCode_Cgrep`!
class TestMinitestPrefix < Minitest::Test
  def test_with_modifier_if
    cgrep_test_13 = 13
    n = 5
    return 1 if n <= 0
    assert_equal 5, n
  end
end

module ProductionCode_Cgrep
  def self.compute(x)
    cgrep_prod_7 = 7
    x * 2
  end

  def self.run_test_harness(x)
    cgrep_prod_8 = 8
    x + 1
  end
end

class FinalProduction_Cgrep
  def tripled(x)
    cgrep_prod_9 = 9
    x * 3
  end
end
