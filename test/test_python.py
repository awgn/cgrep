# Test file for Python test filtering

# Regular function (not a test)
def helper_function():
    return 42

# Test with naming convention
def test_simple_addition():
    assert 1 + 1 == 2
    assert 2 + 2 == 4

# Another regular function
def calculate_something(x, y):
    return x * y

# Test with @pytest decorator
@pytest.fixture
def my_fixture():
    return "test_value"

# Test with @pytest.mark decorator
@pytest.mark.parametrize("input,expected", [(1, 2), (3, 4)])
def test_with_parameters(input, expected):
    assert input + 1 == expected

# Regular class (not a test)
class HelperClass:
    def __init__(self):
        self.value = 10

# Test class with naming convention
class TestMathOperations:
    def test_multiplication(self):
        assert 2 * 3 == 6
    
    def test_division(self):
        assert 10 / 2 == 5

# Test with @unittest decorator
@unittest.skip("Testing skip decorator")
def test_skipped_test():
    assert False

# Another test function
def test_string_operations():
    s = "hello"
    assert s.upper() == "HELLO"
    assert len(s) == 5

# Final regular function
def cleanup():
    pass