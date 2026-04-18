# Test file for Python test filtering

import unittest
import pytest

# Regular function (not a test)
def helper_function():
    cgrep_prod_1 = 1
    return 42

# Test with naming convention
def test_simple_addition():
    cgrep_test_1 = 1
    assert 1 + 1 == 2
    assert 2 + 2 == 4

# Another regular function
def calculate_something(x, y):
    cgrep_prod_2 = 2
    return x * y

# Test with @pytest decorator
@pytest.fixture
def my_fixture():
    cgrep_test_2 = 2
    return "test_value"

# Test with @pytest.mark decorator
@pytest.mark.parametrize("input,expected", [(1, 2), (3, 4)])
def test_with_parameters(input, expected):
    cgrep_test_3 = 3
    assert input + 1 == expected

# Regular class (not a test)
class HelperClass:
    def __init__(self):
        cgrep_prod_3 = 3
        self.value = 10
        self.test_variable = "Not a test" # false positive check

# Test class with naming convention
class TestMathOperations:
    def test_multiplication(self):
        cgrep_test_4 = 4
        assert 2 * 3 == 6

    def test_division(self):
        cgrep_test_5 = 5
        assert 10 / 2 == 5
        
    def _helper_inside_test_class(self):
        # Often test classes have helpers. Whether they are filtered or not depends on the parser.
        cgrep_test_6 = 6
        pass

# Test with @unittest decorator
@unittest.skip("Testing skip decorator")
def test_skipped_test():
    cgrep_test_7 = 7
    assert False

# Another test function
def test_string_operations():
    cgrep_test_8 = 8
    s = "hello"
    assert s.upper() == "HELLO"
    assert len(s) == 5

# Final regular function
def cleanup():
    cgrep_prod_4 = 4
    pass

# --- EXTENSIONS ---

# Async test function
async def test_async_logic():
    cgrep_test_9 = 9
    assert True

# Standard unittest.TestCase class
class MyStandardTest(unittest.TestCase):
    def setUp(self):
        cgrep_test_10 = 10
        
    def test_something(self):
        cgrep_test_11 = 11
        self.assertTrue(True)

# Normal function containing a test_ prefix but not a definition
def normal_function_with_test_vars():
    test_count = 0
    cgrep_prod_5 = 5
    return test_count