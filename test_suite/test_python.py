# Test file for Python test filtering

import unittest
import pytest

# Regular function (not a test)
def helper_function():
    CGREP_IDENTIFIER = 1
    return 42

# Test with naming convention
def test_simple_addition():
    CGREP_IDENTIFIER_TEST = 2
    assert 1 + 1 == 2
    assert 2 + 2 == 4

# Another regular function
def calculate_something(x, y):
    CGREP_IDENTIFIER = 3
    return x * y

# Test with @pytest decorator
@pytest.fixture
def my_fixture():
    CGREP_IDENTIFIER_TEST = 4
    return "test_value"

# Test with @pytest.mark decorator
@pytest.mark.parametrize("input,expected", [(1, 2), (3, 4)])
def test_with_parameters(input, expected):
    CGREP_IDENTIFIER_TEST = 5
    assert input + 1 == expected

# Regular class (not a test)
class HelperClass:
    def __init__(self):
        CGREP_IDENTIFIER = 6
        self.value = 10
        self.test_variable = "Not a test" # false positive check

# Test class with naming convention
class TestMathOperations:
    def test_multiplication(self):
        CGREP_IDENTIFIER_TEST = 7
        assert 2 * 3 == 6

    def test_division(self):
        CGREP_IDENTIFIER_TEST = 8
        assert 10 / 2 == 5
        
    def _helper_inside_test_class(self):
        # Often test classes have helpers. Whether they are filtered or not depends on the parser.
        CGREP_IDENTIFIER_TEST = 9
        pass

# Test with @unittest decorator
@unittest.skip("Testing skip decorator")
def test_skipped_test():
    CGREP_IDENTIFIER_TEST = 10
    assert False

# Another test function
def test_string_operations():
    CGREP_IDENTIFIER_TEST = 11
    s = "hello"
    assert s.upper() == "HELLO"
    assert len(s) == 5

# Final regular function
def cleanup():
    CGREP_IDENTIFIER = 12
    pass

# --- EXTENSIONS ---

# Async test function
async def test_async_logic():
    CGREP_IDENTIFIER_TEST = 13
    assert True

# Standard unittest.TestCase class
class MyStandardTest(unittest.TestCase):
    def setUp(self):
        CGREP_IDENTIFIER_TEST = 14
        
    def test_something(self):
        CGREP_IDENTIFIER_TEST = 15
        self.assertTrue(True)

# Normal function containing a test_ prefix but not a definition
def normal_function_with_test_vars():
    test_count = 0
    CGREP_IDENTIFIER = 16
    return test_count