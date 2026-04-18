package testgo

import "testing"

// 1. Normal production function
func normalProductionFunction() {
	var cgrep_prod_1 = 1
}

// 2. Normal function or helper that might start with Test (depending on exact parser logic, it might be caught if we only check prefix)
// For Go, usually any `func Test...` in a test file is considered a test, but let's test a helper.
func TestifySetupHelper() {
	// If the parser strictly expects (t *testing.T), this might be skipped, but usually just "Test" prefix is enough for a basic regex/token parser.
	// Let's assume we want to identify it as a test if it starts with Test and has `(`.
	var cgrep_test_1 = 1
}

// 3. Standard Go Test
func TestBasicFunctionality(t *testing.T) {
	var cgrep_test_2 = 2
}

// 4. Testify Suite Method (Method Receiver)
type MyTestSuite struct{}

func (suite *MyTestSuite) TestCalculation() {
	var cgrep_test_3 = 3
}

// 5. Normal method on a struct (Not a test)
func (suite *MyTestSuite) SetupTest() {
	var cgrep_prod_2 = 2
}

// 6. Benchmark Test
func BenchmarkMyLogic(b *testing.B) {
	var cgrep_test_4 = 4
}

// 7. Example Test
func ExampleMyLogic() {
	var cgrep_test_5 = 5
}

// 8. Fuzz Test
func FuzzMyLogic(f *testing.F) {
	var cgrep_test_6 = 6
}