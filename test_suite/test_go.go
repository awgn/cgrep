package testgo

import "testing"

// 1. Normal production function
func normalProductionFunction() {
	var CGREP_IDENTIFIER = 1
}

// 2. Normal function or helper that might start with Test (depending on exact parser logic, it might be caught if we only check prefix)
// For Go, usually any `func Test...` in a test file is considered a test, but let's test a helper.
func TestifySetupHelper() {
	// If the parser strictly expects (t *testing.T), this might be skipped, but usually just "Test" prefix is enough for a basic regex/token parser.
	// Let's assume we want to identify it as a test if it starts with Test and has `(`.
	var CGREP_IDENTIFIER_TEST = 2
}

// 3. Standard Go Test
func TestBasicFunctionality(t *testing.T) {
	var CGREP_IDENTIFIER_TEST = 3
}

// 4. Testify Suite Method (Method Receiver)
type MyTestSuite struct{}

func (suite *MyTestSuite) TestCalculation() {
	var CGREP_IDENTIFIER_TEST = 4
}

// 5. Normal method on a struct (Not a test)
func (suite *MyTestSuite) SetupTest() {
	var CGREP_IDENTIFIER = 5
}

// 6. Benchmark Test
func BenchmarkMyLogic(b *testing.B) {
	var CGREP_IDENTIFIER_TEST = 6
}

// 7. Example Test
func ExampleMyLogic() {
	var CGREP_IDENTIFIER_TEST = 7
}

// 8. Fuzz Test
func FuzzMyLogic(f *testing.F) {
	var CGREP_IDENTIFIER_TEST = 8
}