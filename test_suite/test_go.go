package testgo

import "testing"

// 1. Normal production function
func normalProductionFunction() {
	var cgrep_prod_1 = 1
}

// 2. Normal function starting with "Test" followed by lowercase (NOT a Go test according to Go spec)
func TestifySetupHelper() {
	var cgrep_prod_2 = 2
}

// 2b. Test function with interface{}/struct{} in parameters
func TestWithEmptyInterface(x interface{}, y struct{}) {
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

// 4b. Testify method whose return type contains `struct{}` or `interface{}`:
// Must NOT mistake the `{}` of `struct{}` in the return type for the function body!
func (suite *MyTestSuite) TestReturnEmptyStruct() struct{} {
	var cgrep_test_7 = 7
	return struct{}{}
}

// 5. Normal method on a struct (Not a test)
func (suite *MyTestSuite) SetupTest() {
	var cgrep_prod_3 = 3
}

// 5b. Bodyless Go function declaration (e.g. assembly-backed) starting with `Test*`
// followed by a normal production function: must NOT swallow the next function's `{ ... }`!
func TestCpuFeatureSupport() bool

func NormalAfterBodylessDecl() {
	var cgrep_prod_4 = 4
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
