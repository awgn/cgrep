// 1. Normal production function
void normal_production_function() {
    int cgrep_prod_1 = 1;
}

// 2. Normal function containing a variable with a "test" prefix (False Positive check)
void another_normal_function() {
    int test_counter = 0;
    int cgrep_prod_2 = 2;
}

// 3. Normal function calling a function with a "Test" prefix (False Positive check)
void initialize_system() {
    TestEngine_Init();
    int cgrep_prod_3 = 3;
}

// 4. Forward declaration of a test function (Should not swallow the next function)
void test_forward_decl();

void normal_function_after_decl() {
    int cgrep_prod_4 = 4;
}

// 5. Plain C test function
void test_basic_functionality() {
    int cgrep_test_1 = 1;
    assert(1 == 1);
}

// 6. Google Test TEST macro
TEST(MathTest, Addition) {
    int cgrep_test_2 = 2;
    EXPECT_EQ(1 + 1, 2);
}

// 7. Google Test TEST_F macro (Fixture)
TEST_F(DatabaseFixture, Connection) {
    int cgrep_test_3 = 3;
}

// 8. Catch2 TEST_CASE macro
TEST_CASE("Vector operations", "[vector]") {
    int cgrep_test_4 = 4;
    REQUIRE(true);
}

// 9. Google Test TYPED_TEST macro
TYPED_TEST(MyType, DoesSomething) {
    int cgrep_test_5 = 5;
}

// 10 & 11. Catch2 TEST_CASE with SECTION
TEST_CASE("Complex test", "[tag]") {
    int cgrep_test_6 = 6;
    
    SECTION("First section") {
        int cgrep_test_7 = 7;
    }
}

// 12. C++ Class starting with Test (False Positive check - should be normal code)
class TestRunner {
public:
    void run() {
        int cgrep_prod_5 = 5;
    }
};