// 1. Normal production function
void normal_production_function() {
    int CGREP_IDENTIFIER = 1;
}

// 2. Normal function containing a variable with a "test" prefix (False Positive check)
void another_normal_function() {
    int test_counter = 0;
    int CGREP_IDENTIFIER = 2;
}

// 3. Normal function calling a function with a "Test" prefix (False Positive check)
void initialize_system() {
    TestEngine_Init();
    int CGREP_IDENTIFIER = 3;
}

// 4. Forward declaration of a test function (Should not swallow the next function)
void test_forward_decl();

void normal_function_after_decl() {
    int CGREP_IDENTIFIER = 4;
}

// 5. Plain C test function
void test_basic_functionality() {
    int CGREP_IDENTIFIER_TEST = 5;
    assert(1 == 1);
}

// 6. Google Test TEST macro
TEST(MathTest, Addition) {
    int CGREP_IDENTIFIER_TEST = 6;
    EXPECT_EQ(1 + 1, 2);
}

// 7. Google Test TEST_F macro (Fixture)
TEST_F(DatabaseFixture, Connection) {
    int CGREP_IDENTIFIER_TEST = 7;
}

// 8. Catch2 TEST_CASE macro
TEST_CASE("Vector operations", "[vector]") {
    int CGREP_IDENTIFIER_TEST = 8;
    REQUIRE(true);
}

// 9. Google Test TYPED_TEST macro
TYPED_TEST(MyType, DoesSomething) {
    int CGREP_IDENTIFIER_TEST = 9;
}

// 10 & 11. Catch2 TEST_CASE with SECTION
TEST_CASE("Complex test", "[tag]") {
    int CGREP_IDENTIFIER_TEST = 10;
    
    SECTION("First section") {
        int CGREP_IDENTIFIER_TEST = 11;
    }
}

// 12. C++ Class starting with Test (False Positive check - should be normal code)
class TestRunner {
public:
    void run() {
        int CGREP_IDENTIFIER = 12;
    }
};