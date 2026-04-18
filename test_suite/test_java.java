// 1. Normal production method
public void normalProductionMethod() {
    int cgrep_prod_1 = 1;
}

// 2. Normal method containing a variable named "Test" (False Positive check)
public void anotherNormalMethod() {
    String Test = "Not a test";
    int cgrep_prod_2 = 2;
}

// 3. Interface with @Test annotation (Should not swallow next block)
public interface TestInterface {
    @Test
    void testDeclaration();
}

public void methodAfterInterface() {
    int cgrep_prod_3 = 3;
}

// 4. Standard JUnit @Test method
@Test
public void basicTest() {
    int cgrep_test_1 = 1;
    assertEquals(1, 1);
}

// 5. JUnit 5 @ParameterizedTest
@ParameterizedTest
@ValueSource(ints = {1, 2, 3})
public void parameterizedTest(int argument) {
    int cgrep_test_2 = 2;
    assertTrue(argument > 0);
}

// 6. Test method with multiple annotations
@DisplayName("Complex test case")
@Test
@Disabled("Not working yet")
public void complexTest() {
    int cgrep_test_3 = 3;
}

// 7. Spock @TestFactory method (Dynamic tests)
@TestFactory
Stream<DynamicTest> dynamicTests() {
    int cgrep_test_4 = 4;
    return Stream.empty();
}

// 8. Custom annotation that includes "Test" in the name
@IntegrationTest
public void customIntegrationTest() {
    int cgrep_test_5 = 5;
}

// 9. Standard JUnit @BeforeEach setup method (Usually considered part of tests)
@BeforeEach
public void setUp() {
    int cgrep_test_6 = 6;
}

// 10. Standard JUnit @AfterEach teardown method
@AfterEach
public void tearDown() {
    int cgrep_test_7 = 7;
}