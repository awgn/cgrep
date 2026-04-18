// 1. Normal production method
public void normalProductionMethod() {
    int CGREP_IDENTIFIER = 1;
}

// 2. Normal method containing a variable named "Test" (False Positive check)
public void anotherNormalMethod() {
    String Test = "Not a test";
    int CGREP_IDENTIFIER = 2;
}

// 3. Interface with @Test annotation (Should not swallow next block)
public interface TestInterface {
    @Test
    void testDeclaration();
}

public void methodAfterInterface() {
    int CGREP_IDENTIFIER = 3;
}

// 4. Standard JUnit @Test method
@Test
public void basicTest() {
    int CGREP_IDENTIFIER_TEST = 4;
    assertEquals(1, 1);
}

// 5. JUnit 5 @ParameterizedTest
@ParameterizedTest
@ValueSource(ints = {1, 2, 3})
public void parameterizedTest(int argument) {
    int CGREP_IDENTIFIER_TEST = 5;
    assertTrue(argument > 0);
}

// 6. Test method with multiple annotations
@DisplayName("Complex test case")
@Test
@Disabled("Not working yet")
public void complexTest() {
    int CGREP_IDENTIFIER_TEST = 6;
}

// 7. Spock @TestFactory method (Dynamic tests)
@TestFactory
Stream<DynamicTest> dynamicTests() {
    int CGREP_IDENTIFIER_TEST = 7;
    return Stream.empty();
}

// 8. Custom annotation that includes "Test" in the name
@IntegrationTest
public void customIntegrationTest() {
    int CGREP_IDENTIFIER_TEST = 8;
}

// 9. Standard JUnit @BeforeEach setup method (Usually considered part of tests)
@BeforeEach
public void setUp() {
    int CGREP_IDENTIFIER_TEST = 9;
}

// 10. Standard JUnit @AfterEach teardown method
@AfterEach
public void tearDown() {
    int CGREP_IDENTIFIER_TEST = 10;
}