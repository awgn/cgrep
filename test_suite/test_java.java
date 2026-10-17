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

// 11. Production method with annotation whose argument mentions Test (False Positive check)
@SuppressWarnings(TestConstants.UNCHECKED)
public void annotatedProdMethod() {
    int cgrep_prod_4 = 4;
}

// 11b. Production method with domain annotation `@Testimonial` ("Test" followed by lowercase):
@Testimonial
public void domainTestimonialMethod() {
    int cgrep_prod_5 = 5;
}

// 12. Test method with array braces inside parameter annotation
@Test
public void testWithParamArrayAnno(@MyAnno({1, 2}) String input) {
    int cgrep_test_8 = 8;
}

// 13. JUnit 5 @Nested test class
@Nested
class WhenSomethingHappens {
    void helperInNested() {
        int cgrep_test_9 = 9;
    }
}

// 14. Generic JUnit 3 class extending TestCase (`class GenericLegacy<T> extends TestCase`)
class GenericLegacy<T> extends TestCase {
    public void testLegacy() {
        int cgrep_test_10 = 10;
    }
}
