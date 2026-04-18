// Objective-C test example file
#import <XCTest/XCTest.h>

@interface ProductionCode_Cgrep : NSObject
- (int)compute:(int)x;
@end

@implementation ProductionCode_Cgrep
- (int)compute:(int)x {
    int cgrep_prod_1 = 1;
    return x * 2;
}
@end

@interface MyFeatureTests : XCTestCase
@end

@implementation MyFeatureTests
- (void)setUp {
    [super setUp];
    int cgrep_test_1 = 1;
}

- (void)tearDown {
    int cgrep_test_2 = 2;
    [super tearDown];
}

- (void)testSomething_cgrep {
    int cgrep_test_3 = 3;
    XCTAssertEqual(1, 1);
}
@end

@implementation FinalProduction_Cgrep
- (int)tripled:(int)x {
    int cgrep_prod_2 = 2;
    return x * 3;
}
@end
