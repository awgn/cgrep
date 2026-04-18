// Objective-C test example file
#import <XCTest/XCTest.h>

@interface ProductionCode_Cgrep : NSObject
- (int)compute:(int)x;
@end

@implementation ProductionCode_Cgrep
- (int)compute:(int)x {
    int CGREP_IDENTIFIER = 1;
    return x * 2;
}
@end

@interface MyFeatureTests : XCTestCase
@end

@implementation MyFeatureTests
- (void)setUp {
    [super setUp];
    int CGREP_IDENTIFIER_TEST = 1;
}

- (void)tearDown {
    int CGREP_IDENTIFIER_TEST = 2;
    [super tearDown];
}

- (void)testSomething_cgrep {
    int CGREP_IDENTIFIER_TEST = 3;
    XCTAssertEqual(1, 1);
}
@end

@implementation FinalProduction_Cgrep
- (int)tripled:(int)x {
    int CGREP_IDENTIFIER = 2;
    return x * 3;
}
@end
