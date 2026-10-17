// Objective-C test example file
#import <XCTest/XCTest.h>

@interface ProductionCode_Cgrep : NSObject
- (int)compute:(int)x;
- (BOOL)testConnection:(NSString *)host;
- (int)testifyAccount;
@end

@implementation ProductionCode_Cgrep
- (int)compute:(int)x {
    int cgrep_prod_1 = 1;
    return x * 2;
}

// Production method whose selector starts with `testConnection:` but takes a parameter (`:`):
// XCTest methods are strictly 0-argument `- (void)test...` methods!
- (BOOL)testConnection:(NSString *)host {
    int cgrep_prod_3 = 3;
    return cgrep_prod_3 > 0;
}

// Production method whose name starts with `test` followed by lowercase (`testifyAccount`):
- (int)testifyAccount {
    int cgrep_prod_4 = 4;
    return cgrep_prod_4;
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
