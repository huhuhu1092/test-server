//
//  PhotoFrameTests.m
//  PhotoFrameTests
//
//  Created by 陈勇 on 11-10-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PhotoFrameTests.h"
#import "SEUtil.h"
#import "SESystemConfig.h"
@implementation PhotoFrameTests

- (void)setUp
{
    [super setUp];
    
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}

- (void)testExample
{
    //STFail(@"Unit tests are not implemented yet in PhotoFrameTests");
    int v = [SESystemConfig getMaxEdgeDetectValue];
    assert(v == 10);
}

@end
