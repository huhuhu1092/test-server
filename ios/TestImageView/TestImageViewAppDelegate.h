//
//  TestImageViewAppDelegate.h
//  TestImageView
//
//  Created by 陈勇 on 11-9-27.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>

@class TestImageViewViewController;

@interface TestImageViewAppDelegate : NSObject <UIApplicationDelegate>

@property (nonatomic, retain) IBOutlet UIWindow *window;

@property (nonatomic, retain) IBOutlet TestImageViewViewController *viewController;

- (void)createGCDTest;
@end
