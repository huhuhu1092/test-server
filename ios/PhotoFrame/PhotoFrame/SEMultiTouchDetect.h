//
//  SEMultiTouchDetect.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-1.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
enum TOUCH_STATE {NO_TOUCH, TOUCH1, TOUCH2, TOUCH_NUM = TOUCH2};
enum LINE_TYPE {NO_LINE, LINE1, LINE2};
@interface SEMultiTouchDetect : NSObject
{
    enum TOUCH_STATE mTouchState;
    UITouch* mTouches[TOUCH_NUM];
    NSMutableArray* mTouchPointArray[TOUCH_NUM];
    UIView* mViewForPoint;
}
@property (nonatomic, readonly) NSMutableArray* points1;
@property (nonatomic, readonly) NSMutableArray* points2;
@property (nonatomic, readonly) enum TOUCH_STATE mTouchState;
@property (nonatomic, assign) UIView* mViewForPoint;
- (void) initData;
- (CGPoint) getCurrentDeltaPoint;
- (void) touchStateChange: (NSSet*) touches;
@end
