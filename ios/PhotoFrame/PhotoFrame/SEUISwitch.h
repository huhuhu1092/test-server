//
//  SEUISwitch.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-7-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@class SEViewNavigator;
@interface SEUISwitch : UIView
{
    enum TOUCH_STATE {TOUCH_END, TOUCH_START};
    BOOL mIsOn;
    enum TOUCH_STATE mTouchState;
    CGPoint mPrevTouchPoint;
    //UIImageView* mOnImageView;
    //UIImageView* mOffImageView;
    UIImageView* mOnOffImageView;
    UIImageView* mMidImageView;
    float mOnOffWidth;
    float mMidWidth;
    BOOL mMidViewOnEdge;
    BOOL mAnimStart;
    //BOOL mAnimStart[3];
    id mTarget;
    SEL mAction;
}
@property (nonatomic, readonly) BOOL isOn;
- (void) setOn: (BOOL) bOn animated:(BOOL)animated;
- (void) initImageView : (SEViewNavigator*)viewNav;
- (void) setTarget: (id) target action: (SEL) action;
@end
