//
//  SEWidgetView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-3.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
@class SEResLoader;
@class PHImageView;
@class SEFontLoader;
@class SEViewNavigator;

@interface SELightView : UIView
{
    UIImage* imageBg;
    UIImage* icon;
    SEViewNavigator* mViewNav;
    float brightValue;
    float mAlpha;
    BOOL mUseNewValue;
    float mSavedBrightValue;
}
@property (nonatomic, assign) float mSavedBrightValue;
@property (nonatomic, assign) float brightValue;
@property (nonatomic, retain) UIImage* icon;
@property (nonatomic, retain) UIImage* imageBg;
- (id) initWithViewNav: (SEViewNavigator*)viewNav;
- (CGSize) getHintSize;
@end
///////////////
@interface SEDrawingWaitingView : UIView
{
    UIImageView* iconView;
    UIImageView* stageView;
    int mCurrentStage;
    NSTimer* mLoadingTimer;
    float mStartAngle;
    BOOL mStarted;
}
//- (void) setIconViewTransform: (CGAffineTransform) transform;
- (void) setStage: (int) stage;
- (int) getStage;
- (void) start;
- (void) stop;
- (CGSize) getHintSize;
@end
//////////
@interface SEPowerView : UIView
{
    //UIImage* imageBigBackground;
    //UIImage* imageNormalBackground;
    //UIImage* imageBigLowForeground;
    //UIImage* imageNormalLowForeground;
    //UIImage* imageBigFullForeground;
    //UIImage* imageNormalFullForeground;
    //UIImage* currentDrawingImage;
    SEViewNavigator* mViewNav;
    CGSize bigSize;
    CGSize normalSize;
    //NSTimer* mTimerForBatteryCharge;
    BOOL mDisplayMininumIcon;
}
//@property (nonatomic, retain) UIImage* imageBigBackground;
//@property (nonatomic, retain) UIImage* imageNormalBackground;
//@property (nonatomic, retain) UIImage* imageBigFullForeground;
//@property (nonatomic, retain) UIImage* imageBigLowForeground;
//@property (nonatomic, retain) UIImage* imageNormalFullForeground;
//@property (nonatomic, retain) UIImage* imageNormalLowForeground;
//@property (nonatomic, retain) UIImage* currentDrawingImage;
- (id) initWithViewNav: (SEViewNavigator*)viewNav;
- (void) update;
- (CGSize) getHintSize;
@end
///////////
@interface SEDateTimeView : UIView 
{
    UIImage* mDateBgImage;
    SEFontLoader* mFontLoader;
    NSMutableArray* mImageRectArray;
    SEViewNavigator* mViewNav;
    UIImage* mTimeBgImage;
    int mFontStyle;
    NSTimer* mTimeTimer;
    BOOL mDisplayMidPoint;
    
@public
    BOOL mTestUserUpgrade;
}
@property (nonatomic, retain) UIImage* mDateBgImage;
@property (nonatomic, retain) UIImage* mTimeBgImage;
@property (nonatomic, assign) int mFontStyle;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
- (CGSize) getHintSize;
- (id) initWithViewNav: (SEViewNavigator*)viewNav;
@end
