//
//  SEMainDisplay.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-15.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
#import "SEFixedView.h"
#import "SEWidgetView.h"
@class SEResLoader;
@class PHImageView;
@class SEFontLoader;
@class SEViewNavigator;
@class SEDrawTouchView;
@class FontLabel;
enum TOOLBAR_BUTTON_TYPE {PLAY_PAUSE, MUSIC_SELECT, IMAGE_SELECT, OPTION, PREVIEW, MUSIC_IMAGE_LIST, INVALID_TOOLBAR_BUTTON, TOOLBAR_BUTTON_NUM};
@interface SEMainDisplayToolBarView : UIView 
{
    UIButton* mButtons[TOOLBAR_BUTTON_NUM];
    SEViewNavigator* mViewNav;
    id mToolBarButtonHandleTarget;
    SEL mToolBarButtonHandleAction;
    UIImage* imagePortrait;
    UIImage* imageLandscape;
    CGSize mHintSizePortrait;
    CGSize mHintSizeLandscape;
    
}

@property (nonatomic, retain) UIImage* imagePortrait;
@property (nonatomic, retain) UIImage* imageLandscape;
- (void)setClickHandleTarget:(id)target withAction:(SEL)action;
- (id) initWithViewNav:(SEViewNavigator*) viewNav;
- (void) updateFrame;
@end
////////////
@interface SEMainDisplay : SEFixedView
{
    PHImageView* mMainDispalyImageView;
    SEMainDisplayToolBarView* mToolBarView;
    SEDateTimeView* mDateTimeView;
    SEResLoader* mResLoader;
    CGRect mFrame;
    BOOL mToolBarHidden;
    SEViewNavigator* mViewNav;
    SEPowerView* mPowerView;
    SELightView* mLightView;
    SEDrawTouchView* mDrawView;
    SEDrawingWaitingView* mDrawingWaitingView;
    
    UIImageView* mLeftFrameView;
    UIImageView* mRightFrameView;
    UIImageView* mTopFrameView;
    UIImageView* mBottomFrameView;
    CGRect mLeftFrameRect;
    CGRect mRightFrameRect;
    CGRect mTopFrameRect;
    CGRect mBottomFrameRect;
    CGPoint mLeftFrameCenter;
    CGPoint mRightFrameCenter;
    CGPoint mTopFrameCenter;
    CGPoint mBottomFrameCenter;
    BOOL animFinished[4];
    UIImage* mFrameImage;
    FontLabel* mIndicationView;
    int mIndicatorViewTime;
    int mCurrentIndicatorViewTime;
@private
    BOOL mToolBarAnimationEnd;
    BOOL mDateTimeAnimationEnd;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;

- (void) setToolBarButtonHandleTarget: (id) target withAction:(SEL)action;
- (id)  initWithResLoader: (SEResLoader*)resLoader withFrame:(CGRect)frame;
- (void) loadView;
- (void) tapHandler: (UITapGestureRecognizer*)tapGes;
- (void) updateTime;
- (void) hideSignatureView;
- (void) setPlayIcon; 
- (void) setPaurseIcon;
- (void) startSignatureAnim;
- (CGRect) getSignatureRect;
- (float) getToolBarHeight;
- (void) startDrawingLoading;
- (void) stopDrawingLoaing;
- (void) setDrawingLoadingStage : (int)i;
- (int) getDrawingLoadingStage;
- (void) playFrameShowAnim;
- (void) playFrameHideAnim;
- (void) showIndicationView: (NSString*) str time: (int) time;
- (void) hideIndicationView;
- (CGImageRef) createSignatureImageWithPoints: (NSArray*)points colors: (NSArray*)colors;
+ (NSArray*) getFeedChar;
@end
