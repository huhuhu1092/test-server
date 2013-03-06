//
//  SE3DPreview.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-24.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
#import "SEFixedView.h"
#import <OpenGLES/EAGL.h>

#import <OpenGLES/ES1/gl.h>
#import <OpenGLES/ES1/glext.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>
#import "SEWidgetView.h"
@class EAGLView;
@class SE_3DScene;
@class SEResLoader;
@class SE_3DData;
@class SEViewNavigator;
struct DataForNotReady;
@interface SE3DDecorateView : UIView
{
    UIImage* image;
    UIImage* imageBg;
    UIImage* imageMid;
    float mCurrentX;
}
@property (nonatomic, retain) UIImage* imageMid;
@property (nonatomic, retain) UIImage* imageBg;
@property (nonatomic, retain) UIImage* image;
- (void) update: (float) t;
@end
@class SEBackButtonView;
@interface SE3DPreview : SEFixedView
{
@private
    //EAGLContext *context;
    EAGLView* glView;
    NSInteger animationFrameInterval;
    CADisplayLink *displayLink;
    BOOL startDraw;
    SE_3DScene* scene;
    BOOL mStartMoveCamera;
    int mViewPortWidth;
    int mViewPortHeight;
    SEResLoader* mResLoader;
    SEViewNavigator* mViewNav;
    BOOL mStart;
    NSTimeInterval mPrevTime;
    SEPowerView* mPowerView;
    SEDateTimeView* mDateTimeView;
    struct DataForNotReady* mDataForNotReady;//this is the data for drawing when your real data is not ready
    //SE_3DData* data3D;
    //UIView* mDecorateView;
    UIImageView* mDecorateView;
    UIImageView* mSlidView;
    UIImageView* mLeftView;
    UIImage* mScreenImage;
    UIImage* mSceneImage;
    //
    SEBackButtonView* mBackButton;
    CGSize mBackButtonSize;
    BOOL mBackButtonShow;
    BOOL mBackButtonAnimEnd;
    CGAffineTransform mBackButtonBasicTransform;
    NSTimer* mUpdateTimer;
    UIView* mTestView;
    //
    float mTime;
    BOOL mAnimEnd;
}
@property (nonatomic, assign) UIImageView* mLeftView;
@property (nonatomic, retain) UIImage* mSceneImage;
@property (nonatomic, retain) UIImage* mScreenImage;
//@property (nonatomic, readonly) UIImageView* mDecorateView;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
//@property (nonatomic, retain) EAGLContext* context;
@property (nonatomic, assign) int mViewPortWidth;
@property (nonatomic, assign) int mViewPortHeight;
@property (nonatomic, assign) SEResLoader* mResLoader;
- (BOOL) isStartDraw;
- (void)startDraw;
- (void)initData;
- (id) initWithFrame: (CGRect) frame withViewNav: (SEViewNavigator*) viewNav;
- (void) startTransitionAnim;
@end
