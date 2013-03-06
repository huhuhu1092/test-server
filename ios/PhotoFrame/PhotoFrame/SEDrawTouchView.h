//
//  SEDrawTouchView.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-28.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <sys/time.h>
enum DRAW_TOUCH_STATE {DRAW_TOUCH_BEGIN, DRAW_TOUCH_END, DRAW_TOUCH_MOVE};
struct PointIndex
{
    int pointArrayIndex;
    int pointIndex;
};
@interface SEDrawTouchPoint : NSObject
{
    CGFloat lineWidth;
    CGPoint point;
    DRAW_TOUCH_STATE touchState;
    BOOL interpolate;
    long milliTime;
    long sec;
    long usec;
    UIColor* testColor;
    NSTimeInterval timestamp;
    int speedType;
@public
    float timeRatio;
}
@property (nonatomic, assign) int speedType;
@property (nonatomic, assign) NSTimeInterval timestamp;
@property (nonatomic, retain) UIColor* testColor;
@property (nonatomic, assign) long sec;
@property (nonatomic, assign) long usec;
@property (nonatomic, assign) long milliTime;
@property (nonatomic, assign) BOOL interpolate;
@property (nonatomic, assign) CGFloat lineWidth;
@property (nonatomic, assign) CGPoint point;
@property (nonatomic, assign) DRAW_TOUCH_STATE touchState;
+ (long) timeMilliSecond: (struct timeval) time;
@end
@interface SEDrawTouchView : UIView
{
    NSMutableArray* pointArrayList;
    NSMutableArray* currentPointArray;
    NSMutableArray* currentPointColorArray;
    NSMutableArray* interpolatePointsArrayList;
    NSMutableArray* currentInterpolatePointsArray;
    UIImage* background;
    CGFloat maxLineWidth;
    NSTimer* mTimer;
    struct timeval startTime;
    int mFrameNum;
    int mIndexEndForPhrase1;
    int mLastHandleIndex;
    UIColor* mCurrentColor;
    CGFloat minLineWidth;
    NSTimer* mAnimTimer;
    int mCurrentPointArrayIndex;
    int mCurrentPointIndex;
    float mTotalTime;
    BOOL mAnimDrawStart;
    PointIndex mFirst;
    PointIndex mSecond;
    int mCurrentCalculateIndex;
    int mPrevCalculateIndex;
    id mTarget;
    SEL mAction;
    float mLineWidthRatio;
    CGRect mClipRect;
    UIImage* mClipImage;
    CGRect mOrigRect;
    
    void* mLayerData;
    int mLayerWidth;
    int mLayerHeight;
    int mLayerBytesPerRow;
    CGContextRef mLayerContext;
    CGImageRef mLayerCGImage;
    
    int mDrawLeftTime;
    BOOL mDrawInMainScreen;
@private
    float mTimeScale;
    int mPrevPointIndex;
    NSMutableArray* mClipRectArray;
    BOOL mStartMove;
    BOOL mIsDrawSamplePoint;
    ////
    id mTouchBeganHandler;
    SEL mTouchBeganAction;
    id mTouchMoveHandler;
    SEL mTouchMoveAction;
    id mTouchEndHandler;
    SEL mTouchEndAction;
    id mTimerUpdateHandler;
    SEL mTimerUpdateAction;
    
    id mCanConsumePointHandler;
    SEL mCanConsumPointAction;
    
    float mPointsLength;
    float mStep;
}
@property (nonatomic, assign) CGRect mOrigRect;
@property (nonatomic, retain) UIColor* mCurrentColor;
@property (nonatomic, assign) CGFloat maxLineWidth;
@property (nonatomic, retain) UIImage* background;
@property (nonatomic, assign) BOOL mIsDrawSamplePoint;
@property (nonatomic, assign) BOOL mDrawInMainScreen;
- (void) initData;
// get points data's array
// return value is an array about arrays
- (NSMutableArray*) getAllNormalizedPoints;
- (void) setNormalizePoints: (NSMutableArray*)points;
- (void) setNormalizePointsWithOutDraw: (NSMutableArray*)points;
- (NSMutableArray*) getPointColorArray;
- (void) setPointColorArray: (NSMutableArray*)colorArray;
- (void) clearPoints;
- (void) startAnimDraw;
- (void) setAnimTarget: (id) target action: (SEL) action;
- (void) setBackgroundImage: (UIImage*)image;
- (void) setLineWidthRatio: (float)ratio;
- (float) getLineWidthRatio;
- (float) getPointsLength;
- (CGImageRef) getCGImage;
- (void) resize : (CGRect)r;
- (int) getCurrentFrame;
- (float) getCurrentMoveTime;
- (void) setTouchBeganHandler: (id)target action: (SEL)action;
- (void) setTouchMoveHandler: (id)target action: (SEL)action;
- (void) setTouchEndHandler: (id)target action: (SEL)action;
- (void) setTimerUpdateHandler: (id)target action: (SEL)action;
- (void) setHandlerForConsumePoint: (id)target action: (SEL)action;
- (void) setStep: (float)step;
- (CGImageRef) createCGImageWithFrame: (CGRect)frame points: (NSArray*)points color: (NSArray*)colors lineWidthRatio: (float)lineWidthRatio;
@end
@interface SEDrawTouchPannel : UIView
{
    UIImageView* mBackgroundView;
    SEDrawTouchView* mDrawTouchView;
}
@property (nonatomic, assign) UIColor* mCurrentColor;
@property (nonatomic, assign) CGFloat maxLineWidth;
@property (nonatomic, assign) UIImage* background;
- (void) initData;
// get points data's array
// return value is an array about arrays
- (NSMutableArray*) getAllNormalizedPoints;
- (void) setNormalizePoints: (NSMutableArray*)points;
- (NSMutableArray*) getPointColorArray;
- (void) setPointColorArray: (NSMutableArray*)colorArray;
- (void) clearPoints;
- (void) startAnimDraw;
- (void) setAnimTarget: (id) target action: (SEL) action;
- (void) setBackgroundImage: (UIImage*)image;
- (void) setLineWidthRatio: (float)ratio;
- (float) getLineWidthRatio;
- (void) setIsDrawSamplePoint: (BOOL) b;
- (BOOL) isDrawSamplePoint;
- (CGImageRef) getCGImage;
- (void) setDrawInMainDisplay: (BOOL) b;
- (int) getCurrentFrame;
- (float) getCurrentMoveTime;
- (void) setTouchBeganHandler: (id)target action: (SEL)action;
- (void) setTouchMoveHandler: (id)target action: (SEL)action;
- (void) setTouchEndHandler: (id)target action: (SEL)action;
- (void) setTimerUpdateHandler: (id)target action: (SEL)action;
- (void) setHandlerForConsumePoint: (id)target action: (SEL)action;
- (float) getPointsLength;
- (void) setStep: (float)step;
- (float) getLineWidthRatio;
- (CGImageRef) createCGImageWithFrame: (CGRect)frame points: (NSArray*)points color: (NSArray*)colors lineWidthRatio: (float)lineWidthRatio;
@end
