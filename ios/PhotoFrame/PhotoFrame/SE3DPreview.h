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
@class EAGLView;
@class SE_3DScene;
@class SEResLoader;
@class SE_3DData;
@class SEViewNavigator;
@interface SE3DPreview : SEFixedView
{
@private
    EAGLContext *context;
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
    //SE_3DData* data3D;
}
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, retain) EAGLContext* context;
@property (nonatomic, assign) int mViewPortWidth;
@property (nonatomic, assign) int mViewPortHeight;
@property (nonatomic, assign) SEResLoader* mResLoader;
- (void)startDraw;
- (void)initData;
@end
