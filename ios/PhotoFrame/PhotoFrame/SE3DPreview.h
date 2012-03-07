//
//  SE3DPreview.h
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-24.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "SEProtocalDefine.h"
#import <OpenGLES/EAGL.h>

#import <OpenGLES/ES1/gl.h>
#import <OpenGLES/ES1/glext.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>
@class EAGLView;
@class SS_3DScene;
@class SEResLoader;
@class SS_3DData;
@interface SE3DPreview : UIView <SEAdjustContentView>
{
@private
    EAGLContext *context;
    EAGLView* glView;
    NSInteger animationFrameInterval;
    CADisplayLink *displayLink;
    BOOL startDraw;
    SS_3DScene* scene;
    BOOL mStartMoveCamera;
    int mViewPortWidth;
    int mViewPortHeight;
    SEResLoader* mResLoader;
    SS_3DData* data3D;
}
@property (nonatomic, retain) EAGLContext* context;
@property (nonatomic, assign) int mViewPortWidth;
@property (nonatomic, assign) int mViewPortHeight;
@property (nonatomic, assign) SEResLoader* mResLoader;
@end
