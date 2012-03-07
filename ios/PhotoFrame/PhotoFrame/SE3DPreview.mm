//
//  SE3DPreview.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-24.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SE3DPreview.h"
#import "SEResDefine.h"
#import "EAGLView.h"
#import "SS_Model.h"
#import "SS_Scene.h"
@interface SS_3DData : NSObject {
@public
    SS_ModelManager* modelManager;
}
@end
@implementation SS_3DData

- (id) init
{
    self = [super init];
    if(self)
    {
        modelManager = new SS_ModelManager;
        modelManager->loadModel("photoframe.cbf");
        //modelManager->loadModel("MyFrame.cbf");
    }
    return self;
}
- (void) dealloc
{
    delete modelManager;
    [super dealloc];
}
@end
/////////
@implementation SE3DPreview (Private)

-(void) setContext
{
    EAGLContext *aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!aContext)
    {
        //NSLog(@"## use openGL 1.1 ###");
        //aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES1];
        NSLog(@"Failed to create ES context");
        return;
    }
    
	self.context = aContext;
	[aContext release];
	
    [glView setContext:context];
    [glView setFramebuffer];
    animationFrameInterval = 1;
    self->displayLink = nil;
    
}

@end
////////////
@implementation SE3DPreview
@synthesize mResLoader;
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
@synthesize context;
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
        CGRect bounds = CGRectMake(0, 0, frame.size.width, frame.size.height);
        glView = [[EAGLView alloc] initWithFrame:bounds];
        [self addSubview:glView];
        [glView release];
        data3D = [[SS_3DData alloc] init];
        [self setContext];
    }
    return self;
}
- (void) dealloc
{
    [data3D release];
    [super dealloc];
}
/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/
- (BOOL)canAdjust
{
    return NO;
}
- (void)relayout
{}
@end
