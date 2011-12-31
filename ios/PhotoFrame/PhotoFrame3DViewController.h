//
//  PhotoFrame3DViewController.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <OpenGLES/EAGL.h>

//#import <OpenGLES/ES1/gl.h>
//#import <OpenGLES/ES1/glext.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>
@class EAGLView;
@class SS_3DScene;
@interface PhotoFrame3DViewController : UIViewController
{
    EAGLContext *context;
    EAGLView* glView;
    NSInteger animationFrameInterval;
    CADisplayLink *displayLink;
    BOOL startDraw;
    id appDelegate;
    SS_3DScene* scene;
    BOOL mStartMoveCamera;
}
@property (nonatomic, retain)EAGLContext* context;
@property (nonatomic, retain) IBOutlet EAGLView* glView;
@property (nonatomic, retain) id appDelegate;
- (IBAction)addx:(id)sender;
- (IBAction)subx:(id)sender;
- (IBAction)addy:(id)sender;
- (IBAction)suby:(id)sender;
- (IBAction)moveCamera:(id)sender;
@end
