//
//  EAGLView.h
//  OpenGLES_iPhone
//
//  Created by mmalc Crawford on 11/18/10.
//  Copyright 2010 Apple Inc. All rights reserved.
//

#import <UIKit/UIKit.h>

//#import <OpenGLES/ES1/gl.h>
//#import <OpenGLES/ES1/glext.h>
#import <OpenGLES/EAGL.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>

@class EAGLContext;
class SE_Scene;
// This class wraps the CAEAGLLayer from CoreAnimation into a convenient UIView subclass.
// The view content is basically an EAGL surface you render your OpenGL scene into.
// Note that setting the view non-opaque will only work if the EAGL surface has an alpha channel.
@interface EAGLView : UIView {
    // The pixel dimensions of the CAEAGLLayer.
@public
    GLint framebufferWidth;
    GLint framebufferHeight;
    SE_Scene* scene;
@private    
    // The OpenGL ES names for the framebuffer and renderbuffer used to render to this view.
    GLuint defaultFramebuffer, colorRenderbuffer;
    GLuint depthBuffer;
    
    GLuint msaaFramebuffer, msaaRenderbuffer, msaaDepthbuffer;
    float prevY;
}

@property (nonatomic, retain) EAGLContext *context;

- (void)setFramebuffer;
- (BOOL)presentFramebuffer;
- (void) setScene: (SE_Scene*)scene;
@end
