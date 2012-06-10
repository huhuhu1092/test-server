//
//  EAGLView.m
//  OpenGLES_iPhone
//
//  Created by mmalc Crawford on 11/18/10.
//  Copyright 2010 Apple Inc. All rights reserved.
//

#import "EAGLView.h"

#import <QuartzCore/QuartzCore.h>
#import "SS_Scene.h"
#import "SE_Common.h"
#import "SE_Log.h"
static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}

@interface EAGLView (PrivateMethods)
- (void)createFramebuffer;
- (void)deleteFramebuffer;
@end

@implementation EAGLView

@synthesize context;

// You must implement this method
+ (Class)layerClass
{
    return [CAEAGLLayer class];
}

//The EAGL view is stored in the nib file. When it's unarchived it's sent -initWithCoder:.
- (id)initWithCoder:(NSCoder*)coder
{
    self = [super initWithCoder:coder];
	if (self) {
        CAEAGLLayer *eaglLayer = (CAEAGLLayer *)self.layer;
        
        eaglLayer.opaque = TRUE;
        eaglLayer.drawableProperties = [NSDictionary dictionaryWithObjectsAndKeys:
                                        [NSNumber numberWithBool:FALSE], kEAGLDrawablePropertyRetainedBacking,
                                        kEAGLColorFormatRGBA8, kEAGLDrawablePropertyColorFormat,
                                        nil];
    }
    
    return self;
}

- (void)dealloc
{
    [self deleteFramebuffer];    
    [context release];
    
    [super dealloc];
}

- (void)setContext:(EAGLContext *)newContext
{
    if (context != newContext) {
        [self deleteFramebuffer];
        
        [context release];
        context = [newContext retain];
        
        [EAGLContext setCurrentContext:nil];
    }
}

- (void)createFramebuffer
{
    if (context && !defaultFramebuffer) {
        BOOL ret = [EAGLContext setCurrentContext:context];
        SE_ASSERT(ret == YES);
        // Create default framebuffer object.
        glGenFramebuffers(1, &defaultFramebuffer);
        checkGLError();
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFramebuffer);
        checkGLError();
        // Create color render buffer and allocate backing store.
        glGenRenderbuffers(1, &colorRenderbuffer);
        checkGLError();
        glBindRenderbuffer(GL_RENDERBUFFER, colorRenderbuffer);
        checkGLError();
        [context renderbufferStorage:GL_RENDERBUFFER fromDrawable:(CAEAGLLayer *)self.layer];
        //glRenderbufferStorageMultisampleAPPLE(GL_RENDERBUFFER, 4, GL_RGBA8_OES, 768, 1024);
        glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_WIDTH, &framebufferWidth);
        checkGLError();
        glGetRenderbufferParameteriv(GL_RENDERBUFFER, GL_RENDERBUFFER_HEIGHT, &framebufferHeight);
        checkGLError();
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_RENDERBUFFER, colorRenderbuffer);
        checkGLError();
        
        //create depth buffer
        glGenRenderbuffers(1, &depthBuffer);
        checkGLError();
        glBindRenderbuffer(GL_RENDERBUFFER, depthBuffer);
        checkGLError();
        glRenderbufferStorage(GL_RENDERBUFFER, GL_DEPTH_COMPONENT16, framebufferWidth, framebufferHeight);
        checkGLError();
        glFramebufferRenderbuffer(GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, depthBuffer);
        checkGLError();
        if (glCheckFramebufferStatus(GL_FRAMEBUFFER) != GL_FRAMEBUFFER_COMPLETE)
            NSLog(@"Failed to make complete framebuffer object %x", glCheckFramebufferStatus(GL_FRAMEBUFFER));
        checkGLError();
        const GLubyte* vender = glGetString(GL_VENDOR);
        checkGLError();
        const GLubyte* version = glGetString(GL_VERSION);
        checkGLError();
        const GLubyte* renderStr = glGetString(GL_RENDERER);
        checkGLError();
        const GLubyte* ext = glGetString(GL_EXTENSIONS);
        checkGLError();
        NSLog(@"vender = %s\n", vender);
        NSLog(@"version = %s\n", version);
        NSLog(@"render = %s \n", renderStr);
        NSLog(@"ext = %s \n", ext);
    }
}

- (void)deleteFramebuffer
{
    if (context) {
        BOOL ret = [EAGLContext setCurrentContext:context];
        SE_ASSERT(ret == YES);
        if (defaultFramebuffer) {
            glDeleteFramebuffers(1, &defaultFramebuffer);
            defaultFramebuffer = 0;
        }
        
        if (colorRenderbuffer) {
            glDeleteRenderbuffers(1, &colorRenderbuffer);
            colorRenderbuffer = 0;
        }
        if(depthBuffer)
        {
            glDeleteRenderbuffers(1, &depthBuffer);
            depthBuffer = 0;
        }
    }
}

- (void)setFramebuffer
{
    if (context) {
        BOOL ret = [EAGLContext setCurrentContext:context];
        SE_ASSERT(ret == YES);
        if (!defaultFramebuffer)
            [self createFramebuffer];
        checkGLError();
        glBindFramebuffer(GL_FRAMEBUFFER, defaultFramebuffer);
        checkGLError();
        glViewport(0, 0, framebufferWidth, framebufferHeight);
        checkGLError();
    }
}

- (BOOL)presentFramebuffer
{
    BOOL success = FALSE;
    
    if (context) {
        BOOL ret = [EAGLContext setCurrentContext:context];
        SE_ASSERT(ret == YES);
        glBindRenderbuffer(GL_RENDERBUFFER, colorRenderbuffer);
        checkGLError();
        success = [context presentRenderbuffer:GL_RENDERBUFFER];
    }
    
    return success;
}

- (void)layoutSubviews
{
    // The framebuffer will be re-created at the beginning of the next setFramebuffer method call.
    [self deleteFramebuffer];
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    [super touchesBegan:touches withEvent:event];
    NSLog(@" touch begin ");
    
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    [super touchesMoved:touches withEvent:event];
    NSLog(@"touch move");
    CGPoint loc = [[touches anyObject] locationInView:self];
    CGPoint prevLoc = [[touches anyObject] previousLocationInView:self];
    NSLog(@"delta = %f", loc.y - prevLoc.y);
    //scene->moveCamera(loc.x - prevLoc.x, loc.y - prevLoc.y);
}
- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    [super touchesEnded:touches withEvent:event];
    NSLog(@"touch end");
}
- (void)touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    [super touchesCancelled:touches withEvent:event];
    NSLog(@"touch cancel");
}
- (void)setScene: (SE_Scene*)s
{
    self->scene = s;    
}
@end
