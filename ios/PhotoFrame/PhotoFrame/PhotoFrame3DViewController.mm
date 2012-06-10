//
//  PhotoFrame3DViewController.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-26.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PhotoFrame3DViewController.h"
#import "EAGLView.h"
#import <QuartzCore/QuartzCore.h>
#import "SS_Scene.h"
#import "PainterManager.h"
#import "SE_Common.h"

static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}


////////////////////
@interface SS_3DScene : NSObject {
@public
    SE_Scene* scene;
}
- (id) initWithSize: (CGSize) size;
@end
@implementation SS_3DScene
- (id) initWithSize: (CGSize) size
{
    self = [super init];
    if(self)
    {
        PainterManager* pm = [PainterManager painterManager];
        //scene = new SE_Scene((SS_ModelManager*)[pm modelManager], size.width, size.height, 10);
    }
    return self;
}

- (void) dealloc
{
    delete scene;
    [super dealloc];
}

@end
/////////////

@implementation PhotoFrame3DViewController
@synthesize context;
@synthesize appDelegate;
@synthesize glView;
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)didReceiveMemoryWarning
{
    // Releases the view if it doesn't have a superview.
    [super didReceiveMemoryWarning];
    
    // Release any cached data, images, etc that aren't in use.
}

#pragma mark - View lifecycle

/*
// Implement loadView to create a view hierarchy programmatically, without using a nib.
- (void)loadView
{
}
*/


// Implement viewDidLoad to do additional setup after loading the view, typically from a nib.
- (void)viewDidLoad
{
    [super viewDidLoad];
    EAGLContext *aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!aContext) {
        NSLog(@"## use openGL 1.1 ###");
        aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES1];
    }
    
    if (!aContext)
        NSLog(@"Failed to create ES context");
    else if (![EAGLContext setCurrentContext:aContext])
        NSLog(@"Failed to set ES context current");
    
	self.context = aContext;
	[aContext release];
	
    [(EAGLView *)glView setContext:context];
    [(EAGLView *)glView setFramebuffer];
    
    animationFrameInterval = 1;
    self->displayLink = nil;

}


- (void)viewDidUnload
{
    [super viewDidUnload];
    // Release any retained subviews of the main view.
    // e.g. self.myOutlet = nil;
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    return (interfaceOrientation == UIInterfaceOrientationPortrait);
    //return (interfaceOrientation == UIInterfaceOrientationLandscapeLeft) || (interfaceOrientation == UIInterfaceOrientationLandscapeRight);
}

- (void) drawFrame
{
    [(EAGLView *)glView setFramebuffer];
    if(!scene)
    {
        CGSize s;
        s.width = glView->framebufferWidth;
        s.height = glView->framebufferHeight;
        scene = [[SS_3DScene alloc] initWithSize:s];
        const char* ins[] = {"IMG_1839.JPG", "IMG_1840.JPG", "IMG_1841.JPG"};
        std::vector<SE_Scene::PictureID> imageName(3);
        for(int i = 0 ; i < 3 ; i++)
        {
            imageName[i].pictureName = ins[i];
        }
        //scene->scene->create();
        //scene->scene->createPoints();
        scene->scene->create(imageName, 3);
        //scene->scene->createTrackList();
        [glView setScene:scene->scene];
        
    }
    checkGLError();
    //scene->scene->render();
    //scene->scene->renderPoints();
    //if(mStartMoveCamera)
    //    scene->scene->startCameraMove();
    scene->scene->renderScene();
    [(EAGLView *)glView presentFramebuffer];
}
- (void)startDraw
{
    if (!startDraw) {
        CADisplayLink *aDisplayLink = [[UIScreen mainScreen] displayLinkWithTarget:self selector:@selector(drawFrame)];
        [aDisplayLink setFrameInterval:animationFrameInterval];
        [aDisplayLink addToRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
        self->displayLink = aDisplayLink;
        
        startDraw = TRUE;
    }
}
- (void)stopDraw
{
    if (startDraw) {
        [self->displayLink invalidate];
        self->displayLink = nil;
        startDraw = FALSE;
    }
}
- (void)viewWillAppear:(BOOL)animated
{
    [self startDraw];
    
    [super viewWillAppear:animated];
    
    
}

- (void)viewWillDisappear:(BOOL)animated
{
    [self stopDraw];
    
    [super viewWillDisappear:animated];
}


- (void)awakeFromNib
{
    EAGLContext *aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!aContext) {
        NSLog(@"## use openGL 1.1 ###");
        aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES1];
    }
    
    if (!aContext)
        NSLog(@"Failed to create ES context");
    else if (![EAGLContext setCurrentContext:aContext])
        NSLog(@"Failed to set ES context current");
    
	self.context = aContext;
	[aContext release];
	
    [(EAGLView *)self.view setContext:context];
    [(EAGLView *)self.view setFramebuffer];
    
    animationFrameInterval = 1;
    self->displayLink = nil;
}


- (void) dealloc
{
    
    // Tear down context.
    if ([EAGLContext currentContext] == context)
        [EAGLContext setCurrentContext:nil];
    
    [context release];
    
    [super dealloc];

}
- (IBAction)addx:(id)sender
{
    if(scene->scene)
    {
        scene->scene->move(1, 0.3);
    }
}
- (IBAction)subx:(id)sender
{
    if(scene->scene)
    {
        scene->scene->move(1, -0.3);
    }    
}
- (IBAction)addy:(id)sender
{
    if(scene->scene)
    {
        scene->scene->move(2, 0.3);
    }
}
- (IBAction)suby:(id)sender
{
    if(scene->scene)
    {
        scene->scene->move(2, -0.3);
    }
}
- (IBAction)moveCamera:(id)sender
{
    if(scene->scene)
    {
        mStartMoveCamera = YES;
        //scene->scene->mTrackPointsIndex++;
    }
}
@end
