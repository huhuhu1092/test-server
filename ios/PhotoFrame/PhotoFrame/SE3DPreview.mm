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
#import "SE_Log.h"
#import "PainterManager.h"
#import "SelectedImage.h"
#import "SEViewNavigator.h"
#import "SE_TimeProp.h"
#import <QuartzCore/QuartzCore.h>
///////////
static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}

/*
///////////////
@interface SE_3DData : NSObject {
@public
    SS_ModelManager* modelManager;
}
@property (nonatomic, readonly) SS_ModelManager* modelManager;
@end
@implementation SE_3DData
@synthesize modelManager;
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
 */
////////////////////
@interface SE_3DScene : NSObject {
@public
    SE_Scene* scene;
}
- (id) initWithSize: (CGSize) size viewNav:(SEViewNavigator*)viewNav;
@end
@implementation SE_3DScene
- (id) initWithSize: (CGSize) size viewNav:(SEViewNavigator*)viewNav
{
    self = [super init];
    if(self)
    {
        PainterManager* pm = [PainterManager painterManager];
        scene = new SE_Scene((SS_ModelManager*)[pm modelManager],viewNav, size.width, size.height, 10);
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
/*
- (id) retain
{
    int count = [self retainCount];
    return [super retain];
}
*/
- (void)stopDraw
{
    if (startDraw) {
        [self->displayLink invalidate];
        self->displayLink = nil;
        startDraw = FALSE;
    }
}
- (void)removeFromSuperview
{
    [self stopDraw];
    [super removeFromSuperview];
}
@end
////////////
@implementation SE3DPreview
@synthesize mResLoader;
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
@synthesize context;
@synthesize mViewNav;
- (void)tapHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"tap");
    VIEW_SEQ_TYPE seqType = [mViewNav popViewSeqType];
    [mViewNav moveToView:seqType :MAIN_DISPLAY hasAnimation:YES isPush:NO];
    /*
    if(scene)
    {
        mStartMoveCamera = YES;
        scene->scene->incTrackPointIndex();
    }
     */
}
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
        CGRect bounds = CGRectMake(0, 0, frame.size.width, frame.size.height);
        glView = [[EAGLView alloc] initWithFrame:bounds];
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
        [glView addGestureRecognizer:ges];
        [ges release];
        [self addSubview:glView];
        [glView release];
        
        UIImageView* testImage = [[UIImageView alloc] initWithFrame: CGRectMake(0, 0, 128, 128)];
        [self addSubview:testImage];
        testImage.tag = 1111;
        [testImage release];
        //data3D = [[SE_3DData alloc] init];
    }
    return self;
}
- (void) dealloc
{
    scene->scene->removeAllTexture();
    scene->scene->removeAllShaderFromGL();
    scene->scene->removeAllMeshVBO();
    [scene release];
    [context release];
    //[data3D release];
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
- (void)initData
{
    EAGLContext *aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!aContext) {
        NSLog(@"## can not initial opengl es 2.0 context ###");
        return;
    }
        
	self.context = aContext;
	[aContext release];
	
    [(EAGLView *)glView setContext:context];
    [(EAGLView *)glView setFramebuffer];
    
    animationFrameInterval = 1;
    self->displayLink = nil;

}
- (void) setStartMove
{    
    scene->scene->startCameraMove();
}
- (NSString*) getPointEdgeFilePath
{
    NSArray* dirPath = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* dir = [dirPath objectAtIndex:0];
    return [dir stringByAppendingString:@"animation_path.txt"];
}
- (BOOL)whitespaceLine: (NSString*)line
{
    NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
    NSUInteger i;
    for(i = 0 ; i < [line length]; i++)
    {
        unichar c = [line characterAtIndex:i];
        BOOL b = [cs characterIsMember:c];
        if(!b)
            return false;
    }
    return true;
}

- (void) loadPointEdgeData
{
    NSString* filePath = [self getPointEdgeFilePath];
    NSError* error = nil;
    NSString* str = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
    if(str == nil)
    {
        filePath = [[NSBundle mainBundle] pathForResource:@"animation_path" ofType:@"txt"];
        if(filePath)
        {
            str = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:&error];
        }
    }
    if(str == nil)
        return;
    NSArray* dataLines = [str componentsSeparatedByString:@"\n"];
    NSUInteger i;
    //int paramCount = [dataLines count] - 1;
    //paramArrayFromFile = [NSMutableArray arrayWithCapacity:paramCount];
    //PainterParam** params = (PainterParam**)malloc(sizeof(PainterParam*) * paramCount);
    for(i = 0 ; i < [dataLines count] ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([self whitespaceLine:line])
            continue;
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSUInteger j;
        std::list<std::string> lineData;
        for(j = 0 ; j < [tokens count] ; j++)
        {
            NSString* s = [tokens objectAtIndex:j];
            NSString* tok = [s stringByTrimmingCharactersInSet:cs];
            if(![self whitespaceLine:tok])
            {
                lineData.push_back([tok cStringUsingEncoding:NSASCIIStringEncoding]);
            }
        }
        scene->scene->addPointEdge(lineData);
    }

}
- (void) drawFrame
{
    //debug
    NSThread* currThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currThread == mainThread);
    //end
    [(EAGLView *)glView setFramebuffer];
    if(!scene)
    {
        CGSize s;
        s.width = glView->framebufferWidth;
        s.height = glView->framebufferHeight;
        scene = [[SE_3DScene alloc] initWithSize:s viewNav: mViewNav];
        [self loadPointEdgeData];
        //char* imageName[] = {"IMG_1839.JPG", "IMG_1840.JPG", "IMG_1841.JPG"};
        NSArray* images = [mViewNav getUserImageProperty];
        if(images.count > 0)
        {
            std::list<SE_Scene::PictureID> ins;
            for(int i = 0 ; i < images.count ; i++)
            {
                SelectedImage* si = [images objectAtIndex:i];
                if(si.url != nil)
                {
                    SE_Scene::PictureID p;
                    p.pictureDate = [si.urldate cStringUsingEncoding:NSUTF8StringEncoding];
                    p.pictureName = [si.url cStringUsingEncoding:NSUTF8StringEncoding];
                    p.orientation = [si.orientation intValue];
                    int width = [si.width intValue];
                    int height = [si.height intValue];
                    if(width > height)
                    {
                        if(p.orientation == 0 || p.orientation == 1)
                        {
                            p.photoType = SE_Scene::PHOTOH;
                        }
                        else if(p.orientation == 2 || p.orientation == 3)
                        {
                            p.photoType = SE_Scene::PHOTOV;
                        }
                        else 
                        {
                            p.photoType = SE_Scene::PHOTOH;
                        }
                    }
                    else 
                    {
                        if(p.orientation == 0 || p.orientation == 1)
                        {
                            p.photoType = SE_Scene::PHOTOV;
                        }
                        else if(p.orientation == 2 || p.orientation == 3)
                        {
                            p.photoType = SE_Scene::PHOTOH;
                        }
                        else 
                        {
                            p.photoType = SE_Scene::PHOTOV;
                        }
                    }
                    NSLog(@"## url = %@, date = %@, orient = %d , photo type = %d ##", si.url, si.urldate, p.orientation, p.photoType);
                    UIImageView* testImageView = (UIImageView*)[self viewWithTag:1111];
                    UIImage* img = [mViewNav getThumbnailFromCoreData:si.url urlDate:si.urldate];
                    testImageView.image = img;
                    ins.push_back(p);
                }
            }
            //[mViewNav printThumbnail];
            std::vector<SE_Scene::PictureID> imageNames(ins.size());
            std::copy(ins.begin(), ins.end(), imageNames.begin());
            scene->scene->create(imageNames, imageNames.size());
            //scene->scene->createTrackList();
            [glView setScene:scene->scene];
            [self performSelector:@selector(setStartMove) withObject:nil afterDelay:2];
        }
    }
    checkGLError();
    if(mStart == NO)
    {
        mStart = YES;
        mPrevTime = getCurrentTime();
    }
    NSTimeInterval currTime = getCurrentTime();
    NSTimeInterval delta = getInterval(mPrevTime, currTime);
    mPrevTime = currTime;
    //NSLog(@"## delta = %f ###", delta);
    delta = 0.02;
    scene->scene->updateCameraMove(delta * 1000);
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
        //mStartMoveCamera = YES;
    }
}


@end
