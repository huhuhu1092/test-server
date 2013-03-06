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
#import "UserInfo.h"
#import "SEUtil.h"
#import "SS_OpenGL.h"
#import "PhotoFrameAppDelegate.h"
#import <QuartzCore/QuartzCore.h>
///////////
/*
static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}
*/
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
@interface  SEBackButtonView : UIView
{
    UIButton* mBackButton;
    UIImageView* mBackButtonBgView;
}
- (void) setButtonImage: (UIImage*) image highlighted: (UIImage*)imageH;
- (void) setButtonHandler: (id) target action: (SEL)action;
@end 
@implementation SEBackButtonView
- (void) setButtonImage:(UIImage *)image highlighted:(UIImage *)imageH
{
    [mBackButton setBackgroundImage:image forState:UIControlStateNormal];
    [mBackButton setBackgroundImage:imageH forState:UIControlStateHighlighted];
}
- (void) setButtonHandler:(id)target action:(SEL)action
{
    [mBackButton removeTarget:target action:action forControlEvents:UIControlEventTouchUpInside];
    [mBackButton addTarget:target action:action forControlEvents:UIControlEventTouchUpInside];
}

- (void) createChild: (CGRect)frame
{
    mBackButtonBgView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    mBackButtonBgView.image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"DateTimeViewTimeBackground"];
    [self addSubview:mBackButtonBgView];
    [mBackButtonBgView release];
    
    mBackButton = [UIButton buttonWithType:UIButtonTypeCustom];
    mBackButton.frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    [self addSubview:mBackButton];
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild: frame];
    }
    return self;
}

@end
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
    PainterManager* pm = [PainterManager painterManager];
    [pm releaseModelManager];
    [super dealloc];
}

@end
/////////////
@implementation SE3DPreview (Private)
/*
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
 */
/*
- (id) retain
{
    int count = [self retainCount];
    return [super retain];
}
*/
- (void)stopDraw
{
    if (startDraw) 
    {
        /*
        scene->scene->removeAllTexture();
        scene->scene->removeAllShaderFromGL();
        scene->scene->removeAllMeshVBO();
        [scene release];
        scene = nil;
        */
        [self->displayLink invalidate];
        self->displayLink = nil;
        startDraw = FALSE;
    }
}
- (void)MyRemoveFromSuperview
{
    //[self stopDraw];
    [self removeFromSuperview];
    [mViewNav restoreFromPreview3D];
}
@end
////////////
struct DataForNotReady
{
    GLuint texture;
    GLuint program;
};
static CGImageRef createPow2Image(CGImageRef srcImage, CGSize size)
{
    CGContextRef context = MyCreateBitmapContext(size.width, size.height);
    CGSize srcSize = CGSizeMake(CGImageGetWidth(srcImage), CGImageGetHeight(srcImage));
    CGRect rect = CGRectMake((size.width - srcSize.width ) / 2, (size.height - srcSize.height) / 2, srcSize.width, srcSize.height);
    CGContextTranslateCTM(context, 0, size.height);
    CGContextScaleCTM(context, 1, -1);
    CGContextDrawImage(context, rect, srcImage);
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    return newImageRef;
}
static void createTexture(DataForNotReady* data)
{
    if(data->texture == 0)
    {
        glGenTextures(1, &data->texture);
        glBindTexture(GL_TEXTURE_2D, data->texture);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); 
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        //UIImage* image = [UIImage imageNamed:@"thespeedsunLightingMap.png"];
        UIImage* image = nil;
        SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
        if(UIInterfaceOrientationIsPortrait(viewNav.interfaceOrientation))
        {
            image = [UIImage imageNamed:@"3DScreenPortrait1.png"];
        }
        else 
        {
            image = [UIImage imageNamed:@"3DScreen1.png"];
        }
        CGImageRef imageRef2 = [image CGImage];
        CGImageRef imageRef = createPow2Image(imageRef2, CGSizeMake(1024, 1024));
        size_t width = CGImageGetWidth(imageRef);
        size_t height = CGImageGetHeight(imageRef);
        CFDataRef imageData = CGDataProviderCopyData(CGImageGetDataProvider(imageRef));
        const unsigned char* pixels = CFDataGetBytePtr(imageData);
        //unsigned char* newPixels = new unsigned char[width * height * 4];
        
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels);
        CGImageRelease(imageRef);
        CFRelease(imageData);
    }
    else
    {
        glBindTexture(GL_TEXTURE_2D, data->texture);
    }
}
static GLint loadShader(GLenum type, const char* shaderSrc)
{
    GLuint shader;
    GLint compiled;
    shader = glCreateShader(type);
    if(shader == 0)
    {
        return 0;
    }
    glShaderSource(shader, 1, &shaderSrc, 0);
	checkGLError();
    glCompileShader(shader);
	checkGLError();
    glGetShaderiv(shader, GL_COMPILE_STATUS, &compiled);
	checkGLError();
    if(!compiled)
    {
        GLint infoLen = 0;
		glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &infoLen);
		checkGLError();
		if(infoLen > 1)
		{
			char* infoLog = new char[sizeof(char) * infoLen];
			glGetShaderInfoLog(shader, infoLen, 0, infoLog);
			checkGLError();
			LOGI("Error compiling shader: \n%s\n", infoLog);
			delete[] infoLog;
		}
		glDeleteShader(shader);
		checkGLError();
		return 0;
    }
    return shader;
    
}

static void createProgram(DataForNotReady* data)
{
    const char* vertexShaderSrc = "attribute vec3 a_position;\n" 
                               "attribute vec2 a_texcoord;\n"
                               "varying vec2 v_texcoord;\n"
                               "void main()\n"
                               "{\n"
                               "    v_texcoord = a_texcoord;\n"
                               "    gl_Position = vec4(a_position, 1.0);\n"
                               "}\n";
    const char* fragmentShaderSrc = "precision mediump float;\n"
                                 "uniform sampler2D u_tex;\n"
                                 "varying vec2 v_texcoord;\n"
                                 "void main()\n"
                                 "{\n"
                                 "    gl_FragColor = texture2D(u_tex, v_texcoord);\n"
                                 "}\n"
    ;
    if(data->program == 0)
    {
        GLuint vertexShader;
        GLuint fragmentShader;
        GLuint programObject;
        GLint linked;
        vertexShader = ::loadShader(GL_VERTEX_SHADER, vertexShaderSrc);
        if(vertexShader == 0)
        {
            NSLog(@"can not create vertex shader");
            return;
        }
        fragmentShader = ::loadShader(GL_FRAGMENT_SHADER, fragmentShaderSrc);
        if(fragmentShader == 0)
        {
            NSLog(@"can not create fragment shader");
            return ;
        }
        programObject = glCreateProgram();
        if(programObject == 0)
        {
            NSLog(@"can not create program object");
            return ;
        }
        glAttachShader(programObject, vertexShader);
        checkGLError();
        glAttachShader(programObject, fragmentShader);
        checkGLError();
        glLinkProgram(programObject);
        checkGLError();
        glGetProgramiv(programObject, GL_LINK_STATUS, &linked);
        checkGLError();
        if(!linked)
        {
            GLint infoLen = 0;
            glGetProgramiv(programObject, GL_INFO_LOG_LENGTH, &infoLen);
            checkGLError();
            if(infoLen > 1)
            {
                char* infoLog = new char[sizeof(char) * infoLen];
                glGetProgramInfoLog(programObject, infoLen, 0, infoLog);
                checkGLError();
                LOGI("Error linking program: \n%s\n", infoLog);
                delete[] infoLog;
            }
            glDeleteProgram(programObject);
            checkGLError();
            return;
        }
        glDeleteShader(vertexShader);
        checkGLError();
        glDeleteShader(fragmentShader);
        checkGLError();
        data->program = programObject;
    }
    glUseProgram(data->program);
}
static void render(DataForNotReady* data)
{
    createProgram(data);
    float y = ((1024.0 - 768.0) / 2) / 1024.0;
    float vertexArrayLandScape[] = {-1, -1, 0, 0, y,
                           1, -1, 0, 1, y,
                           1, 1, 0, 1 , 1 - y ,
                           -1, 1, 0, 0, 1 - y,
    };
    float vertexArrayPortrait[] = {
        -1, -1, 0, 1, y,
        1, -1, 0, 1, 1 - y,
        1, 1, 0, 0 , 1 - y ,
        -1, 1, 0, 0, y,
    };
    unsigned short indexArray[] = { 0, 2, 3,
                           0, 1, 2
    };
    float* vertexArray = NULL;
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    if(UIInterfaceOrientationIsPortrait(viewNav.interfaceOrientation))
    {
        vertexArray = vertexArrayPortrait;
    }
    else 
    {
        vertexArray = vertexArrayLandScape;
    }
    GLint vertexLoc = glGetAttribLocation(data->program, "a_position"); 
    glVertexAttribPointer(vertexLoc, 3, GL_FLOAT, GL_FALSE, 5 * sizeof(float), vertexArray);
    glEnableVertexAttribArray(vertexLoc);
    
    GLint texCoordLoc = glGetAttribLocation(data->program, "a_texcoord");
    glVertexAttribPointer(texCoordLoc, 2, GL_FLOAT, GL_FALSE, 5 * sizeof(float), vertexArray + 3);
    glEnableVertexAttribArray(texCoordLoc);
    createTexture(data);
    GLint texLoc = glGetUniformLocation(data->program, "u_tex");
    glActiveTexture(GL_TEXTURE1);
    glUniform1i(texLoc, 0);
    
    glDrawElements(GL_TRIANGLES, 6, GL_UNSIGNED_SHORT, indexArray);
}
static void releaseData(DataForNotReady*data)
{
    glDeleteTextures(1, &data->texture);
    glDeleteProgram(data->program);
}
///////////////
@implementation SE3DDecorateView
@synthesize image;
@synthesize imageBg;
@synthesize imageMid;
- (void) dealloc
{
    [imageMid release];
    [imageBg release];
    [image release];
    [super dealloc];
}
- (void) drawRect:(CGRect)rect
{
    float width = self.frame.size.width;
    float height = self.frame.size.height;
    CGContextRef context = UIGraphicsGetCurrentContext();
    
    CGContextSaveGState(context);
    CGContextTranslateCTM(context, width / 2, height / 2);
    CGContextRotateCTM(context, 90.0 * 3.1415926 / 180);
    CGContextTranslateCTM(context, -height / 2, -width/ 2);
    CGContextTranslateCTM(context, height, 0);
    CGContextScaleCTM(context, -1, 1);
    CGContextClipToRect(context, CGRectMake(0, mCurrentX, height, width));
    CGContextDrawImage(context, CGRectMake(0, 0, height, width), [imageBg CGImage]);
    CGContextRestoreGState(context);
    
    CGRect currentRect = CGRectMake(mCurrentX, 0, imageMid.size.width * 2, imageMid.size.height);
    CGRect midRect = CGRectMake(mCurrentX, 0, imageMid.size.width, imageMid.size.height);
    CGContextSaveGState(context);
    CGContextClipToRect(context, currentRect);
    if(mCurrentX < 1024 && mCurrentX > 0)
        CGContextDrawImage(context, rect, [image CGImage]);
    CGContextDrawImage(context, midRect, [imageMid CGImage]);
    CGContextRestoreGState(context);
}
- (void) update: (float) t
{
    mCurrentX = self.frame.size.width - t * self.frame.size.width;
    NSLog(@"current x = %f", mCurrentX);
    [self setNeedsDisplay];
    CGRect rect = CGRectMake(mCurrentX, 0, imageMid.size.width * 2, imageMid.size.height);
    [self setNeedsDisplayInRect:rect];
}
@end
////////////////
@implementation SE3DPreview
@synthesize mResLoader;
@synthesize mViewPortWidth;
@synthesize mViewPortHeight;
//@synthesize context;
@synthesize mViewNav;
@synthesize mLeftView;
@synthesize mScreenImage;
@synthesize mSceneImage;
- (void) timerUpdate: (NSTimer*)timer
{
    if(mAnimEnd)
    {
        [timer invalidate];
        [self startDraw];
        return;
    }
    float t = mTime / 1500;
    if(t > 1)
    {
        t = 1;
        mAnimEnd = YES;
    }
    //[mDecorateView update:t];
    mTime += 20;
    
}
- (void) slideViewAnim
{
    //self.frame = CGRectMake(self.frame.size.width, 0, self.frame.size.width, self.frame.size.height);
    CGRect newRect = CGRectMake(-mSlidView.frame.size.width, 0, mSlidView.frame.size.width, mSlidView.frame.size.height);
    void (^animBlock) (void) = ^{
        mSlidView.frame = newRect;
    };
    void (^animEnd) (BOOL) = ^(BOOL f)
    {
        mAnimEnd = YES;
        [self startDraw];
    };
    float time = (mSlidView.frame.size.width / 1024) * 0.6;
    [UIView animateWithDuration:time delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
};
- (void) startTransitionAnim
{
    /*
    mDecorateView.image = mSceneImage;
    CATransition* t = [CATransition animation];
    t.type = kCATransitionFade;
    t.subtype = kCATransitionFromRight; 
    [mDecorateView.layer addAnimation: t forKey: nil];
     */
    /*
    mDecorateView.imageBg = mScreenImage;
    NSTimer* updateTimer = [NSTimer timerWithTimeInterval:0.02 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:updateTimer forMode:NSDefaultRunLoopMode];
    [mDecorateView setNeedsDisplay];
     */
    mAnimEnd = NO;
    self.frame = CGRectMake(self.frame.size.width + mLeftView.frame.size.width, 0, self.frame.size.width, self.frame.size.height);
    
    CGRect newRect = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height);
    
    mLeftView.frame = CGRectMake(self.frame.size.width, 0, mLeftView.frame.size.width, mLeftView.frame.size.height);
    CGRect leftNewRect = CGRectMake(-mLeftView.frame.size.width, 0, mLeftView.frame.size.width, mLeftView.frame.size.height);
    void (^animBlock) (void) = ^{
        self.frame = newRect;
        //mLeftView.frame = leftNewRect;
    };
    void (^animEnd) (BOOL) = ^(BOOL f)
    {
        [self performSelectorOnMainThread:@selector(slideViewAnim) withObject:nil waitUntilDone:NO];
    };
    [UIView animateWithDuration:1 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock completion:animEnd];
    //////////////////////
    void (^animBlock1) (void) = ^{
        mLeftView.frame = leftNewRect;
    };
    void (^animEnd1) (BOOL) = ^(BOOL f)
    {
        //mAnimEnd = YES;
        //[self startDraw];
        //[self performSelectorOnMainThread:@selector(slideViewAnim) withObject:nil waitUntilDone:NO];
    };
    [UIView animateWithDuration:1 delay: 0 options: UIViewAnimationOptionCurveLinear animations:animBlock1 completion:animEnd1];
}
- (void) realStopDraw
{
    if(mAnimEnd == YES)
    {
        [self stopDraw];
        [self performSelectorOnMainThread:@selector(MyRemoveFromSuperview) withObject:nil waitUntilDone:NO];
    }
}
- (void)tapHandler: (UITapGestureRecognizer*)tap
{
    NSLog(@"tap");
    if(mBackButtonAnimEnd == NO)
        return;
    float dist = 0;
    CGAffineTransform t = mBackButtonBasicTransform;
    if(mBackButtonShow == YES)
    {
        dist = mBackButtonSize.width;
    }
    else
    {
        dist =  -mBackButtonSize.width;
        t = CGAffineTransformTranslate(mBackButtonBasicTransform, mBackButtonSize.width, 0);
    }
    void(^animBlock)(void) = ^(void){
        mBackButton.transform = CGAffineTransformTranslate(t, dist, 0);
    };
    void (^animEnd)(BOOL) = ^(BOOL)
    {
        mBackButtonShow = !mBackButtonShow;
        mBackButtonAnimEnd = YES;
    };
    mBackButtonAnimEnd = NO;
    [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
}
- (CGAffineTransform)createTransform
{
    float w = 1024;
    float h = 768;
    CGAffineTransform t1 = CGAffineTransformMakeTranslation(-h/2, -w/2);
    CGAffineTransform r1 = CGAffineTransformRotate(t1, -90 * 3.1415926 / 180);
    CGAffineTransform t2 = CGAffineTransformTranslate(r1, -h / 2, w / 2);
    return t2;
}
/*
- (UIImage*) rotate90Image: (UIImage*) image
{
    float width = 1024;
    float height = 768;
    UIGraphicsBeginImageContext(CGSizeMake(width, height));
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextTranslateCTM(context, width / 2, height / 2);
    CGContextRotateCTM(context, 3.1415926 / 2);
    CGContextTranslateCTM(context, -height / 2, -width / 2);
    CGContextDrawImage(context, CGRectMake(0, 0, 768, 1024), [image CGImage]);
    
}
 */
- (id) initWithFrame: (CGRect) frame withViewNav: (SEViewNavigator*) viewNav
{
    self = [super initWithFrame:frame];
    if (self) 
    {
        // Initialization code
        self.backgroundColor = [UIColor blueColor];
        mViewNav = viewNav;
        mBackButtonAnimEnd = YES;
        CGRect bounds;
        CGAffineTransform transform;
        if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
        {
            bounds = CGRectMake(0, 0, frame.size.height, frame.size.width);
            transform = [self createTransform];
        }
        else
        {
            bounds = CGRectMake(0, 0, frame.size.width, frame.size.height);    
            transform = CGAffineTransformIdentity;
        }
        //CGRectMake(0, 0, frame.size.width, frame.size.height);
        glView = [[EAGLView alloc] initWithFrame:bounds];
        
        NSLog(@"## glview transform = %f, %f, %f, %f, %f, %f ",glView.transform.a, glView.transform.b, glView.transform.c, glView.transform.d, glView.transform.tx, glView.transform.ty);
        glView.transform = transform;
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
        [glView addGestureRecognizer:ges];
        [ges release];
        [self addSubview:glView];
        [glView release];
        /////////////
        
        mDecorateView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 1024, 768)];
        if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
        {
            mDecorateView.image = [UIImage imageNamed:@"3DScreenPortrait1.png"];
            //mDecorateView.transform = CGAffineTransformMakeRotation(3.1415926 / 2);
        }
        else
        {
            mDecorateView.image = [UIImage imageNamed:@"3DScreen1.png"];
        }
        [self addSubview:mDecorateView];
        [mDecorateView release];
        
        /*
        mDecorateView = [[UIView alloc] initWithFrame:bounds];
        mDecorateView.backgroundColor = [UIColor blackColor];
        [self addSubview:mDecorateView];
        [mDecorateView release];
         */
        /////
        UIImage* midImage = [UIImage imageNamed:@"painterbackground003_side.png"];
        float slideViewWidth = 1024;
        mSlidView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, slideViewWidth, midImage.size.height)];
        [self addSubview:mSlidView];
        [mSlidView release];
        mSlidView.image = midImage;
        //self.mSceneImage = [UIImage imageNamed:@"3DScreen1.png"];
        //mDecorateView.image = self.mSceneImage;
        //mDecorateView.imageMid = [UIImage imageNamed:@"painterbackground003_side.png"];
        /////////////////
        UIImageView* testImage = [[UIImageView alloc] initWithFrame: CGRectMake(0, 0, 128, 128)];
        [self addSubview:testImage];
        testImage.tag = 1111;
        [testImage release];
        
        //data3D = [[SE_3DData alloc] init];
        mDataForNotReady = new DataForNotReady;
        mDataForNotReady->program = 0;
        mDataForNotReady->texture = 0;
        
        mTestView = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 200, 200)];
        [self addSubview:mTestView];
        [mTestView release];
        mTestView.hidden = YES;
        mTestView.backgroundColor = [UIColor redColor];
    }
    return self;
}
- (void) dealloc
{
    if(mDataForNotReady)
    {
        releaseData(mDataForNotReady);
        delete mDataForNotReady;
        mDataForNotReady = NULL;
    }
    [mSceneImage release];
    [mScreenImage release];
    if(scene)
    {
        scene->scene->removeAllTexture();
        scene->scene->removeAllShaderFromGL();
        scene->scene->removeAllMeshVBO();
        [scene release];
    }
    /*
    if ([EAGLContext currentContext] == context)
        [EAGLContext setCurrentContext:nil];
     */
    //[context release];
    [mDateTimeView release];
    [mPowerView release];

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
- (CGAffineTransform) createTransform: (float)x width: (float) width height: (float)height
{
    CGAffineTransform translate1 = CGAffineTransformMakeTranslation(x, 0);
    CGAffineTransform translate2 = CGAffineTransformTranslate(translate1, 0, mViewNav.mViewPortHeight);
    
    CGAffineTransform t3 = CGAffineTransformTranslate(translate2, -width / 2, -height / 2);
    CGAffineTransform r1 = CGAffineTransformRotate(t3, -90 * 3.1415926 / 180.0f);
    CGAffineTransform t4 = CGAffineTransformTranslate(r1, width / 2, height / 2);
    return t4;
    
}

- (void) setPowerViewTransform
{
    CGSize s = [mPowerView getHintSize];
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        mPowerView.transform = CGAffineTransformIdentity;
        mPowerView.frame = CGRectMake(0, 0, s.width, s.height);
        float w = s.width;
        float h = s.height;
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(-w / 2, -h / 2);
        CGAffineTransform r1 = CGAffineTransformRotate(t1, -90 * 3.1415926 / 180.0f);
        CGAffineTransform t2 = CGAffineTransformTranslate(r1, w / 2, h / 2);
        CGAffineTransform t4 = CGAffineTransformTranslate(t2, -s.width, 0);
        mPowerView.transform = t4;
    }
    else
    {
        mPowerView.transform = CGAffineTransformIdentity;
        mPowerView.frame = CGRectMake(0, 0, s.width, s.height);
        float x = mViewNav.mViewPortWidth - s.width;
        mPowerView.transform = CGAffineTransformMakeTranslation(x, 0);
    }
}
- (void) setBackbuttonTransform
{
    CGSize s = mBackButtonSize;
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        mBackButton.transform = CGAffineTransformIdentity;
        mBackButton.frame = CGRectMake(0, 0, s.width, s.height);
        float w = s.width;
        float h = s.height;
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(-w / 2, -h / 2);
        CGAffineTransform r1 = CGAffineTransformRotate(t1, -90 * 3.1415926 / 180.0f);
        CGAffineTransform t2 = CGAffineTransformTranslate(r1, w / 2, h / 2);
        CGAffineTransform t4 = CGAffineTransformTranslate(t2, -s.width, 0);
        CGAffineTransform t5 = CGAffineTransformTranslate(t4, 0, mViewNav.mViewPortWidth - s.height);
        mBackButtonBasicTransform = t5;
        if(mBackButtonShow)
        {
            mBackButton.transform = t5;
        }
        else
        {
            CGAffineTransform t6 = CGAffineTransformTranslate(t5, s.width, 0);
            mBackButton.transform = t6;
        }
    }
    else
    {
        mBackButton.transform = CGAffineTransformIdentity;
        mBackButton.frame = CGRectMake(0, 0, s.width, s.height);
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(mViewNav.mViewPortWidth - s.width, mViewNav.mViewPortHeight - s.height);
        mBackButtonBasicTransform = t1;
        if(mBackButtonShow)
        {
            mBackButton.transform = t1;
            
        }
        else
        {
            CGAffineTransform t2 = CGAffineTransformTranslate(t1, s.width, 0);
            mBackButton.transform = t2;
        }
    }
}

- (void) setDateViewTransform
{
    CGSize dateTimeSize = [mDateTimeView getHintSize];
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        float x = mViewNav.mViewPortWidth - dateTimeSize.height;
        CGAffineTransform transform = [self createTransform: x width:dateTimeSize.width height:dateTimeSize.height];
        mDateTimeView.transform = transform;
    }
    else
    {
        float x = mViewNav.mViewPortHeight - dateTimeSize.height;
        mDateTimeView.transform = CGAffineTransformMakeTranslation(0, x);
    }
}
- (void) setWidgetViewData
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([userInfo.showtime boolValue] == NO)
    {
        mDateTimeView.hidden = YES;
    }
    else 
    {
        mDateTimeView.hidden = NO;
    }
    [mDateTimeView setNeedsDisplay];
    if([userInfo.showpowerview boolValue] == YES)
    {
        mPowerView.hidden = NO;
    }
    else
    {
        mPowerView.hidden = YES;
    }
    [mPowerView setNeedsDisplay];
}
- (void) backButtonHandler:(UIButton*)sender
{
    NSLog(@"back button handler\n");
    [self realStopDraw];
}
- (void) createBackButton
{
    UIImage* image = [mViewNav.mResLoader getImage:@"3DBack"];
    UIImage* imageH = [mViewNav.mResLoader getImage:@"3DBackH"];
    mBackButtonSize = CGSizeMake(image.size.width / 2, image.size.height / 2);
    mBackButtonBasicTransform = CGAffineTransformIdentity;
    mBackButtonAnimEnd = YES;
    mBackButtonShow = NO;
    mBackButton = [[SEBackButtonView alloc] initWithFrame:CGRectMake(0, 0, mBackButtonSize.width, mBackButtonSize.height)];
    mBackButton.backgroundColor = [UIColor clearColor];
    [mBackButton setButtonImage:image highlighted:imageH];
    [mBackButton setButtonHandler:self action:@selector(backButtonHandler:)];
    [self addSubview:mBackButton];
    [mBackButton release];
    /*
    mBackButtonBgView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, image.size.width, image.size.height)];
    mBackButtonBgView.image = [mViewNav.mResLoader getImage:@"DateTimeViewTimeBackground"];
    [self addSubview:mBackButtonBgView];
    [mBackButtonBgView release];
    
    mBackButton = [UIButton buttonWithType:UIButtonTypeCustom];
    mBackButton.frame = CGRectMake(0, 0, mBackButtonSize.width, mBackButtonSize.height);
    [self addSubview:mBackButton];
    [mBackButton setBackgroundImage:image forState:UIControlStateNormal];
    [mBackButton setBackgroundImage:imageH forState:UIControlStateHighlighted];
    [mBackButton addTarget:self action:@selector(backButtonHandler:) forControlEvents:UIControlEventTouchUpInside];
    */
}
- (void) loadView
{
    mDateTimeView = [[SEDateTimeView alloc] initWithViewNav: mViewNav];
    mDateTimeView.backgroundColor = [UIColor clearColor];
    [self addSubview:mDateTimeView];
    /*
    mLightView = [[SELightView alloc] initWithViewNav:mViewNav];
    [self addSubview:mLightView];
    mLightView.backgroundColor = [UIColor clearColor];
    */
    mPowerView = [[SEPowerView alloc] initWithViewNav:mViewNav];
    [self addSubview:mPowerView];
    mPowerView.backgroundColor = [UIColor clearColor];
    
    [self createBackButton];
    [self setDateViewTransform];
    [self setPowerViewTransform];
    [self setWidgetViewData];
    [self setBackbuttonTransform];
    //[self setLightViewTransform];
}
- (void)update
{
    [self setDateViewTransform];
    [self setPowerViewTransform];
    [self setWidgetViewData];
    [self setBackbuttonTransform];
    //[self setLightViewTransform];

    /*
    if([userInfo.sleep boolValue] == YES)
    {
        mLightView.hidden = NO;
    }
    else 
    {
        mLightView.hidden = YES;
    }
    [mLightView setNeedsDisplay];
     */
}

- (void)initData
{
    /*
    EAGLContext *aContext = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];
    
    if (!aContext) {
        NSLog(@"## can not initial opengl es 2.0 context ###");
        return;
    }
        
	self.context = aContext;
	[aContext release];
	
    [(EAGLView *)glView setContext:context];
    [(EAGLView *)glView setFramebuffer];
    */
    animationFrameInterval = 1;
    self->displayLink = nil;
    [self loadView];
    
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
- (int) imageCountWithURLNotNil: (NSArray*)array
{
    int count = 0;
    for(int i = 0  ; i < array.count ; i++)
    {
        SelectedImage* si = [array objectAtIndex:i];
        if(si.url != nil)
        {
            count++;
        }
    }
    return count;
}
- (void) drawSceneNotReady
{
    //glClearColor(0, 1.0, 0.0, 1.0);
    //glClear(GL_COLOR_BUFFER_BIT);
    render(mDataForNotReady);
}
- (void) drawFrame
{
    if(mAnimEnd == NO)
        return;
    //debug
    NSThread* currThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currThread == mainThread);
    [glView setFramebuffer];
    //end
    //[(EAGLView *)glView setFramebuffer];
    double allStartTime = getCurrentTime();
    if(!scene)
    {
        CGSize s;
        s.width = glView->framebufferWidth;
        s.height = glView->framebufferHeight;
        scene = [[SE_3DScene alloc] initWithSize:s viewNav: mViewNav];
        [self loadPointEdgeData];

        NSArray* images = [mViewNav getUserImageProperty];
        if([self imageCountWithURLNotNil:images] == 0)
        {
            images = [mViewNav fetchDefaultSelectedImage];
        }
        NSLog(@"image count = %d", images.count);
        if(images.count > 0)
        {
            UserInfo* userInfo = [mViewNav getUserInfo];
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
                    float actualWidth = 0, actualHeight = 0;
                    [SEUtil calculateImageActualSizeWithWidth:width height:height orientation:(UIImageOrientation)[si.orientation intValue] :&actualWidth :&actualHeight];
                    //assert(actualWidth != 0 && actualHeight != 0);
                    if(actualWidth == 0 || actualHeight == 0)
                        continue;
                    if(actualWidth >= actualHeight)
                    {
                        p.photoType = SE_Scene::PHOTOH;
                    }
                    else 
                    {
                        p.photoType = SE_Scene::PHOTOV;
                    }
                    p.width = width;
                    p.height = height;
                    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
                    {
                        if(p.photoType == SE_Scene::PHOTOH && [userInfo.imagesizefilter boolValue] == YES)
                        {
                            continue;
                        }
                    }
                    else
                    {
                        if(p.photoType == SE_Scene::PHOTOV && [userInfo.imagesizefilter boolValue] == YES)
                        {
                            continue;
                        }
                    }
                    NSLog(@"## url = %@, date = %@, orient = %d , photo type = %d ##", si.url, si.urldate, p.orientation, p.photoType);
                    /*
                    UIView* v = [self viewWithTag:1111];
                    if(v == nil)
                    {
                        UIImageView* testImageView = (UIImageView*)[self viewWithTag:1111];
                       [self addSubview:testImageView];
                    }
                     */
                    //UIImage* img = [mViewNav getThumbnailFromCoreData:si.url urlDate:si.urldate];
                    //testImageView.image = img;
                    ins.push_back(p);
                }
            }
            if(ins.size() == 0)
            {
                images = [mViewNav fetchDefaultSelectedImage];
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
                        float actualWidth = 0, actualHeight = 0;
                        [SEUtil calculateImageActualSizeWithWidth:width height:height orientation:(UIImageOrientation)[si.orientation intValue] :&actualWidth :&actualHeight];
                        assert(actualWidth != 0 && actualHeight != 0);
                        if(actualWidth >= actualHeight)
                        {
                            p.photoType = SE_Scene::PHOTOH;
                        }
                        else 
                        {
                            p.photoType = SE_Scene::PHOTOV;
                        }
                        p.width = width;
                        p.height = height;
                        ins.push_back(p);
                    }
                }
            }
            //[mViewNav printThumbnail];
            std::vector<SE_Scene::PictureID> imageNames(ins.size());
            std::copy(ins.begin(), ins.end(), imageNames.begin());
            scene->scene->create(imageNames, imageNames.size());
            if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
            {
                scene->scene->setViewType(SE_Scene::PORTRAIT);
            }
            else 
            {
                scene->scene->setViewType(SE_Scene::LANDSCAPE);    
            }
            //scene->scene->createTrackList();
            [glView setScene:scene->scene];
            [self setStartMove];
            //[self performSelector:@selector(setStartMove) withObject:nil afterDelay:2];
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
    bool oldBDrawing = scene->scene->isDrawing();
    scene->scene->updateState();
    if(mDecorateView)
    {
        [mDecorateView removeFromSuperview];
        mDecorateView = nil;
    }
    if(mSlidView)
    {
        [mSlidView removeFromSuperview];
        mSlidView = nil;
    }
    if(mLeftView)
    {
        [mLeftView removeFromSuperview];
        mLeftView = nil;
    }
    if(scene->scene->isDrawing())
    {
        if(mDataForNotReady != NULL)
        {
            releaseData(mDataForNotReady);
            delete mDataForNotReady;
            mDataForNotReady = NULL;
        }
        if(oldBDrawing == false)
            delta = 0;
        double starttime = getCurrentTime();
        scene->scene->updateCameraMove(delta * 1000);
        double endtime = getCurrentTime();
        double span = getInterval(starttime, endtime);
        //NSLog(@"updateCameraMove = %f", span);
        scene->scene->renderScene();
    }
    else
    {
        [self drawSceneNotReady];    
    }
    [(EAGLView *)glView presentFramebuffer];
    double allEndTime = getCurrentTime();
    double allSpan = getInterval(allStartTime, allEndTime);
    //NSLog(@"all drawFrame = %f", allSpan);
    static CFTimeInterval prevTime = 0;
    CFTimeInterval timeInterval = self->displayLink.timestamp;
    //NSLog(@"default time interval = %f", timeInterval - prevTime);
    double myTimeInterval = timeInterval - prevTime;
    if(myTimeInterval > 0.044)
    {
        //mTestView.hidden = NO;
    }
    else 
    {
        //mTestView.hidden = YES;
    }
    prevTime = timeInterval;
    if(allSpan < 0.03333)
    {
        double waittime = 0.03333 - allSpan;
        //[NSThread sleepForTimeInterval:waittime];
        
    }
}
- (void) drawTimerUpdate: (NSTimer*)timer
{
    NSTimeInterval t = timer.timeInterval;
    NSLog(@"time interval = %f", t);
    [self drawFrame];
}
- (void)startDraw
{
    if (!startDraw)
    {

        CADisplayLink *aDisplayLink = [[UIScreen mainScreen] displayLinkWithTarget:self selector:@selector(drawFrame)];
        animationFrameInterval = 1;
        [aDisplayLink setFrameInterval:animationFrameInterval];
        CFTimeInterval timeInterval = aDisplayLink.timestamp;
        NSLog(@"default time interval = %f", timeInterval);
        [aDisplayLink addToRunLoop:[NSRunLoop currentRunLoop] forMode:NSDefaultRunLoopMode];
        self->displayLink = aDisplayLink;
    
        /*
        mUpdateTimer = [NSTimer timerWithTimeInterval:0.033 target:self selector:@selector(drawTimerUpdate:) userInfo:nil repeats:YES];
        [[NSRunLoop currentRunLoop] addTimer:mUpdateTimer forMode:NSDefaultRunLoopMode];
        */
        
        
        
        startDraw = TRUE;
    }
}

- (BOOL) isStartDraw
{
    return startDraw;
}
@end
