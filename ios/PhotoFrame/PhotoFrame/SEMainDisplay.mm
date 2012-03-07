//
//  SEMainDisplay.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-15.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEMainDisplay.h"
#import "SEResDefine.h"
#import "PHImageView.h"
#import "SEUtil.h"
struct ToolBarButtonIcon
{
    TOOLBAR_BUTTON_TYPE toolBarButtonType;
    NSString* iconKey;
};
ToolBarButtonIcon gToolbarIcons[] = {
    {OPTION, @"ToolBarOptionIcon"},
    {PLAY_PAUSE, @"ToolBarPlayIcon"},
    {PREVIEW, @"ToolBarPreviewIcon"},
    {MUSIC_SELECT, @"ToolBarMusicSelectIcon"}, 
    {IMAGE_SELECT, @"ToolBarImageSelectIcon"},
    {MUSIC_IMAGE_LIST, @"ToolBarMusicImageListIcon"}
};
static NSString* getIconKey(TOOLBAR_BUTTON_TYPE type)
{
    int count = sizeof(gToolbarIcons)/ sizeof(ToolBarButtonIcon);
    for(int i = 0 ; i < count ; i++)
    {
        if(gToolbarIcons[i].toolBarButtonType == type)
            return gToolbarIcons[i].iconKey;
    }
    return nil;
}
////////////
@implementation SEImageRect
@synthesize startx;
@synthesize starty;
@synthesize endx;
@synthesize endy;
@synthesize block;
@end
//////////////
@implementation SEDateTimeView
- (void) addImageRect:(SEImageRect*)inputImageRect withBlock: (int) block
{
    BOOL needAdd = YES;
    for(SEImageRect* imageRect in mImageRectArray)
    {
        if(imageRect.block == block && imageRect.startx == inputImageRect.startx)
        {
            assert(imageRect.endx == inputImageRect.endx);
            imageRect.endy = inputImageRect.endy;
            needAdd = NO;
            break;
        }
    }
    if(needAdd)
    {
        [mImageRectArray addObject:inputImageRect];
    }
    
};
- (void) resolveDateTimeInfo
{
    UIImageOrientation ot = mDataImage.imageOrientation;
    NSLog(@"ot = %d", ot);
    CGImageRef imageRef = [mDataImage CGImage];
    size_t h = CGImageGetHeight(imageRef);
    size_t w = CGImageGetWidth(imageRef);
    size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
    size_t bpp = CGImageGetBitsPerPixel(imageRef);
    CGBitmapInfo bmpInfo = CGImageGetBitmapInfo(imageRef);
    switch(bmpInfo & kCGBitmapAlphaInfoMask)
    {
        case kCGImageAlphaPremultipliedFirst:
        case kCGImageAlphaFirst:
        case kCGImageAlphaNoneSkipFirst:
            NSLog(@"BGRA\n");
            break;
        default:
            NSLog(@"RGBA\n");
            break;
    }

    if(bpp != 24 && bpp != 32)
        return;
    CGDataProviderRef srcDataProvider = CGImageGetDataProvider(imageRef);
    CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    const UInt8* data = CFDataGetBytePtr(srcData);
    BOOL leftBarrier = NO;
    int block = 0;
    for(size_t y = 0 ; y < h ; y++)
    {
        const UInt8* bytes = data + y * bytesPerRow;
        int startx = 0, endx = 0;
        for(size_t x = 0 ; x < w ; x++)
        {
            int r, g, b, a;
            if(bpp == 24)
            {
                r = bytes[3 * x];
                g = bytes[3 * x + 1];
                b = bytes[3 * x + 2];
                
            }
            else
            {
                /*
                r = bytes[4 * x];
                g = bytes[4 * x + 1];
                b  = bytes[4 * x + 2];
                a = bytes[4 * x + 3];
                 */
                
                a = bytes[4 * x];
                b = bytes[4 * x + 1];
                g = bytes[4 * x + 2];
                r = bytes[4 * x + 3];
                
            }
            //NSLog(@"%d, %d, %d, %d ", r, g, b, a);
            if(r == 255 && g == 0 && b == 0)
            {
                leftBarrier = YES;
                startx = x;
            }
            else if(r == 0 & g == 255 && b ==0)
            {
                if(leftBarrier)
                {
                    endx = x;
                    leftBarrier = NO;
                    SEImageRect* imageRect = [[SEImageRect alloc] init];
                    imageRect.startx = startx;
                    imageRect.endx = endx;
                    imageRect.starty = y;
                    imageRect.endy = y;
                    [imageRect autorelease];
                    [self addImageRect:imageRect withBlock:block];
                }
            }
            else if(r == 0 && g == 0 && b == 255)
            {
                block++;
            }
        }
    }
    CFRelease(srcData);
}
- (void) printImageRect
{
    for(SEImageRect* imageRect in mImageRectArray)
    {
        NSLog(@"image rect starx = %d, starty = %d, endx = %d, endy = %d", imageRect.startx, imageRect.starty, imageRect.endx, imageRect.endy);
    }
}
- (id) initWithFrame:(CGRect)frame withResLoader:(SEResLoader*) resLoader
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mResLoader = resLoader;
        mImageRectArray = [NSMutableArray array];
        [mImageRectArray retain];
        mDataImage = [mResLoader getImage:@"DateTimeDataImage"];
        [self resolveDateTimeInfo];
        //debug
        //[self printImageRect];
        //end
    }
    return self;
}
- (void)dealloc
{
    [mImageRectArray release];
    [super dealloc];
}
@end
////////////
@implementation SEMainDisplayToolBarView
-(TOOLBAR_BUTTON_TYPE) getToolBarButtonType: (id) sender
{
    for(int i = 0 ; i < TOOLBAR_BUTTON_NUM ; i++)
    {
        if(mButtons[i] == sender)
            return (TOOLBAR_BUTTON_TYPE)i;
    }
    return INVALID_TOOLBAR_BUTTON;
}
- (void)setClickHandleTarget:(id)target withAction:(SEL)action
{
    mToolBarButtonHandleTarget = target;
    mToolBarButtonHandleAction = action;
}
- (void) buttonClickHandler: (id)sender
{
    NSLog(@"click\n");
    
    TOOLBAR_BUTTON_TYPE type = [self getToolBarButtonType:sender];
    [mToolBarButtonHandleTarget performSelector:mToolBarButtonHandleAction withObject:(id)&type];

}
- (id) initWithFrame:(CGRect)frame withResLoader:(SEResLoader*) resLoader
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mResLoader = resLoader;
        float startx = 0;
        for(int i = 0 ; i < TOOLBAR_BUTTON_NUM ; i++)
        {
            NSString* iconKey = getIconKey((TOOLBAR_BUTTON_TYPE)i);
            UIImage* icon = [mResLoader getImage:iconKey];
            CGRect frame = CGRectMake(startx, 0, icon.size.width, icon.size.height);
            mButtons[i] = [[UIButton alloc] initWithFrame:frame];
            //mButtons[i].imageView.image = icon;
            [mButtons[i] setBackgroundImage:icon forState:UIControlStateNormal];
            [mButtons[i] addTarget:self action:@selector(buttonClickHandler:) forControlEvents:UIControlEventTouchUpInside];
            startx += icon.size.width;
            [self addSubview:mButtons[i]];
        }
    }
    return self;
}
- (void)dealloc
{
    for(int i = 0 ; i < TOOLBAR_BUTTON_NUM ; i++)
    {
        [mButtons[i] release];
    }
    [super dealloc];
}

@end
////////////////////////
@implementation SEMainDisplay

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}
- (id) initWithResLoader: (SEResLoader*)resLoader withFrame:(CGRect)r
{
    self = [super initWithFrame:r];
    if(self)
    {
        mFrame = r;
        mResLoader =resLoader;
        mToolBarHidden = NO;
    }
    return self;
}
- (void) setToolBarButtonHandleTarget: (id) target withAction:(SEL)action
{
    [mToolBarView setClickHandleTarget:target withAction:action];
}
- (void) setTapGestureToMainDisplay
{
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
    [mMainDispalyImageView addGestureRecognizer:ges];
    [ges release];
}
- (void) createDateTimeView: (CGRect) frame
{
    mDateTimeView = [[SEDateTimeView alloc] initWithFrame:frame withResLoader:mResLoader];
    [self addSubview:mDateTimeView];
}
- (void)loadView
{
    CGRect frame = CGRectMake(0, 0, mFrame.size.width, mFrame.size.height);
    mMainDispalyImageView = [[PHImageView alloc] initWithFrame:frame];
    mMainDispalyImageView.tag = 101;
    mMainDispalyImageView.userInteractionEnabled = YES;
    mMainDispalyImageView.image = [mResLoader getImage:@"MainDisplayBg"];
    [self setTapGestureToMainDisplay];
    UIImage* toolBarBg = [mResLoader getImage:@"MainDisplayToolBarBg"];
    CGSize toolBarSize = toolBarBg.size;
    frame = CGRectMake(0, mFrame.size.height - toolBarSize.height, toolBarSize.width, toolBarSize.height);
    mToolBarView = [[SEMainDisplayToolBarView alloc] initWithFrame:frame withResLoader:mResLoader];
    mToolBarView.hidden = NO;
    mToolBarView.image = toolBarBg;
    mToolBarView.userInteractionEnabled = YES;
    [self addSubview:mMainDispalyImageView];
    [self addSubview:mToolBarView];
    [self createDateTimeView:CGRectMake(20, 300, 500, 300)];
}
- (void) dealloc
{
    [mDateTimeView release];
    [mToolBarView release];
    [mMainDispalyImageView release];
    [super dealloc];
}
- (void)tapHandler: (UITapGestureRecognizer*)tapGes
{
    UIView* tapView = tapGes.view;
    if(tapView == mMainDispalyImageView)
    {
        if(mToolBarHidden == YES)
        {
            mToolBarView.hidden = NO;
            CGPoint p = mToolBarView.center;
            p.y -= mToolBarView.frame.size.height;
            void(^animBlock)(void) = ^(void){
                mToolBarView.center = p;
            };
            void (^animEnd)(BOOL) = ^(BOOL)
            {
                mToolBarHidden = NO;
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
        }
        else
        {
            CGPoint p = mToolBarView.center;
            p.y += mToolBarView.frame.size.height;
            void (^animBlock)(void) = ^{
                mToolBarView.center = p;
            };
            void (^animEnd)(BOOL) = ^(BOOL){
                mToolBarHidden = YES;
                mToolBarView.hidden = YES;
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
        }
    }
}
- (BOOL) canAdjust
{
    return NO;
}
- (void)relayout
{}

@end
