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
//////////////
const static int DATE_TIME_SPACING = 10;
/////////////////////
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
ToolBarButtonIcon gToolbarIconsHighlight[] = {
    {OPTION, @"ToolBarOptionIconHighlight"},
    {PLAY_PAUSE, @"ToolBarPlayIconHighlight"},
    {PREVIEW, @"ToolBarPreviewIconHighlight"},
    {MUSIC_SELECT, @"ToolBarMusicSelectIconHighlight"}, 
    {IMAGE_SELECT, @"ToolBarImageSelectIconHighlight"},
    {MUSIC_IMAGE_LIST, @"ToolBarMusicImageListIconHighlight"}
};
// 0: normal
// 1: highlight
static NSString* getIconKey(TOOLBAR_BUTTON_TYPE type, int state)
{
    int count = sizeof(gToolbarIcons)/ sizeof(ToolBarButtonIcon);
    if(state == 0)
    {
        for(int i = 0 ; i < count ; i++)
        {
            if(gToolbarIcons[i].toolBarButtonType == type)
                return gToolbarIcons[i].iconKey;
        }
    }
    else if(state == 1)
    {
        for(int i = 0 ; i < count ; i++)
        {
            if(gToolbarIconsHighlight[i].toolBarButtonType == type)
                return gToolbarIconsHighlight[i].iconKey;
        }        
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

@interface SEDateTimeView (Private) 
- (id) initWithFrame:(CGRect)frame resLoader: (SEResLoader*)resLoader;
- (CGRect) calculateDateViewFrame: (CGFloat) bottom;
@end
@implementation SEDateTimeView (Private)
- (CGRect) calculateDateViewFrame: (CGFloat) bottom
{
    UIImage* timeBackground = [mResLoader getImage:@"DateTimeViewTimeBackground"];
    UIImage* dateBackground = [mResLoader getImage:@"DateTimeViewDateBackground"];
    CGFloat w = timeBackground.size.width ;
    CGFloat h = dateBackground.size.height + timeBackground.size.height + DATE_TIME_SPACING;
    CGRect frame = CGRectMake(0, bottom - h, w, h);
    return frame;
    //UIImage* amImage = [mFontLoader getImage:@"am" style:FONT_NORMALSTYLE size:FONT_BIG_SIZE];
    //UIImage* colonImage = [mFontLoader getImage:@":" style:<#(enum FONT_STYLE)#> size:<#(enum FONT_SIZE)#>
}
- (void) timerUpdate: (NSTimer*)timer
{
    if(mDisplayMidPoint)
        mDisplayMidPoint = NO;
    else {
        mDisplayMidPoint = YES;
    }
    [self setNeedsDisplay];
}
- (id) initWithFrame:(CGRect)frame resLoader: (SEResLoader*)resLoader
{
    self = [super initWithFrame:frame];
    if(self)
    {
        mFontLoader = [[SEFontLoader alloc] init];
        mResLoader = resLoader;
        mImageRectArray = [NSMutableArray array];
        [mImageRectArray retain];
        mFontStyle = FONT_NORMALSTYLE;
        frame = [self calculateDateViewFrame: frame.origin.y];
        self.frame = frame;
        //mTimeTimer = [[NSTimer timerWithTimeInterval:1 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES] retain];
        mDisplayMidPoint = YES;
        //[[NSRunLoop currentRunLoop] addTimer:mTimeTimer forMode:NSDefaultRunLoopMode];
    }
    return self;
}
    

@end
@implementation SEDateTimeView
@synthesize mResLoader;
@synthesize mFontStyle;
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
/////////////////////////////////

- (void) drawString: (NSString*) str style: (enum FONT_STYLE) fontStyle size : (enum FONT_SIZE) fontSize newLine: (BOOL) bNewLine x: (CGFloat*)startx y: (CGFloat*) starty
{
    CGFloat height = 0;
    for(int i = 0 ; i < str.length ; i++)
    {
        NSRange r;
        r.location = i;
        r.length = 1;
        NSString* numStr = [str substringWithRange:r];
        UIImage* image = [mFontLoader getImage:numStr style:fontStyle size:fontSize];
        [image drawInRect:CGRectMake(*startx, *starty, image.size.width, image.size.height)];
        *startx += image.size.width;
        height = image.size.height;
    } 
    if(bNewLine)
        *starty += height;
}
- (void) drawNumber: (int) num style : (enum FONT_STYLE) fontStyle size : (enum FONT_SIZE) fontSize newLine: (BOOL)bNewLine x : (CGFloat*)startx y : (CGFloat*)starty
{
    NSString* str = nil;
    if(num < 10)
    {
        str = [NSString stringWithFormat:@"%d%d" , 0, num];
    }
    else 
    {
        str = [NSString stringWithFormat:@"%d", num];
    }
    [self drawString:str style:fontStyle size:fontSize newLine:bNewLine x:startx y:starty];
}

- (void)drawRect:(CGRect)rect
{
    if(mTimeBgImage == nil)
    {
        mTimeBgImage = [mResLoader getImage:@"DateTimeViewTimeBackground"];
    }
    CGFloat timeBgWidth = mTimeBgImage.size.width;
    CGFloat timeBgHeight = mTimeBgImage.size.height;
    [mTimeBgImage drawInRect:CGRectMake(0, 0, timeBgWidth, timeBgHeight)];
    NSDate* date = [NSDate date];
    NSCalendar* calendar = [NSCalendar currentCalendar];
    unsigned unitFlags = NSYearCalendarUnit | NSMonthCalendarUnit |  NSDayCalendarUnit | NSWeekdayCalendarUnit | NSHourCalendarUnit | NSMinuteCalendarUnit;
    NSDateComponents* dateComponent = [calendar components:unitFlags fromDate:date];
    int year = [dateComponent year];
    int month = [dateComponent month];
    int day = [dateComponent day];
    int hour = [dateComponent hour];
    int minute = [dateComponent minute];
    CGFloat startx = 0;
    CGFloat starty = 0;
    static CGFloat midStartX = 0;
    [self drawNumber: hour style: (enum FONT_STYLE)mFontStyle size : FONT_BIG_SIZE newLine: NO x : &startx y : &starty];
    if(mDisplayMidPoint)
    {
        [self drawString:@":" style:(enum FONT_STYLE)mFontStyle size:FONT_BIG_SIZE newLine: NO x:&startx y:&starty];
        midStartX = startx;
    }
    else 
    {
        startx = midStartX;
    }
    [self drawNumber: minute style:(enum FONT_STYLE)mFontStyle size:FONT_BIG_SIZE newLine: YES x:&startx y:&starty];
    startx = 0;
    starty += DATE_TIME_SPACING;
    [self drawNumber:year style:(enum FONT_STYLE)mFontStyle size:FONT_NORMAL_SIZE newLine:NO x:&startx y:&starty];
    [self drawString:@"/" style:(enum FONT_STYLE)mFontStyle size:FONT_NORMAL_SIZE newLine:NO x:&startx y:&starty];
    [self drawNumber:month style:(enum FONT_STYLE)mFontStyle size:FONT_NORMAL_SIZE newLine:NO x:&startx y:&starty];
    [self drawString:@"/" style:(enum FONT_STYLE)mFontStyle size:FONT_NORMAL_SIZE newLine:NO x:&startx y:&starty];
    [self drawNumber:day style:(enum FONT_STYLE)mFontStyle size:FONT_NORMAL_SIZE newLine:NO x:&startx y:&starty];
}


- (void)dealloc
{
    [mTimeTimer invalidate];
    [mTimeTimer release];
    [mImageRectArray release];
    [mFontLoader release];
    [mTimeBgImage release];
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
            NSString* iconKey = getIconKey((TOOLBAR_BUTTON_TYPE)i, 0);
            UIImage* icon = [mResLoader getImage:iconKey];
            CGRect frame = CGRectMake(startx, 0, icon.size.width, icon.size.height);
            mButtons[i] = [[UIButton alloc] initWithFrame:frame];
            [mButtons[i] setBackgroundImage:icon forState:UIControlStateNormal];
            
            iconKey = getIconKey((TOOLBAR_BUTTON_TYPE)i, 1);
            icon = [mResLoader getImage:iconKey];
            [mButtons[i] setBackgroundImage:icon forState:UIControlStateHighlighted];
            
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
    mDateTimeView = [[SEDateTimeView alloc] initWithFrame:frame resLoader: mResLoader];
    mDateTimeView.backgroundColor = [UIColor clearColor];
    [self addSubview:mDateTimeView];
}

- (void)loadView
{
    CGRect frame = CGRectMake(0, 0, mFrame.size.width, mFrame.size.height);
    mMainDispalyImageView = [[PHImageView alloc] initWithFrame:frame];
    mMainDispalyImageView.tag = 505;
    mMainDispalyImageView.backgroundColor = [UIColor redColor];
    mMainDispalyImageView.userInteractionEnabled = YES;
    UIImage* bgImage = [mResLoader getImage:@"MainDisplayBg"];
    mMainDispalyImageView.image = [SEUtil drawImage:bgImage inRect:frame];
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
    [self createDateTimeView:CGRectMake(0, frame.origin.y, 500, 300)];
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
- (void)updateTime
{
    
}
@end
