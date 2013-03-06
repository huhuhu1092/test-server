//
//  SEWidgetView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-9-3.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEWidgetView.h"
#import "SEViewNavigator.h"
#import "SEUtil.h"
#import "SEResDefine.h"
#import "UserInfo.h"
#import "SESystemConfig.h"
#import "PhotoFrameAppDelegate.h"
#import "SETestNotification.h"
#import "SESystemDataManager.h"
const static int DATE_TIME_SPACING = 10;

////////////
@implementation SELightView
@synthesize icon;
@synthesize imageBg;
@synthesize brightValue;
@synthesize mSavedBrightValue;
- (void) tapHandler: (UITapGestureRecognizer*)tapGes
{
    NSLog(@"light view handler");
    int version = [SEUtil getSystemVersion];
    if(version >= 5)
    {
        UIScreen* screen = [UIScreen mainScreen];
        NSLog(@"saved value = %f , system value = %f", mSavedBrightValue, mViewNav.mCurrentSystemBright);
        if(mUseNewValue == NO)
        {
            mUseNewValue = YES;
            screen.brightness = mSavedBrightValue;
            self.alpha = 1.0;
        }
        else
        {
            mUseNewValue = NO;
            screen.brightness = mViewNav.mCurrentSystemBright;
            self.alpha = 0.2;
        }
    }
    else
    {
        if(mUseNewValue == NO)
        {
            mUseNewValue = YES;
            [mViewNav setDimViewBrightness:0.0];
            self.alpha = 0.2;
        }
        else
        {
            mUseNewValue = NO;
            [mViewNav setDimViewBrightness:0.5];
            self.alpha = 1.0;
        }
    }
    /*
     if(version <= 4)
     {
     if(dark == YES)
     {
     dark = NO;
     [mViewNav setDimViewBrightness:0.0];
     self.alpha = 0.2;
     }
     else
     {
     dark = YES;
     [mViewNav setDimViewBrightness:0.5];
     self.alpha = 1.0;
     }
     }
     else 
     {
     
     if(dark == YES)
     {
     UIScreen* screen = [UIScreen mainScreen];
     brightValue = screen.brightness;
     screen.brightness = 1.0;
     mAlpha = 0.2;
     self.alpha = 0.2;
     dark = NO;
     }
     else 
     {
     UIScreen* screen = [UIScreen mainScreen];
     screen.brightness = brightValue;
     mAlpha = 1.0;
     self.alpha = 1.0;
     dark = YES;
     }
     }
     */
    //[mViewNav fetchAllSelectedImage];
    [self setNeedsDisplay];
}

- (CGSize) getHintSize
{
    UIImage* image = [mViewNav.mResLoader getImage:@"MainDisplayLightViewBg"];
    if(imageBg == nil)
        self.imageBg = image;
    if(self.icon == nil)
        self.icon = [mViewNav.mResLoader getImage:@"MainDisplayLightViewIcon"];
    return image.size;
}
- (id) initWithViewNav: (SEViewNavigator*)viewNav
{
    self = [super init];
    if(self)
    {
        mViewNav = viewNav;
        CGSize s = [self getHintSize];
        self.frame = CGRectMake(0, 0, s.width, s.height);
        UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
        [self addGestureRecognizer:tap];
        [tap release];
        mAlpha = 1.0;
        int version = [SEUtil getSystemVersion];
        if(version >= 5)
        {
            UIScreen* screen = [UIScreen mainScreen];
            mSavedBrightValue = screen.brightness;
        }
    }
    return self;
}
- (void) drawRect:(CGRect)rect
{
    [imageBg drawAtPoint:CGPointMake(0, 0)];
    CGSize s = icon.size;
    
    //if(brightValue == 1.0)
    {
        UIGraphicsBeginImageContext(CGSizeMake(brightValue * s.width / 2, s.height));
        [icon drawAtPoint:CGPointMake(0, 0 )];
        UIImage* im = UIGraphicsGetImageFromCurrentImageContext();
        UIGraphicsEndImageContext();
        [im drawAtPoint:CGPointMake(0, 0)];
    }
    
    //else
    {
        /*
         UIGraphicsBeginImageContext(CGSizeMake(s.width, s.height));
         [icon drawAtPoint:CGPointMake(0, 0 )];
         UIImage* im = UIGraphicsGetImageFromCurrentImageContext();
         UIGraphicsEndImageContext();
         [im drawAtPoint:CGPointMake(0, 0)];
         */
        //[icon drawAtPoint:CGPointMake(0, 0  )];
    }
    
    
}
@end
////////////////
@implementation SEDrawingWaitingView
- (UIImage*) getBgLoaingIcon
{
    return [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIcon"];
}
- (CGSize) getHintSize
{
    return CGSizeMake(128, 128);
}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        UIImage* image = nil;
        iconView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
        iconView.image = image;
        [self addSubview:iconView];
        [iconView release];
        
        stageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
        [self addSubview:stageView];
        [stageView release];
        mCurrentStage = NO_LOADING_STAGE;
        //self.backgroundColor = [UIColor whiteColor];
    }
    return self;
}
- (void) setIconViewTransform:(CGAffineTransform)transform
{
    iconView.transform = transform;
    [iconView setNeedsDisplay];
    //stageView.transform = transform;
    //[stageView setNeedsDisplay];
}
- (int) getStage
{
    return mCurrentStage;
}
- (void) updateStageImage
{
    UIImage* image = nil;
    switch (mCurrentStage) {
        case LOADING_STAGE1:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIconStage1"];
            
        }
            break;
        case LOADING_STAGE2:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIconStage2"];
        }
            break;
        case LOADING_STAGE3:
        {
            image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:@"LoadingIconStage3"];
        }
            break;
        case NO_LOADING_STAGE:
        {
            image = nil;
        }
            break;
        default:
            break;
    }
    stageView.image = image;
    [stageView setNeedsDisplay];
}
- (void) setStage:(int)stage
{
    
    if(stage == mCurrentStage)
        return;
    mCurrentStage = stage;
    NSLog(@"drawing stage = %d", stage);
}
- (void)loaderTimerUpdate:(NSTimer*)timer
{
    mStartAngle += 30;
    if(mStartAngle >= 360)
    {
        mStartAngle = 0;
    }
    [self setIconViewTransform:CGAffineTransformMakeRotation(mStartAngle * 3.1415926 / 180.0)];
    [self updateStageImage];
}

- (void) start
{
    if(mStarted)
        return;
    mStarted = YES;
    self.hidden = NO;
    //mCurrentStage = LOADING_STAGE1;
    iconView.image = [self getBgLoaingIcon];
    //[self setStage:mCurrentStage];
    mLoadingTimer = [[NSTimer timerWithTimeInterval:0.06 target:self selector:@selector(loaderTimerUpdate:) userInfo:nil repeats:YES] retain];
    [[NSRunLoop currentRunLoop] addTimer:mLoadingTimer forMode:NSDefaultRunLoopMode];

}
- (void) stop
{
    mStarted = NO;
    [mLoadingTimer invalidate];
    [mLoadingTimer release];
    mLoadingTimer = nil;
    mCurrentStage = NO_LOADING_STAGE;
    iconView.image = nil;
    stageView.image = nil;
    [self setNeedsDisplay];
    self.hidden = YES;
}
@end
////////
@implementation SEPowerView

//@synthesize imageBigBackground;
//@synthesize imageNormalBackground;
//@synthesize imageBigLowForeground;
//@synthesize imageBigFullForeground;
//@synthesize imageNormalFullForeground;
//@synthesize imageNormalLowForeground;
//@synthesize currentDrawingImage;
- (void) timerUpdate: (NSTimer*)timer
{
    
}
- (void) createBatteryDisplayTimer
{
    //mTimerForBatteryCharge = [NSTimer timerWithTimeInterval:1 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES];
    //[[NSRunLoop currentRunLoop] addTimer:mTimerForBatteryCharge forMode:NSDefaultRunLoopMode];
}
- (void) update
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    [self setNeedsDisplay];
    /*
    mDisplayPower = YES;
    if([userInfo.showpowerview intValue] == 0)
    {
        if(mTimerForBatteryCharge)
        {
            [mTimerForBatteryCharge invalidate];
            mTimerForBatteryCharge = nil;
        }
    }
    else
    {
        UIDeviceBatteryState state = [[UIDevice currentDevice] batteryState];
        if(state == UIDeviceBatteryStateCharging)
        {
            if(mTimerForBatteryCharge == nil)
            {
                [self createBatteryDisplayTimer];
            }
        }
    }
     */
}
- (CGSize) getHintSize
{
    if(bigSize.width != 0 && bigSize.height != 0)
        return bigSize;
    UIImage* image = [mViewNav.mResLoader getImage:@"PowerViewBigBackground"];;
    bigSize = image.size;
    normalSize = CGSizeMake(60, 41);
    return bigSize;
    /*
    if(self.imageBigBackground == nil)
    {
        image = [mViewNav.mResLoader getImage:@"PowerViewBigBackground"];
        self.imageBigBackground = image;
    }
    
    
    
    if(self.imageNormalBackground == nil)
    {
        image = [mViewNav.mResLoader getImage:@"PowerViewBackground"];
        self.imageNormalBackground = image;
    }
    
    
    
    if(self.imageBigFullForeground == nil)
    {
        image = [mViewNav.mResLoader getImage:@"PowerViewBigFullForeground"];
        self.imageBigFullForeground = image;
    }
    
    
    if(self.imageBigLowForeground == nil)
    {
        image = [mViewNav.mResLoader getImage:@"PowerViewBigLowForeground"];
        self.imageBigLowForeground = image;
    }
    
    
    if(self.imageNormalLowForeground == nil)
    {
        image = [mViewNav.mResLoader getImage:@"PowerViewLowForeground"];
        self.imageNormalLowForeground = image;
    }
    
    
    if(self.imageNormalFullForeground == nil)
    {
        image = [mViewNav.mResLoader getImage:@"PowerViewFullForeground"];
        self.imageNormalFullForeground = image;
    }
    CGSize retSize = CGSizeMake(128, 88);
    return retSize;
     */
}
- (void) batteryLevelChangeObserver:(NSNotification*)note
{
    NSLog(@"battery level change\n");
    [self setNeedsDisplay];
}

- (void) batteryStateChangeObserver: (NSNotification*) note
{
    NSLog(@"battery state chage\n");
    UserInfo* userInfo = [mViewNav getUserInfo];
    BOOL bShow = [userInfo.showpowerview boolValue];
    if(!bShow)
        return;
    UIDeviceBatteryState state = [[UIDevice currentDevice] batteryState];
    //mDisplayPower = YES;
    switch (state)
    {
        case UIDeviceBatteryStateUnknown:
        {
            NSLog(@"unknown battery state");
            [self setNeedsDisplay];
        }
            break;
        case UIDeviceBatteryStateUnplugged:
        {
            [self setNeedsDisplay];
        }
            break;
        case UIDeviceBatteryStateCharging:
        {
            [self setNeedsDisplay];
        }
            break;
        case UIDeviceBatteryStateFull:
        {
            [self setNeedsDisplay];
        }
            break;
    }
}
- (void) tapHandler: (UITapGestureRecognizer*)ges
{
    NSLog(@"power view tap handler");
    mDisplayMininumIcon =  !mDisplayMininumIcon;
    [self setNeedsDisplay];
}
- (void) dealloc
{
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    [super dealloc];
}
- (id) initWithViewNav: (SEViewNavigator*)viewNav
{
    self = [super init];
    if(self)
    {
        mViewNav = viewNav;
        CGSize s = [self getHintSize];
        UIDevice* device = [UIDevice currentDevice];
        device.batteryMonitoringEnabled = YES;
        NSNotificationCenter* notifyCenter = [NSNotificationCenter defaultCenter];
        [notifyCenter addObserver:self selector:@selector(batteryLevelChangeObserver:) name:UIDeviceBatteryLevelDidChangeNotification object:nil];
        [notifyCenter addObserver:self selector:@selector(batteryStateChangeObserver:) name:UIDeviceBatteryStateDidChangeNotification object:nil];
        //[notifyCenter addObserver:self selector:@selector(batteryLevelChangeObserver:) name:[SETestNotification getTestLevelChangeNotificationStr] object:nil];
        self.frame = CGRectMake(0,  0,  s.width, s.height);
        UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
        [self addGestureRecognizer:ges];
        [ges release];
    }
    return self;
}
- (void) drawRect:(CGRect)rect
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    float startx = 0;
    float starty = 0;
    CGSize dstSize = [self getHintSize];
    float batteryValue = [[UIDevice currentDevice] batteryLevel];
    UIImage* foreImage = [mViewNav.mResLoader getImage:@"PowerViewFullForeground"];
    UIImage* bigBackground = [mViewNav.mResLoader getImage:@"PowerViewBigBackground"];
    float foreImageFullWidth = foreImage.size.width;
    float ratio = batteryValue;
    UIDeviceBatteryState state = [[UIDevice currentDevice] batteryState];
    UIImage* chargingImage = nil;
    if(state == UIDeviceBatteryStateCharging)
    {
        chargingImage = [mViewNav.mResLoader getImage:@"ChargingIcon"];
    }
    UIGraphicsBeginImageContext(bigSize);
    float foreStartx = dstSize.width - 30 - foreImageFullWidth;
    float foreStarty = 21;
    [bigBackground drawInRect:CGRectMake(0, 0, bigSize.width, bigSize.height)];
    //CGContextRef contextRef = UIGraphicsGetCurrentContext();
    UIImage* myImage = nil;
    if(ratio <= 0.14)
    {
        //CGContextSetRGBFillColor(contextRef, 1, 0, 0, 1);
        myImage = [SEUtil createImageWithColor:255 g:0 b:0 alphaImage:foreImage];
    }
    else
    {
        //CGContextSetRGBFillColor(contextRef, 42.0/255, 225.0 / 255, 0, 1);
        myImage = [SEUtil createImageWithColor:42 g:255 b:0 alphaImage:foreImage];
    }
    //CGContextFillRect(contextRef, CGRectMake(foreStartx, foreStarty, foreImageFullWidth * ratio, foreImage.size.height));
    //[foreImage drawInRect:CGRectMake(foreStartx, foreStarty, foreImageFullWidth * ratio, foreImage.size.height)];
    [myImage drawInRect:CGRectMake(foreStartx, foreStarty, foreImageFullWidth * ratio, foreImage.size.height)];
    if(chargingImage)
    {
        [chargingImage drawInRect:CGRectMake(0, 0, bigSize.width, bigSize.height)];
    }
    UIImage* synthesisImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    
    int powerViewSize = [userInfo.powerviewsize intValue];
    float powerViewRatio = (powerViewSize - [SESystemConfig getMinPowerViewValue]) / (float)([SESystemConfig getMaxPowerViewValue] - [SESystemConfig getMinPowerViewValue]);
    if(powerViewRatio < 0)
        powerViewRatio = 0;
    if(powerViewRatio > 1)
        powerViewRatio = 1;
    float w = normalSize.width +  (dstSize.width - normalSize.width) * powerViewRatio;
    float h = w * dstSize.height / dstSize.width;
    if(mDisplayMininumIcon)
    {
        w = normalSize.width;
        h = normalSize.height;
    }
    startx = (dstSize.width - w);
    starty = 0;
    //NSLog(@"power view image width = %f, height = %f", w, h);
    if(synthesisImage)
    {
        [synthesisImage drawInRect:CGRectMake(startx, starty, w, h)];
    }

}
@end
//////////////

@interface SEDateTimeView (Private) 

@end
@implementation SEDateTimeView (Private)



- (void) timerUpdate: (NSTimer*)timer
{
    if(mDisplayMidPoint)
        mDisplayMidPoint = NO;
    else
    {
        mDisplayMidPoint = YES;
    }
    [self setNeedsDisplay];
    if(mTestUserUpgrade)
    {
        static NSTimeInterval time = 0;
        time += 1;
        if(time >= 5)
        {
            [mViewNav finishOneImageDrawing];
            time = 0;
        }
    }
}



@end
@implementation SEDateTimeView
@synthesize mViewNav;
@synthesize mFontStyle;
@synthesize mDateBgImage;
@synthesize mTimeBgImage;
/////////////////////////////////
- (CGSize) getHintSize
{
    if(self.mTimeBgImage == nil)
        self.mTimeBgImage = [mViewNav.mResLoader getImage:@"DateTimeViewTimeBackground"];
    if(self.mDateBgImage == nil)
        self.mDateBgImage = [mViewNav.mResLoader getImage:@"DateTimeViewDateBackground"];
    CGFloat w = self.mTimeBgImage.size.width ;
    CGFloat h = self.mDateBgImage.size.height;// + self.mTimeBgImage.size.height + DATE_TIME_SPACING;
    return CGSizeMake(w, h );
}
- (void) drawString: (NSString*) str style: (int) fontStyle size : (CGSize) fontSize newLine: (BOOL) bNewLine x: (CGFloat*)startx y: (CGFloat*) starty
{
    CGFloat height = 0;
    for(int i = 0 ; i < str.length ; i++)
    {
        NSRange r;
        r.location = i;
        r.length = 1;
        NSString* numStr = [str substringWithRange:r];
        UIImage* image = [mFontLoader getImage:numStr style:fontStyle size:FONT_NORMAL_SIZE];
        [image drawInRect:CGRectMake(*startx, *starty, fontSize.width, fontSize.height)];
        *startx += fontSize.width;
        height = fontSize.height;
    } 
    if(bNewLine)
        *starty += height;
}
- (void) drawNumber: (int) num style : (int) fontStyle size : (CGSize) fontSize newLine: (BOOL)bNewLine x : (CGFloat*)startx y : (CGFloat*)starty
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
        mTimeBgImage = [mViewNav.mResLoader getImage:@"DateTimeViewTimeBackground"];
    }
    UserInfo* userInfo = [mViewNav getUserInfo];
    mFontStyle = [mViewNav.mSystemDataManager.timetextstyle intValue];
    //CGFloat timeBgWidth = mTimeBgImage.size.width;
    //CGFloat timeBgHeight = mTimeBgImage.size.height;
    //UIImage* bigImage = [mFontLoader getImage:@"2" style:(int)mFontStyle size:FONT_BIG_SIZE];
    UIImage* normalImage = [mFontLoader getImage:@"2" style:(int)mFontStyle size:FONT_NORMAL_SIZE];
    float ratio = normalImage.size.width / normalImage.size.height;
    float bigFontHeight = 80;
    float bigFontWidth = bigFontHeight * ratio;
    float smallFontHeight = 40;
    float smallFontWidth = smallFontHeight * ratio;
    float w = bigFontWidth * 5;
    float h = bigFontHeight + smallFontHeight;
    
    [mTimeBgImage drawAtPoint:CGPointMake(0, 0)];
    //[mDateBgImage drawAtPoint:CGPointMake(0, timeBgHeight)];
    NSDate* date = [NSDate date];
    NSCalendar* calendar = [NSCalendar currentCalendar];
    unsigned unitFlags = NSYearCalendarUnit | NSMonthCalendarUnit |  NSDayCalendarUnit | NSWeekdayCalendarUnit | NSHourCalendarUnit | NSMinuteCalendarUnit;
    NSDateComponents* dateComponent = [calendar components:unitFlags fromDate:date];
    int year = [dateComponent year];
    int month = [dateComponent month];
    int day = [dateComponent day];
    int hour = [dateComponent hour];
    int minute = [dateComponent minute];
    CGFloat startx = (mTimeBgImage.size.width - w) / 2;
    CGFloat starty = (mTimeBgImage.size.height - h) / 2;
    if(startx < 60)
        startx = 60;
    static CGFloat midStartX = 0;
    [self drawNumber: hour style: mFontStyle size : CGSizeMake(bigFontWidth, bigFontHeight) newLine: NO x : &startx y : &starty];
    if(mDisplayMidPoint)
    {
        [self drawString:@":" style:mFontStyle size:CGSizeMake(bigFontWidth, bigFontHeight) newLine: NO x:&startx y:&starty];
        midStartX = startx;
    }
    else 
    {
        startx = midStartX;
    }
    [self drawNumber: minute style:mFontStyle size:CGSizeMake(bigFontWidth, bigFontHeight) newLine: YES x:&startx y:&starty];
    startx = (mTimeBgImage.size.width - w) / 2;
    if(startx < 60)
        startx = 60;
    starty += DATE_TIME_SPACING;
    [self drawNumber:year style:mFontStyle size:CGSizeMake(smallFontWidth, smallFontHeight) newLine:NO x:&startx y:&starty];
    [self drawString:@"/" style:mFontStyle size:CGSizeMake(smallFontWidth, smallFontHeight) newLine:NO x:&startx y:&starty];
    [self drawNumber:month style:mFontStyle size:CGSizeMake(smallFontWidth, smallFontHeight) newLine:NO x:&startx y:&starty];
    [self drawString:@"/" style:mFontStyle size:CGSizeMake(smallFontWidth, smallFontHeight) newLine:NO x:&startx y:&starty];
    [self drawNumber:day style:mFontStyle size:CGSizeMake(smallFontWidth, smallFontHeight) newLine:NO x:&startx y:&starty];
}
- (id) initWithViewNav: (SEViewNavigator*)viewNav
{
    self = [super init];
    if(self)
    {
        mFontLoader = [[SEFontLoader alloc] init];
        mViewNav = viewNav;
        mImageRectArray = [NSMutableArray array];
        [mImageRectArray retain];
        mFontStyle = 1;
        CGSize size = [self getHintSize];
        self.frame = CGRectMake(0, 0, size.width, size.height);
        mTimeTimer = [[NSTimer timerWithTimeInterval:1 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES] retain];
        mDisplayMidPoint = YES;
        [[NSRunLoop currentRunLoop] addTimer:mTimeTimer forMode:NSDefaultRunLoopMode];
    }
    return self;
}

- (void)dealloc
{
    [mTimeTimer invalidate];
    [mImageRectArray release];
    [mFontLoader release];
    [mTimeBgImage release];
    [mDateBgImage release];
    [super dealloc];
}
@end
////////////

