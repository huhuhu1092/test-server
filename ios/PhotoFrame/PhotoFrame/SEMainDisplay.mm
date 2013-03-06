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
#import "SEViewNavigator.h"
#import "UserInfo.h"
#import "SEDrawTouchView.h"
#import "PainterManager.h"
#import "Signature.h"
#import "SESystemConfig.h"
#import "FontLabel.h"
//////////////
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
@implementation SEMainDisplayToolBarView
@synthesize imagePortrait;
@synthesize imageLandscape;
- (void)drawRect:(CGRect)rect
{

    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        [imagePortrait drawInRect:CGRectMake(0, 0, rect.size.width, rect.size.height)];
    }
    else {
        [imageLandscape drawInRect:CGRectMake(0, 0, rect.size.width, rect.size.height)];
    }

}
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
/*
- (void) setImage:(UIImage *)image
{
    mBackgroundView.image = image;
}
- (UIImage*) image
{
    return mBackgroundView.image;
}
 */
- (CGSize) getHintSize
{
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        if(self.imagePortrait == nil)
            self.imagePortrait = [mViewNav.mResLoader getImage:@"MainDisplayToolBarBgPortrait"];
        mHintSizePortrait = imagePortrait.size;
        mHintSizePortrait.width /= 2;
        mHintSizePortrait.height /= 2;
        return mHintSizePortrait;
    }
    else
    {
        if(self.imageLandscape == nil)
            self.imageLandscape= [mViewNav.mResLoader getImage:@"MainDisplayToolBarBg"];
        mHintSizeLandscape = imageLandscape.size;
        mHintSizeLandscape.width /= 2;
        mHintSizeLandscape.height /=2;
        return mHintSizeLandscape;
    }
}
- (void) updateFrame
{
    CGSize s = [self getHintSize];
    float step = s.width / 6;
    float startx = 0;
    for(int i = 0 ; i < TOOLBAR_BUTTON_NUM ; i++)
    {
        NSString* iconKey = getIconKey((TOOLBAR_BUTTON_TYPE)i, 0);
        UIImage* icon = [mViewNav.mResLoader getImage:iconKey];
        float iconWidth = icon.size.width / 2;
        float iconHeight = icon.size.height / 2;
        float offset = (step - iconWidth) / 2;
        CGRect frame = CGRectMake(startx + offset, 0, iconWidth, iconHeight);
        mButtons[i].frame = frame;
        //[mButtons[i] backgroundRectForBounds:CGRectMake((step - icon.size.width) / 2, 0, icon.size.width, icon.size.height)];
        startx += step;
    }
}
- (void) setPlayIcon
{
    UIImage* icon = [mViewNav.mResLoader getImage:@"ToolBarPlayIcon"];
    [mButtons[PLAY_PAUSE] setBackgroundImage:icon forState:UIControlStateNormal];
    icon = [mViewNav.mResLoader getImage:@"ToolBarPlayIconHighlight"];
    [mButtons[PLAY_PAUSE] setBackgroundImage:icon forState:UIControlStateHighlighted];
}
- (void) setPaurseIcon
{
    UIImage* icon = [mViewNav.mResLoader getImage:@"ToolBarPauseIcon"];
    [mButtons[PLAY_PAUSE] setBackgroundImage:icon forState:UIControlStateNormal];
    icon = [mViewNav.mResLoader getImage:@"ToolBarPauseIconHighlight"];
    [mButtons[PLAY_PAUSE] setBackgroundImage:icon forState:UIControlStateHighlighted];    
}
- (id) initWithViewNav:(SEViewNavigator*) viewNav
{
    self = [super init];
    if(self)
    {
        mViewNav = viewNav;
        CGSize s = [self getHintSize];
        CGRect tframe = CGRectMake(0, 0, s.width, s.height);
        self.frame = tframe;
        float step = s.width / 6;
        float startx = 0;
        for(int i = 0 ; i < TOOLBAR_BUTTON_NUM ; i++)
        {
            NSString* iconKey = getIconKey((TOOLBAR_BUTTON_TYPE)i, 0);
            UIImage* icon = [mViewNav.mResLoader getImage:iconKey];
            float iconWidth = icon.size.width / 2;
            float iconHeight = icon.size.height / 2;
            float offset = (step - iconWidth) / 2;
            CGRect frame = CGRectMake(startx + offset, 0, iconWidth, iconHeight);
            mButtons[i] = [[UIButton alloc] init];//[[UIButton alloc] initWithFrame:frame];
            [mButtons[i] setBackgroundImage:icon forState:UIControlStateNormal];
            mButtons[i].frame = frame;
            iconKey = getIconKey((TOOLBAR_BUTTON_TYPE)i, 1);
            icon = [mViewNav.mResLoader getImage:iconKey];
            [mButtons[i] setBackgroundImage:icon forState:UIControlStateHighlighted];
            
            [mButtons[i] addTarget:self action:@selector(buttonClickHandler:) forControlEvents:UIControlEventTouchUpInside];
            startx += step;//icon.size.width;
            [self addSubview:mButtons[i]];
            [mButtons[i] release];
        }
        //[self updateFrameSize];
    }
    return self;
}
- (void)dealloc
{
    [imagePortrait release];
    [imageLandscape release];
    for(int i = 0 ; i < TOOLBAR_BUTTON_NUM ; i++)
    {
        [mButtons[i] release];
    }
    [super dealloc];
}

@end
////////////////////////
@implementation SEMainDisplay
@synthesize mViewNav;

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
        mToolBarAnimationEnd = YES;
        mDateTimeAnimationEnd = YES;
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
        mToolBarAnimationEnd = YES;
        mDateTimeAnimationEnd = YES;
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
- (void) createDateTimeView
{
    mDateTimeView = [[SEDateTimeView alloc] initWithViewNav: mViewNav];
    mDateTimeView.backgroundColor = [UIColor clearColor];
    [self addSubview:mDateTimeView];
}
- (void) createLightView
{
    mLightView = [[SELightView alloc] initWithViewNav:mViewNav];
    [self addSubview:mLightView];
    mLightView.backgroundColor = [UIColor clearColor];
}
- (void) createPowerView
{
    mPowerView = [[SEPowerView alloc] initWithViewNav:mViewNav];
    [self addSubview:mPowerView];
    mPowerView.backgroundColor = [UIColor clearColor];
}
- (CGAffineTransform) createTransform: (float)x width: (float) width height: (float)height
{
    CGAffineTransform translate1 = CGAffineTransformMakeTranslation(x, 0);
    CGAffineTransform translate2 = CGAffineTransformTranslate(translate1, 0, mViewNav.mViewPortHeight);
    
    CGAffineTransform t3 = CGAffineTransformTranslate(translate2, -width / 2, -height / 2);
    CGAffineTransform r1 = CGAffineTransformRotate(t3, -90 * 3.1415926 / 180.0f);
    CGAffineTransform t4 = CGAffineTransformTranslate(r1, width / 2, height / 2);
    return t4;

}
- (void) setDrawingWaitingViewTransform
{
    CGSize s = [mDrawingWaitingView getHintSize];
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        mDrawingWaitingView.transform = CGAffineTransformIdentity;
        mDrawingWaitingView.frame = CGRectMake(0, 0, s.width, s.height);
        float w = s.width;
        float h = s.height;
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(0, mViewNav.mViewPortHeight);
        CGAffineTransform t2 = CGAffineTransformTranslate(t1, -w / 2, -h / 2);
        CGAffineTransform r1 = CGAffineTransformRotate(t2, -90 * 3.1415926 / 180.0f);
        CGAffineTransform t3 = CGAffineTransformTranslate(r1, w / 2, h / 2);
        mDrawingWaitingView.transform = t3;
    }
    else
    {
        mDrawingWaitingView.transform = CGAffineTransformIdentity;
        mDrawingWaitingView.frame = CGRectMake(0, 0, s.width, s.height);
        float x = 2;
        mDrawingWaitingView.transform = CGAffineTransformMakeTranslation(x, 0);
    }
}
- (void) setLightViewTransform
{
    CGSize s = [mLightView getHintSize];
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        mLightView.transform = CGAffineTransformIdentity;
        mLightView.frame = CGRectMake(0, 0, s.width, s.height);
        float w = s.width;
        float h = s.height;
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(0, mViewNav.mViewPortHeight);
        CGAffineTransform t2 = CGAffineTransformTranslate(t1, -w / 2, -h / 2);
        CGAffineTransform r1 = CGAffineTransformRotate(t2, -90 * 3.1415926 / 180.0f);
        CGAffineTransform t3 = CGAffineTransformTranslate(r1, w / 2, h / 2);
        mLightView.transform = t3;
    }
    else
    {
        mLightView.transform = CGAffineTransformIdentity;
        mLightView.frame = CGRectMake(0, 0, s.width, s.height);
        float x = 2;
        mLightView.transform = CGAffineTransformMakeTranslation(x, 0);
    }
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
- (void) setToolBarAndDateViewTransform
{
    CGSize toolSize = [mToolBarView getHintSize];
    CGSize dateTimeSize = [mDateTimeView getHintSize];
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        mToolBarView.transform = CGAffineTransformIdentity;
        mToolBarView.frame = CGRectMake(0, 0, toolSize.width, toolSize.height);
        float x = mViewNav.mViewPortWidth - toolSize.height;
        CGAffineTransform transform = [self createTransform: x width:toolSize.width height:toolSize.height];
        if(mToolBarHidden == YES)
        {
            transform = CGAffineTransformTranslate(transform, 0, toolSize.height);
        }
        mToolBarView.transform = transform;
        x = mViewNav.mViewPortWidth - toolSize.height - dateTimeSize.height;
        transform = [self createTransform: x width:dateTimeSize.width height:dateTimeSize.height];
        transform = CGAffineTransformTranslate(transform, -60, 6);
        if(mToolBarHidden == YES)
        {
            transform = CGAffineTransformTranslate(transform, 0, toolSize.height);
        }
        mDateTimeView.transform = transform;
    }
    else
    {
        mToolBarView.transform = CGAffineTransformIdentity;
        mToolBarView.frame = CGRectMake(0, 0, toolSize.width, toolSize.height);
        float x = mViewNav.mViewPortHeight - toolSize.height;
        if(mToolBarHidden == YES)
        {
            mToolBarView.transform = CGAffineTransformMakeTranslation(0, x + toolSize.height);
        }
        else {
            mToolBarView.transform = CGAffineTransformMakeTranslation(0, x);
        }
        x = mViewNav.mViewPortHeight - toolSize.height - dateTimeSize.height;
        CGAffineTransform t;
        if(mToolBarHidden == YES)
        {
             t = CGAffineTransformMakeTranslation(0, x + toolSize.height);
        }
        else
        {
            t = CGAffineTransformMakeTranslation(0, x);
        }
        t = CGAffineTransformTranslate(t, -60, 6);
        mDateTimeView.transform = t;
    }
}

- (void) setDrawViewTransform
{
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGSize s = [SESystemConfig getCurrentDrawViewSize];
        float w = s.width;
        float h = s.height;
        [mDrawView setLineWidthRatio:w / 717.0];
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(-w / 2, -h / 2);
        CGAffineTransform r1 = CGAffineTransformRotate(t1, -90 * 3.1415926 / 180.0f);
        CGAffineTransform t2 = CGAffineTransformTranslate(r1, -w / 2, h / 2);
        //CGAffineTransform t4 = CGAffineTransformTranslate(t2, 0, 0);
        CGSize toolSize = [mToolBarView getHintSize];
        float toolbarHeight = toolSize.height;
        CGAffineTransform t4 = CGAffineTransformTranslate(t2, -12, mViewNav.mViewPortWidth - h - toolbarHeight);
        
        mDrawView.transform = CGAffineTransformIdentity;
        mDrawView.frame = CGRectMake(0, 0, w, h);
        [mDrawView resize:CGRectMake(0, 0, w, h)];
        //NSLog(@"drawView size = %f, %f", w, h);
        mDrawView.transform = t4;
    }
    else
    {
        mDrawView.transform = CGAffineTransformIdentity;
        UserInfo* userInfo = [mViewNav getUserInfo];
        [self setDrawViewFrame:userInfo :mDrawView];
    }
}
/*
- (void) signatureAnimEnd
{
    mDrawView.hidden = YES;
    PainterManager* painterManager = [PainterManager painterManager];
    [painterManager paintSignatureToImage:[mDrawView getCGImage]];
    [mViewNav signatureAnimEnd];
}
 */
- (void) hideSignatureView
{
    mDrawView.hidden = YES;
}

- (void) setDrawViewFrame: (UserInfo*) userInfo :(SEDrawTouchView*)v
{
    float h = mToolBarView.frame.size.height;
    CGSize s = [SESystemConfig getCurrentDrawViewSize];
    [mDrawView setLineWidthRatio:s.width / 717.0];
    v.frame = CGRectMake(mViewNav.mViewPortWidth - s.width - 12, mViewNav.mViewPortHeight - s.height - h, s.width, s.height);
    [v resize:CGRectMake(0, 0, s.width, s.height)];
    //NSLog(@"drawView size = %f, %f", s.width, s.height);
}
- (void) createDrawingWaitingView
{
    SEDrawingWaitingView* v =[[SEDrawingWaitingView alloc] initWithFrame:CGRectMake(0, 0, 128, 128)];
    [self addSubview:v];
    [v release];
    mDrawingWaitingView = v;
}
- (void) createDrawView
{
    CGSize s = [SESystemConfig getCurrentDrawViewSize];
    SEDrawTouchView* v = [[SEDrawTouchView alloc] initWithFrame:CGRectMake(0, 0, s.width, s.height)];
    //v.mDrawInMainScreen = YES;
    [v setStep:0.2];
    //UserInfo* userInfo = [mViewNav getUserInfo];
    //[self setDrawViewFrame:userInfo :v];
    [v initData];
    /*
    Signature* sig = [mViewNav getCurrentSignature];
    if(sig != nil)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        if(sd.points != nil && sd.points.count > 0)
        {
            [v setPointColorArray:sd.colors];
            [v setNormalizePointsWithOutDraw:sd.points];
        }
    }
     */
    v.userInteractionEnabled = NO;
    v.backgroundColor = [UIColor clearColor];
    v.hidden = YES;
    [self addSubview:v];
    [v release];
    mDrawView = v;
    //mDrawView.backgroundColor= [UIColor redColor];
    [mDrawView setLineWidthRatio:mDrawView.frame.size.width / 717.0];
    //[mDrawView setAnimTarget:self action:@selector(signatureAnimEnd)];
}
- (void) setWidgetVisibility
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
    if([userInfo.showsignatureview boolValue])
    {
        
    }

}
//YES : indicate that we can play anim
// NO : indicate that we can not play anim
- (void) createFrameView
{
    mLeftFrameRect = CGRectMake(0, 0, 10, 768);
    mLeftFrameView = [[UIImageView alloc] initWithFrame:mLeftFrameRect];
    [self addSubview:mLeftFrameView];
    [mLeftFrameView release];
    mLeftFrameCenter = mLeftFrameView.center;
    
    mRightFrameRect = CGRectMake(1014, 0, 10, 768);
    mRightFrameView = [[UIImageView alloc] initWithFrame:mRightFrameRect];
    [self addSubview:mRightFrameView];
    [mRightFrameView release];
    mRightFrameCenter = mRightFrameView.center;
    
    mTopFrameRect = CGRectMake(0, 0, 1024, 10);
    mTopFrameView = [[UIImageView alloc] initWithFrame: mTopFrameRect];   
    [self addSubview:mTopFrameView];
    [mTopFrameView release];
    mTopFrameCenter = mTopFrameView.center;
    
    mBottomFrameRect = CGRectMake(0, 758, 1024, 10);
    mBottomFrameView = [[UIImageView alloc] initWithFrame: mBottomFrameRect];
    [self addSubview:mBottomFrameView];
    [mBottomFrameView release];
    mBottomFrameCenter = mBottomFrameView.center;
    
    mFrameImage = [mViewNav.mResLoader getImage:@"DrawingFinishFrameImage"];
    [mFrameImage retain];
    mLeftFrameView.hidden = YES;
    mRightFrameView.hidden = YES;
    mTopFrameView.hidden = YES;
    mBottomFrameView.hidden = YES;
}
- (void)loadView
{
    self.clipsToBounds = YES;
    CGRect tframe = CGRectMake(0, 0, mFrame.size.width, mFrame.size.height);
    mMainDispalyImageView = [[PHImageView alloc] initWithFrame:tframe];
    mMainDispalyImageView.tag = 505;
    mMainDispalyImageView.backgroundColor = [UIColor redColor];
    mMainDispalyImageView.userInteractionEnabled = YES;
    mMainDispalyImageView.mViewNav = mViewNav;
    UIImage* bgImage = [mResLoader getImage:@"MainDisplayBg"];
    mMainDispalyImageView.image = [SEUtil drawImage:bgImage inRect:tframe];
    [self setTapGestureToMainDisplay];

    mToolBarView = [[SEMainDisplayToolBarView alloc] initWithViewNav:mViewNav];
    mToolBarView.hidden = NO;
    mToolBarView.userInteractionEnabled = YES;
    mToolBarView.backgroundColor = [UIColor clearColor];
    [self addSubview:mMainDispalyImageView];
    [self createFrameView];
    [self addSubview:mToolBarView];
    [self createDateTimeView];
    [self createPowerView];
    [self createDrawingWaitingView];
    //[self createLightView];
    [self createDrawView];
    [self setToolBarAndDateViewTransform];
    [self setPowerViewTransform];
    [self setDrawingWaitingViewTransform];
    [self setDrawViewTransform];
    [self setWidgetVisibility];
    //[self setLightViewTransform];
}
- (void) dealloc
{
    [mDateTimeView release];
    [mToolBarView release];
    [mMainDispalyImageView release];
    [mPowerView release];
    [mFrameImage release];
    [super dealloc];
}
- (void) setPlayIcon
{
    [mToolBarView setPlayIcon];
}
- (void) setPaurseIcon
{
    [mToolBarView setPaurseIcon];
}
- (void)tapHandler: (UITapGestureRecognizer*)tapGes
{
    UIView* tapView = tapGes.view;
    if(mToolBarAnimationEnd == NO || mDateTimeAnimationEnd == NO) 
        return;
    NSLog(@"## tool bar animation start ##");
    if(tapView == mMainDispalyImageView)
    {
        if(mToolBarHidden == YES)
        {
            mToolBarView.hidden = NO;
            //CGPoint p = mToolBarView.center;
            //p.y -= mToolBarView.frame.size.height;
            CGAffineTransform t = mToolBarView.transform;
            mToolBarView.transform = CGAffineTransformIdentity;
            float height = mToolBarView.frame.size.height;
            mToolBarView.transform = t;
            NSLog(@"tool bar height = %f", height);
            
            void(^animBlock)(void) = ^(void){
                //mToolBarView.center = p;
                
                NSLog(@"tx = %f, ty = %f", t.tx, t.ty);
                //mToolBarView.transform = CGAffineTransformIdentity;
                //mToolBarView.frame = frame;
                mToolBarView.transform = CGAffineTransformTranslate(t, 0, -height);
            };
            void (^animEnd)(BOOL) = ^(BOOL)
            {
                mToolBarHidden = NO;
                mToolBarAnimationEnd = YES;
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
            //date time animation;
            //CGPoint dateViewPoint = mDateTimeView.center;
            //dateViewPoint.y -= mToolBarView.frame.size.height;
            CGAffineTransform dateTransform = mDateTimeView.transform;
            float dateHeight = mDateTimeView.frame.size.height;
            void (^dateTimeAnimBlock) (void) = ^(void) 
            {
                //mDateTimeView.center = dateViewPoint;
                mDateTimeView.transform = CGAffineTransformTranslate(dateTransform, 0, -height);
            };
            void (^dateTimeAnimEnd) (BOOL) = ^(BOOL)
            {
                mDateTimeAnimationEnd = YES;
            };
            [UIView animateWithDuration:0.5 animations:dateTimeAnimBlock completion:dateTimeAnimEnd];
        }
        else
        {
            //CGPoint p = mToolBarView.center;
            //p.y += mToolBarView.frame.size.height;
            CGAffineTransform t = mToolBarView.transform;
            mToolBarView.transform = CGAffineTransformIdentity;
            CGRect frame = mToolBarView.frame;
            mToolBarView.transform = t;
            float height = frame.size.height;
            
            NSLog(@"tool bar height = %f", height);
            void (^animBlock)(void) = ^{
                //mToolBarView.center = p;
                
                //mToolBarView.transform = CGAffineTransformIdentity;
                //mToolBarView.frame = frame;
                mToolBarView.transform = CGAffineTransformTranslate(t, 0, height);
            };
            void (^animEnd)(BOOL) = ^(BOOL){
                mToolBarHidden = YES;
                mToolBarView.hidden = YES;
                mToolBarAnimationEnd = YES;
            };
            [UIView animateWithDuration:0.5 animations:animBlock completion:animEnd];
            
            //CGPoint dateTimePoint = mDateTimeView.center;
            //dateTimePoint.y += mToolBarView.frame.size.height;
            CGAffineTransform dateTransform = mDateTimeView.transform;
            float dateHeight = mDateTimeView.frame.size.height;
            void (^dateTimeAnimBlock) (void) = ^(void)
            {
                //mDateTimeView.center = dateTimePoint;
                mDateTimeView.transform = CGAffineTransformTranslate(dateTransform, 0, height);
            };
            void (^dateTimeAnimEnd) (BOOL) = ^(BOOL)
            {
                mDateTimeAnimationEnd = YES;
            };
            [UIView animateWithDuration:0.5 animations:dateTimeAnimBlock completion:dateTimeAnimEnd];
        }
        mToolBarAnimationEnd = NO;
        mDateTimeAnimationEnd = NO;
    }
}
- (void)update
{
    [mToolBarView updateFrame];
    [self setToolBarAndDateViewTransform];
    [self setPowerViewTransform];
    [self setDrawingWaitingViewTransform];
    //[self setLightViewTransform];
    [self setDrawViewTransform];
    [self setIndicationTransform];
    mMainDispalyImageView.mRotateScreen = YES;
    [mPowerView update];
    [self setWidgetVisibility];
}
- (void)updateTime
{
    
}
- (void) actionForAnimEnd
{
    mDrawView.hidden = YES;
    PainterManager* pm = [PainterManager painterManager];
    CGRect dstRect = [self getSignatureRect];
    [pm paintSignatureToImage: [mDrawView getCGImage] frame: dstRect];
    [pm promptNextImageDrawAfterAnimation];
}
- (void) startSignatureAnim
{
    //mDateTimeView->mTestUserUpgrade = bTest;
    Signature* sig = [mViewNav getCurrentSignature];
    if(sig != nil)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        if(sd.points != nil && sd.points.count > 0)
        {
            [mDrawView setPointColorArray:sd.colors];
            [mDrawView setNormalizePointsWithOutDraw:sd.points];
        }
        else
        {
            [mDrawView clearPoints];
            [mDrawView setNeedsDisplay];
        }
        mDrawView.hidden = NO;
        [mDrawView setAnimTarget:self action:@selector(actionForAnimEnd)];
        [mDrawView startAnimDraw];
    }

}
//rect coordinate is in frame of reference of upper left origin and x axis to right y axis to down
- (CGRect) getSignatureRect
{
    CGRect rect;
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGSize toolBarSize = [mToolBarView getHintSize];
        CGRect origDrawViewRect = mDrawView.mOrigRect;
        rect.origin.x = mViewNav.mViewPortWidth - toolBarSize.height - origDrawViewRect.size.height;
        rect.origin.y = 12;
        rect.size.width = origDrawViewRect.size.height;
        rect.size.height = origDrawViewRect.size.width;
    }
    else 
    {
        rect = mDrawView.frame;
    }
    return rect;
}
- (void) startDrawingLoading
{
    [mDrawingWaitingView start];
}
- (void) stopDrawingLoaing
{
    [mDrawingWaitingView stop];
}
- (void) setDrawingLoadingStage: (int)i;
{
    [mDrawingWaitingView setStage:i];
}
- (int) getDrawingLoadingStage
{
    return [mDrawingWaitingView getStage];
}
- (void) dismissFrameView
{
    mLeftFrameView.hidden = YES;
    mRightFrameView.hidden = YES;
    mTopFrameView.hidden = YES;
    mBottomFrameView.hidden = YES;
    mLeftFrameView.image = nil;
    mRightFrameView.image = nil;
    mTopFrameView.image = nil;
    mBottomFrameView.image = nil;
}

- (void) frameAnimEndHandler: (BOOL) bShow
{
    NSLog(@"enter anim end handler ");
    for(int i = 0 ; i < 4 ; i++)
    {
        NSLog(@"animFinished %d = %d ", i, animFinished[i]);
        if(animFinished[i] == NO)
            return;
    }
    if(bShow == NO)
    {
        [self dismissFrameView];
    }
}
- (void) setFrameViewImage
{
    mTopFrameView.image = mFrameImage;
    mBottomFrameView.image = mFrameImage;
    mRightFrameView.image = mFrameImage;
    mLeftFrameView.image = mFrameImage;
}
- (void) playFrameShowAnim
{
    if(mTopFrameView == nil || mBottomFrameView == nil || mLeftFrameView == nil || mRightFrameView == nil)
        return;
    if(mTopFrameView.hidden == NO || mBottomFrameView.hidden == NO || mLeftFrameView.hidden == NO || mRightFrameView.hidden == NO)
        return;
    mLeftFrameView.hidden = NO;
    mRightFrameView.hidden = NO;
    mTopFrameView.hidden = NO;
    mBottomFrameView.hidden = NO;
    [self setFrameViewImage];
    for(int i = 0 ; i < 4 ; i++)
    {
        animFinished[i] = NO;
    }
    CGPoint leftCenter = mLeftFrameCenter;
    mLeftFrameView.center = CGPointMake(leftCenter.x - 10, leftCenter.y);
    void (^leftAnimEndBlock) (BOOL) = ^(BOOL){
        animFinished[0] = YES;
        [self frameAnimEndHandler: YES];
    };
    void (^leftAnimBlock)(void) = ^{
        mLeftFrameView.center = leftCenter;
    };
    [UIView animateWithDuration:1.5 animations:leftAnimBlock completion:leftAnimEndBlock];
    
    CGPoint rightCenter = mRightFrameCenter;
    mRightFrameView.center = CGPointMake(rightCenter.x + 10, rightCenter.y);
    void (^rightAnimEndBlock)(BOOL) = ^(BOOL) {
        animFinished[1] = YES;
        [self frameAnimEndHandler: YES];
    };
    void (^rightAnimBlock) (void) = ^{
        mRightFrameView.center = rightCenter;
    };
    [UIView animateWithDuration:1.5 animations:rightAnimBlock completion:rightAnimEndBlock];
    
    CGPoint topCenter = mTopFrameCenter;
    mTopFrameView.center = CGPointMake(topCenter.x, topCenter.y - 10);
    void (^topAnimEndBlock)(BOOL) = ^(BOOL) {
        animFinished[2] = YES;
        [self frameAnimEndHandler: YES];
    };
    void (^topAnimBlock)(void) = ^{
        mTopFrameView.center = topCenter;
    };
    [UIView animateWithDuration:1.5 animations:topAnimBlock completion:topAnimEndBlock];
    
    CGPoint bottomCenter = mBottomFrameCenter;
    mBottomFrameView.center = CGPointMake(bottomCenter.x, bottomCenter.y + 10);
    void (^bottomAnimEndBlock)(BOOL) = ^(BOOL)
    {
        animFinished[3] = YES;
        [self frameAnimEndHandler: YES];
    };
    void (^bottomAnimBlock) (void) = ^{
        mBottomFrameView.center = bottomCenter;
    }; 
    [UIView animateWithDuration:1.5 animations:bottomAnimBlock completion:bottomAnimEndBlock];    
}
- (void) drawIndication: (NSTimer*)timer
{
    mCurrentIndicatorViewTime -= 30;
    float alpha = mCurrentIndicatorViewTime / (float)mIndicatorViewTime;
    alpha = mCurrentIndicatorViewTime / (7.0 * 1000);
    //NSLog(@"alpha = %f", alpha);
    if(alpha < 0)
        alpha = 0;
    if(alpha > 1)
        alpha = 1;
    mIndicationView.alpha = alpha;
    if(mCurrentIndicatorViewTime <= 0)
    {
        mIndicationView.hidden = YES;
        [timer invalidate];
    }
}
- (void) setIndicationTransform
{
    float width = 700;
    float height = 100;
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGAffineTransform t1 = CGAffineTransformMakeTranslation(-width / 2, -height / 2);
        CGAffineTransform t2 = CGAffineTransformRotate(t1, -3.1415926 / 2);
        CGAffineTransform t3 = CGAffineTransformTranslate(t2, width/2, height / 2);
        float yTranslate = mViewNav.mViewPortWidth / 2;
        float xTranslate = mViewNav.mViewPortHeight - (mViewNav.mViewPortHeight - width) / 2;
        CGAffineTransform t4 = CGAffineTransformTranslate(t3, -xTranslate, yTranslate);
        mIndicationView.transform = CGAffineTransformIdentity;
        mIndicationView.frame = CGRectMake(0, 0, width, height);
        mIndicationView.transform = t4;
    }
    else 
    {
        CGRect r = CGRectMake((self.frame.size.width - width)/ 2, self.frame.size.height / 2, width, height);
        mIndicationView.transform = CGAffineTransformIdentity;
        mIndicationView.frame = r;
    }
}
- (void) hideIndicationView
{
    mIndicationView.hidden = YES;
}
- (void) showIndicationView: (NSString*) str time: (int) time
{
    if(mIndicationView == nil)
    {
        float width = 700;
        float height = 100;
        CGRect r = CGRectMake((self.frame.size.width - width)/ 2, self.frame.size.height / 2, width, height);
        mIndicationView = [[FontLabel alloc] initWithFrame:r zFont:[[FontManager sharedManager] zFontWithName:[SESystemConfig getFontName] pointSize:30]];
        mIndicationView.backgroundColor = [UIColor clearColor];
        mIndicationView.textColor = [UIColor whiteColor];
        mIndicationView.textAlignment = UITextAlignmentCenter;
        mIndicationView.numberOfLines = 2;
        mIndicationView.lineBreakMode = UILineBreakModeWordWrap;
        [self addSubview:mIndicationView];
        [mIndicationView release];
    }
    mIndicationView.hidden = NO;
    [self setIndicationTransform];
    mIndicationView.text = str;
    mIndicationView.hidden = NO;
    mCurrentIndicatorViewTime = time * 1000;
    mIndicatorViewTime = time * 1000;
    NSTimer* indicationTimer = [NSTimer timerWithTimeInterval:0.03 target:self selector:@selector(drawIndication:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:indicationTimer forMode:NSDefaultRunLoopMode];
    
}
- (void) playFrameHideAnim
{
    if(mLeftFrameView == nil || mRightFrameView == nil || mTopFrameView == nil || mBottomFrameView == nil)
        return;
    if(mLeftFrameView.hidden == YES || mRightFrameView.hidden == YES || mTopFrameView.hidden == YES || mBottomFrameView.hidden == YES)
        return;
    for(int i = 0 ; i < 4 ; i++)
    {
        animFinished[i] = NO;
    }
    CGPoint leftCenter = mLeftFrameCenter;
    leftCenter.x = leftCenter.x - 10;

    void (^leftAnimEndBlock) (BOOL) = ^(BOOL){
        animFinished[0] = YES;
        [self frameAnimEndHandler: NO];
    };
    void (^leftAnimBlock)(void) = ^{
        mLeftFrameView.center = leftCenter;
    };
    [UIView animateWithDuration:1.5 animations:leftAnimBlock completion:leftAnimEndBlock];
    
    CGPoint rightCenter = mRightFrameCenter;
    rightCenter.x = rightCenter.x + 10;
    void (^rightAnimEndBlock)(BOOL) = ^(BOOL) {
        animFinished[1] = YES;
        [self frameAnimEndHandler: NO];
    };
    void (^rightAnimBlock) (void) = ^{
        mRightFrameView.center = rightCenter;
    };
    [UIView animateWithDuration:1.5 animations:rightAnimBlock completion:rightAnimEndBlock];
    
    CGPoint topCenter = mTopFrameCenter;
    topCenter.y -= 10;
    void (^topAnimEndBlock)(BOOL) = ^(BOOL) {
        animFinished[2] = YES;
        [self frameAnimEndHandler: NO];
    };
    void (^topAnimBlock)(void) = ^{
        mTopFrameView.center = topCenter;
    };
    [UIView animateWithDuration:1.5 animations:topAnimBlock completion:topAnimEndBlock];
    
    CGPoint bottomCenter = mBottomFrameCenter;
    bottomCenter.y += 10;
    void (^bottomAnimEndBlock)(BOOL) = ^(BOOL)
    {
        animFinished[3] = YES;
        [self frameAnimEndHandler: NO];
    };
    void (^bottomAnimBlock) (void) = ^{
        mBottomFrameView.center = bottomCenter;
    }; 
    [UIView animateWithDuration:1.5 animations:bottomAnimBlock completion:bottomAnimEndBlock];    
}
- (float) getToolBarHeight
{
    CGSize s = [mToolBarView getHintSize];
    return s.height;
}
- (CGImageRef) createSignatureImageWithPoints: (NSArray*)points colors: (NSArray*)colors
{
    return [mDrawView createCGImageWithFrame:CGRectMake(0, 0, mDrawView.mOrigRect.size.width, mDrawView.mOrigRect.size.height) points:points color:colors lineWidthRatio:[mDrawView getLineWidthRatio]];
}
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObject:[NSNumber numberWithUnsignedChar:'O']];
    return ret;
}
@end
