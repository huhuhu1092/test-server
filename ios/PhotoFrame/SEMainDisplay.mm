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
struct ToolBarButtonIcon
{
    TOOLBAR_BUTTON_TYPE toolBarButtonType;
    NSString* iconKey;
};
ToolBarButtonIcon gToolbarIcons[] = {
    {OPTION, @"ToolBarOptionIcon"},
    {PLAY_PAUSE, @"ToolBarPlayPauseIcon"},
    {PREVIEW, @"ToolBarPreviewIcon"}
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
@implementation SEMainDisplayToolBarView
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
            mButtons[i].imageView.image = icon;
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
    self = [super init];
    if(self)
    {
        mFrame = r;
        mResLoader =resLoader;
        mRootView = [[UIView alloc] initWithFrame:mFrame];
        mToolBarHidden = YES;
    }
    return self;
}
- (void) setTapGestureToMainDisplay
{
    UITapGestureRecognizer* ges = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
    [mMainDispalyImageView addGestureRecognizer:ges];
    [ges release];
}
- (void)loadView
{
    CGRect frame = CGRectMake(0, 0, mFrame.size.width, mFrame.size.height);
    mMainDispalyImageView = [[PHImageView alloc] initWithFrame:frame];
    [self setTapGestureToMainDisplay];
    UIImage* toolBarBg = [mResLoader getImage:@"MainDisplayToolBarBg"];
    CGSize toolBarSize = toolBarBg.size;
    frame = CGRectMake(0, mFrame.size.height, toolBarSize.width, toolBarSize.height);
    mToolBarView = [[SEMainDisplayToolBarView alloc] initWithFrame:frame withResLoader:mResLoader];
    mToolBarView.hidden = YES;
    [mRootView addSubview:mMainDispalyImageView];
    [mRootView addSubview:mToolBarView];
    self.view = mRootView;
}
- (void) dealloc
{
    [mRootView release];
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
            CGPoint p = mToolBarView.center;
            p.y -= mToolBarView.frame.size.height;
            void(^animBlock)(void) = ^(void){
                mToolBarView.center = p;
            };
            void (^animEnd)(BOOL) = ^(BOOL)
            {
                mToolBarView = NO;
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
@end
