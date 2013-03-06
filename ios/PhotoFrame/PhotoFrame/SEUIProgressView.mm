//
//  SEUIProgressView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-7-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUIProgressView.h"
#import "SEViewNavigator.h"
#import "SEResDefine.h"
#import "SEUtil.h"
#import "PhotoFrameAppDelegate.h"
@implementation SEUIProgressView
@synthesize mBackgroundImageStr;
@synthesize mForegroundImageStr;
- (void)dealloc
{
    [mBackgroundImageStr release];
    [mForegroundImageStr release];
    [mGroupForgroundViewArray release];
    [mForegroundImageGroupStr release];
    [mGroupPercentArray release];
    [super dealloc];
}

- (void) createChild: (CGRect)frame
{
    float x = 6;
    
    mDefaultForegroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mDefaultForegroundView];
    [mDefaultForegroundView release];
    
    mForegroundView = [[UIImageView alloc] initWithFrame:CGRectMake(x, 0, frame.size.width - 2 * x, frame.size.height)];
    [self addSubview:mForegroundView];
    [mForegroundView release];
    
    mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mBackgroundView];
    [mBackgroundView release];
    
    
    
    ///////
    self.backgroundColor = [UIColor clearColor];
    mForegroundView.hidden = YES;
}
- (id) initWithCoder:(NSCoder *)aDecoder
{
    self = [super initWithCoder:aDecoder];
    if(self)
    {
        [self createChild:self.frame];
    }
    return self;
}
-(id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];
    }
    return self;
}
/*
- (void) drawRect:(CGRect)rect
{
    [mBackground drawInRect:rect];
    if(percent > 0)
    {
        CGRect fgRect = CGRectMake(rect.origin.x, rect.origin.y, rect.size.width * percent, rect.size.height);
        [mForeground drawInRect:fgRect];
    }
}
 */
- (CGFloat) percent
{
    return percent;
}
- (void) setPercent:(CGFloat)p
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    float x = 6;
    if(fabsf(percent - p) <= 0.00001)
    {
        return;
    }
    percent = p;
    mForegroundView.hidden = NO;
    mForegroundView.frame = CGRectMake(x, 0, (self.frame.size.width - 2 * x)* percent, self.frame.size.height);
    
    UIImage* image = nil;
    if(mForegroundImageStr == nil)
    {
        image = [viewNav.mResLoader getImage:@"UserUpgradeProgressViewForeground"];
    }
    else {
        image = [viewNav.mResLoader getImage:mForegroundImageStr];
    }
    mForegroundView.image = image;
    [self setNeedsDisplay];
    [mBackgroundView setNeedsDisplay];
    [mForegroundView setNeedsDisplay];
    [mDefaultForegroundView setNeedsDisplay];
}
- (void) setForegroundImage: (UIImage*)image
{
    mForegroundView.image = image;
}
- (void) initData: (SEViewNavigator*)viewNav
{
    
    UIImage* pvBg = nil;
    if(mBackgroundImageStr == nil)
        pvBg = [viewNav.mResLoader getImage:@"UserUpgradeProgressViewBackground"];
    else {
        pvBg = [viewNav.mResLoader getImage:mBackgroundImageStr];
    }
    UIImage* pvFg = nil;
    if(mForegroundImageStr == nil)
    {
        pvFg = [viewNav.mResLoader getImage:@"UserUpgradeProgressViewForeground"];
    }
    else {
        pvFg = [viewNav.mResLoader getImage:mForegroundImageStr];
    }
    UIImage* pvDefault = [viewNav.mResLoader getImage:@"OptionsUIProgress"];
    //self.mBackground = [pvBg resizableImageWithCapInsets:UIEdgeInsetsMake(0, 10, 0, 10)];
    //self.mForeground = [pvFg resizableImageWithCapInsets:UIEdgeInsetsMake(0, 10, 0, 10)];
    mBackgroundView.image = pvBg;
    mForegroundView.image = pvFg;
    mDefaultForegroundView.image = pvDefault;
}
- (void) setForegroundGroupImageStr: (NSArray*) imageStrArray
{
    [mForegroundImageGroupStr release];
    mForegroundImageGroupStr = [[NSMutableArray array] retain];
    for(int i = 0 ; i < imageStrArray.count ; i++)
    {
        [mForegroundImageGroupStr addObject:[imageStrArray objectAtIndex:i]];
    }
    for(int i = 0 ; i < mGroupForgroundViewArray.count ; i++)
    {
        UIView* v = [mGroupForgroundViewArray objectAtIndex:i];
        [v removeFromSuperview];
    }
    [mGroupForgroundViewArray release];
    mGroupForgroundViewArray = [[NSMutableArray array] retain];
    for(int i = 0 ; i < imageStrArray.count ; i++)
    {
        UIImageView* view = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 10, 10)];
        [self addSubview:view];
        [view release];
        [mGroupForgroundViewArray addObject:view];
    }
    [self bringSubviewToFront:mBackgroundView];
    [mForegroundView removeFromSuperview];
    mForegroundView = nil;
}
- (void) setGroupPercent: (NSArray*)percentArray
{
    [mGroupPercentArray release];
    mGroupPercentArray = [[NSMutableArray array] retain];
    for(int i = 0 ; i < percentArray.count ; i++)
    {
        [mGroupPercentArray addObject:[percentArray objectAtIndex:i]];
    }
    float x = 6;
    float everyPartMostWidth = (self.frame.size.width - 2 * x) / mGroupPercentArray.count;
    assert(percentArray.count == mGroupForgroundViewArray.count);
    float startx = x;
    NSMutableArray* everyPartWidth = [NSMutableArray array];
    for(int i = 0 ; i < percentArray.count ; i++)
    {
        float currentPercent = [[percentArray objectAtIndex:i] floatValue];
        float width = currentPercent * everyPartMostWidth;
        [everyPartWidth addObject:[NSNumber numberWithFloat:width]];
    }
    for(int i = 0 ; i < percentArray.count ; i++)
    {
        UIImageView* view = [mGroupForgroundViewArray objectAtIndex:i];
        float width = [[everyPartWidth objectAtIndex:i] floatValue];
        view.frame = CGRectMake(startx, 0, width, self.frame.size.height);
        startx += width;
        UIImage* image = [[[PhotoFrameAppDelegate getViewNavigator] mResLoader] getImage:[mForegroundImageGroupStr objectAtIndex:i]];
        view.image = image;
    }
}
- (NSArray*) getGroupPercent
{
    NSArray* ret = [NSArray array];
    for(int i = 0 ; i < mGroupPercentArray.count ; i++)
    {
        ret = [ret arrayByAddingObject:[mGroupPercentArray objectAtIndex:i]];
    }
    return ret;
}
@end
