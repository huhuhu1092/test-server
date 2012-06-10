//
//  SEOperationView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-4-8.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEOperationView.h"
#import "SEViewNavigator.h"
#import "SEResDefine.h"
//////////
NSString* gOperationBgImages[] = {@"OperationViewShareBackgroiund", @"OperationViewDeleteBackground"};
NSString* gOperationBgHImages[] = {@"OperationViewShareBackgroundH", @"OperationViewDeleteBackgroundH"};
NSString* gOperationForeImages[] = {@"OperationViewShareForeground", @"OperationViewDeleteForeground"};
NSString* gOperationForeHImages[] = {@"OperationViewShareForegroundH" , @"OperationViewDeleteForegroundH"};
///////////////////////////////////////
@implementation SEOperationView
@synthesize mForeImage;
@synthesize mBgImage;
@synthesize mType;
@synthesize mViewNav;
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/
- (CGRect) calculateFrame
{
    return CGRectMake(0, 0, mBgImage.size.width, mBgImage.size.height);
}
- (void)dealloc
{
    [mBackgroundView release];
    [mForegroundView release];
    [mBgImage release];
    [mForeImage release];
    [super dealloc];
}
- (void) initData
{
    mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, mBgImage.size.width, mBgImage.size.height)];
    CGFloat startx = (mBgImage.size.width - mForeImage.size.width) / 2;
    CGFloat starty = (mBgImage.size.height - mForeImage.size.height) / 2;
    mForegroundView = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, mForeImage.size.width, mForeImage.size.height)];
    mBackgroundView.image = mBgImage;
    mForegroundView.image = mForeImage;
    [self addSubview:mBackgroundView];
    [self addSubview:mForegroundView];
}
- (BOOL) highlighted
{
    return mHighlighted;
}
- (void) setHighlighted:(BOOL)h
{
    if(mHighlighted != h)
    {
        mHighlighted = h;
        if(mHighlighted)
        {
            mBgImage = [mViewNav.mResLoader getImage:gOperationBgHImages[(int)mType]];
            mBackgroundView.image = mBgImage;
            
            mForeImage = [mViewNav.mResLoader getImage:gOperationForeHImages[(int)mType]];
            mForegroundView.image = mForeImage;
        }
        else
        {
            mBgImage = [mViewNav.mResLoader getImage:gOperationBgImages[mType]];
            mForeImage = [mViewNav.mResLoader getImage:gOperationForeImages[mType]];
            mBackgroundView.image = mBgImage;
            mForegroundView.image = mForeImage;
        }
    }
}
@end

@implementation SEOperationViewGroup
@synthesize mOperationHandler;
@synthesize mViewNav;
- (CGRect)calculateFrame
{
    CGFloat width = 0;
    CGFloat height = 0;
    for(int i = 0 ; i < OPERATION_NUM ; i++)
    {
        NSString* str = gOperationBgImages[i];
        UIImage* image = [mViewNav.mResLoader getImage:str];
        width += image.size.width;
        height = image.size.height;
    }
    return CGRectMake(0, 0, width, height);
}
- (void) initData
{
    CGFloat startx = 0;
    for(int i = 0 ; i < OPERATION_NUM ; i++)
    {
        SEOperationView* opv = [[SEOperationView alloc] init];
        opv.mType = (OPERATION_TYPE)i;
        opv.mViewNav = mViewNav;
        NSString* strBg = gOperationBgImages[i];
        UIImage* bgImage = [mViewNav.mResLoader getImage:strBg];
        NSString* strFore = gOperationForeImages[i];
        UIImage* foreImage = [mViewNav.mResLoader getImage:strFore];
        opv.mBgImage = bgImage;
        opv.mForeImage = foreImage;
        [opv initData];
        CGRect frame = [opv calculateFrame];
        opv.frame = CGRectMake(startx, 0, frame.size.width, frame.size.height);
        startx += frame.size.width;
        [self addSubview:opv];
        //[opv release];
        mOperationViews[i] = opv;
    }
}
- (BOOL) intersectRect: (CGRect) r1 withRect: (CGRect) r2
{
    if((r1.origin.x + r1.size.width) < r2.origin.x)
        return NO;
    if((r2.origin.x + r2.size.width) < r1.origin.x)
        return NO;
    if((r1.origin.y + r1.size.height) < r2.origin.y)
        return NO;
    if((r2.origin.y + r2.size.height) < r1.origin.y)
        return NO;
    return YES;
}
- (void) intersectOperationView: (CGRect) rect
{
    BOOL found = NO;
    for(int i = 0 ; i < OPERATION_NUM ; i++)
    {
        SEOperationView* opv = mOperationViews[i];
        CGRect r = [self convertRect:rect toView:opv];
        CGRect bounds = opv.bounds;
        //NSLog(@"r = %f, %f, %f, %f", r.origin.x, r.origin.y, r.size.width, r.size.height);
        //NSLog(@"bounds = %f, %f", bounds.size.width, bounds.size.height);
        BOOL bIntersect = [self intersectRect:r withRect:bounds];
        //NSLog(@"bIntersect = %d", bIntersect);
        if(bIntersect && !found)
        {
            opv.highlighted = YES;
            found = YES;
        }
        else
            opv.highlighted = NO;
    }
}
- (void) dealloc
{
    for(int i = 0 ; i < OPERATION_NUM ; i++)
    {
        [mOperationViews[i] release];
    }
    [super dealloc];
}
- (void) operate
{
    for(int i = 0 ; i < OPERATION_NUM ; i++)
    {
        if(mOperationViews[i].highlighted)
        {
            [mOperationHandler handleOperation:(OPERATION_TYPE)i view:mOperationViews[i]];
            break;
        }
    }
}
@end
