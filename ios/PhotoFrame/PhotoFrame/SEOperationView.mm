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
#import "PhotoFrameAppDelegate.h"
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
@synthesize foregroundImageNormalStr;
@synthesize foregroundImageHighlightedStr;
@synthesize backgroundImageNormalStr;
@synthesize backgroundImageHighlightedStr;
- (id)initWithFrame:(CGRect)frame resources: (NSArray*) resourceArray
{
    self = [super initWithFrame:frame];
    if (self) 
    {
        // Initialization code
        mViewNav = [PhotoFrameAppDelegate getViewNavigator];
        self.backgroundImageNormalStr = [resourceArray objectAtIndex:0];
        self.foregroundImageNormalStr = [resourceArray objectAtIndex:1];
        self.backgroundImageHighlightedStr = [resourceArray objectAtIndex:2];
        self.foregroundImageHighlightedStr = [resourceArray objectAtIndex:3];
        mHighlighted = YES;
        [self setHighlighted:NO];
        float bgViewWidth = mBgImage.size.width;
        float bgViewHeight = mBgImage.size.height;
        mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, bgViewWidth, bgViewHeight)];
        float foreImageWidth = mForeImage.size.width / 2;
        float foreImageHeight = mForeImage.size.height / 2;
        CGFloat startx = (bgViewWidth - foreImageWidth) / 2;
        CGFloat starty = (bgViewHeight - foreImageHeight) / 2;
        mForegroundView = [[UIImageView alloc] initWithFrame:CGRectMake(startx, starty, foreImageWidth, foreImageHeight)];
        mBackgroundView.image = mBgImage;
        mForegroundView.image = mForeImage;
        [self addSubview:mBackgroundView];
        [self addSubview:mForegroundView];
        [mBackgroundView release];
        [mForegroundView release];
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
    [mType release];
    [backgroundImageHighlightedStr release];
    [backgroundImageNormalStr release];
    [foregroundImageHighlightedStr release];
    [foregroundImageNormalStr release];
    [super dealloc];
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
            mBgImage = [mViewNav.mResLoader getImage:backgroundImageHighlightedStr];
            mBackgroundView.image = mBgImage;
            
            mForeImage = [mViewNav.mResLoader getImage:foregroundImageHighlightedStr];
            mForegroundView.image = mForeImage;
        }
        else
        {
            mBgImage = [mViewNav.mResLoader getImage:backgroundImageNormalStr];
            mForeImage = [mViewNav.mResLoader getImage:foregroundImageNormalStr];
            mBackgroundView.image = mBgImage;
            mForegroundView.image = mForeImage;
        }
    }
}
@end

@implementation SEOperationViewGroup
@synthesize mOperationHandler;
//@synthesize mViewNav;
- (void) initData
{
    CGFloat startx = 0;
    CGFloat height = 0;
    for(int i = 0 ; i < mOperators.count ; i++)
    {
        SEOperationView* opv = [[SEOperationView alloc] initWithFrame:CGRectMake(0, 0, 10, 10) resources:[mImageResources objectAtIndex:i]];
        opv.mType = [mOperators objectAtIndex: i];
        CGRect frame = [opv calculateFrame];
        opv.frame = CGRectMake(startx, 0, frame.size.width, frame.size.height);
        startx += frame.size.width;
        height = frame.size.height;
        [self addSubview:opv];
        [mOperationViews addObject: opv];
        [opv release];
    }
    self.frame = CGRectMake(0, 0, startx, height);
}
- (id) initWithOperators:(NSArray*) operators  withImageResource: (NSArray*) resources
{
    self = [super init];
    if(self)
    {
        mOperators = [[NSMutableArray arrayWithArray:operators] retain];
        mOperationViews = [[NSMutableArray arrayWithArray:[NSArray array]] retain];
        mImageResources = [[NSMutableArray arrayWithArray:resources] retain];
        assert(mOperators.count == mImageResources.count);
        [self initData];
    }
    return self;
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
- (BOOL) intersectOperationView: (CGRect) rect
{
    BOOL found = NO;
    for(int i = 0 ; i < mOperationViews.count ; i++)
    {
        SEOperationView* opv = [mOperationViews objectAtIndex:i];
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
    return found;
}
- (void) dealloc
{
    [mOperators release];
    [mOperationViews release];
    [mImageResources release];
    [mOperationHandler release];
    [super dealloc];
}
- (void) operate
{
    for(int i = 0 ; i < mOperationViews.count ; i++)
    {
        SEOperationView* opView = [mOperationViews objectAtIndex:i];
        if([opView highlighted])
        {
            [mOperationHandler handleOperation: opView.mType view:opView];
            break;
        }
    }
}
@end
