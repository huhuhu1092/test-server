//
//  SEPopupView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-4-12.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEPopupView.h"

@implementation SEPopupView
@synthesize mContentView;
- (id)initWithFrame:(CGRect)frame
{
    frame = CGRectMake(0, 0, frame.size.width, frame.size.height);
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}
- (id) init
{
    self = [super init];
    if(self)
    {
        self.backgroundColor = [UIColor clearColor];
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
- (void)showAt: (CGPoint)p parent: (UIView*)parent
{
    if(isShow)
        return;
    self.frame = CGRectMake(p.x, p.y, self.frame.size.width, self.frame.size.height);
    CGRect bounds = self.bounds;
    mContentView.frame = CGRectMake((bounds.size.width - mContentView.bounds.size.width) / 2, (bounds.size.height - mContentView.bounds.size.height) / 2, mContentView.bounds.size.width, mContentView.bounds.size.height);
    [self addSubview:mContentView];
    [parent addSubview:self];
    isShow = YES;
    CGAffineTransform m = CGAffineTransformMakeScale(0.5, 0.5);
    self.transform = m;
    void (^animBlock)(void) = ^{
        self.transform = CGAffineTransformIdentity;
    };
    void (^animEnd) (BOOL) = ^(BOOL){

    };
    int opts = UIViewAnimationOptionCurveLinear;
    [UIView animateWithDuration:0.2 delay: 0 options: opts animations:animBlock completion:animEnd];
}
- (void) dealloc
{
    [mContentView release];
    [super dealloc];
}
- (void)dismiss
{
    [self removeFromSuperview];
    isShow = NO;
}
@end
