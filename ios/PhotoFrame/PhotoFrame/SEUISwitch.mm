//
//  SEUISwitch.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-7-9.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUISwitch.h"
#import "SEViewNavigator.h"
#import "SEResDefine.h"
@implementation SEUISwitch
@synthesize isOn = mIsOn;
enum {MOVE_TO_LEFT, MOVE_TO_RIGHT};
- (void) setViewToOnPosition
{
    float startx = self.frame.size.width / 2;
    mOnOffImageView.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height);
    mMidImageView.frame = CGRectMake(startx, 0, mMidWidth, self.frame.size.height);
    //mOffImageView.frame = CGRectMake(self.frame.size.width - mMidWidth, 0, self.frame.size.width, self.frame.size.height);
}
- (void) setViewToOffPosition
{
    //mOnImageView.frame = CGRectMake(-self.frame.size.width + mMidWidth, 0, self.frame.size.width, self.frame.size.height);
    mOnOffImageView.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height);
    mMidImageView.frame = CGRectMake(0, 0, mMidWidth, self.frame.size.height);
    //mOffImageView.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height);
}
- (void) setData
{
    if(mIsOn)
    {
        [self setViewToOnPosition];
    }
    else
    {
        [self setViewToOffPosition];
    }
}
- (BOOL) animAllEnd
{
    BOOL end = mAnimStart == NO;
    /*
    for(int i = 0 ; i < 3 ; i++)
    {
        if(mAnimStart[i] == YES)
        {
            end = NO;
            break;
        }
    }
     */
    return end;
}
- (void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mTouchState == TOUCH_START)
    {
        return;
    }
    if([self animAllEnd] == NO)
        return;
    mTouchState = TOUCH_START;
    mMidViewOnEdge = NO;
    CGPoint p = [[touches anyObject] locationInView:self];
    mPrevTouchPoint = p;
}

- (void) allAnimStart
{
    /*
    for(int i = 0 ; i < 3 ; i++)
    {
        mAnimStart[i] = YES;
    }
     */
    mAnimStart = YES;
}
- (void) setOnValue: (int)dir
{
    if([self animAllEnd])
    {
        if(dir == MOVE_TO_LEFT)
        {
            mIsOn = NO;
        }
        else 
        {
            mIsOn = YES;
        }
        [mTarget performSelector:mAction withObject:[NSNumber numberWithBool:mIsOn]];
    }
}
- (void)startViewAnimation: (CGPoint) midViewDestStartPoint : (CGPoint) onViewDestStartPoint : (CGPoint)offViewDestStartPoint : (int) dir
{
    [self allAnimStart];
    void (^animMidBlock) (void) = ^{
        mMidImageView.frame = CGRectMake(midViewDestStartPoint.x, midViewDestStartPoint.y, mMidImageView.frame.size.width, mMidImageView.frame.size.height);;
    };
    void (^animMidEndBlock) (BOOL) = ^(BOOL){
        mAnimStart = NO;
        [self setOnValue:dir];
    };
    [UIView animateWithDuration:0.2 animations:animMidBlock completion:animMidEndBlock];
    /*
    void (^animOnBlock) (void) = ^{
        mOnImageView.frame = CGRectMake(onViewDestStartPoint.x, onViewDestStartPoint.y, mOnImageView.frame.size.width, mOnImageView.frame.size.height);
    };
    void (^animOnEndBlock)(BOOL) = ^(BOOL)
    {
        mAnimStart[1] = NO;
        [self setOnValue:dir];
    };
    [UIView animateWithDuration:0.2 animations:animOnBlock completion:animOnEndBlock];
    
    void (^animOffBlock) (void) =  ^{
        mOffImageView.frame = CGRectMake(offViewDestStartPoint.x, offViewDestStartPoint.y, mOffImageView.frame.size.width, mOffImageView.frame.size.height);
    };
    void (^animOffEndBlock) (BOOL) = ^(BOOL){
        mAnimStart[2] = NO;
        [self setOnValue:dir];
    };
    [UIView animateWithDuration:0.2 animations:animOffBlock completion:animOffEndBlock];
    */
    
}
- (void) startMoveToLeftAnimation
{
    CGPoint midViewDestStartPoint = CGPointMake(0, 0);
    CGPoint onViewDestStartPoint = CGPointMake(mMidWidth, 0);
    CGPoint offViewDestStartPoint = CGPointMake(0, 0);
    [self startViewAnimation:midViewDestStartPoint:onViewDestStartPoint :offViewDestStartPoint : MOVE_TO_LEFT];
}
- (void) startMoveToRightAnimation
{
    CGPoint midViewDestStartPoint = CGPointMake(mMidWidth, 0);
    CGPoint onViewDestStartPoint = CGPointMake(0, 0);
    CGPoint offViewDestStartPoint = CGPointMake(mMidWidth, 0);
    [self startViewAnimation: midViewDestStartPoint : onViewDestStartPoint : offViewDestStartPoint : MOVE_TO_RIGHT];
}
- (void) touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mTouchState != TOUCH_START)
        return;
    mTouchState = TOUCH_END;
    float midPoint = self.frame.size.width / 2;
    if(mMidViewOnEdge == NO)
    {
        if(mMidImageView.center.x < midPoint)
        {
            [self startMoveToLeftAnimation];
        }
        else if(mMidImageView.center.x >= midPoint)
        {
            [self startMoveToRightAnimation];
        }
    }
    else
    {
        if(mMidImageView.center.x > mMidWidth)
        {
            mIsOn = YES;
        }
        else 
        {
            mIsOn = NO;
        }
        [mTarget performSelector:mAction withObject:[NSNumber numberWithBool:mIsOn]];
    }
}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mTouchState != TOUCH_START)
        return;
    mTouchState = TOUCH_END;
    float midPoint = self.frame.size.width / 2;
    if(mMidViewOnEdge == NO)
    {
        if(mMidImageView.center.x < midPoint)
        {
            [self startMoveToLeftAnimation];
        }
        else if(mMidImageView.center.x >= midPoint)
        {
            [self startMoveToRightAnimation];
        }
    }
    else
    {
        if(mMidImageView.center.x > mMidWidth)
        {
            mIsOn = YES;
        }
        else 
        {
            mIsOn = NO;
        }
        [mTarget performSelector:mAction withObject:[NSNumber numberWithBool:mIsOn]];
    }
    
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    if(mTouchState != TOUCH_START)
        return;
    CGPoint p = [[touches anyObject] locationInView:self];
    float deltax = p.x - mPrevTouchPoint.x;
    
    //CGPoint center = mOnImageView.center;
    //mOnImageView.center = CGPointMake(center.x + deltax, center.y);
    //center = mOffImageView.center;
    //mOffImageView.center = CGPointMake(center.x + deltax, center.y);
    CGPoint center = mMidImageView.center;
    mMidImageView.center = CGPointMake(center.x + deltax, center.y);
    
    if(mMidImageView.frame.origin.x < 0)
    {
        [self setViewToOffPosition];
        mMidViewOnEdge = YES;
    }
    if(mMidImageView.frame.origin.x + mMidImageView.frame.size.width > self.frame.size.width)
    {
        [self setViewToOnPosition];
        mMidViewOnEdge = YES;
    }
    mPrevTouchPoint = p;
}
- (void) setOn:(BOOL)bOn animated:(BOOL)animated
{
    if(bOn == mIsOn)
        return;
    BOOL old = mIsOn;
    if(animated)
    {
        if(old)
        {
            [self startMoveToLeftAnimation];
        }
        else 
        {
            [self startMoveToRightAnimation];
        }
    }
    else 
    {
        mIsOn = bOn;
        if(mIsOn)
        {
            [self setViewToOnPosition];
        }
        else
        {
            [self setViewToOffPosition];
        }
    }
}
- (void) tapHandler: (UITapGestureRecognizer*)ges
{
    if(mIsOn)
    {
        [self startMoveToLeftAnimation];
    }
    else 
    {
        [self startMoveToRightAnimation];
    }
}
- (void) initImageView: (SEViewNavigator*)viewNav
{
    mOnOffWidth = self.frame.size.width;
    mMidWidth = self.frame.size.width / 2;
    if(mOnOffImageView == nil)
    {
        mOnOffImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        UIImage* image = [viewNav.mResLoader getImage:@"UISwitchOnOffImage"];
        NSLog(@"on image size = %f, %f", image.size.width, image.size.height);
        mOnOffImageView.image = image;
        [self addSubview:mOnOffImageView];
    }
    /*
    if(mOnImageView == nil)
    {
        mOnImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        UIImage* image = [viewNav.mResLoader getImage:@"UISwitchOnImage"];
        NSLog(@"on image size = %f, %f", image.size.width, image.size.height);
        mOnImageView.image = image;
        [self addSubview:mOnImageView];
    }
    if(mOffImageView == nil)
    {
        mOffImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        UIImage* image = [viewNav.mResLoader getImage:@"UISwitchOffImage"];
        NSLog(@"off image size = %f, %f", image.size.width, image.size.height);
        mOffImageView.image = image;
        [self addSubview:mOffImageView];
    }
     */
    if(mMidImageView == nil)
    {
        mMidImageView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width / 2, self.frame.size.height)];
        UIImage* image = [viewNav.mResLoader getImage:@"UISwitchMidImage"];
        NSLog(@"mid image size = %f, %f", image.size.width, image.size.height);
        mMidImageView.image = image;
        [self addSubview:mMidImageView];
    }
    UITapGestureRecognizer* tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapHandler:)];
    [self addGestureRecognizer:tap];
    [tap release];
    [self setData];
    self.backgroundColor = [UIColor clearColor];
}
- (void) setTarget: (id) target action: (SEL) action
{
    mTarget = target;
    mAction = action;
}
- (void) dealloc
{
    [mMidImageView release];
    [mOnOffImageView release];
    //[mOffImageView release];
    [super dealloc];
}
@end
