//
//  SEMultiTouchDetect.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-3-1.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEMultiTouchDetect.h"

@implementation SEMultiTouchDetect
@synthesize mViewForPoint;
@synthesize mTouchState;
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}
- (void) dealloc
{
    for(int i = 0 ; i < TOUCH_NUM ; i++)
    {
        [mTouchPointArray[0] release];
        [mTouchPointArray[1] release];
    }
    [super dealloc];
}
- (NSMutableArray*) points1
{
    return mTouchPointArray[0];
}
- (NSMutableArray*) points2
{
    return mTouchPointArray[1];
}
- (void) initData
{
    mTouchPointArray[0] = [NSMutableArray array];
    mTouchPointArray[1] = [NSMutableArray array];
    [mTouchPointArray[0] retain];
    [mTouchPointArray[1] retain];
    
}
- (enum LINE_TYPE) pointInLine: (CGPoint) prevPt :(CGPoint)currPt withState: (UITouchPhase) phase;
{
    CGPoint pt1;
    BOOL hasPt1 = NO;
    CGPoint pt2;
    BOOL hasPt2 = NO;
    if([mTouchPointArray[0] count] > 0)
    {
        pt1 = [[mTouchPointArray[0] lastObject] CGPointValue];
        NSLog(@"hasPt1: %f, %f\n", pt1.x, pt1.y);
        hasPt1 = YES;
    }
    if (mTouchPointArray[1].count > 0) 
    {
        pt2 = [[mTouchPointArray[1] lastObject] CGPointValue];
        hasPt2 = YES;
        NSLog(@"hasPt2: %f, %f\n", pt2.x, pt2.y);
        
    }
    NSLog(@"prevPt = %f, %f\n", prevPt.x, prevPt.y);
    NSLog(@"currPt = %f, %f\n", currPt.x, currPt.y);
    if(phase == UITouchPhaseMoved)
    {
        
        if(hasPt1 && prevPt.x == pt1.x && prevPt.y == pt1.y)
        {
            return LINE1;
        }
        else if(hasPt2 && prevPt.x == pt2.x && prevPt.y == pt2.y)
        {
            return LINE2;
        }
        else
        {
            NSLog(@"has no prev point\n");
            return NO_LINE;
        }
    }
    else if(phase == UITouchPhaseEnded)
    {
        
        if(hasPt1 && currPt.x == pt1.x && currPt.y == pt1.y)
        {
            return LINE1;
        }
        else if(hasPt2 && currPt.x == pt2.x && currPt.y == pt2.y)
        {
            return LINE2;
        }
        if(hasPt1 && prevPt.x == pt1.x && prevPt.y == pt1.y)
        {
            return LINE1;
        }
        else if(hasPt2 && prevPt.x == pt2.x && prevPt.y == pt2.y)
        {
            return LINE2;
        }

        else
        {
            NSLog(@"has no prev point\n");
            return NO_LINE;
        }
        
    }
    else
        return NO_LINE;
}
- (void) touchStateChange: (NSSet*) touches
{
    if(mViewForPoint == nil)
        return;
    NSArray* touchObjs = [touches allObjects];
    /*
    for(UITouch* t in touchObjs)
    {
        NSLog(@"### touch = %@ ##\n", t);
    }
     */
    NSUInteger count = [touchObjs count];
    if(count == 1)
    {
        UITouch* touch = [touchObjs objectAtIndex:0];
        switch (touch.phase) 
        {
            case UITouchPhaseBegan:
            {
                if(mTouchState == NO_TOUCH)
                {
                    mTouchState = TOUCH1;
                    assert(mTouches[0] == nil);
                    assert(mTouches[1] == nil);
                    assert(mTouchPointArray[0].count == 0);
                    assert(mTouchPointArray[1].count == 0);
                    CGPoint pt = [[touches anyObject] locationInView:mViewForPoint];
                    mTouches[0] = touch;
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:pt]];
                }
                else if(mTouchState == TOUCH1)
                {
                    mTouchState = TOUCH2;
                    assert(mTouches[0] != nil);
                    assert(mTouches[1] == nil);
                    assert(mTouchPointArray[0].count > 0);
                    assert(mTouchPointArray[1].count == 0);
                    CGPoint pt = [[touches anyObject] locationInView:mViewForPoint];
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:pt]];
                    mTouches[1] = touch;
                }
                else
                {
                    NSLog(@"has more than two finger on touch");
                }
            }
                break;
            case UITouchPhaseMoved:
            {
                CGPoint prevPt = [[touches anyObject] previousLocationInView:mViewForPoint];
                CGPoint currPt = [[touches anyObject] locationInView:mViewForPoint];
                if(mTouches[0] == nil && mTouches[1] == nil)
                    return;
                if(mTouches[0] == touch)
                {
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:currPt]];
                }
                else if(mTouches[1] == touch)
                {
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:currPt]];
                }
                else
                {
                    
                }
                /*
                else
                {
                    assert(0);
                    NSLog(@"touch is not in touches array\n");
                }
                 */
                
                /*
                CGPoint prevPt = [[touches anyObject] previousLocationInView:mViewForPoint];
                CGPoint currPt = [[touches anyObject] locationInView:mViewForPoint];
                LINE_TYPE line = [self pointInLine:prevPt :currPt withState:UITouchPhaseMoved];
                if(line == LINE1)
                {
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:currPt]];
                }
                else if(line == LINE2)
                {
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:currPt]];
                }
                else
                {
                    assert(0);
                    NSLog(@"has no prev point\n");
                }
                */
            }
                break;
            case UITouchPhaseEnded:
            {
                if(mTouchState == TOUCH2)
                {
                    mTouchState = TOUCH1;
                    CGPoint prevPt = [[touches anyObject] previousLocationInView:mViewForPoint];
                    CGPoint currPt = [[touches anyObject] locationInView:mViewForPoint];
                    if(mTouches[0] == touch)
                    {
                        mTouches[0] = mTouches[1];
                        mTouches[1] = nil;
                        [mTouchPointArray[0] removeAllObjects];
                        NSMutableArray* tmp = mTouchPointArray[0];
                        mTouchPointArray[0] = mTouchPointArray[1];
                        mTouchPointArray[1] = tmp;
                    }
                    else if(mTouches[1] == touch)
                    {
                        mTouches[1] = nil;
                        [mTouchPointArray[1] removeAllObjects];
                    }
                    /*
                    else
                    {
                        assert(0);
                    }
                     */
                    /*
                    if([self pointInLine:prevPt :currPt withState: UITouchPhaseEnded] == LINE1)
                    {
                        [mTouchPointArray[0] removeAllObjects];
                        NSMutableArray* tmp = mTouchPointArray[0];
                        mTouchPointArray[0] = mTouchPointArray[1];
                        mTouchPointArray[1] = tmp;
                    }
                    else if([self pointInLine:prevPt :currPt withState: UITouchPhaseEnded] == LINE2)
                    {
                        [mTouchPointArray[1] removeAllObjects];
                    }
                    else
                    {
                        assert(0);
                        NSLog(@"has no prev point\n");
                    }
                     */
                }
                else if(mTouchState == TOUCH1)
                {
                    mTouchState = NO_TOUCH;
                    mTouches[0] = nil;
                    mTouches[1] = nil;
                    [mTouchPointArray[0] removeAllObjects];
                    [mTouchPointArray[1] removeAllObjects];
                }
                else
                {
                    NSLog(@" view touch end with exception\n");
                }
            }
                break;
            case UITouchPhaseCancelled:
            {
                if(mTouchState == TOUCH2)
                {
                    mTouchState = TOUCH1;
                    CGPoint prevPt = [[touches anyObject] previousLocationInView:mViewForPoint];
                    CGPoint currPt = [[touches anyObject] locationInView:mViewForPoint];
                    if(mTouches[0] == touch)
                    {
                        mTouches[0] = mTouches[1];
                        mTouches[1] = nil;
                        [mTouchPointArray[0] removeAllObjects];
                        NSMutableArray* tmp = mTouchPointArray[0];
                        mTouchPointArray[0] = mTouchPointArray[1];
                        mTouchPointArray[1] = tmp;
                    }
                    else if(mTouches[1] == touch)
                    {
                        mTouches[1] = nil;
                        [mTouchPointArray[1] removeAllObjects];
                    }
                    /*
                    else
                    {
                        assert(0);
                    }
                     */
                    /*
                    if([self pointInLine:prevPt :currPt withState:UITouchPhaseEnded] == LINE1)
                    {
                        [mTouchPointArray[0] removeAllObjects];
                        NSMutableArray* tmp = mTouchPointArray[0];
                        mTouchPointArray[0] = mTouchPointArray[1];
                        mTouchPointArray[1] = tmp;
                    }
                    else if([self pointInLine:prevPt :currPt withState:UITouchPhaseEnded] == LINE2)
                    {
                        [mTouchPointArray[1] removeAllObjects];
                    }
                    else
                    {
                        assert(0);
                        NSLog(@"has no prev point\n");
                    }
                    */
                }
                else if(mTouchState == TOUCH1)
                {
                    mTouchState = NO_TOUCH;
                    [mTouchPointArray[0] removeAllObjects];
                    [mTouchPointArray[1] removeAllObjects];
                    mTouches[0] = nil;
                    mTouches[1] = nil;
                }
                else
                {
                    NSLog(@"scroll view touch cancel with exception\n");
                }
            }
                
                break;
            default:
                break;
        }
    }
    else if(count == 2)
    {
        UITouch* touch = [touches anyObject];
        switch (touch.phase) 
        {
            case UITouchPhaseBegan:
            {
                if(mTouchState == NO_TOUCH)
                {
                    mTouchState = TOUCH2;
                    NSArray* t = [touches allObjects];
                    CGPoint pt1 = [[t objectAtIndex:0] locationInView:mViewForPoint];
                    CGPoint pt2 = [[t objectAtIndex:1] locationInView:mViewForPoint];
                    assert(mTouchPointArray[0].count == 0);
                    assert(mTouchPointArray[1].count == 0);
                    assert(mTouches[0] == nil);
                    assert(mTouches[1] == nil);
                    mTouches[0] = [t objectAtIndex:0];
                    mTouches[1] = [t objectAtIndex:1];
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:pt1]];
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:pt2]];
                    NSLog(@"add point int touch 2\n");
                }
                else
                {
                    NSLog(@"scroll view touch count 2 touches began exception\n");
                }
            }
                break;
            case UITouchPhaseMoved:
            {
                NSArray* t = [touches allObjects];
                CGPoint prevPt1 = [[t objectAtIndex:0] previousLocationInView:mViewForPoint];
                CGPoint currPt1 = [[t objectAtIndex:0] locationInView:mViewForPoint];
                
                CGPoint prevPt2 = [[t objectAtIndex:1] previousLocationInView:mViewForPoint];
                CGPoint currPt2 = [[t objectAtIndex:1] locationInView:mViewForPoint];
                UITouch* t1 = [t objectAtIndex:0];
                UITouch* t2 = [t objectAtIndex:1];
                if(mTouches[0] == t1 || mTouches[0] == t2)
                {
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:currPt1]];
                }
                else if(mTouches[1] == t1 || mTouches[1] == t2)
                {
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:currPt1]];
                }
                /*
                else
                {
                    assert(0);
                }
                 */
                /*
                if([self pointInLine:prevPt1 :currPt1 withState:UITouchPhaseMoved] == LINE1)
                {
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:currPt1]];
                }
                else if([self pointInLine:prevPt1 :currPt1 withState:UITouchPhaseMoved] == LINE2)
                {
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:currPt1]];
                }
                else
                {
                    assert(0);
                    NSLog(@"no prev point");
                }
                
                if([self pointInLine:prevPt2 :currPt2 withState:UITouchPhaseMoved] == LINE1)
                {
                    [mTouchPointArray[0] addObject:[NSValue valueWithCGPoint:currPt2]];
                }
                else if([self pointInLine:prevPt2 :currPt2 withState:UITouchPhaseMoved] == LINE2)
                {
                    [mTouchPointArray[1] addObject:[NSValue valueWithCGPoint:currPt2]];
                }
                else
                {
                    assert(0);
                    NSLog(@"no prev point");
                }
                */
            }
                break;
            case UITouchPhaseEnded:
            {
                if(mTouchState == TOUCH2)
                {
                    mTouchState = NO_TOUCH;
                    [mTouchPointArray[0] removeAllObjects];
                    [mTouchPointArray[1] removeAllObjects];
                    mTouches[0] = nil;
                    mTouches[1] = nil;
                    NSLog(@" delete all touch 2\n");
                }
                else
                {
                    NSLog(@"scroll view touch count 2 touches end exception\n");
                }
            }
                break;
            case UITouchPhaseCancelled:
            {
                if(mTouchState == TOUCH2)
                {
                    mTouchState = NO_TOUCH;
                    [mTouchPointArray[0] removeAllObjects];
                    [mTouchPointArray[1] removeAllObjects];
                    mTouches[0] = nil;
                    mTouches[1] = nil;
                    NSLog(@" delete all touch 2\n");
                }
                else
                {
                    NSLog(@"scroll view touch count 2 touches cancel exception\n");
                }
            }
                break;
            default:
                break;
        }
    }
}
- (CGPoint) getCurrentDeltaPoint
{
    if(mViewForPoint == nil)
        return CGPointMake(0, 0);
    if(mTouchPointArray[0].count >= 2)
    {
        CGPoint pt1 = [[mTouchPointArray[0] lastObject] CGPointValue];
        CGPoint pt2 = [[mTouchPointArray[0] objectAtIndex:mTouchPointArray[0].count - 2] CGPointValue];
        return CGPointMake(pt1.x - pt2.x, pt1.y - pt2.y);
    }
    else if(mTouchPointArray[1].count >= 2)
    {
        CGPoint pt1 = [[mTouchPointArray[1] lastObject] CGPointValue];
        CGPoint pt2 = [[mTouchPointArray[1] objectAtIndex:mTouchPointArray[1].count - 2] CGPointValue];
        return CGPointMake(pt1.x - pt2.x, pt1.y - pt2.y);
    }
    else
        return CGPointMake(0, 0);
}

@end
