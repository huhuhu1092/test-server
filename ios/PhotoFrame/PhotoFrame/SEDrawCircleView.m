//
//  SEDrawCircleView.m
//  PhotoFrame
//
//  Created by 陈勇 on 12-11-29.
//  Copyright (c) 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEDrawCircleView.h"

@implementation SEDrawCircleView
- (float) quadicRoot1: (float)a : (float)b : (float)c
{
    return (-b + sqrtf(b * b - 4 * a * c)) / 2 * a;
}
- (float) quadicRoot2: (float)a : (float)b : (float)c
{
    return (-b - sqrtf(b * b - 4 * a * c)) / 2 * a;
}
- (void) timerUpdate: (NSTimer*)timer
{
    mCurrentTime += 20;
    float t = mCurrentTime / 1000;
    if(t > 1)
    {
        mCurrentTime = -20;
        return;
    }
    [mResultPoints removeAllObjects];
    for(int i = 0 ; i < mCirclePoints.count ; i++)
    {
        CGPoint p1 = [[mCirclePoints objectAtIndex:i] CGPointValue];
        CGPoint delta = [[mDeltaPoints objectAtIndex:i] CGPointValue];
        CGPoint p;
        p.x = p1.x + delta.x * t;
        p.y = p1.y + delta.y * t;
        [mResultPoints addObject:[NSValue valueWithCGPoint:p]];
    }
    [self setNeedsDisplay];
}
- (void) createData
{
    float radius = 50;
    float xradius2 = 4 * radius * radius;
    float yradius2 = radius * radius;
    float pi = 3.1415926;
    NSMutableArray* circlePoints = [NSMutableArray array];
    NSMutableArray* ellipsePoints = [NSMutableArray array];
    
    for(float i = 0 ; i < 360 ; i += 0.1)
    {
        float radian = pi * i / 180;
        float x = radius * cosf(radian);
        float y = radius * sinf(radian);
        float m = 0;
        float b = 0;
        CGPoint p = CGPointMake(x, y);
        if(i != 90 || i != 270)
        {
            m = tanf(radian);
            b = y - m * x;
            float qa = 1 / 4 + m * m;
            float qb = 2 * m * b;
            float qc = b * b - radius * radius;
            float answerx1 = [self quadicRoot1:qa:qb :qc];
            float answerx2 = [self quadicRoot2:qa :qb :qc];
            //NSLog(@"x1 = %f, x2 = %f",answerx1, answerx2);
            float answery1 = 0;
            NSValue* v1 = nil;
            //float answery2 = 0;
            if(answerx1 * x > 0)
            {
                if(y > 0)
                    answery1 = sqrtf(radius * radius - answerx1 * answerx1 / 4);
                else 
                {
                    answery1 = -sqrtf(radius * radius - answerx1 * answerx1 / 4);
                }
                v1 = [NSValue valueWithCGPoint:CGPointMake(answerx1, answery1)];
            }
            else
            {
                assert(answerx2 * x >= 0);
                if(y > 0)
                {
                    answery1 = sqrtf(radius * radius - answerx2 * answerx2 / 4);
                }
                else
                {
                    answery1 = -sqrtf(radius * radius - answerx2 * answerx2 / 4);
                }
                v1 = [NSValue valueWithCGPoint:CGPointMake(answerx2, answery1)];
            }
            //float answery1 = sqrtf(radius * radius - answerx1 * answerx1 / 4);
            //float answery2 = -sqrtf(radius * radius - answerx1 * answerx1 / 4);
            //float answery3 = sqrtf(radian * radian - answerx2 * answerx2 / 4);
            //float answery4 = -sqrtf(radian * radian - answerx2 * answerx2 / 4);
            NSValue* v = [NSValue valueWithCGPoint:p];
            [circlePoints addObject:v];
            //NSValue* v1 = [NSValue valueWithCGPoint:CGPointMake(answerx1, answery1)];
            //NSValue* v2 = [NSValue valueWithCGPoint:CGPointMake(answerx1, answery2)];
            //NSValue* v3 = [NSValue valueWithCGPoint:CGPointMake(answerx2, answery3)];
            //NSValue* v4 = [NSValue valueWithCGPoint:CGPointMake(answerx2, answery4)];
            NSMutableArray* array = [NSMutableArray array];
            [array addObject:v1];
            //[array addObject:v2];
            //[array addObject: v3];
            //[array addObject:v4];
            [ellipsePoints addObject:array];
        }
        else
        {
            CGPoint p1 = CGPointMake(x, y);
            CGPoint p2 = CGPointMake(x, -y);
            NSValue* v = [NSValue valueWithCGPoint:p];
            [circlePoints addObject:v];
            
            NSValue* v1 = [NSValue valueWithCGPoint:p1];
            //NSValue* v2 = [NSValue valueWithCGPoint:p2];
            //NSValue* v3 = [NSValue valueWithCGPoint:p1];
            //NSValue* v4 = [NSValue valueWithCGPoint:p2];
            //\\NSArray* array = [NSArray arrayWithObjects:v1, v2,v3, v4, nil];
            NSArray* array = [NSArray arrayWithObject:v1];
            [ellipsePoints addObject:array];
            
        }
    }
    mCirclePoints = [circlePoints retain];
    mEllipsePoints = [ellipsePoints retain];
    mDeltaPoints = [[NSMutableArray array] retain];
    mResultPoints = [[NSMutableArray array] retain];
    for(int i = 0 ; i < circlePoints.count ; i++)
    {
        CGPoint p1 = [[circlePoints objectAtIndex:i] CGPointValue];
        CGPoint p2 = [[[ellipsePoints objectAtIndex:i] objectAtIndex:0] CGPointValue];
        CGPoint p;
        p.x = p2.x - p1.x;
        p.y = p2.y - p1.y;
        [mDeltaPoints addObject:[NSValue valueWithCGPoint:p]];
    }
    mCurrentTime = -20;
    mUpdateTimer = [[NSTimer timerWithTimeInterval:0.2 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES] retain];
    [[NSRunLoop currentRunLoop] addTimer:mUpdateTimer forMode:NSDefaultRunLoopMode];

}
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
        [self createData];
    }
    return self;
}


// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    float x = self.frame.size.width / 2;
    float y = self.frame.size.height / 2;
    CGContextConcatCTM(context, CGAffineTransformMakeTranslation(x, y));
    UIColor* color = [UIColor redColor];
    [color set];
    for(int i = 0 ; i < mCirclePoints.count ; i++)
    {
        NSValue* v = [mCirclePoints objectAtIndex:i];
        CGPoint pt = [v CGPointValue];
        float lineWidth = 1.0;
        CGContextFillEllipseInRect(context, CGRectMake(pt.x , pt.y , lineWidth, lineWidth));
        NSArray* array = [mEllipsePoints objectAtIndex:i];
        CGPoint p1 = [[array objectAtIndex:0] CGPointValue];
        //CGPoint p2 = [[array objectAtIndex:1] CGPointValue];
        //CGPoint p3 = [[array objectAtIndex:2] CGPointValue];
        //CGPoint p4 = [[array objectAtIndex:3] CGPointValue];
        CGContextFillEllipseInRect(context, CGRectMake(p1.x , p1.y , lineWidth, lineWidth));
        if(mResultPoints.count > 0)
        {
            CGPoint p2 = [[mResultPoints objectAtIndex:i] CGPointValue];
            CGContextFillEllipseInRect(context, CGRectMake(p2.x , p2.y , lineWidth, lineWidth));
        }
    }
    CGContextRestoreGState(context);
}

@end
