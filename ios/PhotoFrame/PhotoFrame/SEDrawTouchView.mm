//
//  SEDrawTouchView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-28.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEDrawTouchView.h"
#define POINT(x) [[points objectAtIndex:(x)] CGPointValue]
@implementation SEDrawTouchView
@synthesize background;
@synthesize lineWidth;
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}
- (void) initData
{
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    lineWidth = 8;
}
- (void)dealloc
{
    [background release];
    [pointArrayList release];
    [super dealloc];
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"draw touch view touch begin\n");
    CGPoint pt = [[touches anyObject] locationInView:self];
    NSMutableArray* pointArray = [NSMutableArray array];
    [pointArrayList addObject:pointArray];
    currentPointArray = pointArray;
    [pointArray addObject:[NSValue valueWithCGPoint:pt]];
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    NSLog(@"draw touch view touch move \n");
    CGPoint pt = [[touches anyObject] locationInView:self];
    [currentPointArray addObject: [NSValue valueWithCGPoint:pt]];
    [self setNeedsDisplay];
}
- (void) touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    [self setNeedsDisplay];
}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    [self setNeedsDisplay];
}
- (void) clearPoints
{
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
}
- (NSMutableArray*) getAllNormalizedPoints
{
    if(pointArrayList == nil)
        return nil;
    CGFloat width = self.frame.size.width;
    CGFloat height = self.frame.size.height;
    NSMutableArray* retPointList = [NSMutableArray array];
    for(NSMutableArray* points in pointArrayList)
    {
        if(points.count < 2)
        {
            NSMutableArray* a = [NSMutableArray array];
            CGPoint p = [[points objectAtIndex:0] CGPointValue];
            p.x = p.x / width;
            p.y = p.y  / height;
            [a addObject:[NSValue valueWithCGPoint:p]];
            [retPointList addObject:a];
        }
        else
        {
            NSMutableArray* a = [NSMutableArray array];
            for(int i = 0 ; i < points.count; i++)
            {
                CGPoint pt1 = POINT(i);
                pt1.x = pt1.x / width;
                pt1.y = pt1.y / height;
                [a addObject:[NSValue valueWithCGPoint:pt1]];
            }
            [retPointList addObject:a];
        }
    }
    return retPointList;    
}
- (void) setNormalizePoints: (NSMutableArray*)points
{
    currentPointArray = nil;
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    for(NSArray* obj in points)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(NSValue* v in obj)
        {
            CGPoint p = [v CGPointValue];
            p.x = self.frame.size.width * p.x;
            p.y = self.frame.size.height * p.y;
            [a addObject:[NSValue valueWithCGPoint:p]];
        }
        [pointArrayList addObject:a];
    }
    [self setNeedsDisplay];
}
- (void) drawRect: (CGRect)rect
{
    [background drawInRect:rect];
    if(pointArrayList == nil)
        return;
    for(NSMutableArray* points in pointArrayList)
    {
        [[UIColor blackColor] set];
        CGContextRef context = UIGraphicsGetCurrentContext();
        CGContextSetLineWidth(context, lineWidth);
        if(points.count < 2)
        {
            CGPoint pt = POINT(0);
            CGContextFillRect(context, CGRectMake(pt.x - lineWidth / 2, pt.y - lineWidth / 2, lineWidth, lineWidth));
            //CGContextMoveToPoint(context, pt.x, pt.y);
            //CGContextAddLineToPoint(context, pt.x, pt.y);
            //CGContextStrokePath(context);
            //CGContextDrawPath(context, kCGPathStroke);
        }
        else
        {
            for(int i = 0 ; i < (points.count - 1) ; i++)
            {
                CGPoint pt1 = POINT(i);
                CGPoint pt2 = POINT(i + 1);
                CGContextMoveToPoint(context, pt1.x, pt1.y);
                CGContextAddLineToPoint(context, pt2.x, pt2.y);
                CGContextStrokePath(context);
            }
        }
    }
}
@end
