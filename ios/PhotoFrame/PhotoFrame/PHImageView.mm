//
//  PHImageView.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PHImageView.h"
#import "PGMDataReader.h"
#import <QuartzCore/QuartzCore.h>
#define POINT(x) [[points objectAtIndex:(x)] CGPointValue]
@interface MyLayer : CALayer
{
    UIImage* image;
    NSArray* clippingRectList;
}
@property (nonatomic, assign) NSArray* clippingRectList;
@property (nonatomic, assign) UIImage* image;
@end

@implementation MyLayer
@synthesize image;
@synthesize clippingRectList;
/*
- (void) display
{
    NSLog(@"display");
}
 */
- (void) drawInContext:(CGContextRef)ctx
{
    NSLog(@"draw in context");
    if(clippingRectList)
    {
        
    }
}
@end
@implementation MyClipRect
@synthesize clipRect;
@end

@implementation PHImageView
@synthesize image;
/*
+ (Class) layerClass
{
    return [MyLayer class];
}
*/
- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    
    }
    
    return self;
}

- (void) drawRect:(CGRect)rect
{
    //[image drawInRect:CGRectMake(0, 0, 1024, 768)];
    NSDate* startDate = [NSDate date];
    [image drawInRect:CGRectMake(0, 0, 1024, 768)];
    if(pointArrayList && pointArrayList.count > 0)
    {
        lineWidth = 8;
        CGContextRef context = UIGraphicsGetCurrentContext();
        CGContextSaveGState(context);
        for(NSMutableArray* points in pointArrayList)
        {
            [[UIColor blackColor] set];
            CGContextSetLineWidth(context, lineWidth);
            CGContextSetLineCap(context, kCGLineCapRound);
            if(points.count < 2)
            {
                CGPoint pt = POINT(0);
                CGContextFillEllipseInRect(context, CGRectMake(pt.x - lineWidth / 2, pt.y - lineWidth / 2, lineWidth, lineWidth));
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
        CGContextRestoreGState(context);
    }
    /*
    if(clippingRectList)
    {
        CGContextRef con = UIGraphicsGetCurrentContext();
        CGContextSaveGState(con);
        NSUInteger i;
        CGRect* clipRects = (CGRect*)malloc(sizeof(CGRect) * clippingRectList.count);
        for(i = 0 ; i < [clippingRectList count] ; i++)
        {
            MyClipRect* cr = [clippingRectList objectAtIndex:i];
            CGRect r = cr->clipRect;
            clipRects[i] = r;
        }
        CGContextClipToRects(con, clipRects, clippingRectList.count);
        //CGImageRef imageRef = [image CGImage];
        //CGContextDrawImage(con, rect, imageRef);
        [image drawInRect:CGRectMake(0, 0, 1024, 768)];
        free(clipRects);
        CGContextRestoreGState(con);
        //[self setNeedsDisplay];
        //[self performSelectorOnMainThread:@selector(setNeedsDisplay) withObject:nil waitUntilDone:NO];
    }
    else
    {
        //CGContextDrawImage(con, rect, imageRef);
        [image drawInRect:rect];
    }
     */
    NSDate* endDate = [NSDate date];
    NSTimeInterval interval = [endDate timeIntervalSinceDate:startDate];
    //NSLog(@"### draw one frame time = %f ####", interval);
}
/*
- (void)drawRect:(CGRect)rect
{
    SS_Canvas* currentCanvas = SS_GetCurrentCanvas();
    BOOL canDraw = SS_DrawCanvas(currentCanvas);
    
    //CGImageRef imageRef = [image CGImage];
    if(canDraw == 0)
    {
        [clippingRectList release];
        clippingRectList = nil;
    }
    CALayer* layer = self.layer;
    if(clippingRectList)
    {
        CGContextRef con = UIGraphicsGetCurrentContext();
        CGContextSaveGState(con);
        NSUInteger i;
        CGRect* clipRects = (CGRect*)malloc(sizeof(CGRect) * clippingRectList.count);
        for(i = 0 ; i < [clippingRectList count] ; i++)
        {
            MyClipRect* cr = [clippingRectList objectAtIndex:i];
            CGRect r = cr->clipRect;
            //CGContextAddRect(con, r);
            clipRects[i] = r;
        }
        CGContextClipToRects(con, clipRects, clippingRectList.count);
        CGImageRef imageRef = [image CGImage];
        CGContextDrawImage(con, rect, imageRef);
        //[image drawInRect:rect];
        free(clipRects);
        CGContextRestoreGState(con);
        //[self setNeedsDisplay];
        [self performSelectorOnMainThread:@selector(setNeedsDisplay) withObject:nil waitUntilDone:NO];
    }
    else
    {
        //CGContextDrawImage(con, rect, imageRef);
        [image drawInRect:rect];
    }
    
}
 */
- (void) clearClippingList
{
    [clippingRectList release];
    clippingRectList = nil;
}
- (void) setClipRectList: (const MyClipRect**) clipRect count:(int)size
{
    [clippingRectList release];
    clippingRectList = nil;
    clippingRectList = [NSArray arrayWithObjects: clipRect count: size];
    [clippingRectList retain];
    //MyLayer* layer = (MyLayer*)self.layer;
    //layer.image = image;
    //layer.clippingRectList = clippingRectList;
}
- (void)dealloc
{
    [pointArrayList release];
    [image release];
    [clippingRectList release];
    [super dealloc];
}
- (void) setPoints:(NSArray*)points
{
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    CGSize size = CGSizeMake(320 , 240);
    CGPoint startPoint = CGPointMake(1024 - 320, 768 - 240);
    for(NSArray* obj in points)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(NSValue* v in obj)
        {
            CGPoint p = [v CGPointValue];
            p.x = startPoint.x + size.width * p.x;
            p.y = startPoint.y + size.height * p.y;
            [a addObject:[NSValue valueWithCGPoint:p]];
        }
        [pointArrayList addObject:a];
    }

}
@end
