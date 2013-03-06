//
//  SEDrawTouchView.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-2-28.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEDrawTouchView.h"
#import "SEViewNavigator.h"
#import "PhotoFrameAppDelegate.h"
#import "SEUtil.h"
#import "SESystemConfig.h"
#import "UserInfo.h"
#import <QuartzCore/QuartzCore.h>
#import <CoreFoundation/CoreFoundation.h>
#define POINT(x) [[points objectAtIndex:(x)] CGPointValue]
//const CGFloat startLineWidth = 4;
//const CGFloat endLineWidth = 10;
//SPEED_TYPE1 is slowest, and then SPEED_TYPE2, the fateste is SPEED_TYPE3
enum SPEED_TYPE {SPEED_TYPE1, SPEED_TYPE2, SPEED_TYPE3};

static float getMinLineWidth(float maxLineWidth)
{
    return maxLineWidth / 6;
}
@interface InflectionPoint : NSObject   
{
    int pointArrayIndex;
    int pointIndex;
}
@property (nonatomic, assign) int pointArrayIndex;
@property (nonatomic, assign) int pointIndex;
@end
@implementation InflectionPoint

@synthesize pointIndex;
@synthesize pointArrayIndex;

@end
/////////////////////////////////////////
@interface MyDrawViewLayer : CALayer
{
    UIImage* image;
    NSArray* clippingRectList;
}
@property (nonatomic, assign) NSArray* clippingRectList;
@property (nonatomic, assign) UIImage* image;
@end

@implementation MyDrawViewLayer
@synthesize image;
@synthesize clippingRectList;
/*
 - (void) display
 {
 NSLog(@"display");
 }
 */
/*
- (void) drawInContext:(CGContextRef)ctx
{
    NSLog(@"draw in context");
    if(clippingRectList)
    {
        
    }
}
 */
@end

//////////
@implementation SEDrawTouchPoint
@synthesize lineWidth;
@synthesize touchState;
@synthesize point;
@synthesize interpolate;
@synthesize milliTime;
@synthesize sec;
@synthesize usec;
@synthesize testColor;
@synthesize timestamp;
@synthesize speedType;
- (id) init
{
    self = [super init];
    if(self)
    {
        lineWidth = -1;
        milliTime = 0;
        self.testColor = [UIColor colorWithRed:0 green:0 blue:1.0 alpha:1.0];
    }
    return self;
}
- (void) dealloc
{
    [testColor release];
    [super dealloc];
}
+ (long) timeMilliSecond: (struct timeval) time
{
    long ret = time.tv_sec * 1000 + time.tv_usec / 1000;
    return ret;
}
@end
/////////////////////////////////////
@interface SEDrawTouchView (Private)
- (CGImageRef) createLayerContent: (CGRect)frame;
- (void) initLayerData: (CGRect) frame;
- (void) releaseContext;
- (void) createContext;
- (void) setContextFillColor: (CGContextRef)context color: (UIColor*)color;
- (void) clearBackBuffer;
- (NSArray*) getDeltaTimeBetweenPoint;
- (void) printPointsDeltaTime;
- (CGPoint) midPoint: (CGPoint)p1 :(CGPoint)p2;
- (CGFloat) pointDist: (CGPoint) p1 : (CGPoint) p2;
- (CGFloat) pointDist: (NSArray*) pointArray start: (int) startIndex end: (int) endIndex;
- (CGPoint) createVector: (CGPoint)startPoint endPoint: (CGPoint)endPoint;
- (CGPoint) createPerpendicularVector: (CGPoint) v;
- (CGPoint) createPerpendicularVector: (CGPoint) startPoint endPoint: (CGPoint) endPoint;
- (CGPoint) vectorNormalize: (CGPoint)v;
- (CGPoint) addVector: (CGPoint) v1 :(CGPoint)v2;
- (CGPoint) createPoint: (CGPoint) startPoint fromNormalVector:(CGPoint)v len: (float)len;
- (BOOL) isClockWise: (CGPoint) v1 :(CGPoint) v2;
- (CGFloat) vectorAngle: (CGPoint) v1 : (CGPoint)v2;
- (UIColor*) inverseColor: (UIColor*)color;
- (int) totalPointCount;
- (void) drawPoint: (SEDrawTouchPoint*)point context: (CGContextRef)context lineWidth: (float) lineWidth;
- (NSMutableArray*) fillPoint: (int)startIndex;
- (BOOL) isLineWidthEqual : (float) v1 : (float)v2;
- (void) calculateLineWith: (NSMutableArray*) pointArray startLineWidth: (float) startLineWidth endLineWidth: (float)endLineWidth startIndex: (int) startIndex endIndex: (int)endIndex dist: (float) totalDist;
- (CGFloat) calculateLineWidth : (CGFloat) dist;
- (void) setPointLineWidth: (int) lastPointIndex : (int)endIndex : (float) dist;
- (CGRect) createRect: (SEDrawTouchPoint*) p1 : (SEDrawTouchPoint*) p2;
- (void) setMyDisplay;
- (void) calculatePointSpeed;
- (NSArray*) fillPointByCurve: (NSArray*)srcPoints;
- (NSArray*) fillPointBetween: (SEDrawTouchPoint*)startDrawPoint : (SEDrawTouchPoint*)endDrawPoint;
- (void) fillPoint;
- (void) setPointLineWidth;
- (BOOL) isVectorParallel: (CGPoint)v1 : (CGPoint)v2;
- (int) findIntersection: (float) u0 :(float)u1 : (float) v0 : (float)v1 : (float*)w;
- (int) findLineIntersection: (CGPoint) P0 : (CGPoint)D0 : (CGPoint) P1 :(CGPoint) D1 : (CGPoint*)I;
- (NSArray*) createBezierCurveInterpolatePoint: (NSArray*)pointArray : (int) startIndex : (float) step;
- (NSArray*) createCurveInterpolatePoint: (NSArray*)pointArray : (int)startIndex;
- (NSArray*) mergeAllPointsBetween: (NSArray*)pointArray :(NSArray*) interpolatePointsArray : (int) firstIndex : (int)nextIndex : (BOOL) addOriginPoint;
- (void) calculatePointArrayLineWidth: (NSArray*)pointArray interpolatePointsArray: (NSArray*)interpolatePointsArray;
- (void) interpolatePointsToFirstPoint: (NSArray*)pointArray interpolatePointsArray: (NSArray*)interpolatePointsArray;
- (UIImage*) getBackgroundContent: (CGRect)r;
- (void) createRegion: (int)startIndex : (int)endIndex;
- (void) calculateLineWidthByBezier: (NSArray*)pointArray : (NSArray*) interpolatePointsArray : (int)startIndex : (int)endIndex;
- (void) createCGImageFromBackBuffer;
- (void) drawPointsToBackBuffer: (NSArray*)pointArray : (NSArray*)interpolatePointsArray startIndex : (int)startIndex endIndex : (int)endIndex : (UIColor*)color;
- (void) interpolatePointsByBezier: (NSArray*)pointArray interpolatePointsArray: (NSArray*)interpolatePointsArray : (int)startIndex : (int) endIndex;
- (void) interpolatePointsInPointArray: (NSArray*) pointArray interpolatePointsArray: (NSArray*)interpolatePointsArray;
- (void) calculatePointAccululate: (NSTimer*)timer;
- (NSArray*) createCorrespondingPoints: (NSArray*)points;
- (NSArray*) calculateInflectionPointsInArray: (NSArray*)points;
- (NSArray*) calculateInflectionPoint;
- (void) drawLineWithInterpolatePoints;
- (BOOL) isPointInClipRect: (SEDrawTouchPoint*)p rect:(CGRect)frame;
- (void) drawPointsAndInterpolatePoints: (CGContextRef) context;
- (void) drawSamplePoints: (CGContextRef)context;
- (void) drawPoints;
- (void) drawSampleLine;
- (void) drawLine: (SEDrawTouchPoint*)startPoint endPoint:(SEDrawTouchPoint*)endPoint context: (CGContextRef) context;
- (void) drawLineFill;
- (void) drawCurveFill;
- (void) drawBezierTestPoint;
- (void) drawCurve;
- (void) getPointIndexSpan: (float) ratio : (PointIndex*) first : (PointIndex*)second;
- (float) milliSecondSpan: (struct timeval) t1 : (struct timeval) t2;
- (int) getTotalSamplePointNum;
- (void) actionAfterAnimEnd;
- (void) clearAllInterpolatePoint;
- (float) createLineWidth: (float)currentLineWidth;
- (BOOL) isTouchTimeExceed;
//@property (nonatomic, retain) NSArray* mClipRectArray;
@end
//////////////////
@implementation SEDrawTouchView
@synthesize background;
@synthesize maxLineWidth;
@synthesize mCurrentColor;
@synthesize mIsDrawSamplePoint;
@synthesize mOrigRect;
@synthesize mDrawInMainScreen;
- (void) setMClipRectArray: (NSMutableArray*)array
{
    [mClipRectArray release];
    mClipRectArray = [array retain];
}
- (NSMutableArray*) getMClipRectArray
{
    return mClipRectArray;
}
/*
+ (Class)layerClass
{
    return [MyDrawViewLayer class];
}
 */
- (BOOL) isTouchTimeExceed
{
    /*
    if(mFrameNum >= 334)
        return YES;
    else {
        return NO;
    }
     */
    NSNumber* ret = [mCanConsumePointHandler performSelector:mCanConsumPointAction];
    BOOL exceed = [ret boolValue] == NO;
    return exceed;
    
}
- (SPEED_TYPE) getSpeedType: (double) speed
{
    double speed1 = 10 / 0.015;
    double speed2 = 30 / 0.015;
    //NSLog(@"speed1 = %f, speed2 = %f, speed = %f", speed1, speed2, speed);
    //double speed3 = self.frame.size.width / 2;
    if(speed <= speed1)
        return SPEED_TYPE1;
    else if(speed > speed1 && speed <= speed2)
        return SPEED_TYPE2;
    else
        return SPEED_TYPE3;

}
- (CGImageRef) createLayerContent: (CGRect)frame
{
    return [SEUtil createEmptyCGImage:frame.size];
}
- (void) initLayerData: (CGRect) frame
{
    mLayerWidth = frame.size.width;
    mLayerHeight = frame.size.height;
    mLayerBytesPerRow = mLayerWidth * 4;
    int bitmapByteCount     = (mLayerBytesPerRow * mLayerHeight);
    
    mLayerData = malloc( bitmapByteCount );// 3
    if(mLayerData != NULL)
    {
        memset(mLayerData, 0, bitmapByteCount);
    }
}
- (void) releaseContext
{
    if(mLayerContext)
        CGContextRelease(mLayerContext);
    mLayerContext = NULL;
    if(mLayerData)
        free(mLayerData);
    mLayerData = NULL;
}
- (void) createContext
{
    if(mLayerData == NULL)
        return;
    CGColorSpaceRef colorSpace;
    colorSpace = CGColorSpaceCreateDeviceRGB();// 2
    mLayerContext = CGBitmapContextCreate (mLayerData,// 4
                                     mLayerWidth,
                                     mLayerHeight,
                                     8,      // bits per component
                                     mLayerBytesPerRow,
                                     colorSpace,
                                     kCGImageAlphaPremultipliedLast);
                                     //kCGImageAlphaNoneSkipLast);
    if(mLayerContext == NULL)
    {
        free(mLayerData);
        mLayerData = NULL;
        CGColorSpaceRelease(colorSpace);
        return;
    }
    CGColorSpaceRelease( colorSpace );// 6
    CGImageRef newImageRef = CGBitmapContextCreateImage(mLayerContext);
    mLayerCGImage = newImageRef;
    CGContextSetShouldAntialias(mLayerContext, YES);
}
- (void) setContextFillColor: (CGContextRef)context color: (UIColor*)color
{
    CGColorRef colorRef = [color CGColor];
    CGContextSetFillColorWithColor(context, colorRef);
}
- (void) clearBackBuffer
{
    if(mLayerContext)
    {
        [self setContextFillColor:mLayerContext color:[UIColor clearColor]];
        CGContextClearRect(mLayerContext, CGRectMake(0, 0, mOrigRect.size.width, mOrigRect.size.height));
        [self createCGImageFromBackBuffer];
    }
}
//frame must be (0, 0, w, h)
- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self)
    {
        // Initialization code
        CALayer* layer = self.layer;
        layer.opacity = 1;
        layer.opaque = NO;
        layer.contents = nil;//(id)imageRef;
        layer.backgroundColor = NULL;
        //layer.delegate = self;
        [self initLayerData:frame];
        [self createContext];
        mOrigRect = frame;
        mClipRectArray = [[NSMutableArray array] retain];
        
    }
    return self;
}
- (float) getPointsLength
{
    /*
    float dist = 0;
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        for(int j = 0 ; j < points.count - 1; j++)
        {
            SEDrawTouchPoint* p1 = [points objectAtIndex:j];
            SEDrawTouchPoint* p2 = [points objectAtIndex:j + 1];
            dist += [self pointDist:p1.point :p2.point];
        }
    }
    return dist;
     */
    return mPointsLength;
}
- (NSArray*) getDeltaTimeBetweenPoint
{
    NSMutableArray* deltaTimesArray = [NSMutableArray array];
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        NSMutableArray* onePointsTimes = [NSMutableArray array];
        NSNumber* num = [NSNumber numberWithInt:0];
        [onePointsTimes addObject:num];
        for(int j = 1 ; j  < points.count ; j++)
        {
            SEDrawTouchPoint* p1 = [points objectAtIndex:j - 1];
            SEDrawTouchPoint* p2 = [points objectAtIndex:j];
            NSTimeInterval t = p2.timestamp - p1.timestamp;
            t *= 1000;
            num = [NSNumber numberWithInt:t];
            //num = [NSNumber numberWithInt:p2.milliTime - p1.milliTime];
            [onePointsTimes addObject:num];
        }
        [deltaTimesArray addObject: onePointsTimes];
    }
    return deltaTimesArray;
}
- (void) printPointsDeltaTime
{
    NSArray* allPointsDeltaTime = [self getDeltaTimeBetweenPoint];
    for(int i = 0 ; i < allPointsDeltaTime.count ; i++)
    {
        NSArray* onePointLineDeltaTime = [allPointsDeltaTime objectAtIndex:i];
        for(int j = 0 ; j < onePointLineDeltaTime.count ; j++)
        {
            NSNumber* num = [onePointLineDeltaTime objectAtIndex:j];
            NSLog(@"## point %d , delta time = %d", j, [num intValue]);
        }
    }
}
- (CGPoint) midPoint: (CGPoint)p1 :(CGPoint)p2
{
    return CGPointMake((p1.x + p2.x) / 2, (p1.y + p2.y) / 2);
}
- (CGFloat) pointDist: (CGPoint) p1 : (CGPoint) p2
{
    CGFloat deltax = p1.x - p2.x;
    CGFloat deltay = p1.y - p2.y;
    return sqrtf(deltax * deltax + deltay * deltay);
}
- (CGFloat) pointDist: (NSArray*) pointArray start: (int) startIndex end: (int) endIndex
{
    CGFloat dist = 0;
    assert(startIndex <= endIndex);
    for(int i = startIndex ; i < endIndex ; i++)
    {
        CGPoint p1 = [[pointArray objectAtIndex:i] point ];
        CGPoint p2 = [[pointArray objectAtIndex:i + 1] point];
        dist += [self pointDist:p1 : p2];
    }
    return dist;
}
- (CGPoint) createVector: (CGPoint)startPoint endPoint: (CGPoint)endPoint
{
    CGPoint v = CGPointMake(endPoint.x - startPoint.x, endPoint.y - startPoint.y);
    return v;
}
- (CGPoint) createPerpendicularVector: (CGPoint) v
{
    return CGPointMake(-v.y, v.x);
}
- (CGPoint) createPerpendicularVector: (CGPoint) startPoint endPoint: (CGPoint) endPoint
{
    CGPoint v = [self createVector:startPoint endPoint:endPoint];
    return [self createPerpendicularVector:v];
}
- (CGPoint) vectorNormalize: (CGPoint)v
{
    float dist = [self pointDist:CGPointMake(0, 0) :v];
    v = CGPointMake(v.x / dist, v.y / dist);
    return v;
}
- (CGPoint) addVector: (CGPoint) v1 :(CGPoint)v2
{
    return CGPointMake(v1.x + v2.x , v1.y + v2.y);
}

- (CGPoint) createPoint: (CGPoint) startPoint fromNormalVector:(CGPoint)v len: (float)len
{
    return CGPointMake(startPoint.x + v.x * len, startPoint.y + v.y * len);
}
- (BOOL) isClockWise: (CGPoint) v1 :(CGPoint) v2
{
    float tv1[3] = {v1.x, v1.y, 0};
    float tv2[3] = {v2.x, v2.y, 0};
    //y * rv.z - z * rv.y, z * rv.x - x * rv.z, x * rv.y - y * rv.x
    //float x = tv1[1] * tv2[2] - tv1[2] * tv1[1];
    //float y = tv1[2] * tv2[0] - tv1[0] * tv2[2];
    float z = tv1[0] * tv2[1] - tv1[1] * tv2[0];
    if(z > 0)
        return YES;
    else 
    {
        return NO;
    }
}


- (CGFloat) vectorAngle: (CGPoint) v1 : (CGPoint)v2
{
    CGFloat v1x = v1.x;
    CGFloat v1y = v1.y;
    CGFloat v2x = v2.x;
    CGFloat v2y = v2.y;
    NSLog(@"v1x = %f, v1y = %f", v1x, v1y);
    NSLog(@"v2x = %f, v2y = %f", v2x, v2y);
    
    CGFloat dot = v1x * v2x + v1y * v2y;
    CGFloat len1 = sqrtf(v1x * v1x + v1y * v1y);
    CGFloat len2 = sqrtf(v2x * v2x + v2y * v2y);
    CGFloat cos = dot / (len1 * len2);
    NSLog(@"dot = %f", dot);
    NSLog(@"len1 = %f, len2 = %f", len1, len2);
    NSLog(@"cos = %f", cos);
    //assert(cos >= -1 && cos <= 1);
    CGFloat angle = acosf(cos) * 180 / 3.1415926;
    return angle;
}
- (UIColor*) inverseColor: (UIColor*)color
{
    float r, g ,b,a;
    [color getRed:&r green:&g blue:&b alpha:&a];
    return [UIColor colorWithRed:1 - r green:1 - g blue:1 - b alpha:1.0];
}
- (int) getTotalSamplePointNum
{
    int count = 0;
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        count += points.count;
    }
    return count;
}
- (int) totalPointCount
{
    int count = 0;
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        NSArray* interpolatePointsArray = [interpolatePointsArrayList objectAtIndex:i];
        count += points.count;
        for(int j = 0 ; j < interpolatePointsArray.count ; j++)
        {
            NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:j];
            count += interpolatePoints.count;
        }
    }
    return count;
}
- (void) drawPoint: (SEDrawTouchPoint*)point context: (CGContextRef)context lineWidth: (float) lineWidth
{
    //float lineWidth = //[self createLineWidth:point.lineWidth];
    CGContextSetShouldAntialias(context, YES);
    CGContextSetLineWidth(context, lineWidth);
    CGContextSetLineCap(context, kCGLineCapRound);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGPoint pt = point.point;
    CGContextFillEllipseInRect(context, CGRectMake(pt.x - lineWidth / 2, pt.y - lineWidth / 2, lineWidth, lineWidth));
}

- (NSMutableArray*) fillPoint: (int)startIndex
{
    int nextIndex = startIndex + 1;
    float dist = [self pointDist:currentPointArray start:startIndex end:nextIndex];
    CGPoint p1 = [[currentPointArray objectAtIndex:startIndex] point];
    CGPoint p2 = [[currentPointArray objectAtIndex:nextIndex] point];
    NSMutableArray* points = [NSMutableArray array];
    float stepDist = 20;
    //if(dist <= stepDist)
        return points;
    CGPoint v = CGPointMake((p2.x - p1.x) / dist, (p2.y - p1.y) / dist);
    float leftDist = dist;
    CGPoint startPoint = p1;
    [points addObject:[currentPointArray objectAtIndex:startIndex]];
    while (leftDist > stepDist) 
    {
        CGPoint newPoint = CGPointMake(startPoint.x + v.x * stepDist, startPoint.y + v.y * stepDist);   
        float pointToEndDist = [self pointDist:newPoint :p2];
        if(pointToEndDist < stepDist)
            break;
        SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
        point.point = newPoint;
        point.interpolate = YES;
        [points addObject:point];
        [point release];
        startPoint = newPoint;
        leftDist -= stepDist;
    }
    /*
    float slope = 0;
    int num = 5;
    if(dist > 100)
    {
        num = 10;
    }
    if(fabsf(p1.x - p2.x) < 0.01)
    {
        slope = 0;
        deltax = 0;
        deltay = (p2.y - p1.y) / (num - 1);
    }
    else
    {
        slope = (p2.y - p1.y) / (p2.x - p1.x);
        deltax = p2.x - p1.x;
        deltay = slope * deltax;
    }
    
    float stepx = deltax / (num - 1);
    float stepy = deltay / (num - 1);
    for(int i = 0 ; i < num ; i++)
    {
        CGPoint p;
        p.x = p1.x + i * stepx;
        p.y = p1.y + i * stepy;
        SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
        point.point = p;
        point.interpolate = YES;
        [points addObject:point];
    }
     */
    return points;
    /*
    if(p1.x < p2.x)
    {
        CGPoint p;
        p.x = p1.x + deltax;
        p.y = p1.y + deltay;
        while (p.x < p2.x) 
        {
            SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
            point.point = p;
            [points addObject:point];
            p.x = p.x + deltax;
            p.y = p.y + deltay;
        }
        return points;
    }
    else
    {
        CGPoint p;
        p.x = p1.x + deltax;
        p.y = p1.y + deltay;
        while (p.x > p2.x) 
        {
            SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
            point.point = p;
            [points addObject:point];
            p.x = p.x + deltax;
            p.y = p.y + deltay;
        }
        return points;
    }
     */
}
- (BOOL) isLineWidthEqual : (float) v1 : (float)v2
{
    return fabsf(v1 - v2) < 0.001;
}
- (void) calculateLineWith: (NSMutableArray*) pointArray startLineWidth: (float) startLineWidth endLineWidth: (float)endLineWidth startIndex: (int) startIndex endIndex: (int)endIndex dist: (float) totalDist
{
    float step = (endLineWidth - startLineWidth) / totalDist;
    float dist = 0;
    for(int i = startIndex ; i < endIndex - 1 ; i++)
    {
        SEDrawTouchPoint* p1 = [currentPointArray objectAtIndex:i];
        SEDrawTouchPoint* p2 = [currentPointArray objectAtIndex:(i + 1)];
        dist += [self pointDist:p1.point :p2.point];
        p1.lineWidth = startLineWidth + dist * step;
        p2.lineWidth = startLineWidth + dist * step;
        NSLog(@"## point line width = %f", p1.lineWidth);
    }
    SEDrawTouchPoint* p = [pointArray objectAtIndex:endIndex];
    p.lineWidth = endLineWidth;
}
- (CGFloat) calculateLineWidth : (CGFloat) dist
{
    CGFloat width = mOrigRect.size.width;
    CGFloat height = mOrigRect.size.height;
    CGFloat len = sqrtf(width * width + height * height);
    CGFloat ratio = 0;
    if(dist * 10  < len)
       ratio = dist * 10 / len;
    else {
        ratio = 1;
    }
    CGFloat step = maxLineWidth - getMinLineWidth(maxLineWidth);
    CGFloat ret = maxLineWidth - ratio * step;
    NSLog(@"maxLine Width = %f, currentLineWidth = %f ", maxLineWidth, ret);
    return ret;
    
}
- (void) setPointLineWidth: (int) lastPointIndex : (int)endIndex : (float) dist
{
    float endLineWidth = [self calculateLineWidth:dist];
    NSLog(@"end line width = %f ", endLineWidth);
    SEDrawTouchPoint* lastPoint = [currentPointArray objectAtIndex:lastPointIndex];
    float startLineWidth = lastPoint.lineWidth;
    float newDist = 0;
    float step = 0;
    if(lastPointIndex == endIndex)
    {
        if(lastPointIndex == 0)
        {
            SEDrawTouchPoint* p = [currentPointArray objectAtIndex:lastPointIndex];
            p.lineWidth = getMinLineWidth(maxLineWidth);
        }
        return;
    }
    if(dist <= 1)
    {
        step = (endLineWidth - startLineWidth) / (endIndex - lastPointIndex);
    }
    else 
    {
        step = (endLineWidth - startLineWidth) / dist;
    }
    for(int i = lastPointIndex ; i <= endIndex - 1 ; i++)
    {
        SEDrawTouchPoint* p1 = [currentPointArray objectAtIndex:i];
        SEDrawTouchPoint* p2 = [currentPointArray objectAtIndex:(i + 1)];
        if(dist <= 1)
        {
            p1.lineWidth = startLineWidth + (i - lastPointIndex) * step;
        }
        else 
        {
            newDist += [self pointDist:p1.point :p2.point];
            p1.lineWidth = startLineWidth + newDist * step;
            p2.lineWidth = startLineWidth + newDist * step;
        }
        //NSLog(@"## point line width = %f", p1.lineWidth);
    }
    SEDrawTouchPoint* p = [currentPointArray objectAtIndex:endIndex];
    p.lineWidth = endLineWidth;
}
- (CGRect) createRect: (SEDrawTouchPoint*) p1 : (SEDrawTouchPoint*) p2
{
    /*
    float start1x = p1.point.x - p1.lineWidth;
    float start2x = p2.point.x + p2.lineWidth;
    float start1y = p1.point.y - p1.lineWidth;
    float start2y = p2.point.y + p2.lineWidth;
    float startx = start1x < start2x ? start1x : start2x;
    float starty = start1y < start2y ? start1y : start2y;
    float end1x = p1.point.x + p1.lineWidth;
    float end2x = p2.point.x + p2.lineWidth;
    float endx = end1x < end2x ? end2x : end1x;
    float end1y = p1.point.y + p1.lineWidth;
    float end2y = p2.point.y + p2.lineWidth;
    float endy = end1y < end2y ? end2y : end1y;
    float w = fabsf(endx - startx);
    float h = fabsf(endy - starty);
    return CGRectMake(startx, starty, w, h);
     */
    float startx = p1.point.x > p2.point.x ? p2.point.x : p1.point.x;
    float starty = p1.point.y > p2.point.y ? p2.point.y : p1.point.y;
    
    float endx = p1.point.x > p2.point.x ? p1.point.x : p2.point.x;
    float endy = p1.point.y > p2.point.y ? p1.point.y : p2.point.y;
    float lineWidth = p1.lineWidth > p2.lineWidth ? p1.lineWidth : p2.lineWidth;
    startx -= lineWidth;
    starty -= lineWidth;
    endx += lineWidth;
    endy += lineWidth;
    return CGRectMake(startx, starty, endx - startx, endy - starty);
}
- (void) setMyDisplay
{
    for(int i = mLastHandleIndex ; i < currentPointArray.count - 1; i++)
    {
        SEDrawTouchPoint* p1 = [currentPointArray objectAtIndex:i];
        SEDrawTouchPoint* p2 = [currentPointArray objectAtIndex:i + 1];
        CGRect r = [self createRect:p1 :p2];
        [self setNeedsDisplayInRect:r];
    }
}
- (void) calculatePointSpeed
{
    for(int i = mLastHandleIndex + 1 ; i <= currentPointArray.count - 1 ; i++)
    {
        SEDrawTouchPoint* prevPoint = [currentPointArray objectAtIndex:i - 1];
        SEDrawTouchPoint* currentPoint = [currentPointArray objectAtIndex:i];
        //NSTimeInterval span = currentPoint.timestamp - prevPoint.timestamp;
        //NSLog(@"point time span = %f", span);
        double dist = [self pointDist:currentPoint.point :prevPoint.point];
        //NSLog(@"point dist = %f", dist);
        //double speed = dist / span;
        float MIN_SPEED = 10;
        float MAX_SPEED = 50;
        //float TOTAL_COUNT = MAX_SPEED + 50;
        float TOTAL_COUNT = 80;
        float lineWidthStep = (maxLineWidth - minLineWidth) / TOTAL_COUNT;
        int currentSpeedType = -1;
        float currentLineWidth = 0;
        if(dist > TOTAL_COUNT)
            dist = TOTAL_COUNT;
        float destLineWidth = minLineWidth + dist * lineWidthStep;
        float absDeltaLineWidth = dist * lineWidthStep;
        float midLineWidth = maxLineWidth * 2 / 3;
        if(i == 1)
        {
            NSLog(@"pre line width = %f", prevPoint.lineWidth);
        }
        if(dist <= MIN_SPEED)
        {
            currentSpeedType = SPEED_TYPE1;
        }
        else if(dist > MIN_SPEED && dist <= MAX_SPEED)
        {
            currentSpeedType = SPEED_TYPE2;
        }
        else 
        {
            currentSpeedType = SPEED_TYPE3;
        }
        currentLineWidth = maxLineWidth + dist * (minLineWidth - maxLineWidth) / TOTAL_COUNT;
        float prevLineWidth = prevPoint.lineWidth;
        float deltaLineWidth = fabsf(prevLineWidth - fabsf(currentLineWidth));
        if(deltaLineWidth > prevLineWidth / 2)
        {
            deltaLineWidth = prevLineWidth / 2;
        }
        if(currentLineWidth < prevLineWidth)
        {
            currentLineWidth = prevLineWidth - deltaLineWidth;
        }
        else
        {
            currentLineWidth = prevLineWidth + deltaLineWidth;
        }
    
        if(currentLineWidth > maxLineWidth)
        {
            currentLineWidth = maxLineWidth;
        }
        if(currentLineWidth < minLineWidth)
        {
            currentLineWidth = minLineWidth;
        }
        /*
        if(currentSpeedType == SPEED_TYPE2 || currentSpeedType == SPEED_TYPE1)
        {
            if(absDeltaLineWidth > prevPoint.lineWidth / 8)
                absDeltaLineWidth = prevPoint.lineWidth / 8;
        }
        if(prevPoint.speedType == currentSpeedType)
        {
            if(currentSpeedType == SPEED_TYPE1)
            {
                if(prevPoint.lineWidth < maxLineWidth)
                {
                    currentLineWidth = prevPoint.lineWidth + absDeltaLineWidth;
                    if(currentLineWidth > maxLineWidth)
                        currentLineWidth = maxLineWidth;
                }
                else
                {
                    currentLineWidth = prevPoint.lineWidth;
                    
                }
            }
            else if(currentSpeedType == SPEED_TYPE2)
            {
                if(prevPoint.lineWidth <= minLineWidth)
                {
                    currentLineWidth = minLineWidth;
                }
                else if(prevPoint.lineWidth > midLineWidth)
                {
                    currentLineWidth = prevPoint.lineWidth - absDeltaLineWidth;
                    if(currentLineWidth < midLineWidth)
                        currentLineWidth = midLineWidth;
                }
                else
                {
                    currentLineWidth = prevPoint.lineWidth - absDeltaLineWidth;
                    if(currentLineWidth < minLineWidth)
                        currentLineWidth = minLineWidth;
                }
            }
            else
            {
                if(prevPoint.lineWidth > minLineWidth)
                {
                    currentLineWidth = prevPoint.lineWidth - absDeltaLineWidth;
                    if(currentLineWidth < minLineWidth)
                        currentLineWidth = minLineWidth;
                }
                else 
                {
                    currentLineWidth = prevPoint.lineWidth;
                }
            }
        }
        else if(prevPoint.speedType < currentSpeedType)
        {
            if(prevPoint.speedType == SPEED_TYPE1 && currentSpeedType == SPEED_TYPE2)
            {
                currentLineWidth = prevPoint.lineWidth - absDeltaLineWidth;
                if(currentLineWidth < midLineWidth)
                {
                    if(currentLineWidth > minLineWidth)
                    {
                    }
                    else 
                    {
                        currentLineWidth = minLineWidth;
                    }
                }
            }
            else if(prevPoint.speedType == SPEED_TYPE1 && currentSpeedType == SPEED_TYPE3)
            {
                currentLineWidth = prevPoint.lineWidth - absDeltaLineWidth;
                if(currentLineWidth < minLineWidth)
                    currentLineWidth = minLineWidth;
                    
            }
            else if(prevPoint.speedType == SPEED_TYPE2 && currentSpeedType == SPEED_TYPE3)
            {
                currentLineWidth = prevPoint.lineWidth - absDeltaLineWidth;
                //NSLog(@"### current line Width = %f ###", currentLineWidth);
                if(currentLineWidth < minLineWidth)
                    currentLineWidth = minLineWidth;
            }
        }
        else
        {
            if(prevPoint.speedType == SPEED_TYPE2 && currentSpeedType == SPEED_TYPE1)
            {
                currentLineWidth = prevPoint.lineWidth + absDeltaLineWidth;
                if(currentLineWidth > maxLineWidth)
                    currentLineWidth = maxLineWidth;
            }
            else if(prevPoint.speedType == SPEED_TYPE3 && currentSpeedType == SPEED_TYPE1)
            {
                currentLineWidth = prevPoint.lineWidth + absDeltaLineWidth;
                if(currentLineWidth > maxLineWidth)
                    currentLineWidth = maxLineWidth;
            }
            else if(prevPoint.speedType == SPEED_TYPE3 && currentSpeedType == SPEED_TYPE2)
            {
                currentLineWidth = prevPoint.lineWidth + absDeltaLineWidth;
                if(currentLineWidth > midLineWidth)
                    currentLineWidth = midLineWidth;
            }
        }
         */
        //NSLog(@"min width = %f, maxWidth = %f", minLineWidth, maxLineWidth);
        //NSLog(@"absDeltaLineWidth = %f", absDeltaLineWidth);
        //NSLog(@"prev speed type = %d, current speed type = %d", prevPoint.speedType, currentSpeedType);
        //NSLog(@"prev line width = %f", prevPoint.lineWidth);
        //NSLog(@"current line width = %f", currentLineWidth);
        assert(currentLineWidth != 0);
        currentPoint.lineWidth = currentLineWidth;
        /*
        if(i == 1)
        {
            if(dist <= MIN_SPEED)
            {
                currentSpeedType = SPEED_TYPE1;
                currentPoint.lineWidth = maxLineWidth;
            }
            else if(dist > MIN_SPEED && dist <= MAX_SPEED)
            {
                currentSpeedType = SPEED_TYPE2;
                currentPoint.lineWidth = maxLineWidth - dist * lineWidthStep;
            }
            else
            {
                currentSpeedType = SPEED_TYPE3;
                currentPoint.lineWidth = minLineWidth;
            }
        }
        else
        {
            if(prevPoint.lineWidth == maxLineWidth)
            {
                 if(dist <= MIN_SPEED)
                 {
                     currentPoint.speedType = SPEED_TYPE1;
                     currentPoint.lineWidth = maxLineWidth;
                 }
                 else if(dist > MIN_SPEED && dist <= MAX_SPEED)
                 {
                     currentSpeedType = SPEED_TYPE2;
                     currentPoint.lineWidth = maxLineWidth - dist * lineWidthStep;
                 }
                 else
                 {
                     currentSpeedType = SPEED_TYPE3;
                     currentPoint.lineWidth = minLineWidth;
                 }

            }
            else if(prevPoint.lineWidth == minLineWidth)
            {
                if(dist <= 5)
                {
                    currentPoint.speedType = SPEED_TYPE1;
                    currentPoint.lineWidth = maxLineWidth;
                }
                else if(dist > 5 && dist <= MIN_SPEED)
                {
                    currentPoint.speedType = SPEED_TYPE1;
                    currentPoint.lineWidth = minLineWidth;
                }
                else if(dist > MIN_SPEED && dist <= MAX_SPEED)
                {
                    currentSpeedType = SPEED_TYPE2;
                    currentPoint.lineWidth = minLineWidth + dist * lineWidthStep;
                }
                else
                {
                    currentSpeedType = SPEED_TYPE3;
                    currentPoint.lineWidth = maxLineWidth;
                }
            }
            else
            {
                if(dist <= MIN_SPEED)
                {
                    currentPoint.speedType = SPEED_TYPE1;
                    float lineStep = (maxLineWidth - prevPoint.lineWidth * 1.5) / MAX_SPEED;
                    if(lineStep < 0)
                        lineStep = 0;
                    currentPoint.lineWidth = prevPoint.lineWidth + dist * lineStep;
                }
                else if(dist > MIN_SPEED && dist <= MAX_SPEED)
                {
                    currentSpeedType = SPEED_TYPE2;
                    float lineStep = (prevPoint.lineWidth - minLineWidth * 1.5) / MAX_SPEED;
                    if(lineStep < 0)
                        lineStep = 0;
                    currentPoint.lineWidth = prevPoint.lineWidth - dist * lineStep;
                }
                else
                {
                    currentSpeedType = SPEED_TYPE3;
                    currentPoint.lineWidth = minLineWidth;
                }
            }
        }
         */
        currentPoint.speedType = currentSpeedType;
        switch (currentPoint.speedType) {
            case SPEED_TYPE1:
            {
                currentPoint.testColor = [UIColor redColor];
            }
                break;
            case SPEED_TYPE2:
            {
                currentPoint.testColor = [UIColor blueColor];
            }
                break;
            case SPEED_TYPE3:
            {
                currentPoint.testColor = [UIColor greenColor];
            }
                break;
            default:
                break;
        }
        /*
        if(dist <= MIN_SPEED)
        {
            t = SPEED_TYPE1;
            currentPoint.lineWidth = maxLineWidth - dist * lineWidthStep;
            currentPoint.testColor = [UIColor redColor];
        }
        else if(dist > MIN_SPEED && dist <= MAX_SPEED)
        {
            t = SPEED_TYPE2;
            currentPoint.lineWidth = maxLineWidth - dist * lineWidthStep;
            currentPoint.testColor = [UIColor blueColor];

        }
        else 
        {
            t = SPEED_TYPE3;
            currentPoint.lineWidth = minLineWidth;
            currentPoint.testColor = [UIColor greenColor];
        }
         */
        /*
        //NSLog(@"speed type =  %d", t);
        currentPoint.speedType = t;
        switch (t) {
            case SPEED_TYPE1:
            {
                currentPoint.testColor = [UIColor redColor];
                currentPoint.lineWidth = maxLineWidth;
            }
                break;
            case SPEED_TYPE2:
            {
                currentPoint.testColor = [UIColor blueColor];
                currentPoint.lineWidth = maxLineWidth * 2 / 3;
            }
                break;
            case SPEED_TYPE3:
            {
                currentPoint.testColor = [UIColor greenColor];
                currentPoint.lineWidth = maxLineWidth / 3;
            }
                break;
            default:
                break;
        }
         */
    }
}
- (NSArray*) fillPointByCurve: (NSArray*)srcPoints
{
    NSArray* points = [NSArray array];
    for (int index = 4; index < srcPoints.count; index++)
    {
        SEDrawTouchPoint* drawP0 = [srcPoints objectAtIndex:index - 3];
        SEDrawTouchPoint* drawP1 = [srcPoints objectAtIndex:index - 2];
        SEDrawTouchPoint* drawP2 = [srcPoints objectAtIndex:index - 1];
        SEDrawTouchPoint* drawP3 = [srcPoints objectAtIndex:index];
        CGPoint p0 = drawP0.point;
        CGPoint p1 = drawP1.point;
        CGPoint p2 = drawP2.point;
        CGPoint p3 = drawP3.point;
        int granularity = 4;
        // now add n points starting at p1 + dx/dy up until p2 using Catmull-Rom splines
        for (int i = 1; i < granularity; i++)
        {
            float t = (float) i * (1.0f / (float) granularity);
            float tt = t * t;
            float ttt = tt * t;
            
            CGPoint pi; // intermediate point
            pi.x = 0.5 * (2*p1.x+(p2.x-p0.x)*t + (2*p0.x-5*p1.x+4*p2.x-p3.x)*tt + (3*p1.x-p0.x-3*p2.x+p3.x)*ttt);
            pi.y = 0.5 * (2*p1.y+(p2.y-p0.y)*t + (2*p0.y-5*p1.y+4*p2.y-p3.y)*tt + (3*p1.y-p0.y-3*p2.y+p3.y)*ttt);
            SEDrawTouchPoint* ppp = [[SEDrawTouchPoint alloc] init];
            ppp.point = pi;
            points = [points arrayByAddingObject:ppp];
            [ppp release];
        }
        SEDrawTouchPoint* ppp = [[SEDrawTouchPoint alloc] init];
        ppp.point = p2;
        points = [points arrayByAddingObject:ppp];
        [ppp release];
    }
    return points;
}
- (NSArray*) fillPointBetween: (SEDrawTouchPoint*)startDrawPoint : (SEDrawTouchPoint*)endDrawPoint
{
    float dist = [self pointDist:startDrawPoint.point :endDrawPoint.point];
    float stepDist = 2;
    NSMutableArray* points = [NSMutableArray array];
    if(dist <= stepDist)
    {
        //[points addObject:startDrawPoint];
        //[points addObject:endDrawPoint];
        return points;
    }
    CGPoint p1 = startDrawPoint.point;
    CGPoint p2 = endDrawPoint.point;
    
    CGPoint v = CGPointMake((p2.x - p1.x) / dist, (p2.y - p1.y) / dist);
    float leftDist = dist;
    CGPoint startPoint = p1;
    //[points addObject:startDrawPoint];
    while (leftDist > stepDist) 
    {
        CGPoint newPoint = CGPointMake(startPoint.x + v.x * stepDist, startPoint.y + v.y * stepDist);   
        //float pointToEndDist = [self pointDist:newPoint :p2];
        //if(pointToEndDist < stepDist)
        //    break;
        SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
        point.point = newPoint;
        point.interpolate = YES;
        [points addObject:point];
        [point release];
        startPoint = newPoint;
        leftDist -= stepDist;
    }
    //[points addObject:endDrawPoint];
    return points;
}
- (void) fillPoint
{
    for(int i = mLastHandleIndex ; i < currentPointArray.count - 1; i++)
    {
        SEDrawTouchPoint* firstPoint = [currentPointArray objectAtIndex:i];
        SEDrawTouchPoint* secondPoint = [currentPointArray objectAtIndex:i + 1];
        
    }
}
- (void) setPointLineWidth
{
    SEDrawTouchPoint* firstPoint = [currentPointArray objectAtIndex:mLastHandleIndex];
    if(mLastHandleIndex == 0)
    {
        firstPoint.lineWidth = maxLineWidth;
    }
    SEDrawTouchPoint* prevPoint = firstPoint; // prev speed point
    int prevPointIndex = mLastHandleIndex;
    for(int i = (mLastHandleIndex + 1) ; i < currentPointArray.count ; i++)
    {
        SEDrawTouchPoint* point = [currentPointArray objectAtIndex:i];
        if(point.speedType != prevPoint.speedType)
        {
            int betweenPointCount = i - prevPointIndex - 1;
            float dist = [self pointDist:point.point :prevPoint.point];
            if(betweenPointCount > 0)
            {
                
            }
            else
            {
                
            }
        }
    }
}
- (BOOL) isVectorParallel: (CGPoint)v1 : (CGPoint)v2
{
    float angle = [self vectorAngle:v1 :v2];
    if(angle <= 5 || angle >= 175)
        return YES;
    else 
    {
        return NO;
    }
}
- (int) findIntersection: (float) u0 :(float)u1 : (float) v0 : (float)v1 : (float*)w
{
    if(u1 < u0 || u0 > v1)
        return 0;
    if(u1 > v0)
    {
        if(u0 < v1)
        {
            if(u0 < v0)
            {
                w[0] = v0;
            }
            else
            {
                w[0] = u0;    
            }
            if(u1 > v1)
            {
                w[1] = v1;
            }
            else
            {
                w[1] = u1;
            }
            
        }
        else
        {
            w[0] = u0;
            return 1;
        }
    }
    else 
    {
        w[0] = u1;
        return 1;
    }
}
- (int) findLineIntersection: (CGPoint) P0 : (CGPoint)D0 : (CGPoint) P1 : (CGPoint) D1 : (CGPoint*)I
{
    CGPoint E = CGPointMake(P1.x - P0.x, P1.y - P0.y);
    float kross = D0.x * D1.y - D0.y * D1.x;
    float sqrKross = kross * kross;
    float sqrLen0 = D0.x * D0.x + D0.y * D0.y;
    float sqrLen1 = D1.x * D1.x + D1.y * D1.y;
    float sqrEpsilon = 0.0001;
    if(sqrKross > sqrEpsilon * sqrLen0 * sqrLen1)
    {
        float s= (E.x * D1.y - E.y * D1.x) / kross;
        if(s < 0 || s > 1)
        {
            return 0;
        }
        float t = (E.x * D0.y - E.y * D0.x) / kross;
        if(t < 0 || t > 1)
            return 0;
        I[0] = CGPointMake(P0.x + s * D0.x , P0.y + s * D0.y);
        return 1;
    }
    float sqrLenE = E.x * E.x + E.y * E.y;
    kross = E.x * D0.y - E.y * D0.x;
    sqrKross = kross * kross;
    if(sqrKross > sqrEpsilon * sqrLen0 * sqrLenE)
    {
        return 0;
        
    }
    float d = D0.x * E.x + D0.y * E.y;
    float s0 = d / sqrLen0;
    float s1 = s0 + (D0.x * D1.x + D0.y * D1.y) / sqrLen0;
    float w[2];
    float smin = MIN(s0, s1);
    float smax = MAX(s0, s1);
    int imax = [self findIntersection:0 :1 :smin :smax :w];
    for(int n = 0 ; n < imax; n++)
    {
        I[n] = CGPointMake(P0.x + w[n] * D0.x, P0.y + w[n] * D0.y);
    }
    return imax;
}
- (NSArray*) createBezierCurveInterpolatePoint: (NSArray*)pointArray : (int) startIndex : (float)step
{
    NSMutableArray* outPoints = [NSMutableArray array];
    SEDrawTouchPoint* p1 = [pointArray objectAtIndex:startIndex];
    SEDrawTouchPoint* p2 = nil;
    SEDrawTouchPoint* p3 = nil;
    if(startIndex + 1 < pointArray.count)
    {
        p2 = [pointArray objectAtIndex:startIndex + 1];
    }
    if(startIndex + 2 < pointArray.count)
    {
        p3 = [pointArray objectAtIndex:startIndex + 2];
    }
    if(p2 == nil || p3 == nil)
        return outPoints;
    float dist = [self pointDist:p1.point :p2.point] + [self pointDist: p2.point :p3.point];
    /*
    float step = 2;
    if(mDrawInMainScreen)
    {
        step = 0.2;
    }
     */
    int n = (int)(dist / step);
    if(n > 1)
        n -= 1;
    float s = 1 / (float)n;
    CGPoint startPoint = [self midPoint:p1.point :p2.point];
    CGPoint endPoint = [self midPoint:p2.point :p3.point];
    for(int i = 0 ; i <= n ; i++)
    {
        float t = s * i;
        float x = startPoint.x * (1 - t) * (1 - t) + p2.point.x * 2 * t * (1 - t) + endPoint.x * t * t;
        float y = startPoint.y * (1 - t) * (1 - t) + p2.point.y * 2 * t * (1 - t) + endPoint.y * t * t;
        SEDrawTouchPoint* newP = [[SEDrawTouchPoint alloc] init];
        newP.point = CGPointMake(x, y);
        [outPoints addObject:newP];
        [newP release];
    }
    return outPoints;
}
- (NSArray*) createCurveInterpolatePoint : (NSArray*)pointArray : (int)startIndex
{
    NSArray* points = [NSArray array];
    int index = startIndex;
    SEDrawTouchPoint* drawP0 = [currentPointArray objectAtIndex:index];
    SEDrawTouchPoint* drawP1 = nil;
    if((index + 1) < pointArray.count)
    {
        drawP1 = [currentPointArray objectAtIndex:index + 1];
    }
    
    SEDrawTouchPoint* drawP2 = nil;
    if((index + 2) < pointArray.count)
    {
        drawP2 =  [currentPointArray objectAtIndex:index + 2];
    }
    SEDrawTouchPoint* drawP3 = nil;
    if((index + 3) < pointArray.count)
    {
        drawP3 = [currentPointArray objectAtIndex:index + 3];
    }
    if(drawP0 == nil || drawP1 == nil || drawP2 == nil || drawP3 == nil)
        return points;
    CGPoint p0 = drawP0.point;
    CGPoint p1 = drawP1.point;
    CGPoint p2 = drawP2.point;
    CGPoint p3 = drawP3.point;
    /*
    CGPoint v1 = [self createVector:p1 endPoint:p0];
    CGPoint v2 = [self createVector:p2 endPoint:p1];
    CGPoint v3 = [self createVector:p3 endPoint:p2];
    if([self isVectorParallel:v1 :v2] && [self isVectorParallel:v2 :v3])
    {
        return points;
    }
    */
    int granularity = 4;
    // now add n points starting at p1 + dx/dy up until p2 using Catmull-Rom splines
    for (int i = 1; i < granularity; i++)
    {
        float t = (float) i * (1.0f / (float) granularity);
        float tt = t * t;
        float ttt = tt * t;
        
        CGPoint pi; // intermediate point
        pi.x = 0.5 * (2*p1.x+(p2.x-p0.x)*t + (2*p0.x-5*p1.x+4*p2.x-p3.x)*tt + (3*p1.x-p0.x-3*p2.x+p3.x)*ttt);
        pi.y = 0.5 * (2*p1.y+(p2.y-p0.y)*t + (2*p0.y-5*p1.y+4*p2.y-p3.y)*tt + (3*p1.y-p0.y-3*p2.y+p3.y)*ttt);
        SEDrawTouchPoint* ppp = [[SEDrawTouchPoint alloc] init];
        ppp.point = pi;
        points = [points arrayByAddingObject:ppp];
        [ppp release];
    }
    return points;
}
- (NSArray*) mergeAllPointsBetween: (NSArray*)pointArray : (NSArray*)interpolatePointsArray : (int) firstIndex : (int)nextIndex : (BOOL) addOriginPoint
{
    NSMutableArray* allPoints = [NSMutableArray array];
    for(int n = firstIndex ; n < nextIndex ; n++)
    {
        NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:n];
        if(n > firstIndex && addOriginPoint)
        {
            [allPoints addObject:[pointArray objectAtIndex:n]];
        }
        [allPoints addObjectsFromArray:interpolatePoints];
    }
    return allPoints;
}
- (void) calculatePointArrayLineWidth : (NSArray*)pointArray interpolatePointsArray : (NSArray*)interpolatePointsArray
{
    assert(pointArray.count == interpolatePointsArray.count);
    //for(int i = 0 ; i < pointArray.count ; i++)
    int i = 0;
    while(i < pointArray.count)
    {
        SEDrawTouchPoint* currentPoint = [pointArray objectAtIndex:i];
        SEDrawTouchPoint* nextPoint = nil;
        int nextIndex = 0;
        for(int j = i + 1 ; j < pointArray.count ; j++)
        {
            SEDrawTouchPoint* p = [pointArray objectAtIndex:j];
            if(p.speedType != currentPoint.speedType)
            {
                nextPoint = p;
                nextIndex = j;
                break;
            }
        }
        if(nextPoint == nil)
        {
            break;
        }
        assert(i < nextIndex);
        NSArray* allPoints = nil;
        float lineWidthStep = 0;
        SEDrawTouchPoint* nextNextPoint = nil;
        float dist = [self pointDist:currentPoint.point :nextPoint.point];
        //if(nextPoint.speedType == SPEED_TYPE1 || nextPoint.speedType == SPEED_TYPE2)
        if(dist < 15)
        {
            if(nextIndex + 1 < pointArray.count)
            {
                nextNextPoint = [pointArray objectAtIndex:nextIndex + 1];
                
                allPoints = [self mergeAllPointsBetween:pointArray :interpolatePointsArray :i :nextIndex + 1 : NO];
                lineWidthStep = (nextNextPoint.lineWidth - currentPoint.lineWidth) / (allPoints.count + 1);
                nextPoint.speedType = currentPoint.speedType;
                
            }
            else
            {
                allPoints = [self mergeAllPointsBetween:pointArray :interpolatePointsArray :i :nextIndex: NO];
                lineWidthStep = (nextPoint.lineWidth - currentPoint.lineWidth) / (allPoints.count + 1);
            }
        }
        else
        {
            allPoints = [self mergeAllPointsBetween:pointArray :interpolatePointsArray :i :nextIndex :NO];
            lineWidthStep = (nextPoint.lineWidth - currentPoint.lineWidth) / (allPoints.count + 1);
        }
        for(int k = 0 ; k < allPoints.count ; k++)
        {
            float lineWidth = currentPoint.lineWidth + (k + 1) * lineWidthStep;
            SEDrawTouchPoint* p = [allPoints objectAtIndex:k];
            p.lineWidth = lineWidth;
        }
        if(nextNextPoint != nil)
        {
            i = nextIndex + 1;
        }
        else 
        {
            i = nextIndex;
        }
        //SEDrawTouchPoint
    }
    
}
- (void) interpolatePointsToFirstPoint: (NSArray*)pointArray interpolatePointsArray: (NSArray*)interpolatePointsArray
{
    SEDrawTouchPoint* firstPoint = [pointArray objectAtIndex:0];
    SEDrawTouchPoint* secondPoint = [pointArray objectAtIndex:1];
    NSMutableArray* interpolatePoints = [interpolatePointsArray objectAtIndex:0];
    if(interpolatePoints.count == 0)
    {
        NSArray* array = [self fillPointBetween:firstPoint:secondPoint];
        [interpolatePoints addObjectsFromArray:array];
    }
}
- (UIImage*) getBackgroundContent: (CGRect)r
{
    static int i = 0;
    if(r.origin.x < 0 || r.origin.y < 0 || r.size.width <= 0 || r.size.height <= 0)
        return NULL;
    if(r.size.width > mOrigRect.size.width || r.size.height > mOrigRect.size.height)
        return NULL;
    UIImage* screenImage = nil;
    UIGraphicsBeginImageContext(r.size);
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextConcatCTM(context, CGAffineTransformMakeTranslation(-r.origin.x, -r.origin.y));
    [self.layer renderInContext:context];
    screenImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    //[SEUtil savePNGImageToDocument:screenImage withName:[NSString stringWithFormat:@"clipimage_%d.png", i++]]; 
    return screenImage;
}

- (void) createRegion: (int)startIndex : (int)endIndex
{
    CGPoint minPoint = CGPointMake(99999, 99999);
    CGPoint maxPoint = CGPointMake(-99999, -99999);
    for(int i = startIndex ; i <= endIndex ; i++)
    {
        NSArray* interpolatePoints = [currentInterpolatePointsArray objectAtIndex:i];
        if(interpolatePoints.count > 0)
        {
            for(int j = 0 ; j < interpolatePoints.count ; j++)
            {
                SEDrawTouchPoint* p = [interpolatePoints objectAtIndex:j];
                CGPoint point = p.point;
                //[self setNeedsDisplayInRect:CGRectMake(point.x - maxLineWidth / 2, point.y - maxLineWidth / 2, maxLineWidth, maxLineWidth)];
                
                if(point.x > maxPoint.x)
                    maxPoint.x = point.x;
                if(point.x < minPoint.x)
                    minPoint.x = point.x;
                if(point.y > maxPoint.y)
                    maxPoint.y = point.y;
                if(point.y < minPoint.y)
                    minPoint.y = point.y;
            }
        }
    }
    if(minPoint.x == 99999 || minPoint.y == 99999 || maxPoint.x == -99999 || maxPoint.y == -99999)
        return;
    float lineWidth = [self createLineWidth: maxLineWidth];
    minPoint.x -=  lineWidth;
    minPoint.y -= lineWidth;
    maxPoint.x += lineWidth;
    maxPoint.y += lineWidth;
    //NSLog(@"min point = %f , %f", minPoint.x, minPoint.y);
    //NSLog(@"max point = %f, %f", maxPoint.x, maxPoint.y);
    
    mClipRect = CGRectMake(minPoint.x, minPoint.y, maxPoint.x - minPoint.x, maxPoint.y - minPoint.y);
    [self setNeedsDisplayInRect:CGRectMake(minPoint.x, minPoint.y, maxPoint.x - minPoint.x, maxPoint.y - minPoint.y)];
    //[mClipRectArray addObject:[NSValue valueWithCGRect:mClipRect]];
    //NSLog(@"mClipRect size = %f, %f", mClipRect.size.width, mClipRect.size.height);
}
- (void) calculateLineWidthByBezier : (NSArray*)pointArray : (NSArray*) interpolatePointsArray : (int)startIndex : (int)endIndex
{
    
    SEDrawTouchPoint* currentPoint = [pointArray objectAtIndex:startIndex];
    SEDrawTouchPoint* nextPoint = [pointArray objectAtIndex:endIndex];
    NSArray* allPoints = [interpolatePointsArray objectAtIndex:startIndex];
    
    float lineWidthStep = (nextPoint.lineWidth - currentPoint.lineWidth) / (allPoints.count - 1);
    float startLineWidth = currentPoint.lineWidth;
    for(int n = 0 ; n < allPoints.count ; n++)
    {
        SEDrawTouchPoint* p = [allPoints objectAtIndex:n];
        p.lineWidth = startLineWidth + lineWidthStep * n;
    }
    
    /*
    SEDrawTouchPoint* startPoint = [pointArray objectAtIndex:startIndex];
    SEDrawTouchPoint* nextDifferentSpeedPoint = nil;
    int nextDifferentSpeedPointIndex = -1;
    float lineWidthStep = 0;
    for(int i = startIndex + 1 ; i < endIndex ; i++)
    {
        SEDrawTouchPoint* p = [pointArray objectAtIndex:i];
        if(p.speedType != startPoint.speedType)
        {
            nextDifferentSpeedPoint = p;
            nextDifferentSpeedPointIndex = i;
            break;
        }
    }
    if(nextDifferentSpeedPoint == nil)
    {
        NSArray* allPoints = [self mergeAllPointsBetween:pointArray :currentInterpolatePointsArray :startIndex :endIndex :NO];
        for(int i = 0 ; i < allPoints.count; i++)
        {
            SEDrawTouchPoint* p = [allPoints objectAtIndex:i];
            p.lineWidth = startPoint.lineWidth;
        }
        return;
    }
    
    NSArray* allPoints = nil;
    float dist = [self pointDist:pointArray start:startIndex end:nextDifferentSpeedPointIndex];
    if(dist < 20)
    {
        int index = nextDifferentSpeedPointIndex + 1;
        //float allDist = dist;
        while(index < endIndex)
        {
            //SEDrawTouchPoint* p = [pointArray objectAtIndex:index];
            float nextDist = dist + [self pointDist:pointArray start:nextDifferentSpeedPointIndex end:index];
            if(nextDist > 20)
            {
                break;
            }    
        }
        if(index < endIndex)
        {
            SEDrawTouchPoint* p = [pointArray objectAtIndex:index];
            allPoints = [self mergeAllPointsBetween:pointArray :currentInterpolatePointsArray :startIndex :index :NO];
            lineWidthStep = (p.lineWidth - startPoint.lineWidth) / (allPoints.count - 1);
        }
        else
        {
            allPoints = [self mergeAllPointsBetween:pointArray :currentInterpolatePointsArray :startIndex:endIndex :NO]; 
            lineWidthStep = 0;
        }
    }
    else
    {
        SEDrawTouchPoint* nextNextPoint = [pointArray objectAtIndex:nextDifferentSpeedPointIndex + 1];
        if(nextNextPoint.speedType != nextDifferentSpeedPoint.speedType)
        {
            allPoints = [self mergeAllPointsBetween:pointArray :currentInterpolatePointsArray :startIndex :nextDifferentSpeedPointIndex + 1 :NO];
            lineWidthStep = (nextNextPoint.lineWidth - startPoint.lineWidth) / (allPoints.count - 1);
        }
        else
        {
            allPoints = [self mergeAllPointsBetween:pointArray :currentInterpolatePointsArray :startIndex :nextDifferentSpeedPointIndex :NO];
            lineWidthStep = (nextDifferentSpeedPoint.lineWidth - startPoint.lineWidth) / (allPoints.count - 1);
        }
    }
    for(int k = 0 ; k < allPoints.count ; k++)
    {
        SEDrawTouchPoint* p = [allPoints objectAtIndex:k];
        p.lineWidth = startPoint.lineWidth + k * lineWidthStep;
    }
     */
    
}
- (CGImageRef) getCGImage
{
    return mLayerCGImage;
}
- (void) createCGImageFromBackBuffer
{
    if(mLayerContext != NULL)
    {
        CGContextFlush(mLayerContext);
        CGImageRelease(mLayerCGImage);
        mLayerCGImage = CGBitmapContextCreateImage(mLayerContext);
        //mLayerCGImage = CGImageRetain(mLayerCGImage);
    }
}
- (void) drawPointsToBackBuffer: (NSArray*)pointArray : (NSArray*)interpolatePointsArray startIndex: (int)startIndex endIndex: (int)endIndex : (UIColor*)color
{
    if(mLayerContext == NULL)
        return;
    [self setContextFillColor:mLayerContext color:color];
    for(int i = startIndex ; i < endIndex ; i++)
    {
        NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:i];
        if(interpolatePoints.count > 0)
        {
            for(int n = 0 ; n < interpolatePoints.count ; n++)
            {
                SEDrawTouchPoint* p = [interpolatePoints objectAtIndex:n];
                float lineWidth = [self createLineWidth:p.lineWidth];
                [self drawPoint:p context:mLayerContext lineWidth: lineWidth];
            }
        }
    }
    [self setContextFillColor:mLayerContext color:[UIColor whiteColor]];
    //if(mIsDrawSamplePoint)
    //    [self drawSamplePoints:mLayerContext];
}
- (void) interpolatePointsByBezier : (NSArray*)pointArray interpolatePointsArray : (NSArray*)interpolatePointsArray : (int) startIndex : (int)endIndex
{
    assert(pointArray.count == interpolatePointsArray.count);
    assert(endIndex <= pointArray.count);
    /*
     */
    //int oldIndex = startIndex;
    struct timeval timeStartTime;
    gettimeofday(&timeStartTime, NULL);
    //NSLog(@"startIndex = %d", startIndex);
    int count = 0;
    for(int i = startIndex ; i < endIndex ; i++)
    {
        NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:i];
        if(interpolatePoints.count == 0)
        {
            count++;
            NSArray* pointsByBezier = [self createBezierCurveInterpolatePoint:pointArray :i: mStep];
            if(pointsByBezier.count > 0)
            {
                NSMutableArray* ret = [interpolatePointsArray objectAtIndex:i];
                [ret addObjectsFromArray:pointsByBezier];
                [self calculateLineWidthByBezier: pointArray : interpolatePointsArray: i : i + 1];
                [self createRegion:i :i + 1];
            }
            else 
            {
                int oldIndex = mCurrentCalculateIndex;
                mCurrentCalculateIndex = i;
                mPrevCalculateIndex = oldIndex;
                break;
            }
        }
    }
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = timeStartTime.tv_sec * 1000 + timeStartTime.tv_usec / 1000;
    //NSLog(@"### interpolate bezier time = %d ####", endms - startms);
    //NSLog(@"count = %d", count);
}

- (void) interpolatePointsInPointArray: (NSArray*) pointArray interpolatePointsArray : (NSArray*)interpolatePointsArray
{
    assert(pointArray.count == interpolatePointsArray.count);
    
    for(int i = 0 ; i < pointArray.count - 1 ; i++)
    {
        int currentIndex = i + 1;
        NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:currentIndex];
        if(interpolatePoints.count == 0)
        {
            NSArray* curveInterpolatePoints = [self createCurveInterpolatePoint:pointArray: i];
            if(curveInterpolatePoints.count > 0)
            {
                NSMutableArray* newPointsArray = [NSMutableArray arrayWithArray:[NSArray array]];
                SEDrawTouchPoint* nextPoint = [pointArray objectAtIndex:currentIndex + 1];
                SEDrawTouchPoint* currentPoint = [pointArray objectAtIndex:currentIndex];
                [newPointsArray addObject:currentPoint];
                [newPointsArray addObjectsFromArray:curveInterpolatePoints];
                [newPointsArray addObject: nextPoint];
                NSMutableArray* allPoints = [NSMutableArray array];
                for(int j = 0 ; j < newPointsArray.count - 1 ; j++)
                {
                    SEDrawTouchPoint* p1 = [newPointsArray objectAtIndex:j];
                    SEDrawTouchPoint* p2 = [newPointsArray objectAtIndex:j + 1];
                    NSArray* array = [self fillPointBetween:p1 :p2];
                    if(j > 0)
                    {
                        [allPoints addObject: p1];
                    }
                    [allPoints addObjectsFromArray:array];
                }
                
                if(allPoints.count > 0)
                {
                    float lineWidthStep = (nextPoint.lineWidth - currentPoint.lineWidth) / (allPoints.count + 1);
                    float startLineWidth = currentPoint.lineWidth;
                    for(int n = 0 ; n < allPoints.count ; n++)
                    {
                        SEDrawTouchPoint* p = [allPoints objectAtIndex:n];
                        p.lineWidth = startLineWidth + lineWidthStep * (n + 1);
                    }
                }
                
                NSMutableArray* ret = [interpolatePointsArray objectAtIndex:currentIndex];
                [ret addObjectsFromArray:allPoints];
            }
            else
            {
                break;    
            }
        }
    }

}
- (void) calculatePointAccululate: (NSTimer*)timer
{
    if(mFrameNum == 3)
    {
        NSLog(@"## point accumulate ##");
        NSLog(@"## accumulate point num = %d ##", currentPointArray.count);
        if(currentPointArray.count <= 2)
        {
            SEDrawTouchPoint* p1 = [currentPointArray objectAtIndex:0];
            SEDrawTouchPoint* p2 = nil;
            if(currentPointArray.count == 2)
                p2 = [currentPointArray objectAtIndex:1];
            p1.lineWidth = maxLineWidth;
            p2.lineWidth = maxLineWidth;
            NSLog(@"p1.linewidth = %f, maxLineWidth = %f", p1.lineWidth, maxLineWidth);
        }
        else
        {
            float totalDist = 0;
            for(int i = 1 ; i <= mLastHandleIndex ; i++)
            {
                SEDrawTouchPoint* prevP = [currentPointArray objectAtIndex:i - 1];
                SEDrawTouchPoint* currentP = [currentPointArray objectAtIndex:i];
                float dist = [self pointDist:prevP.point :currentP.point];
                totalDist += dist;
            }
            NSLog(@"accumulate total dist = %f", totalDist);
            NSLog(@"accumulate mLastHandleIndex = %d", mLastHandleIndex);
            if(totalDist < 5)
            {
                for(int i = 0 ; i <= mLastHandleIndex ; i++)
                {
                    SEDrawTouchPoint* currentP = [currentPointArray objectAtIndex:i];
                    currentP.lineWidth = maxLineWidth;
                }
            }
        }
    }
}
- (void) updateBgBuffer
{
    
}
- (void) timerUpdate: (NSTimer*)timer
{
    //NSLog(@"## draw touch timer update ");
    if(currentPointArray.count == 0)
        return;
    if([self isTouchTimeExceed])
        return;
    struct timeval timeStartTime;
    gettimeofday(&timeStartTime, NULL);

    mFrameNum++;
    minLineWidth = getMinLineWidth(maxLineWidth);
    //NSLog(@"mLastHandleIndex = %d", mLastHandleIndex);
    int count = currentPointArray.count;
    //NSLog(@"count = %d", count);
    SEDrawTouchPoint* firstPoint = [currentPointArray objectAtIndex:mLastHandleIndex];
    //UIColor* inverseColor = [self inverseColor:firstPoint.testColor];
    CGFloat lastPointLineWidth = firstPoint.lineWidth;
    float dist = [self pointDist:currentPointArray start:mLastHandleIndex end:count - 1];
    //NSLog(@"dist = %f", dist);
    if(mLastHandleIndex == 0)
    {
        SEDrawTouchPoint* p = [currentPointArray objectAtIndex:0];
        if(p.lineWidth == -1)
            p.lineWidth = minLineWidth;
        p.speedType = SPEED_TYPE1;
    }
    //[mClipImage release];
    //mClipImage = [[self getBackgroundContent: CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)] retain];
    //NSLog(@"timer update clipimage = %f, %f", mClipImage.size.width, mClipImage.size.height);
    [self calculatePointAccululate: timer];
    [self calculatePointSpeed];
    int startIndex = 0;
    if(mLastHandleIndex == 0)
    {
        startIndex = 0;
    }
    else 
    {
        startIndex = mCurrentCalculateIndex;
    }
    [self interpolatePointsByBezier:currentPointArray interpolatePointsArray:currentInterpolatePointsArray : startIndex: currentPointArray.count];
    [self drawPointsToBackBuffer:currentPointArray :currentInterpolatePointsArray startIndex:mPrevCalculateIndex endIndex:mCurrentCalculateIndex : mCurrentColor];
    [self createCGImageFromBackBuffer];
    [self clearAllInterpolatePoint];
    //[self interpolatePointsToFirstPoint:currentPointArray interpolatePointsArray:currentInterpolatePointsArray];
    //[self interpolatePointsInPointArray:currentPointArray interpolatePointsArray:currentInterpolatePointsArray];
    //[self calculatePointArrayLineWidth:currentPointArray interpolatePointsArray:currentInterpolatePointsArray];
    
    //[self setMyDisplay];
    //[self setNeedsDisplay];
   // NSLog(@"###### last point time = %ld #####", [[currentPointArray objectAtIndex:currentPointArray.count - 1] sec]);
    for(int i = mLastHandleIndex ; i < currentPointArray.count - 1; i++)
    {
        SEDrawTouchPoint* p1 = [currentPointArray objectAtIndex:i];
        SEDrawTouchPoint* p2 = [currentPointArray objectAtIndex:i + 1];
        mPointsLength += [self pointDist:p1.point :p2.point];
    }
    mLastHandleIndex = currentPointArray.count - 1;  
    [mTimerUpdateHandler performSelector:mTimerUpdateAction];
    //[self setNeedsDisplay];
    
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = timeStartTime.tv_sec * 1000 + timeStartTime.tv_usec / 1000;
    NSLog(@"### timer update time = %d ####", endms - startms);  
}
- (void) initData
{
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    
    [interpolatePointsArrayList release];
    interpolatePointsArrayList = [[NSMutableArray array] retain];
    
    currentPointColorArray = [NSMutableArray array];
    [currentPointColorArray retain];
    maxLineWidth = 10;
    self.mCurrentColor = [UIColor blackColor];
    mFirst.pointArrayIndex = -1;
    mFirst.pointIndex = -1;
    mSecond.pointArrayIndex = -1;
    mSecond.pointIndex = -1;
    mLineWidthRatio = 1;
    mPointsLength = 0;
    mStep = 2;
}
- (void)dealloc
{
    [background release];
    [pointArrayList release];
    [currentPointColorArray release];
    [mCurrentColor release];
    CGImageRelease(mLayerCGImage);
    [self releaseContext];
    [mClipRectArray release];
    [super dealloc];
}
- (void) resize : (CGRect)r
{
    if(mOrigRect.size.width != r.size.width || mOrigRect.size.height != r.size.height)
    {
        NSLog(@"resize draw touch view");
        if(mLayerCGImage)
        {
            CGImageRelease(mLayerCGImage);
            mLayerCGImage = NULL;
        }
        [self releaseContext];
        mOrigRect = r;
    }
}
- (void)touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    //NSLog(@"draw touch view touch begin\n");
    if([self isTouchTimeExceed])
        return;
    mStartMove = YES;
    UITouch* touch = [touches anyObject];
    CGPoint pt = [[touches anyObject] locationInView:self];
    NSMutableArray* pointArray = [NSMutableArray array];
    [pointArrayList addObject:pointArray];
    currentPointArray = pointArray;
    [currentPointColorArray addObject:mCurrentColor];
    
    NSMutableArray* interpolatePoints = [NSMutableArray array];
    [interpolatePointsArrayList addObject:interpolatePoints];
    currentInterpolatePointsArray = interpolatePoints;
    
    SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
    point.speedType = SPEED_TYPE1;
    point.testColor = [UIColor redColor];
    point.point = pt;
    point.touchState = DRAW_TOUCH_BEGIN;
    struct timeval time;
    gettimeofday(&time, NULL);
    point.milliTime = [SEDrawTouchPoint timeMilliSecond:time];
    point.sec = time.tv_sec;
    point.usec = time.tv_usec;
    point.timestamp = touch.timestamp;
    [pointArray addObject:point];
    [point release];
    NSMutableArray* interpolates = [NSMutableArray array];
    [currentInterpolatePointsArray addObject:interpolates];
    mTimer = [NSTimer timerWithTimeInterval:0.03 target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:mTimer forMode:NSDefaultRunLoopMode];
    mIndexEndForPhrase1 = 0;
    mLastHandleIndex = 0;
    mFrameNum = 0;
    mCurrentCalculateIndex = 0;
    /*
    CGRect clipRect;
    clipRect.origin.x = pt.x - maxLineWidth / 2;
    clipRect.origin.y = pt.y - maxLineWidth / 2;
    clipRect.size.width = maxLineWidth;
    clipRect.size.height = maxLineWidth;
    [self setNeedsDisplayInRect:clipRect];
     */
}
- (void)touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event
{
    if([self isTouchTimeExceed])
        return;
    NSSet* ttt = [event touchesForView:self];
    UITouch* touch = [touches anyObject];
    //NSLog(@"draw touch view touch move = %d \n", ttt.count);
    CGPoint pt = [[touches anyObject] locationInView:self];
    SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
    point.point = pt;
    point.touchState = DRAW_TOUCH_MOVE;
    struct timeval time;
    gettimeofday(&time, NULL);
    point.sec = time.tv_sec;
    point.usec = time.tv_usec;
    point.milliTime = [SEDrawTouchPoint timeMilliSecond:time];
    point.timestamp = touch.timestamp;
    [currentPointArray addObject: point];
    [point release];
    
    NSMutableArray* interpolates = [NSMutableArray array];
    [currentInterpolatePointsArray addObject:interpolates];
    //NSLog(@"## total point count = %d ##", [self totalPointCount]);
    //[self setNeedsDisplay];
}
- (void) touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event
{
    if([self isTouchTimeExceed])
    {
        mStartMove = NO;
        [mTimer invalidate];
        mTimer = nil;
        return;
    }
    NSLog(@"#### points num = %d ##", currentPointArray.count);
    UITouch* touch = [touches anyObject];
    [mTimer invalidate];
    mTimer = nil;
    CGPoint pt = [[touches anyObject] locationInView:self];
    SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
    point.point = pt;
    point.touchState = DRAW_TOUCH_MOVE;
    struct timeval time;
    gettimeofday(&time, NULL);
    point.sec = time.tv_sec;
    point.usec = time.tv_usec;
    point.milliTime = [SEDrawTouchPoint timeMilliSecond:time];
    point.timestamp = touch.timestamp;
    [currentPointArray addObject: point];
    [point release];
    
    NSMutableArray* interpolates = [NSMutableArray array];
    [currentInterpolatePointsArray addObject: interpolates];
    /*
    SEDrawTouchPoint* firstPoint = [currentPointArray objectAtIndex:mLastHandleIndex];
    point.lineWidth = firstPoint.lineWidth;
    point.testColor = [UIColor blackColor];
     */
    [self timerUpdate:nil];
    mStartMove = NO;
    //[self printPointsDeltaTime];
    //[SEUtil savePNGImageToDocument:[UIImage imageWithCGImage: mLayerCGImage] withName:@"signatureBG"];
}
- (void) touchesCancelled:(NSSet *)touches withEvent:(UIEvent *)event
{
    if([self isTouchTimeExceed])
    {
        mStartMove = NO;
        [mTimer invalidate];
        mTimer = nil;
        return;
    }
    UITouch* touch = [touches anyObject];
    [mTimer invalidate];
    mTimer = nil;
    CGPoint pt = [[touches anyObject] locationInView:self];
    SEDrawTouchPoint* point = [[SEDrawTouchPoint alloc] init];
    point.point = pt;
    point.touchState = DRAW_TOUCH_MOVE;
    struct timeval time;
    gettimeofday(&time, NULL);
    point.sec = time.tv_sec;
    point.usec = time.tv_usec;
    point.milliTime = [SEDrawTouchPoint timeMilliSecond:time];
    point.timestamp = touch.timestamp;
    [currentPointArray addObject: point];
    [point release];
    
    NSMutableArray* interpolates = [NSMutableArray array];
    [currentInterpolatePointsArray addObject: interpolates];
    /*
    SEDrawTouchPoint* firstPoint = [currentPointArray objectAtIndex:mLastHandleIndex];
    point.lineWidth = firstPoint.lineWidth;
    point.testColor = [UIColor blackColor];
     */
    [self timerUpdate:nil];
    mStartMove = NO;
    //[self setNeedsDisplay];
}
- (void) clearPoints
{
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    
    [interpolatePointsArrayList release];
    interpolatePointsArrayList = [[NSMutableArray array] retain];
    
    [currentPointColorArray release];
    currentPointColorArray = [NSMutableArray array];
    [currentPointColorArray retain];
    mPointsLength = 0;
    [self clearBackBuffer];
}
- (NSMutableArray*) getPointColorArray
{
    return currentPointColorArray;
}
- (NSMutableArray*) getAllNormalizedPoints
{
    if(pointArrayList == nil || pointArrayList.count == 0)
        return nil;
    CGFloat width = mOrigRect.size.width;
    CGFloat height = mOrigRect.size.height;
    NSMutableArray* retPointList = [NSMutableArray array];
    for(NSMutableArray* points in pointArrayList)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(int i = 0 ; i < points.count; i++)
        {
            SEDrawTouchPoint* point = [points objectAtIndex:i];
            SEDrawTouchPoint* newPoint = [[SEDrawTouchPoint alloc] init];
            CGPoint pt = point.point;
            pt.x = pt.x / width;
            pt.y = pt.y / height;
            newPoint.point = pt;
            newPoint.lineWidth = point.lineWidth;
            newPoint.milliTime = point.milliTime;
            newPoint.sec = point.sec;
            newPoint.usec = point.usec;
            [a addObject:newPoint];
            [newPoint release];
        }
        [retPointList addObject:a];
    }
    return retPointList;    
}
- (void) setBackgroundImage: (UIImage*)image
{
    self.background = image;
}
- (void) setLineWidthRatio: (float)ratio
{
    mLineWidthRatio = ratio;
}
- (float) getLineWidthRatio
{
    return mLineWidthRatio;
}

- (int) getCurrentFrame
{
    return mFrameNum;
}
- (float) getCurrentMoveTime
{
    return mFrameNum * 0.03;
}
- (void) setTimerUpdateHandler: (id)target action: (SEL)action
{
    mTimerUpdateHandler = target;
    mTimerUpdateAction = action;
}
- (void) setHandlerForConsumePoint: (id)target action: (SEL)action
{
    mCanConsumPointAction = action;
    mCanConsumePointHandler = target;
}

- (void) setTouchBeganHandler: (id)target action: (SEL)action
{
    mTouchBeganAction = action;
    mTouchBeganHandler = target;
}
- (void) setTouchMoveHandler: (id)target action: (SEL)action
{
    mTouchMoveHandler = target;
    mTouchMoveAction = action;
}
- (void) setTouchEndHandler: (id)target action: (SEL)action
{
    mTouchEndHandler = target;
    mTouchEndAction = action;
}
- (void) setPointColorArray: (NSMutableArray*)colorArray
{
    [currentPointColorArray release];
    currentPointColorArray = [colorArray retain];
}
- (void) setStep: (float)step
{
    mStep = step;
}
- (void) setNormalizePointsWithOutDraw: (NSMutableArray*)points
{
    mPointsLength = 0;
    mCurrentCalculateIndex = 0;
    mPrevCalculateIndex = 0;
    currentPointArray = nil;
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    CGSize s = CGSizeMake(mOrigRect.size.width, mOrigRect.size.height);
    for(NSArray* obj in points)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(SEDrawTouchPoint* point in obj)
        {
            CGPoint p = point.point;
            p.x = s.width * p.x;
            p.y = s.height * p.y;
            SEDrawTouchPoint* newPoint = [[SEDrawTouchPoint alloc] init];
            newPoint.point = p;
            newPoint.lineWidth = point.lineWidth;
            newPoint.milliTime = point.milliTime;
            newPoint.sec = point.sec;
            newPoint.usec = point.usec;
            [a addObject:newPoint];
            [newPoint release];
        }
        [pointArrayList addObject:a];
    }
    [interpolatePointsArrayList release];
    interpolatePointsArrayList = [[NSMutableArray array] retain];
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        NSMutableArray* interpolatePointsArray = [NSMutableArray array];
        for(int j = 0 ; j < points.count ; j++)
        {
            //SEDrawTouchPoint* point = [points objectAtIndex:j];
            //NSLog(@"point line width = %f", point.lineWidth);
            NSArray* interpolatePoints = [NSMutableArray array];
            [interpolatePointsArray addObject:interpolatePoints];
            if(j < points.count - 1)
            {
                SEDrawTouchPoint* p1 = [points objectAtIndex:j];
                SEDrawTouchPoint* p2 = [points objectAtIndex:j + 1];
                mPointsLength += [self pointDist:p1.point :p2.point];
            }
        }
        [interpolatePointsArrayList addObject:interpolatePointsArray];
        
    }
    mPrevCalculateIndex = 0;
    mCurrentCalculateIndex = 0;
    mLastHandleIndex = 0;
    currentInterpolatePointsArray = nil;
    currentPointArray = nil;
}
- (CGImageRef) createCGImageWithFrame: (CGRect)frame points: (NSArray*)points color: (NSArray*)colors lineWidthRatio: (float)lineWidthRatio
{
    CGSize s = CGSizeMake(frame.size.width, frame.size.height);
    NSMutableArray* myPointArrayList = [NSMutableArray array];
    for(NSArray* obj in points)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(SEDrawTouchPoint* point in obj)
        {
            CGPoint p = point.point;
            p.x = s.width * p.x;
            p.y = s.height * p.y;
            SEDrawTouchPoint* newPoint = [[SEDrawTouchPoint alloc] init];
            newPoint.point = p;
            newPoint.lineWidth = point.lineWidth;
            newPoint.milliTime = point.milliTime;
            newPoint.sec = point.sec;
            newPoint.usec = point.usec;
            [a addObject:newPoint];
            [newPoint release];
        }
        [myPointArrayList addObject:a];
    }
    NSMutableArray* myInterpolatePointsArrayList = [NSMutableArray array];
    for(int i = 0 ; i < myPointArrayList.count ; i++)
    {
        NSArray* points = [myPointArrayList objectAtIndex:i];
        NSMutableArray* interpolatePointsArray = [NSMutableArray array];
        for(int j = 0 ; j < points.count ; j++)
        {
            NSArray* interpolatePoints = [NSMutableArray array];
            [interpolatePointsArray addObject:interpolatePoints];
        }
        [myInterpolatePointsArrayList addObject:interpolatePointsArray];
    }
    float width = s.width;
    float height = s.height;
    float bytesPerRow = width * 4;
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGContextRef drawViewContext = CGBitmapContextCreate (NULL,
                                                          width,
                                                          height,
                                                          8,      // bits per component
                                                          bytesPerRow,
                                                          colorSpace,
                                                          kCGImageAlphaPremultipliedLast);
    CGColorSpaceRelease(colorSpace);
    UserInfo* userInfo = [[PhotoFrameAppDelegate getViewNavigator] getUserInfo];
    UIColor* autoColor = [[PhotoFrameAppDelegate getViewNavigator] mSignatureAutoColor];
    for(int i = 0 ; i < myPointArrayList.count ; i++)
    {
        NSMutableArray* points = [myPointArrayList objectAtIndex:i];
        NSMutableArray* interpolatePoints = [myInterpolatePointsArrayList objectAtIndex:i];
        UIColor* color = [colors objectAtIndex:i];
        if([userInfo.signatureautocolor boolValue] == YES)
        {
            color = autoColor;
        }
        [self setContextFillColor:drawViewContext color:color];
        for(int j = 0 ; j < points.count ; j++)
        {
            NSArray* pointsByBezier = [self createBezierCurveInterpolatePoint:points :j: 0.2];
            if(pointsByBezier.count > 0)
            {
                NSMutableArray* ret = [interpolatePoints objectAtIndex:j];
                [ret addObjectsFromArray:pointsByBezier];
                [self calculateLineWidthByBezier:points :interpolatePoints :j :j + 1];
                for(int k = 0 ; k < pointsByBezier.count ; k++)
                {
                    SEDrawTouchPoint* point = [pointsByBezier objectAtIndex:k];
                    float lineWidth = point.lineWidth;
                    float newLineWidth = lineWidth * lineWidthRatio;
                    if(newLineWidth < 1)
                    {
                        newLineWidth =  1;
                    }
                    [self drawPoint:point context:drawViewContext lineWidth: newLineWidth];
                }
                [ret removeAllObjects];
            }
            //[pool release];
        }
    }
    CGImageRef newImageRef = CGBitmapContextCreateImage(drawViewContext);
    CGContextRelease(drawViewContext);
    return newImageRef;
}
- (void) setNormalizePoints: (NSMutableArray*)points
{
    [self clearBackBuffer];
    mPointsLength = 0;
    mCurrentCalculateIndex = 0;
    mPrevCalculateIndex = 0;
    currentPointArray = nil;
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    CGSize s = CGSizeMake(mOrigRect.size.width, mOrigRect.size.height);
    for(NSArray* obj in points)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(SEDrawTouchPoint* point in obj)
        {
            CGPoint p = point.point;
            p.x = s.width * p.x;
            p.y = s.height * p.y;
            SEDrawTouchPoint* newPoint = [[SEDrawTouchPoint alloc] init];
            newPoint.point = p;
            newPoint.lineWidth = point.lineWidth;
            newPoint.milliTime = point.milliTime;
            newPoint.sec = point.sec;
            newPoint.usec = point.usec;
            [a addObject:newPoint];
            [newPoint release];
        }
        [pointArrayList addObject:a];
    }
    [interpolatePointsArrayList release];
    interpolatePointsArrayList = [[NSMutableArray array] retain];
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        NSMutableArray* interpolatePointsArray = [NSMutableArray array];
        for(int j = 0 ; j < points.count ; j++)
        {
            //SEDrawTouchPoint* point = [points objectAtIndex:j];
            //NSLog(@"point line width = %f", point.lineWidth);
            NSArray* interpolatePoints = [NSMutableArray array];
            [interpolatePointsArray addObject:interpolatePoints];
            if(j < points.count - 1)
            {
                SEDrawTouchPoint* p1 = [points objectAtIndex:j];
                SEDrawTouchPoint* p2 = [points objectAtIndex:j + 1];
                mPointsLength += [self pointDist:p1.point :p2.point];
            }
        }
        [interpolatePointsArrayList addObject:interpolatePointsArray];
        
    }
    ///
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        NSMutableArray* interpolatePoints = [interpolatePointsArrayList objectAtIndex:i];
        currentInterpolatePointsArray = interpolatePoints;
        mCurrentCalculateIndex = 0;
        UIColor* color = [currentPointColorArray objectAtIndex:i];
        [self interpolatePointsByBezier:points interpolatePointsArray:interpolatePoints : 0 : points.count];
        [self drawPointsToBackBuffer:points :interpolatePoints startIndex:0 endIndex:points.count: color];
    }
    mPrevCalculateIndex = 0;
    mCurrentCalculateIndex = 0;
    mLastHandleIndex = 0;
    currentInterpolatePointsArray = nil;
    currentPointArray = nil;
    [self createCGImageFromBackBuffer];
    [self setNeedsDisplay];
}
- (float) createLineWidth: (float)currentLineWidth
{
    float newLineWidth = currentLineWidth * mLineWidthRatio;
    if(newLineWidth < 1)
        return 1;
    else 
    {
        return newLineWidth;
    }
}
- (void) drawAnim
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    for(int i = 0 ; i <= mSecond.pointArrayIndex ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        UIColor* color = nil;
        if(viewNav.mSignatureAutoColor == nil)
        {
            color = [currentPointColorArray objectAtIndex:i];
        }
        else 
        {
            color = viewNav.mSignatureAutoColor;
        }
        [color set];
        int endIndex = points.count - 1;
        if(i == mSecond.pointArrayIndex)
            endIndex = mSecond.pointIndex;
        if(points.count < 2)
        {
            SEDrawTouchPoint* point = [points objectAtIndex:0];
            float lineWidth = [self createLineWidth:point.lineWidth];
            CGContextSetLineWidth(context, lineWidth);
            CGContextSetLineCap(context, kCGLineCapRound);
            CGContextSetLineJoin(context, kCGLineJoinRound);
            CGPoint pt = point.point;
            lineWidth = [self createLineWidth:point.lineWidth];
            CGContextFillEllipseInRect(context, CGRectMake(pt.x , pt.y , lineWidth, lineWidth));
        }
        else
        {
            //
            for(int j = 0 ; j < endIndex ; j++)
            {
                SEDrawTouchPoint* point1 = [points objectAtIndex:j];
                SEDrawTouchPoint* point2 = [points objectAtIndex:j + 1];
                CGPoint pt1 = point1.point;
                CGPoint pt2 = point2.point;
                
                CGContextSetLineJoin(context, kCGLineJoinRound);
                CGContextSetLineCap(context, kCGLineCapRound);
                float lineWidth = [self createLineWidth:point1.lineWidth];
                //CGContextSetLineWidth(context, point1.lineWidth);
                CGContextSetLineWidth(context, lineWidth);
                CGContextBeginPath(context);
                CGContextMoveToPoint(context, pt1.x, pt1.y);
                CGContextAddLineToPoint(context, pt2.x, pt2.y);
                CGContextAddLineToPoint(context, pt1.x, pt1.y);
                CGContextStrokePath(context);
            }
        }
    }
    CGContextRestoreGState(context);
}
- (BOOL) isInflectionPoint: (NSArray*)inflectionPointIndexArray pointArrayIndex: (int)pointArrayIndex piontIndex: (int)pointIndex
{
    for(int i = 0 ; i < inflectionPointIndexArray.count; i++)
    {
        InflectionPoint* p = [inflectionPointIndexArray objectAtIndex:i];
        if(p.pointArrayIndex == pointArrayIndex && p.pointIndex == pointIndex)
            return  YES;
    }
    return NO;
}
- (NSArray*) calculateInflectionPointsInArray: (NSArray*)points
{
    NSMutableArray* inflectionPointIndexArray = [NSMutableArray arrayWithArray:[NSArray array]];
    int j = 0;
    int start = -1, mid = -1, end = -1;
    while (j < points.count) 
    {
        start = j;
        if((j + 1) < points.count)
        {
            mid = j + 1;
        }
        else 
        {
            mid = -1;
            break;
        }
        if((j + 2) < points.count)
        {
            end = j + 2;
        }
        else 
        {
            end = -1;
            break;
        }
        SEDrawTouchPoint* startPoint = [points objectAtIndex:start];
        SEDrawTouchPoint* midPoint = [points objectAtIndex:mid];
        SEDrawTouchPoint* endPoint = [points objectAtIndex:end];
        CGFloat v1x = midPoint.point.x - startPoint.point.x;
        CGFloat v1y = midPoint.point.y - startPoint.point.y;
        CGFloat v2x = endPoint.point.x - midPoint.point.x;
        CGFloat v2y = endPoint.point.y - midPoint.point.y;
        //NSLog(@"v1x = %f, v1y = %f", v1x, v1y);
        //NSLog(@"v2x = %f, v2y = %f", v2x, v2y);
        
        CGFloat dot = v1x * v2x + v1y * v2y;
        CGFloat len1 = sqrtf(v1x * v1x + v1y * v1y);
        CGFloat len2 = sqrtf(v2x * v2x + v2y * v2y);
        CGFloat cos = dot / (len1 * len2);
        //NSLog(@"dot = %f", dot);
        //NSLog(@"len1 = %f, len2 = %f", len1, len2);
        //NSLog(@"cos = %f", cos);
        //assert(cos >= -1 && cos <= 1);
        CGFloat angle = acosf(cos) * 180 / 3.1415926;
        //NSLog(@"angle = %f", angle);
        if(angle >= 2 && angle <= 178)
        {
            InflectionPoint* inflectionPointIndex = [[InflectionPoint alloc] init];
            inflectionPointIndex.pointArrayIndex = 0;
            inflectionPointIndex.pointIndex = mid;
            [inflectionPointIndexArray addObject:inflectionPointIndex];
            [inflectionPointIndex release];
        }
        j++;
    }
    return inflectionPointIndexArray;
}
- (NSArray*) createCorrespondingPoints: (NSArray*)points
{
    NSArray* inflectionPoints = [self calculateInflectionPointsInArray:points];
    NSMutableArray* correspondPointArray = [NSMutableArray arrayWithArray:[NSArray array]];
    for(int i = 0 ; i < points.count - 1; i++)
    {
        SEDrawTouchPoint* currentPoint = [points objectAtIndex:i];
        CGPoint newCurrentPoint;
        if([self isInflectionPoint:inflectionPoints pointArrayIndex:0 piontIndex:i])
        {
            SEDrawTouchPoint* prevPoint = [points objectAtIndex:i - 1];
            SEDrawTouchPoint* nextPoint = [points objectAtIndex:i + 1];
            CGPoint v1 = [self createVector:prevPoint.point endPoint:currentPoint.point];
            v1 = [self vectorNormalize:v1];
            CGPoint v2 = [self createVector:currentPoint.point endPoint:nextPoint.point];
            v2 = [self vectorNormalize:v2];
            CGPoint inverseV1 = CGPointMake(-v1.x, -v1.y);
            CGPoint v;
            if([self isClockWise: v1 : v2])
            {
                v = [self addVector:inverseV1 :v2];
                v = [self vectorNormalize:v];
                newCurrentPoint = [self createPoint:currentPoint.point fromNormalVector:v len:currentPoint.lineWidth];
            }
            else
            {
                v = [self addVector:inverseV1 :v2];
                v = [self vectorNormalize:v];
                v = CGPointMake(-v.x, -v.y);
                newCurrentPoint = [self createPoint:currentPoint.point fromNormalVector:v len:currentPoint.lineWidth];
            }
            
        }
        else
        {
            SEDrawTouchPoint* nextPoint = [points objectAtIndex:i + 1];
            CGPoint v1 = [self createPerpendicularVector:currentPoint.point endPoint:nextPoint.point];
            v1 = [self vectorNormalize:v1];
            newCurrentPoint = [self createPoint:currentPoint.point fromNormalVector:v1 len:currentPoint.lineWidth];
        }
        SEDrawTouchPoint* currentDrawTouchPoint = [[SEDrawTouchPoint alloc] init];
        currentDrawTouchPoint.point = newCurrentPoint;
        currentDrawTouchPoint.lineWidth = currentPoint.lineWidth;
        [correspondPointArray addObject:currentDrawTouchPoint];
        [currentDrawTouchPoint release];
    }
    SEDrawTouchPoint* lastPoint = [points lastObject];
    SEDrawTouchPoint* prevPoint = [points objectAtIndex:points.count - 2];
    CGPoint v = [self createPerpendicularVector:prevPoint.point endPoint:lastPoint.point];
    v = [self vectorNormalize:v];
    CGPoint newLastPoint = [self createPoint:lastPoint.point fromNormalVector:v len:lastPoint.lineWidth];
    SEDrawTouchPoint* lastDrawTouchPoint = [[SEDrawTouchPoint alloc] init];
    lastDrawTouchPoint.point = newLastPoint;
    lastDrawTouchPoint.lineWidth = lastPoint.lineWidth;
    [correspondPointArray addObject:lastDrawTouchPoint];
    [lastDrawTouchPoint release];
    return correspondPointArray;
/*
        if(angle >= 2 && angle <= 178)
        {
            CGPoint v1 = [self createVector:startPoint.point endPoint:midPoint.point];
            CGPoint v2 = [self createVector:midPoint.point endPoint:endPoint.point];
            CGPoint v = [self addVector:v1 :v2];
            CGPoint vv = [self createPerpendicularVector:v];
            v1 = [self createPerpendicularVector:startPoint.point endPoint:midPoint.point];
            v2 = [self createPerpendicularVector:midPoint.point endPoint:endPoint.point];
            CGPoint newStartPoint = [self createPoint:startPoint.point fromNormalVector:v1 len:startPoint.lineWidth];
            CGPoint newMidPoint = [self createPoint:midPoint.point fromNormalVector:vv len:midPoint.lineWidth];
             CGPoint newEndPoint = [self createPoint:endPoint.point fromNormalVector:v2 len:endPoint.lineWidth];
            if(j == 0)
            {
                
            }
        }
        else
        {
            CGPoint v1 = [self createPerpendicularVector:startPoint.point endPoint:endPoint.point];
            CGPoint newStartPoint = [self createPoint:startPoint.point fromNormalVector:v1 len:startPoint.lineWidth];
            CGPoint newMidPoint = [self createPoint: midPoint.point fromNormalVector:v1 len:midPoint.lineWidth];
            CGPoint newEndPoint = [self createPoint:endPoint.point fromNormalVector:v1 len:endPoint.lineWidth];
            
            SEDrawTouchPoint* newDrawStartPoint = [[SEDrawTouchPoint alloc] init];
            newDrawStartPoint.point = newStartPoint;
            [correspondPointArray addObject:newDrawStartPoint];
            [newDrawStartPoint release];
            
            SEDrawTouchPoint* newDrawMidPoint = [[SEDrawTouchPoint alloc] init];
            newDrawMidPoint.point = newMidPoint;
            [correspondPointArray addObject:newDrawMidPoint];
            [newDrawMidPoint release];
            
            SEDrawTouchPoint* newDrawEndPoint = [[SEDrawTouchPoint alloc] init];
            newDrawEndPoint.point = newEndPoint;
            [correspondPointArray addObject:newDrawEndPoint];
            [newDrawEndPoint release];
            
        }
        j++;
    }
*/
}

- (NSArray*) calculateInflectionPoint
{
    NSMutableArray* inflectionPointIndexArray = [NSMutableArray arrayWithArray:[NSArray array]];
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        int j = 0;
        int start = -1, mid = -1, end = -1;
        while (j < points.count) 
        {
            start = j;
            if((j + 1) < points.count)
            {
                mid = j + 1;
            }
            else 
            {
                mid = -1;
                break;
            }
            if((j + 2) < points.count)
            {
                end = j + 2;
            }
            else 
            {
                end = -1;
                break;
            }
            SEDrawTouchPoint* startPoint = [points objectAtIndex:start];
            SEDrawTouchPoint* midPoint = [points objectAtIndex:mid];
            SEDrawTouchPoint* endPoint = [points objectAtIndex:end];
            CGFloat v1x = midPoint.point.x - startPoint.point.x;
            CGFloat v1y = midPoint.point.y - startPoint.point.y;
            CGFloat v2x = endPoint.point.x - midPoint.point.x;
            CGFloat v2y = endPoint.point.y - midPoint.point.y;
            NSLog(@"v1x = %f, v1y = %f", v1x, v1y);
            NSLog(@"v2x = %f, v2y = %f", v2x, v2y);
            
            CGFloat dot = v1x * v2x + v1y * v2y;
            CGFloat len1 = sqrtf(v1x * v1x + v1y * v1y);
            CGFloat len2 = sqrtf(v2x * v2x + v2y * v2y);
            CGFloat cos = dot / (len1 * len2);
            NSLog(@"dot = %f", dot);
            NSLog(@"len1 = %f, len2 = %f", len1, len2);
            NSLog(@"cos = %f", cos);
            //assert(cos >= -1 && cos <= 1);
            CGFloat angle = acosf(cos) * 180 / 3.1415926;
            NSLog(@"angle = %f", angle);
            if(angle >= 2 && angle <= 178)
            {
                InflectionPoint* inflectionPointIndex = [[InflectionPoint alloc] init];
                inflectionPointIndex.pointArrayIndex = i;
                inflectionPointIndex.pointIndex = mid;
                [inflectionPointIndexArray addObject:inflectionPointIndex];
                [inflectionPointIndex release];
            }
            j++;
        }
        
    }
    return inflectionPointIndexArray;
}


- (void) drawLineWithInterpolatePoints
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    assert(interpolatePointsArrayList.count == 0 || pointArrayList.count == interpolatePointsArrayList.count );
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        NSMutableArray* interpolatePointsArray = nil;
        if(interpolatePointsArrayList.count > 0)
        {
            interpolatePointsArray = [interpolatePointsArrayList objectAtIndex:i];
        }
        assert(interpolatePointsArray.count == 0 || interpolatePointsArray.count == points.count);
        if(points.count < 4)
            continue;
        for(int j = 0 ; j < points.count - 1 ; j++)
        {
            //NSMutableArray* newPointsArray = [NSMutableArray array];
            SEDrawTouchPoint* p = [points objectAtIndex:j];
            SEDrawTouchPoint* p1 = [points objectAtIndex:j + 1];
            NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:j];
            if(interpolatePoints.count > 0)
            {
                //[newPointsArray addObjectsFromArray:interpolatePoints];
                SEDrawTouchPoint* startPoint = p;
                for(int n = 0 ; n < interpolatePoints.count ; n++)
                {
                    SEDrawTouchPoint* p2 = [interpolatePoints objectAtIndex:n];
                    float lineWidth = [self createLineWidth:startPoint.lineWidth];
                    CGContextBeginPath(context);
                    CGContextSetLineWidth(context, lineWidth);
                    CGContextSetLineCap(context, kCGLineCapRound);
                    CGContextSetLineJoin(context, kCGLineJoinRound);
                    CGContextMoveToPoint(context, startPoint.point.x, startPoint.point.y);
                    CGContextAddLineToPoint(context, p2.point.x, p2.point.y);
                    CGContextStrokePath(context);
                    startPoint = p2;
                }
                CGContextBeginPath(context);
                float lineWidth = [self createLineWidth:startPoint.lineWidth];
                CGContextSetLineWidth(context, startPoint.lineWidth);
                CGContextSetLineCap(context, kCGLineCapRound);
                CGContextSetLineJoin(context, kCGLineJoinRound);
                CGContextMoveToPoint(context, startPoint.point.x, startPoint.point.y);
                CGContextAddLineToPoint(context, p1.point.x, p1.point.y);
                CGContextStrokePath(context);
            }
            else
            {
                CGContextBeginPath(context);
                float lineWidth = [self createLineWidth:p.lineWidth];
                CGContextSetLineWidth(context, p.lineWidth);
                CGContextSetLineCap(context, kCGLineCapRound);
                CGContextSetLineJoin(context, kCGLineJoinRound);
                CGContextMoveToPoint(context, p.point.x, p.point.y);
                CGContextAddLineToPoint(context, p1.point.x, p1.point.y);
                CGContextStrokePath(context);
            }
            
        }
    }
    CGContextRestoreGState(context);
}
- (BOOL) isPointInClipRect: (SEDrawTouchPoint*)p rect:(CGRect)frame
{
    CGRect r = CGRectMake(p.point.x - p.lineWidth / 2, p.point.y - p.lineWidth / 2, p.lineWidth, p.lineWidth);
    float startx = r.origin.x;
    float starty = r.origin.y;
    float endx = r.origin.x + r.size.width;
    float endy = r.origin.y + r.size.height;
    if(startx <= frame.origin.x + frame.size.width && startx >= frame.origin.x && starty <= frame.origin.y + frame.size.height && starty >= frame.origin.y)
        return YES;
    if(endx <= frame.origin.x + frame.size.width && endx >= frame.origin.x && endy <= frame.origin.y + frame.size.height && endy >= frame.origin.y)
        return YES;
}
- (void) drawPointsAndInterpolatePoints: (CGContextRef) context
{
    //[self drawBackgroundContent:context];
    //CGImageRef newImageRef = [self getBackgroundContent];
    if(currentInterpolatePointsArray == nil || currentInterpolatePointsArray.count == 0)
        return;
    if(currentPointArray == nil || currentPointArray.count == 0)
        return;
    if(context == nil)
        context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    if(mClipImage != nil)
    {
        CGRect clipRect = CGContextGetClipBoundingBox(context);
        NSLog(@"clip rect = %f, %f, %f, %f", mClipRect.origin.x, mClipRect.origin.y, mClipRect.size.width, mClipRect.size.height);
        NSLog(@"real clip rect = %f, %f, %f, %f", clipRect.origin.x, clipRect.origin.y , clipRect.size.width, clipRect.size.height);
        NSLog(@"image size = %f, %f", mClipImage.size.width, mClipImage.size.height);
        CGContextDrawImage(context, CGRectMake(0, 0, mOrigRect.size.width, mOrigRect.size.height), [mClipImage CGImage]);
    }
    
    assert(interpolatePointsArrayList.count == 0 || pointArrayList.count == interpolatePointsArrayList.count );
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        //NSMutableArray* points = [pointArrayList objectAtIndex:i];
        NSMutableArray* points = currentPointArray;
        NSMutableArray* interpolatePointsArray = nil;
        if(interpolatePointsArrayList.count > 0)
        {
            //interpolatePointsArray = [interpolatePointsArrayList objectAtIndex:i];
            interpolatePointsArray = currentInterpolatePointsArray;
        }
        assert(interpolatePointsArray.count == 0 || interpolatePointsArray.count == points.count);

        UIColor* color = [currentPointColorArray objectAtIndex:i];
        [color set];
        if(points.count <= 2)
        {
            if(points.count == 1)
            {
                SEDrawTouchPoint* p = [points objectAtIndex:0];
                float lineWidth = [self createLineWidth:p.lineWidth];
                [self drawPoint: p context:context lineWidth:lineWidth];
            }
            else
            {
                SEDrawTouchPoint* p1 = [points objectAtIndex:0];
                SEDrawTouchPoint* p2 = [points objectAtIndex:1];
                [self drawLine:p1 endPoint:p2 context:context];
            }
        }
        else
        {
            int allPointCount = 0;
            int drawPointCount = 0;
            for(int j = 0 ;  j < points.count ; j++)
            {
               // SEDrawTouchPoint* p = [points objectAtIndex:j];
                NSArray* interpolatePoints = [interpolatePointsArray objectAtIndex:j];
                allPointCount += interpolatePoints.count;
                //UIColor* color;
                int prevIndex = -1;
                for(int k = 0 ; k < interpolatePoints.count ; k++)
                {
                    SEDrawTouchPoint* ip = [interpolatePoints objectAtIndex:k];
                    SEDrawTouchPoint* prevP = nil;
                    if(prevIndex != -1)
                    {
                        prevP = [interpolatePoints objectAtIndex:prevIndex];
                    }
                    //color = [UIColor blackColor];
                    //[color set];
                    float lineWidth = [self createLineWidth:ip.lineWidth];
                    float dist = 0;
                    if(prevIndex != -1)
                    {
                        dist = [self pointDist:ip.point :prevP.point];
                        //NSLog(@"dist = %f", dist);
                    }
                    if(dist >= 1 || prevP == nil)
                    {
                        //if([self isPointInClipRect:ip rect:clipRect])
                        [self drawPoint: ip context:context lineWidth:lineWidth];
                        prevIndex = k;
                        drawPointCount++;
                    }
                }
            }
            //NSLog(@"## all point count = %d ##", allPointCount);
            //NSLog(@"## draw point count = %d ##", drawPointCount);
        }
    }
    CGContextRestoreGState(context);
}
- (void) drawSamplePoints : (CGContextRef)context
{
    //CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        for(int j = 0 ;  j  < points.count ; j++)
        {
            SEDrawTouchPoint* point = [points objectAtIndex:j];
            UIColor* color = [UIColor whiteColor];
            [self setContextFillColor:context color:color];
            [self drawPoint:point context:context lineWidth:2];
        }
    }
    CGContextRestoreGState(context);
}

- (void) drawPoints
{
    //UIBezierPath
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        //NSArray* correspoindsPoints = [self createCorrespondingPoints:points];
        //assert(points.count == correspoindsPoints.count);
        if(points.count < 4)
            continue;
        //NSArray* inflectionPoints = [ self calculateInflectionPointsInArray:points];
        NSArray* curvePoints = [self fillPointByCurve: points];
        for(int j = 0 ;  j < curvePoints.count - 1 ; j++)
        {
            SEDrawTouchPoint* point = [curvePoints objectAtIndex:j];
            SEDrawTouchPoint* nextPoint = [curvePoints objectAtIndex:j + 1];
            NSArray* betweenPoints = [self fillPointBetween:point :nextPoint];
            UIColor* color = [UIColor yellowColor];
            [color set];
            /*
            UIColor* color = point.testColor;
            if(color == nil)
                color = [UIColor blackColor];
            [color set];
             */
            /*
            if([self isInflectionPoint:inflectionPoints pointArrayIndex:0 piontIndex:j])
            {
                UIColor* color = [UIColor yellowColor];
                [color set];
            }
            else {
                UIColor* color = [UIColor blueColor];
                [color set];
            }
             */
            float lineWidth = 2.0f;//[self createLineWidth:point.lineWidth];
            CGContextSetLineWidth(context, lineWidth);
            CGContextSetLineCap(context, kCGLineCapRound);
            CGContextSetLineJoin(context, kCGLineJoinRound);
            CGPoint pt = point.point;
            //CGContextFillEllipseInRect(context, CGRectMake(pt.x - point.lineWidth / 2, pt.y - point.lineWidth / 2, point.lineWidth, point.lineWidth));
            CGContextFillEllipseInRect(context, CGRectMake(pt.x , pt.y , lineWidth, lineWidth));
            //CGContextFillEllipseInRect(context, CGRectMake(point1.point.x, point1.point.y, lineWidth, lineWidth));
            for(int k = 0 ; k < betweenPoints.count ; k++)
            {
                SEDrawTouchPoint* p = [betweenPoints objectAtIndex:k];
                UIColor* color = [UIColor blackColor];
                [color set];
                CGPoint pt = p.point;
                CGContextFillEllipseInRect(context, CGRectMake(pt.x , pt.y , lineWidth, lineWidth));
            }
            
        }
        /*
        for(int j = 0 ; j < correspoindsPoints.count ; j++)
        {
            SEDrawTouchPoint* point1 = [correspoindsPoints objectAtIndex:j];
            UIColor* color = [UIColor blackColor];
            [color set];
            float lineWidth = 1.0f;//[self createLineWidth:point.lineWidth];
            CGContextSetLineWidth(context, lineWidth);
            CGContextSetLineCap(context, kCGLineCapRound);
            CGContextSetLineJoin(context, kCGLineJoinRound);
            CGPoint pt = point1.point;
            CGContextFillEllipseInRect(context, CGRectMake(pt.x , pt.y , lineWidth, lineWidth));
        }
         */
    }
    CGContextRestoreGState(context);
}
- (void) drawSampleLine
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    float linewidth = 10;
    CGPoint startPoint = CGPointMake(mOrigRect.size.width / 2, mOrigRect.size.height / 2);
    for(int i = 0 ; i < 10 ; i++)
    {
        CGContextSetLineWidth(context, linewidth);
        CGContextSetLineCap(context, kCGLineCapRound);
        CGContextSetLineJoin(context, kCGLineJoinRound);
        CGPoint p1 = startPoint;
        CGPoint p2 = CGPointMake(p1.x + 1 , p1.y);
        CGContextBeginPath(context);
        CGContextMoveToPoint(context, p1.x , p1.y);
        CGContextAddLineToPoint(context, p2.x, p2.y);
        CGContextStrokePath(context);
        startPoint = p2;
        linewidth -= 1;
    }
    
    startPoint = CGPointMake(mOrigRect.size.width / 2, mOrigRect.size.height / 2 + 100);
    CGPoint p1 = CGPointMake(startPoint.x, startPoint.y - 5);
    CGPoint p2 = CGPointMake(startPoint.x + 10, startPoint.y - 1);
    CGPoint p3 = CGPointMake(startPoint.x + 10, startPoint.y + 1);
    CGPoint p4 = CGPointMake(startPoint.x, startPoint.y + 5);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, p1.x, p1.y);
    CGContextAddLineToPoint(context, p2.x, p2.y);
    CGContextAddLineToPoint(context, p3.x, p3.y);
    CGContextAddLineToPoint(context, p4.x, p4.y);
    CGContextAddLineToPoint(context, p1.x, p1.y);
    CGContextFillPath(context);
    CGContextRestoreGState(context);
    
}

- (void) drawLine: (SEDrawTouchPoint*)startPoint endPoint:(SEDrawTouchPoint*)endPoint context: (CGContextRef) context
{
    float lineWidth = [self createLineWidth:startPoint.lineWidth];
    CGContextSetLineWidth(context, lineWidth);
    CGContextSetLineCap(context, kCGLineCapRound);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGPoint p1 = startPoint.point;
    CGPoint p2 = endPoint.point;
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, p1.x , p1.y);
    CGContextAddLineToPoint(context, p2.x, p2.y);
    CGContextStrokePath(context);
}
- (void) drawLineFill
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        SEDrawTouchPoint* p1 = [points objectAtIndex:0];
        SEDrawTouchPoint* p2 = nil;
        if(points.count > 1)
            p2 = [points objectAtIndex:1];
        UIColor* color = [currentPointColorArray objectAtIndex:i];
        NSArray* correspondingPoints = [self createCorrespondingPoints:points];
        assert(correspondingPoints.count == points.count);
        NSArray* allPoints = [points arrayByAddingObjectsFromArray:correspondingPoints];
        assert(allPoints.count == correspondingPoints.count + points.count);
        points = allPoints;
        if(points.count <= 2)
        {
            if(points.count == 1)
            {
                SEDrawTouchPoint* point = [points objectAtIndex:0];
                [self drawPoint: point context:context lineWidth:[self createLineWidth:point.lineWidth]];
            }
            else
            {
                SEDrawTouchPoint* startPoint = [points objectAtIndex:0];
                SEDrawTouchPoint* midPoint = [points objectAtIndex:1];
                [self drawLine: startPoint endPoint: midPoint context: context];
            }
            continue;
        }
        //points = [points arrayByAddingObject:p1];
        //points = [points arrayByAddingObject:p2];
        CGContextBeginPath(context);
        CGContextSetLineWidth(context, 1);
        int startIndex = -1;
        int midIndex = -1;
        int endIndex = -1;
        for(int j = 0 ; j < points.count ; j++)
        {
            startIndex = j;
            midIndex = -1;
            endIndex = -1;
            if((j + 1) < points.count)
            {
                midIndex = j + 1;
            }
            else
            {
                midIndex = -1;
                break;
            }
            if((j + 2) < points.count)
            {
                endIndex = j + 2;
            }
            else
            {
                endIndex = -1;
                break;
            }
            SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
            SEDrawTouchPoint* midPoint = [points objectAtIndex:midIndex];
            SEDrawTouchPoint* endPoint = [points objectAtIndex:endIndex];
            CGPoint mid1 = [self midPoint:startPoint.point :midPoint.point];
            CGPoint mid2 = [self midPoint:midPoint.point :endPoint.point];
            CGContextMoveToPoint(context, mid1.x, mid1.y);
            CGContextAddQuadCurveToPoint(context, midPoint.point.x, midPoint.point.y, mid2.x, mid2.y);
        }
        
        /*
        SEDrawTouchPoint* firstPoint = [points objectAtIndex:0];
        SEDrawTouchPoint* secondPoint = [points objectAtIndex:1];
        CGPoint firstSecondMidPoint = [self midPoint:firstPoint.point :secondPoint.point];
        assert(midIndex != -1);
        assert(startIndex != -1);
        SEDrawTouchPoint* midPoint = [points objectAtIndex:midIndex];
        SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
        CGContextAddLineToPoint(context, midPoint.point.x, midPoint.point.y);
        CGContextAddLineToPoint(context, firstPoint.point.x, firstPoint.point.y);
        CGContextAddLineToPoint(context, firstSecondMidPoint.x, firstSecondMidPoint.y);
        //CGContextFillPath(context);
        CGContextDrawPath(context, kCGPathEOFill);
        //CGPoint mid1 = [self midPoint:startPoint.point : midPoint.point];
        //CGPoint mid2 = 
        */
    }
    CGContextRestoreGState(context);
}
- ( void) drawBezierCurve
{
    
}
- (void) drawCurveFill
{
    /*
    CGPoint p1 = CGPointMake(self.frame.size.width / 2, self.frame.size.height / 2);
    CGPoint p2 = CGPointMake(p1.x + 100 , p1.y);
    CGPoint p3 = CGPointMake(p1.x + 50, p1.y + 50);
    CGPoint p4 = CGPointMake(p1.x + 50 , p1.y - 50);
    UIColor* color = [UIColor blackColor];
    CGColorRef c = [color CGColor];
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    
    CGContextSetShouldAntialias(context, YES);
    CGContextSetLineWidth(context, 1);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, p1.x, p1.y);
    CGContextAddQuadCurveToPoint(context, p4.x, p4.y, p2.x, p2.y);
    CGContextAddQuadCurveToPoint(context, p3.x, p3.y, p1.x, p1.y);
    //CGContextClosePath(context);
    //CGContextDrawPath(context, kCGPathFill);
    CGContextFillPath(context);
    CGPoint v = [self createVector:p1 endPoint:p2];
    CGPoint vv = [self createPerpendicularVector:v];
    CGPoint nvv = [self vectorNormalize:vv];
    CGPoint newPoint = CGPointMake(p1.x + nvv.x * 20, p1.y + nvv.y * 20);
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, p1.x, p1.y);
    CGContextAddLineToPoint(context, newPoint.x, newPoint.y);
    CGContextStrokePath(context);
    CGContextRestoreGState(context);
     */
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        SEDrawTouchPoint* p1 = [points objectAtIndex:0];
        SEDrawTouchPoint* p2 = nil;
        if(points.count > 1)
            p2 = [points objectAtIndex:1];
        UIColor* color = [currentPointColorArray objectAtIndex:i];
        NSArray* correspondingPoints = [self createCorrespondingPoints:points];
        assert(correspondingPoints.count == points.count);
        NSArray* allPoints = [points arrayByAddingObjectsFromArray:correspondingPoints];
        assert(allPoints.count == correspondingPoints.count + points.count);
        points = allPoints;
        if(points.count <= 2)
        {
            if(points.count == 1)
            {
                SEDrawTouchPoint* point = [points objectAtIndex:0];
                [self drawPoint: point context:context lineWidth:[self createLineWidth:point.lineWidth]];
            }
            else
            {
                SEDrawTouchPoint* startPoint = [points objectAtIndex:0];
                SEDrawTouchPoint* midPoint = [points objectAtIndex:1];
                [self drawLine: startPoint endPoint: midPoint context: context];
            }
            continue;
        }
        //points = [points arrayByAddingObject:p1];
        //points = [points arrayByAddingObject:p2];
        CGContextBeginPath(context);
        CGContextSetLineWidth(context, 1);
        int startIndex = -1;
        int midIndex = -1;
        int endIndex = -1;
        for(int j = 0 ; j < points.count ; j++)
        {
            startIndex = j;
            midIndex = -1;
            endIndex = -1;
            if((j + 1) < points.count)
            {
                midIndex = j + 1;
            }
            else
            {
                midIndex = -1;
                break;
            }
            if((j + 2) < points.count)
            {
                endIndex = j + 2;
            }
            else
            {
                endIndex = -1;
                break;
            }
            SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
            SEDrawTouchPoint* midPoint = [points objectAtIndex:midIndex];
            SEDrawTouchPoint* endPoint = [points objectAtIndex:endIndex];
            CGPoint mid1 = [self midPoint:startPoint.point :midPoint.point];
            CGPoint mid2 = [self midPoint:midPoint.point :endPoint.point];
            CGContextMoveToPoint(context, mid1.x, mid1.y);
            CGContextAddQuadCurveToPoint(context, midPoint.point.x, midPoint.point.y, mid2.x, mid2.y);
        }
        

        SEDrawTouchPoint* firstPoint = [points objectAtIndex:0];
        SEDrawTouchPoint* secondPoint = [points objectAtIndex:1];
        CGPoint firstSecondMidPoint = [self midPoint:firstPoint.point :secondPoint.point];
        assert(midIndex != -1);
        assert(startIndex != -1);
        SEDrawTouchPoint* midPoint = [points objectAtIndex:midIndex];
        SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
        CGContextAddLineToPoint(context, midPoint.point.x, midPoint.point.y);
        CGContextAddLineToPoint(context, firstPoint.point.x, firstPoint.point.y);
        CGContextAddLineToPoint(context, firstSecondMidPoint.x, firstSecondMidPoint.y);
        //CGContextFillPath(context);
        CGContextDrawPath(context, kCGPathEOFill);
        //CGPoint mid1 = [self midPoint:startPoint.point : midPoint.point];
        //CGPoint mid2 = 
        
    }
    CGContextRestoreGState(context);
}
- (void) drawBezierTestPoint
{
    NSMutableArray* points = [NSMutableArray array];
    SEDrawTouchPoint* p1 = [[SEDrawTouchPoint alloc] init];
    p1.point = CGPointMake(100, 100);
    [points addObject:p1];
    [p1 release];
    
    SEDrawTouchPoint* p2 = [[SEDrawTouchPoint alloc] init];
    p2.point = CGPointMake(150, 154);
    [points addObject:p2];
    [p2 release];
    
    SEDrawTouchPoint* p3 = [[SEDrawTouchPoint alloc] init];
    p3.point = CGPointMake(200, 100);
    [points addObject:p3];
    [p3 release];
    
    NSArray* newPoints = [self createBezierCurveInterpolatePoint:points :0 : mStep];
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    [self drawPoint:p1 context:context lineWidth:2];
    [self drawPoint:p2 context:context lineWidth:2];
    [self drawPoint:p3 context:context lineWidth:2];
    for(int i = 0 ; i < newPoints.count  ; i++)
    {
        SEDrawTouchPoint* p =[newPoints objectAtIndex:i];
        [self drawPoint:p context:context lineWidth:2];
        NSLog(@"p = %f , %f", p.point.x, p.point.y);
    }

    CGContextRestoreGState(context);
}
- (void) drawCurve
{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    CGContextSetShouldAntialias(context, YES);
    //NSArray* inflectionPointArray = [self calculateInflectionPoint];
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSMutableArray* points = [pointArrayList objectAtIndex:i];
        UIColor* color = [currentPointColorArray objectAtIndex:i];
        [color set];
        if(points.count <= 2)
        {
            if(points.count == 1)
            {
                SEDrawTouchPoint* point = [points objectAtIndex:0];
                [self drawPoint: point context:context lineWidth:[self createLineWidth:point.lineWidth]];
            }
            else
            {
                SEDrawTouchPoint* startPoint = [points objectAtIndex:0];
                SEDrawTouchPoint* midPoint = [points objectAtIndex:1];
                [self drawLine: startPoint endPoint: midPoint context: context];
            }
            continue;
        }
        int j = 0;
        int startIndex = -1;
        int midIndex = -1;
        int endIndex = -1;
        int prevIndex = -1;
        while (j < points.count) 
        {
            startIndex = j;
            midIndex = -1;
            endIndex = -1;
            if((j - 1) >= 0)
            {
                prevIndex = j - 1;
            }
            else 
            {
                prevIndex = -1;
            }
            if((j + 1) < points.count)
            {
                midIndex = j + 1;
            }
            else
            {
                midIndex = -1;
                break;
            }
            if((j + 2) < points.count)
            {
                endIndex = j + 2;
            }
            else
            {
                endIndex = -1;
                break;
            }
            
            SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
            SEDrawTouchPoint* midPoint = [points objectAtIndex:midIndex];
            SEDrawTouchPoint* endPoint = [points objectAtIndex:endIndex];
            SEDrawTouchPoint* prevPoint = nil;
            if(prevIndex != -1)
            {
                prevPoint = [points objectAtIndex:prevIndex];
            }
            //SEDrawTouchPoint* nextPoint = [points objectAtIndex:nextIndex];
            CGPoint mid1 = [self midPoint:startPoint.point :midPoint.point];
            CGPoint mid2 = [self midPoint:midPoint.point :endPoint.point];
            float prevLineWidth = [self createLineWidth:prevPoint.lineWidth];
            float lineWidth1 = [self createLineWidth: startPoint.lineWidth];
            float lineWidth2 = [self createLineWidth:midPoint.lineWidth];
            float lineWidth3 = [self createLineWidth:endPoint.lineWidth];
            float lineWidth = (lineWidth1 + lineWidth3) / 2;
            if(prevPoint != nil)
            {
                prevLineWidth = (prevLineWidth + lineWidth2) / 2;
                if(lineWidth != prevLineWidth)
                {
                    lineWidth = (lineWidth + prevLineWidth) / 2;
                }
            }
            /*
            float ratio = lineWidth1 / prevLineWidth;
            if(ratio < 1)
                ratio = 1 / ratio;
            if(ratio >= 2 && prevPoint != nil)
            {
                NSMutableArray* points = [NSMutableArray array];
                CGPoint p1 = [self midPoint:prevPoint.point :startPoint.point];
                CGPoint p2 = mid1;
                float dist = [self pointDist:p1 :p2];
                CGPoint v = CGPointMake((p2.x - p1.x) / dist, (p2.y - p1.y) / dist);
                float stepDist = dist / 5;
                float lineWidthStep = (startPoint.lineWidth - prevPoint.lineWidth) / 4;
                CGPoint startPoint = p1;
                for(int i = 0 ; i < 5 ; i++)
                {
                    float pointLineWidth = prevPoint.lineWidth + i * lineWidthStep;
                    CGPoint nextPoint = CGPointMake(startPoint.x + v.x * stepDist, startPoint.y + v.y * stepDist);
                    CGContextSetLineWidth(context, pointLineWidth);
                    CGContextSetLineCap(context, kCGLineCapRound);
                    CGContextSetLineJoin(context, kCGLineJoinRound);
                    CGContextSetFlatness(context, 0.1f);
                    CGContextBeginPath(context);
                    CGContextMoveToPoint(context, startPoint.x, startPoint.y);
                    CGContextAddLineToPoint(context, nextPoint.x, nextPoint.y);
                    CGContextStrokePath(context);
                    startPoint = nextPoint;
                }
                
            }
            else 
             */
            {
                
            CGContextSetLineWidth(context, lineWidth);
            CGContextSetLineCap(context, kCGLineCapRound);
            CGContextSetLineJoin(context, kCGLineJoinRound);
            CGContextSetFlatness(context, 0.1f);
            CGContextBeginPath(context);
            CGContextMoveToPoint(context, mid1.x, mid1.y);
            CGContextAddQuadCurveToPoint(context, midPoint.point.x, midPoint.point.y, mid2.x, mid2.y);
            CGContextStrokePath(context);
            }
            j++;
        }
   /*     
        if(midIndex == -1)
        {
            SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
            [self drawPoint: startPoint context:context];
        }
        else if(endIndex == -1)
        {
            //SEDrawTouchPoint* prevPoint = [points objectAtIndex:<#(NSUInteger)#>
            SEDrawTouchPoint* startPoint = [points objectAtIndex:startIndex];
            SEDrawTouchPoint* midPoint = [points objectAtIndex:midIndex];
            [self drawLine: startPoint endPoint: midPoint context: context];
        }
       */ 
    }
    CGContextRestoreGState(context);
}
- (void) drawLine
{
     CGContextRef context = UIGraphicsGetCurrentContext();
     CGContextSaveGState(context);
     CGContextSetShouldAntialias(context, YES);
    UIColor* color = [UIColor whiteColor];
    [color set];
     for(int i = 0 ; i < pointArrayList.count ; i++)
     {
         NSMutableArray* points = [pointArrayList objectAtIndex:i];
         //UIColor* color = [currentPointColorArray objectAtIndex:i];
         //[color set];
         if(points.count < 2)
         {
             SEDrawTouchPoint* point = [points objectAtIndex:0];
             float lineWidth = 1.0;//[self createLineWidth:point.lineWidth];
             CGContextSetLineWidth(context, lineWidth);
             CGContextSetLineCap(context, kCGLineCapRound);
             CGContextSetLineJoin(context, kCGLineJoinRound);
             CGPoint pt = point.point;
             //CGContextFillEllipseInRect(context, CGRectMake(pt.x - point.lineWidth / 2, pt.y - point.lineWidth / 2, point.lineWidth, point.lineWidth));
             CGContextFillEllipseInRect(context, CGRectMake(pt.x + lineWidth / 2, pt.y + lineWidth / 2, lineWidth, lineWidth));
         }
         else
         {
             //
             for(int i = 0 ; i < (points.count - 1) ; i++)
             {
                 SEDrawTouchPoint* point1 = [points objectAtIndex:i];
                 SEDrawTouchPoint* point2 = [points objectAtIndex:i + 1];
                 CGPoint pt1 = point1.point;
                 CGPoint pt2 = point2.point;
                 
                 CGContextSetLineJoin(context, kCGLineJoinRound);
                 CGContextSetLineCap(context, kCGLineCapRound);
                 float lineWidth = 1.0;//[self createLineWidth:point1.lineWidth];
                 CGContextSetLineWidth(context, lineWidth);
                 CGContextBeginPath(context);
                 CGContextMoveToPoint(context, pt1.x + lineWidth / 2, pt1.y + lineWidth / 2);
                 CGContextAddLineToPoint(context, pt2.x + lineWidth / 2, pt2.y + lineWidth / 2);
                 CGContextAddLineToPoint(context, pt1.x + lineWidth / 2, pt1.y + lineWidth / 2);
                 CGContextStrokePath(context);
             }
         }
     }
     CGContextRestoreGState(context);
}
/*
- (void) drawLayer:(CALayer *)layer inContext:(CGContextRef)ctx
{
    NSLog(@"### draw layer ####");
}
 */
/*
- (void) drawLayer:(CALayer *)layer inContext:(CGContextRef)ctx
{
    //[background drawInRect:rect];
    if(pointArrayList == nil)
        return;
    struct timeval timeStartTime;
    gettimeofday(&timeStartTime, NULL);
    CGRect clipRect = CGContextGetClipBoundingBox(ctx);
    NSLog(@"## clip rect size = %f, %f", clipRect.size.width, clipRect.size.height);
    //NSLog(@"frame size = %f, %f", self.frame.size.width, self.frame.size.height);
    CGContextDrawImage(ctx, CGRectMake(0, 0, self.frame.size.width, self.frame.size.height), mLayerCGImage);
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = timeStartTime.tv_sec * 1000 + timeStartTime.tv_usec / 1000;
    NSLog(@"### drawrect time = %d ####", endms - startms);    
}

*/
- (void) drawRect: (CGRect)rect
{
    //[background drawInRect:rect];
    if(pointArrayList == nil)
        return;
    /*
     if(mAnimDrawStart)
     {
     [self drawAnim];
     return;
     }
     */
    //if(mClipRectArray.count == 0)
    //    return;
    struct timeval timeStartTime;
    gettimeofday(&timeStartTime, NULL);
    CGContextRef ctx = UIGraphicsGetCurrentContext();
    CGContextSaveGState(ctx);
    //CGRect* rects = (CGRect*)malloc(sizeof(CGRect) * mClipRectArray.count);
    //NSLog(@"clip rect count = %d", mClipRectArray.count);
    //for(int i = 0 ; i < mClipRectArray.count ; i++)
    //{
    //   NSValue* v = [mClipRectArray objectAtIndex:i];
    //    CGRect r = [v CGRectValue];
    //    rects[i] = r;
    //}
    //CGContextClipToRects(ctx, rects, mClipRectArray.count);
    CGRect clipRect = CGContextGetClipBoundingBox(ctx);
    /*
    if(clipRect.size.width == self.frame.size.width && clipRect.size.height == self.frame.size.height)
    {
        if(mStartMove == YES)
        {
            CGContextRestoreGState(ctx);
            return;
        }
    }
     */
    //NSLog(@"## clip rect size = %f, %f", clipRect.size.width, clipRect.size.height);
    //NSLog(@"frame size = %f, %f", self.frame.size.width, self.frame.size.height);
    CGContextDrawImage(ctx, CGRectMake(0, 0, mOrigRect.size.width, mOrigRect.size.height), mLayerCGImage);
    CGContextRestoreGState(ctx);
    //free(rects);
    //[mClipRectArray removeAllObjects];
    /*
     CGColorRef bgColor = layer.backgroundColor;
     const CGFloat* components = CGColorGetComponents(bgColor);
     int componentsNum = CGColorGetNumberOfComponents(bgColor);
     for(int i = 0 ; i < componentsNum ; i++)
     {
     NSLog(@"component %d : %f", i, components[i]);
     }
     
     //[self drawBezierTestPoint];
     //[self drawLine];
     //[self drawSamplePoints];
     //[self drawLineWithInterpolatePoints];
     [self drawPointsAndInterpolatePoints: ctx];
     //[self drawSamplePoints];
     //[self drawSampleLine];
     //[self drawLine];
     //[self drawPoints];
     //[self drawCurve];
     //[self drawCurveFill];
     //[self drawSampleLine];
     */
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = timeStartTime.tv_sec * 1000 + timeStartTime.tv_usec / 1000;
    //NSLog(@"### drawrect time = %d ####", endms - startms); 
    /*
    //[background drawInRect:rect];
    if(pointArrayList == nil)
        return;
    if(mAnimDrawStart)
    {
        [self drawAnim];
        return;
    }
    struct timeval timeStartTime;
    gettimeofday(&timeStartTime, NULL);
    //[self drawBezierTestPoint];
    //[self drawLine];
    //[self drawSamplePoints];
    //[self drawLineWithInterpolatePoints];
    //[mClipImage release];
    //mClipImage = [self getBackgroundContent:CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
    [self drawPointsAndInterpolatePoints: nil];
    [self drawSamplePoints];
    //[self drawSampleLine];
    //[self drawLine];
    //[self drawPoints];
    //[self drawCurve];
    //[self drawCurveFill];
    //[self drawSampleLine];
    
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = timeStartTime.tv_sec * 1000 + timeStartTime.tv_usec / 1000;
    //NSLog(@"### drawrect time = %d ####", endms - startms);
     */
}

- (void) getPointIndexSpan: (float) ratio : (PointIndex*) first : (PointIndex*)second
{
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        for(int j = 0 ;  j < points.count ; j++)
        {
            SEDrawTouchPoint* p = [points objectAtIndex:j];
            if(p->timeRatio == -1)
                continue;
            //NSLog(@"p->timeRatio = %f", p->timeRatio);
            if(ratio == p->timeRatio)
            {
                first->pointArrayIndex = i;
                first->pointIndex  = j;
                second->pointArrayIndex = i;
                second->pointIndex = j;
                return;
            }
            else if(ratio < p->timeRatio)
            {
                if(j == 0)
                {
                    if(i == 0)
                    {
                        first->pointArrayIndex = 0;
                        first->pointIndex = 0;
                        second->pointArrayIndex = 0;
                        second->pointIndex = 0;
                        
                    }
                    else
                    {
                        first->pointArrayIndex = i - 1;
                        NSArray* lastArray = [pointArrayList objectAtIndex:i - 1];
                        first->pointIndex = lastArray.count - 1;
                        second->pointArrayIndex = i;
                        second->pointIndex = j;
                    }
                }
                else
                {
                    first->pointArrayIndex = i;
                    first->pointIndex = j - 1;
                    second->pointArrayIndex = i;
                    second->pointIndex = j;
                }
                return;
            }
        }
    }
}
- (void) actionAfterAnimEnd
{
    [mTarget performSelector:mAction];
    [self clearPoints];
    if(mLayerCGImage)
    {
        CGImageRelease(mLayerCGImage);
    }
    [self releaseContext];
}
- (void) getPlaceInPointArrayByIndex: (int) pointIndex : (int*)outPointArrayIndex : (int*)outPointIndex
{
    int index = 0;
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        for(int j = 0 ; j < points.count ; j++)
        {
            if(index == pointIndex)
            {
                *outPointArrayIndex = i;
                *outPointIndex = j;
                return;
            }
            else 
            {
                index++;
            }
        }
    }
}
- (void) clearAllInterpolatePoint
{
    for(int i = 0 ;i < interpolatePointsArrayList.count ; i++)
    {
        NSArray* interpolatePointsArray = [interpolatePointsArrayList objectAtIndex:i];
        for(int j = 0 ; j < interpolatePointsArray.count ; j++)
        {
            NSMutableArray* interpolatePoints = [interpolatePointsArray objectAtIndex:j];
            [interpolatePoints removeAllObjects];
        }
    }
}
- (void) drawAnimUpdate : (NSTimer*) timer
{
    NSLog(@"## time interval = %f ##", [timer timeInterval]);
    struct timeval timeStartTime;
    gettimeofday(&timeStartTime, NULL);
    float time = mFrameNum * 20; //20 ms
    mFrameNum++;
    float timeRatio;
    if(time >= mTotalTime)
    {
        timeRatio = 1;
        [mAnimTimer invalidate];
        mAnimTimer = nil;
        [mTarget performSelector:mAction];
        return;
    }
    else 
    {
        timeRatio = time / mTotalTime;
    }
    //NSLog(@"time ratio = %f", timeRatio);
    int samplePointNum = [self getTotalSamplePointNum];
    float n = samplePointNum * timeRatio / mTimeScale;
    int index = n;
    //NSLog(@"totalIndex = %d", index);
    int pointArrayIndex = -1;
    int pointIndex = -1;
    [self getPlaceInPointArrayByIndex:index :&pointArrayIndex :&pointIndex];
    //NSLog(@"pointArrayIndex = %d, pointIndex = %d", pointArrayIndex, pointIndex);
    if(pointArrayIndex == -1 || pointIndex == -1)
    {
        mAnimDrawStart = NO;
        [self actionAfterAnimEnd];
        return;
    }
    
    int endIndex = pointIndex;
    pointIndex = mPrevPointIndex;
    /*
    if(pointIndex > (mPrevPointIndex + 1))
    {
        pointIndex = mPrevPointIndex + 1;
    }
     */
    mLastHandleIndex = 0;
    mCurrentCalculateIndex = 0;
    mPrevCalculateIndex = 0;
    currentPointArray = [pointArrayList objectAtIndex:pointArrayIndex];
    currentInterpolatePointsArray = [interpolatePointsArrayList objectAtIndex:pointArrayIndex];
    UIColor* color = [currentPointColorArray objectAtIndex:pointArrayIndex];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    if(viewNav.mSignatureAutoColor != nil)
    {
        color = viewNav.mSignatureAutoColor;
    }
    NSLog(@"startIndex = %d, endIndex = %d", pointIndex, endIndex);
    [self interpolatePointsByBezier:currentPointArray interpolatePointsArray:currentInterpolatePointsArray : pointIndex : endIndex];
    [self drawPointsToBackBuffer:currentPointArray :currentInterpolatePointsArray startIndex:pointIndex endIndex:endIndex : color];
    [self createCGImageFromBackBuffer];
    mPrevPointIndex = endIndex;
    [self clearAllInterpolatePoint];
    struct timeval endTime;
    int endms, startms;
    gettimeofday(&endTime, NULL);
    endms = endTime.tv_sec * 1000 + endTime.tv_usec / 1000;
    startms = timeStartTime.tv_sec * 1000 + timeStartTime.tv_usec / 1000;
    NSLog(@"### draw siganim time = %d ####", endms - startms); 
}
- (float) milliSecondSpan: (struct timeval) t1 : (struct timeval) t2
{
    return (t2.tv_sec - t1.tv_sec) * 1000 + (t2.tv_usec - t1.tv_usec) / (float)1000.0;
}
- (void) setAnimTarget: (id) target action: (SEL) action
{
    mTarget = target;
    mAction = action;
}

- (void) startAnimDraw
{
    if(pointArrayList.count == 0 || currentPointColorArray.count == 0)
    {
        [self actionAfterAnimEnd];
        return;
    }
    assert(pointArrayList.count == currentPointColorArray.count);
    if(mLayerContext == NULL)
    {
        [self initLayerData:CGRectMake(0, 0, mOrigRect.size.width, mOrigRect.size.height)];
        [self createContext];
    }
    if(mLayerContext == NULL)
    {
        [self actionAfterAnimEnd];
        return;
    }
    NSTimeInterval timeInterval = 0;
    mCurrentPointArrayIndex = 0;
    mCurrentPointIndex = 0;
    mPrevPointIndex = 0;
    int samplePointNum = [self getTotalSamplePointNum];
    if(samplePointNum == 0)
    {
        [self actionAfterAnimEnd];
        return;
    }
    if(timeInterval == 0)
    {
        timeInterval = samplePointNum * 15;
    }
    mTotalTime = timeInterval;
    mTimeScale = mTotalTime / (float)(samplePointNum * 15);
    //NSLog(@"mTimeScale = %f", mTimeScale);
    mFrameNum = 0;
    [self clearAllInterpolatePoint];
    [self clearBackBuffer];
    [self setNeedsDisplay];
    /*
    ///////////////////////////////////////////////////////
    NSArray* firstArray = [pointArrayList objectAtIndex:0];
    SEDrawTouchPoint* firstPoint = [firstArray objectAtIndex:0];
    struct timeval firstTime;
    firstTime.tv_sec = firstPoint.sec;
    firstTime.tv_usec = firstPoint.usec;
    
    NSArray* lastArray = [pointArrayList objectAtIndex:pointArrayList.count - 1];
    SEDrawTouchPoint* lastPoint = [lastArray objectAtIndex:lastArray.count - 1];
    struct timeval lastTime;
    lastTime.tv_sec = lastPoint.sec;
    lastTime.tv_usec = lastPoint.usec;
    float timeSpan = [self milliSecondSpan:firstTime :lastTime];
    //for test
    NSLog(@"timeSpan = %f", timeSpan);
    for(int i = 0 ; i < pointArrayList.count ; i++)
    {
        NSArray* points = [pointArrayList objectAtIndex:i];
        UIColor * color = [currentPointColorArray objectAtIndex:i];
        for(int j = 0 ; j < points.count ; j++)
        {
            SEDrawTouchPoint* p = [points objectAtIndex:j];
            if(p.sec != 0)
            {
                struct timeval time;
                time.tv_sec = p.sec;
                time.tv_usec = p.usec;
                p->timeRatio =  [self milliSecondSpan:firstTime :time]/ timeSpan;
            }
            else
            {
                p->timeRatio = -1;    
            }
            NSLog(@"x = %f, y = %f, sec = %ld, usec = %ld, lineWidth = %f, ratio = %f ", p.point.x, p.point.y, p.sec, p.usec, p.lineWidth, p->timeRatio);
        }
    }
    mCurrentPointArrayIndex = 0;
    mCurrentPointIndex = 0;
    mFrameNum = 0;
    mTotalTime = 3000;
    mAnimDrawStart = YES;
    //end
     */
    mAnimTimer = [NSTimer timerWithTimeInterval:0.02 target:self selector:@selector(drawAnimUpdate:) userInfo:nil repeats:YES];
    [[NSRunLoop currentRunLoop] addTimer:mAnimTimer forMode:NSDefaultRunLoopMode];
}
@end

@implementation SEDrawTouchPannel
@dynamic maxLineWidth;
@dynamic mCurrentColor;
@dynamic background;
- (void) createChild:(CGRect)frame
{
    mBackgroundView = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, frame.size.width, frame.size.height)];
    [self addSubview:mBackgroundView];
    [mBackgroundView release];
    
    float leftPadding = 3;
    float rightPadding = 3;
    float topPadding = 3;
    float bottomPadding = 4;
    float delta = 125 - 114;
    mDrawTouchView = [[SEDrawTouchView alloc] initWithFrame:CGRectMake(leftPadding, topPadding, frame.size.width - leftPadding - delta, frame.size.height - topPadding - delta)];
    [self addSubview:mDrawTouchView];
    [mDrawTouchView release];
    //mDrawTouchView.backgroundColor = [UIColor clearColor];

}
- (id) initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if(self)
    {
        [self createChild:frame];
    }
    return self;
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
- (UIColor*) getMCurrentColor
{
    return mDrawTouchView.mCurrentColor;
}
- (void) setMCurrentColor:(UIColor *)mCurrentColor
{
    mDrawTouchView.mCurrentColor = mCurrentColor;
}
- (void) setMaxLineWidth:(CGFloat)maxLineWidth
{
    mDrawTouchView.maxLineWidth = maxLineWidth;
}
- (CGFloat) getMaxLineWidth
{
    return mDrawTouchView.maxLineWidth;
}
- (UIImage*) getBackground
{
    return mDrawTouchView.background ;
}
- (void) setBackground:(UIImage *)background
{
    mDrawTouchView.background = background;
    mBackgroundView.image = background;
}
- (void) initData
{
    [mDrawTouchView initData];
}
// get points data's array
// return value is an array about arrays
- (NSMutableArray*) getAllNormalizedPoints
{
    return [mDrawTouchView getAllNormalizedPoints];
}
- (void) setNormalizePoints: (NSMutableArray*)points
{
    [mDrawTouchView setNormalizePoints:points];
}
- (NSMutableArray*) getPointColorArray
{
    return [mDrawTouchView getPointColorArray];
}
- (void) setPointColorArray: (NSMutableArray*)colorArray
{
    [mDrawTouchView setPointColorArray:colorArray];
}
- (void) clearPoints
{
    [mDrawTouchView clearPoints];
}
- (void) startAnimDraw
{
    [mDrawTouchView startAnimDraw];
}
- (void) setAnimTarget: (id) target action: (SEL) action
{
    [mDrawTouchView setAnimTarget:target action:action];
}
- (void) setBackgroundImage: (UIImage*)image
{
    [mDrawTouchView setBackgroundImage:image];
}
- (void) setLineWidthRatio: (float)ratio
{
    [mDrawTouchView setLineWidthRatio:ratio];
}
- (void) setNeedsDisplay
{
    [super setNeedsDisplay];
    [mDrawTouchView setNeedsDisplay];
}
- (void) setIsDrawSamplePoint: (BOOL) b
{
    mDrawTouchView.mIsDrawSamplePoint = b;
}
- (BOOL) isDrawSamplePoint
{
    return mDrawTouchView.mIsDrawSamplePoint;
}
- (void) setDrawInMainDisplay: (BOOL) b
{
    mDrawTouchView.mDrawInMainScreen = b;
}
- (CGImageRef) getCGImage
{
    return [mDrawTouchView getCGImage];
}
- (float) getCurrentMoveTime
{
    return [mDrawTouchView getCurrentMoveTime];
}
- (int) getCurrentFrame
{
    return [mDrawTouchView getCurrentFrame];
}
- (void) setTouchBeganHandler: (id)target action: (SEL)action
{
    [mDrawTouchView setTouchBeganHandler:target action:action];
}
- (void) setTouchMoveHandler: (id)target action: (SEL)action
{
    [mDrawTouchView setTouchMoveHandler:target action:action];
}
- (void) setTouchEndHandler: (id)target action: (SEL)action
{
    [mDrawTouchView setTouchEndHandler:target action:action];
}
- (void) setTimerUpdateHandler: (id)target action: (SEL)action
{
    [mDrawTouchView setTimerUpdateHandler:target action:action];
}
- (void) setHandlerForConsumePoint: (id)target action: (SEL)action
{
    [mDrawTouchView setHandlerForConsumePoint:target action:action];
}
- (float) getPointsLength
{
    return [mDrawTouchView getPointsLength];
}
- (float) getLineWidthRatio
{
    return [mDrawTouchView getLineWidthRatio];
}
- (void) setStep: (float)step
{
    [mDrawTouchView setStep:step];
}
- (CGImageRef) createCGImageWithFrame: (CGRect)frame points: (NSMutableArray*)points color: (NSMutableArray*)colors lineWidthRatio: (float)lineWidthRatio
{
    return [mDrawTouchView createCGImageWithFrame:frame points:points color:colors lineWidthRatio:lineWidthRatio];
}
@end