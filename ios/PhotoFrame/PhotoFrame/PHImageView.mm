//
//  PHImageView.m
//  PhotoFrame
//
//  Created by 陈勇 on 11-11-10.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PHImageView.h"
#import "PGMDataReader.h"
#import "SEViewNavigator.h"
#import "SEResDefine.h"
#import "UserInfo.h"
#import "SEDrawTouchView.h"
#import "SESystemConfig.h"
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
@synthesize mViewNav;
@synthesize image;
@synthesize mRotateScreen;
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
- (CGImageRef) createSignatureImage: (CGImageRef)imageRef frame: (CGRect) frame
{
    CGImageRef srcImage = [image CGImage];
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    size_t width = CGImageGetWidth(srcImage);
    size_t height = CGImageGetHeight(srcImage);
    size_t bytesPerRow = width * 4;
    UInt8* data = (UInt8*)malloc(height * bytesPerRow);
    CGContextRef context = CGBitmapContextCreate((void*)data, width, height, 8, bytesPerRow, colorSpace, kCGImageAlphaNoneSkipLast);
    CGRect r = frame;
    NSLog(@"### paint frame = %f, %f, %f, %f ##", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    CGContextDrawImage(context, CGRectMake(0, 0, mViewNav.mViewPortWidth, mViewNav.mViewPortHeight), srcImage);
    CGContextSaveGState(context);
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        CGContextScaleCTM(context, 1, -1);
        CGContextTranslateCTM(context, r.origin.x, r.origin.y + r.size.height);
        CGContextRotateCTM(context, -3.1415926 / 2);
        CGRect drawRect = CGRectMake(0, 0, r.size.height, r.size.width);
        CGContextDrawImage(context, drawRect, imageRef);
    }
    else
    {
        CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        CGContextScaleCTM(context, 1, -1);
        CGContextDrawImage(context, r, imageRef);

    }
    CGContextRestoreGState(context);
    //draw gidea information in image
    /*
    float textwidth = 700;
    float textheight = 10;
    NSString* ituneStr = [SESystemConfig getITuneConnetionStr];
    const char* str = [ituneStr cStringUsingEncoding:NSASCIIStringEncoding];
    size_t strLen = strlen(str);
    CGContextSaveGState(context);
    UIColor* color = [UIColor whiteColor];
    CGColorRef colorRef = [color CGColor];
    CGColorSpaceRef colorSpaceRef = CGColorSpaceCreateDeviceRGB();
    CGContextSetFillColorSpace(context, colorSpaceRef);
    CGContextSetFillColorWithColor(context, colorRef);
    CGContextSetTextMatrix(context, CGAffineTransformIdentity);
    CGContextSelectFont(context, "Helvetica", 20,  kCGEncodingMacRoman);  
    CGContextSetTextDrawingMode(context, kCGTextFill);
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGContextTranslateCTM(context, mViewNav.mViewPortWidth, 0);
        //CGContextScaleCTM(context, 1, -1);
        //CGContextTranslateCTM(context, mViewNav.mViewPortWidth - textheight, textwidth);
        CGContextRotateCTM(context, 3.1415926 / 2);
        CGContextShowTextAtPoint(context, 0, 0, str, strLen);
    }
    else
    {
        //CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        //CGContextScaleCTM(context, 1, -1);
        //CGContextTranslateCTM(context, 10, textwidth);
        CGContextShowTextAtPoint(context, 0, 0, str, strLen);
    }
    CGColorSpaceRelease(colorSpaceRef);
    CGContextRestoreGState(context);
     */
    //end
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    free(data);
    CGColorSpaceRelease(colorSpace);
    return newImageRef;
}
- (void) paintSignatureImage: (CGImageRef)imageRef frame: (CGRect) frame
{
    /*
    CGImageRef srcImage = [image CGImage];
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    size_t width = CGImageGetWidth(srcImage);
    size_t height = CGImageGetHeight(srcImage);
    size_t bytesPerRow = width * 4;
    UInt8* data = (UInt8*)malloc(height * bytesPerRow);
    CGContextRef context = CGBitmapContextCreate((void*)data, width, height, 8, bytesPerRow, colorSpace, kCGImageAlphaNoneSkipLast);
    CGRect r = frame;
    NSLog(@"### paint frame = %f, %f, %f, %f ##", frame.origin.x, frame.origin.y, frame.size.width, frame.size.height);
    CGContextDrawImage(context, CGRectMake(0, 0, mViewNav.mViewPortWidth, mViewNav.mViewPortHeight), srcImage);
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        CGContextScaleCTM(context, 1, -1);
        CGContextTranslateCTM(context, r.origin.x, r.origin.y + r.size.height);
        CGContextRotateCTM(context, -3.1415926 / 2);
        CGRect drawRect = CGRectMake(0, 0, r.size.height, r.size.width);
        CGContextDrawImage(context, drawRect, imageRef);
    }
    else
    {
        CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        CGContextScaleCTM(context, 1, -1);
        CGContextDrawImage(context, r, imageRef);
    }
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    free(data);
    CGColorSpaceRelease(colorSpace);
    */
    CGRect r = frame;
    CGImageRef newImageRef = [self createSignatureImage:imageRef frame:frame];
    self.image = [UIImage imageWithCGImage:newImageRef];
    SS_MyRect rect;
    rect.x = frame.origin.x;
    rect.y = frame.origin.y;
    rect.w = frame.size.width;
    rect.h = frame.size.height;
    //SS_DrawImageToGBackground(imageRef, rect);
    SS_Canvas* currentCanvas = SS_GetCurrentCanvas();
    SS_CanvasDrawImage(currentCanvas, newImageRef, rect);
    CGImageRelease(newImageRef);
    [self setNeedsDisplayInRect:r];
}
- (void) drawRect:(CGRect)rect
{
    NSDate* startDate = [NSDate date];
    [image drawAtPoint:CGPointMake(0, 0 )];
    /*
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    [[UIColor redColor] set];
    CGContextMoveToPoint(context, 0, 380);
    CGContextAddLineToPoint(context, 1023, 380);
    CGContextStrokePath(context);
    CGContextRestoreGState(context);
     */
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

    if(needDrawFrame)
    {
        needDrawFrame = NO;
        [blockImage drawInRect:CGRectMake(0, 0, 1024, 10)];
        [blockImage drawInRect:CGRectMake(0, 0, 10, 768)];
        [blockImage drawInRect:CGRectMake(1014, 0, 10, 768)];
        [blockImage drawInRect:CGRectMake(0, 758, 1024, 10)];
    }
    
    //draw gidea information in image
    /*
    float textwidth = 700;
    float textheight = 50;
    char str[] = "testasdfasdfa";
    size_t strLen = strlen(str);
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSaveGState(context);
    //CGContextSetFillColorSpace(context, getThe)
    UIColor* color = [UIColor redColor];
    CGColorRef colorRef = [color CGColor];
    //
    CGColorSpaceRef colorSpaceRef = CGColorSpaceCreateDeviceRGB();
    CGContextSetFillColorSpace(context, colorSpaceRef);
    CGContextSetFillColorWithColor(context, colorRef);
    //CGContextFillRect(context, CGRectMake(100, 100, 100, 100));
    CGContextSetTextMatrix(context, CGAffineTransformMakeRotation(M_PI));
    CGContextSelectFont(context, "Helvetica", 40,  kCGEncodingMacRoman);  
     CGContextSetTextDrawingMode(context, kCGTextFill);
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        CGContextScaleCTM(context, 1, -1);
        CGContextTranslateCTM(context, mViewNav.mViewPortWidth - textheight, textwidth);
        CGContextRotateCTM(context, -3.1415926 / 2);
        CGContextShowTextAtPoint(context, 0, 0, str, strLen);
    }
    else
    {
        //CGContextTranslateCTM(context, 0, mViewNav.mViewPortHeight);
        //CGContextScaleCTM(context, 1, -1);
        //CGContextTranslateCTM(context, 10, textwidth);
        CGContextShowTextAtPoint(context, 100, 100, str, strLen);
    }
    CGColorSpaceRelease(colorSpaceRef);
    CGContextRestoreGState(context);
     */
    //end
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
    [left release];
    [right release];
    [top release];
    [bottom release];
    [super dealloc];
}
- (void) setPoints:(NSArray*)points
{
    [pointArrayList release];
    pointArrayList = [NSMutableArray array];
    [pointArrayList retain];
    CGSize size = CGSizeMake(320 , 240);
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([userInfo.signaturesize intValue] == 1) // medium
    {
        size = CGSizeMake(420, 340);
    }
    else if([userInfo.signaturesize intValue] == 2)//big
    {
        size = CGSizeMake(640, 480);
    }
    CGPoint startPoint = CGPointMake(1024 - size.width, 768 - size.height);
    for(NSArray* obj in points)
    {
        NSMutableArray* a = [NSMutableArray array];
        for(SEDrawTouchPoint* v in obj)
        {
            NSLog(@"point = %f, %f", v.point.x, v.point.y);
            CGPoint p = v.point;
            p.x = startPoint.x + size.width * p.x;
            p.y = startPoint.y + size.height * p.y;
            [a addObject:[NSValue valueWithCGPoint:p]];
        }
        [pointArrayList addObject:a];
    }

}
- (void) animEndHandler
{
    NSLog(@"enter anim end handler ");
    for(int i = 0 ; i < 4 ; i++)
    {
        NSLog(@"animFinished %d = %d ", i, animFinished[i]);
        if(animFinished[i] == NO)
            return;
    }
    [left removeFromSuperview];
    [right removeFromSuperview];
    [top removeFromSuperview];
    [bottom removeFromSuperview];
    needDrawFrame = YES;
    [self setNeedsDisplay];
}
- (void) playFrameAnim
{
    if(blockImage == nil)
        blockImage = [mViewNav.mResLoader getImage:@"DrawingFinishFrameImage"];
    if(left == nil)
    {
        left = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 10, 768)];
        left.image = blockImage;
    }
    if(right == nil)
    {
        right = [[UIImageView alloc] initWithFrame:CGRectMake(1014, 0, 10, 768)];
        right.image = blockImage;
    }
    if(top == nil)
    {
        top = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, 1024, 10)];   
        top.image = blockImage;
    }
    if(bottom == nil)
    {
        bottom = [[UIImageView alloc] initWithFrame:CGRectMake(0, 758, 1024, 10)];
        bottom.image = blockImage;
    }
    NSLog(@"subviews = %d", self.subviews.count);
    if(self.subviews.count != 0)
        return;
    [self addSubview:left];
    [self addSubview:right];
    [self addSubview:top];
    [self addSubview:bottom];
    for(int i = 0 ; i < 4 ; i++)
    {
        animFinished[i] = NO;
    }
    CGPoint leftCenter = left.center;
    left.center = CGPointMake(leftCenter.x - 10, leftCenter.y);
    void (^leftAnimEndBlock) (BOOL) = ^(BOOL){
        animFinished[0] = YES;
        [self animEndHandler];
    };
    void (^leftAnimBlock)(void) = ^{
        left.center = leftCenter;
    };
    [UIView animateWithDuration:1.5 animations:leftAnimBlock completion:leftAnimEndBlock];
    
    CGPoint rightCenter = right.center;
    right.center = CGPointMake(rightCenter.x + 10, rightCenter.y);
    void (^rightAnimEndBlock)(BOOL) = ^(BOOL) {
        animFinished[1] = YES;
        [self animEndHandler];
    };
    void (^rightAnimBlock) (void) = ^{
        right.center = rightCenter;
    };
    [UIView animateWithDuration:1.5 animations:rightAnimBlock completion:rightAnimEndBlock];
    
    CGPoint topCenter = top.center;
    top.center = CGPointMake(topCenter.x, topCenter.y - 10);
    void (^topAnimEndBlock)(BOOL) = ^(BOOL) {
        animFinished[2] = YES;
        [self animEndHandler];
    };
    void (^topAnimBlock)(void) = ^{
        top.center = topCenter;
    };
    [UIView animateWithDuration:1.5 animations:topAnimBlock completion:topAnimEndBlock];
    
    CGPoint bottomCenter = bottom.center;
    bottom.center = CGPointMake(bottomCenter.x, bottomCenter.y + 10);
    void (^bottomAnimEndBlock)(BOOL) = ^(BOOL)
    {
        animFinished[3] = YES;
        [self animEndHandler];
    };
    void (^bottomAnimBlock) (void) = ^{
        bottom.center = bottomCenter;
    }; 
    [UIView animateWithDuration:1.5 animations:bottomAnimBlock completion:bottomAnimEndBlock];
}
@end
