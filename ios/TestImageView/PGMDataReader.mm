//
//  PGMDataReader.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-7.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PGMDataReader.h"
#import <Foundation/Foundation.h>
#import "SSImageLoader.h"
#include "ppmtool.h"
#include "gimpressionist.h"
struct _PGMRetData
{
    const char* data;
    int len;
};
@interface PGMDataReader : NSObject
- (struct _PGMRetData) read:(NSString*)fileName;
+ (UIImageView*)getImageView;
@end

@implementation PGMDataReader

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}
- (struct _PGMRetData) read:(NSString*)fileName
{
    NSArray* fileNameArray = [fileName componentsSeparatedByString:@"."];
    assert([fileNameArray count] == 2);
    NSString* s = [fileNameArray objectAtIndex:0];
    NSString* filePath = [[NSBundle mainBundle] pathForResource:s ofType:@"pgm"];
    NSData* fileData = [NSData dataWithContentsOfFile:filePath];
    if(fileData)
    {
        struct _PGMRetData ret;
        ret.data = (const char*)[fileData bytes];
        ret.len= [fileData length];
        return ret;
    }
    else
    {
        struct _PGMRetData ret;
        ret.data = NULL;
        ret.len = 0;
        return ret;
    }
}
+ (UIImageView*)getImageView
{
    UIWindow* window = [[UIApplication sharedApplication] keyWindow];
    UIViewController* viewController = window.rootViewController;
    UIView* rootView = viewController.view;
    UIView* v = [rootView viewWithTag:1];
    if(v)
    {
        UIImageView* imageView = (UIImageView*)v;
        return imageView;
    }
    else
        return nil;
}
@end
void getPgmData(const char* fn, const char** outData, int* outLen)
{
    PGMDataReader* pgmData = [[PGMDataReader alloc] init];
    NSString* str = [NSString stringWithFormat:@"%s", fn];
    struct _PGMRetData ret = [pgmData read:str];
    *outData = ret.data;
    *outLen = ret.len;
    [pgmData release];
}
void SE_setBackground(struct ppm* background)
{
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^(void)
                   {

                       UIImageView* imageView = [PGMDataReader getImageView];
                       if(imageView)
                       {
                           CGImageRef cgImage = [SSImageLoader createCGImage:background];
                           UIImage* image = [UIImage imageWithCGImage:cgImage];
                           imageView.image = image;
                           CGImageRelease(cgImage);
                       }
                   });
}
void SE_startBrushPaintInMainQueue(BrushPiece inputbp)
{
    __block BrushPiece bp = inputbp;
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^(void)
                   {
                       //LOGI("## bp.x = %d, bp.y = %d ##", bp.x, bp.y);
                       unsigned char* data = gBackground.col;
                       int dstrowstride = gBackground.width * 3;
                       int srcrowstride = bp.data.width * 3;
                       int startx = bp.x;
                       int starty = bp.y;
                       int y;
                       int x;
                       for(y = 0 ; y < bp.data.height ; y++)
                       {
                           guchar* row = bp.data.col + y * srcrowstride;
                           guchar* dstrow = data + starty * dstrowstride;
                           guchar* alpharow = bp.alpha.col + y * bp.alpha.width;
                           startx = bp.x;
                           for(x = 0 ; x < bp.data.width ; x++)
                           {
                               guchar* src = row + x * 3;
                               guchar* srcalpha = alpharow + x;
                               guchar* dst = dstrow + startx * 3;
                               
                               if(srcalpha[0] == 255)
                               {
                                   dst[0] = src[0];
                                   dst[1] = src[1];
                                   dst[2] = src[2];
                               }
                               else if(srcalpha[0] != 0)
                               {
                                   dst[0] = ((srcalpha[0]) / 255.0f) * src[0] + (1 - (srcalpha[0]) / 255.0f) * dst[0];
                                   dst[1] = ((srcalpha[0]) / 255.0f) * src[1] + (1 - (srcalpha[0]) / 255.0f) * dst[1];
                                   dst[2] = ((srcalpha[0]) / 255.0f) * src[2] + (1 - (srcalpha[0]) / 255.0f) * dst[2];
                               }
                               startx++;
                           }
                           starty++;
                       }
                       ppm_kill(&bp.data);
                       ppm_kill(&bp.alpha);
                       UIImageView* imageView = [PGMDataReader getImageView];
                       if(imageView)
                       {
                           CGImageRef cgImage = [SSImageLoader createCGImage:&gBackground];
                           UIImage* image = [UIImage imageWithCGImage:cgImage];
                           imageView.image = image;
                           CGImageRelease(cgImage);
                       }
                   });
}
void SE_startBrushPaint()
{
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^(void)
                   {
                       
    BrushPiece bp = getNextBrushPiece();
    //LOGI("## bp.x = %d, bp.y = %d ##", bp.x, bp.y);
    if(bp.x == -2 && bp.y == -2)
        return;
    if(bp.x == -1 && bp.y == -1)
    {
        SE_startBrushPaint();
        return;
    }
                       unsigned char* data = gBackground.col;
    int dstrowstride = gBackground.width * 3;
    int srcrowstride = bp.data.width * 3;
    int startx = bp.x;
    int starty = bp.y;
    int y;
    int x;
    for(y = 0 ; y < bp.data.height ; y++)
    {
        guchar* row = bp.data.col + y * srcrowstride;
        guchar* dstrow = data + starty * dstrowstride;
        guchar* alpharow = bp.alpha.col + y * bp.alpha.width;
        startx = bp.x;
        for(x = 0 ; x < bp.data.width ; x++)
        {
            guchar* src = row + x * 3;
            guchar* srcalpha = alpharow + x;
            guchar* dst = dstrow + startx * 3;
                               
            if(srcalpha[0] == 255)
            {
                dst[0] = src[0];
                dst[1] = src[1];
                dst[2] = src[2];
            }
            else if(srcalpha[0] != 0)
            {
                dst[0] = ((srcalpha[0]) / 255.0f) * src[0] + (1 - (srcalpha[0]) / 255.0f) * dst[0];
                dst[1] = ((srcalpha[0]) / 255.0f) * src[1] + (1 - (srcalpha[0]) / 255.0f) * dst[1];
                dst[2] = ((srcalpha[0]) / 255.0f) * src[2] + (1 - (srcalpha[0]) / 255.0f) * dst[2];
            }
            startx++;
        }
        starty++;
    }
    ppm_kill(&bp.data);
    ppm_kill(&bp.alpha);
                       UIImageView* imageView = [PGMDataReader getImageView];
                       if(imageView)
                       {
                           CGImageRef cgImage = [SSImageLoader createCGImage:&gBackground];
                           UIImage* image = [UIImage imageWithCGImage:cgImage];
                           imageView.image = image;
                           CGImageRelease(cgImage);
                           SE_startBrushPaint();
                       }
                   });
}