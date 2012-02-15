//
//  SEUtil.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-21.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUtil.h"
#import <stdarg.h>
#import <AssetsLibrary/AssetsLibrary.h>
static CGContextRef MyCreateBitmapContext (int pixelsWide,
                                    int pixelsHigh)
{
    CGContextRef    context = NULL;
    CGColorSpaceRef colorSpace;
    void *          bitmapData;
    int             bitmapByteCount;
    int             bitmapBytesPerRow;
    
    bitmapBytesPerRow   = (pixelsWide * 4);// 1
    bitmapByteCount     = (bitmapBytesPerRow * pixelsHigh);
    
    colorSpace = CGColorSpaceCreateDeviceRGB();// 2
    bitmapData = malloc( bitmapByteCount );// 3
    memset(bitmapData, 0, bitmapByteCount);
    if (bitmapData == NULL)
    {
        fprintf (stderr, "Memory not allocated!");
        return NULL;
    }
    context = CGBitmapContextCreate (bitmapData,// 4
                                     pixelsWide,
                                     pixelsHigh,
                                     8,      // bits per component
                                     bitmapBytesPerRow,
                                     colorSpace,
                                     kCGImageAlphaNoneSkipLast);
    if (context== NULL)
    {
        free (bitmapData);// 5
        fprintf (stderr, "Context not created!");
        return NULL;
    }
    CGColorSpaceRelease( colorSpace );// 6
    
    return context;// 7
}
@implementation SEUtil

- (id)init
{
    self = [super init];
    if (self) {
        // Initialization code here.
    }
    
    return self;
}
+ (NSArray*) getFilePathWithType: (id) firstType, ...
{
    id eachObj = firstType;
    va_list argList;
    NSArray* array = [NSArray array];
    va_start(argList, firstType);
    while (eachObj != nil) 
    {
        NSString* type = (NSString*)eachObj;
        NSArray* currArray = [[NSBundle mainBundle] pathsForResourcesOfType:type inDirectory:nil];
        array = [array arrayByAddingObjectsFromArray:currArray];
        eachObj = va_arg(argList, id);
    } 
    va_end(argList);
    return array;
}
+ (CGImageRef) copyImageRef: (CGImageRef) srcImage
{
    CGColorSpaceRef srcColorRef = CGImageGetColorSpace(srcImage);
    return CGImageCreateCopyWithColorSpace(srcImage, srcColorRef);   
}
+ (UIImage*) cropUIImage: (UIImage*) image withRect:(CGSize)s;
{
    CGContextRef con = MyCreateBitmapContext(s.width, s.height);    
    CGImageRef imageRef = [image CGImage];
    CGContextDrawImage(con, CGRectMake(0, 0, s.width, s.height), imageRef);
    CGImageRef retImageRef = CGBitmapContextCreateImage(con);
    UIImage* retImage = [UIImage imageWithCGImage:retImageRef];
    CGContextRelease(con);
    CGImageRelease(retImageRef);
    return retImage;
}
+ (UIImage*) transformImage: (UIImage*)srcImage transProperty: (struct UIImageTransformProperty)p
{
    float alpha = p.alpha;
    CGImageRef srcImageRef = [srcImage CGImage];
    size_t width = CGImageGetWidth(srcImageRef);
    size_t height = CGImageGetHeight(srcImageRef);
    size_t bytesPerRow = 4 * width;
    UInt8* data = (UInt8 *)malloc(height * bytesPerRow);
    if(data == NULL)
        return nil;
    CGDataProviderRef srcDataProvider = CGImageGetDataProvider(srcImageRef);
    CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    
    const UInt8* srcDataPtr = CFDataGetBytePtr(srcData);
    size_t k = 0;
    for(size_t i = 0 ; i < height ; i++)
    {
        for(size_t j = 0 ; j < width ; j++)
        {
            data[k] = srcDataPtr[k] * alpha + ( 1.0 - alpha) * p.r;
            k++;
            data[k] = srcDataPtr[k] * alpha + ( 1.0 - alpha) * p.g;
            k++;
            data[k] = srcDataPtr[k] * alpha + ( 1.0 - alpha) * p.b;
            k++;
            data[k] = srcDataPtr[k];
            k++;
        }
    }
    CFDataRef dstData = CFDataCreateWithBytesNoCopy(NULL, data, bytesPerRow * height, kCFAllocatorNull);
    CGDataProviderRef dstDataProvider = CGDataProviderCreateWithCFData(dstData);
    CFRelease(dstData);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGImageRef dstImageRef = CGImageCreate(width, height, 8, 32, bytesPerRow, colorSpace, kCGImageAlphaNoneSkipLast, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    UIImage* uiImage = [UIImage imageWithCGImage:dstImageRef];
    CGImageRelease(dstImageRef);
    CGColorSpaceRelease(colorSpace);
    return uiImage;
}
+ (CGImageRef) getImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib
{
    __block CGImageRef retImage  = NULL;
    __block int accessError = 0;
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        CGImageRef image = [asset thumbnail];
        /*
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef imageFull = [rep fullResolutionImage];
        //UIImage* uiImage = [UIImage imageWithCGImage:imageFull];
        UIImage* retUIImage = [UIImage imageWithCGImage:imageFull scale:0.1 orientation:UIImageOrientationLeft];
        retImage = [retUIImage CGImage];
        CGImageRetain(retImage);
        //size_t w = CGImageGetWidth(image);
        //size_t h = CGImageGetHeight(image);
        //NSLog(@"image w = %lu, h = %lu", w, h);
         */
        retImage = [SEUtil copyImageRef:image];
    };
    ALAssetsLibraryAccessFailureBlock failHandler = ^(NSError *error)
    {
        if(error)
        {
            accessError = 1;
            NSLog(@"read photo lib error : %@", [error localizedDescription]);
        }
    };
    [assetLib assetForURL:url resultBlock:getAsset failureBlock:failHandler];
    if(!accessError)
        return retImage;
    else
        return NULL;
}
@end
