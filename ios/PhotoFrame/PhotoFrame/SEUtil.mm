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
    if(image == NULL)
        return NULL;
    if(s.width == 0 || s.height == 0)
        return NULL;
    CGContextRef con = MyCreateBitmapContext(s.width, s.height);    
    CGImageRef imageRef = [image CGImage];
    CGContextDrawImage(con, CGRectMake(0, 0, s.width, s.height), imageRef);
    CGImageRef retImageRef = CGBitmapContextCreateImage(con);
    UIImage* retImage = [UIImage imageWithCGImage:retImageRef];
    CGContextRelease(con);
    CGImageRelease(retImageRef);
    return retImage;
}
+ (const UInt8*) getCGImageData: (CGImageRef) image
{
    CGDataProviderRef srcDataProvider = CGImageGetDataProvider(image);
    CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    
    const UInt8* srcDataPtr = CFDataGetBytePtr(srcData);
    CFRelease(srcData);
    return srcDataPtr;
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
+ (CGImageRef) getFullRepresentationImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib
{
    __block CGImageRef retImage  = NULL;
    __block int accessError = 0;
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = [rep fullResolutionImage];
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
+ (CGImageRef) getImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib
{
    __block CGImageRef retImage  = NULL;
    __block int accessError = 0;
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = [rep fullResolutionImage];
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        CGSize s = [SEUtil computeFitSize:srcS toDst:CGSizeMake(200, 200)];
        retImage = [SEUtil fastScale:image withRect:s];
        /*       
        CGImageRef image = [asset thumbnail];
        retImage = [SEUtil copyImageRef:image];
         */
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
+ (void)removeAllSubviews: (UIView*)view
{
    NSArray* subviews = view.subviews;
    for(UIView* v in subviews)
    {
        [v removeFromSuperview];
    }
}
+ (CGImageRef) fastScale: (CGImageRef)image withRect: (CGSize) s
{
    int w = s.width;
    int h = s.height;
    UInt8* data = (UInt8*)malloc(w * h * 4);
    if(data == NULL)
        return NULL;
    int dstBytesPerRow = w * 4;
    int srcW = CGImageGetWidth(image);
    int srcH = CGImageGetHeight(image);
    int srcBytesPerRow = CGImageGetBytesPerRow(image);
    int srcBpp = CGImageGetBitsPerPixel(image);
    switch (srcBpp) 
    {
        case 24:
        {
            srcBpp = 3;
        }
            break;
        case 32:
        {
            srcBpp = 4;
        }
            break;
        default:
            srcBpp = 0;
            break;
    }
    if(srcBpp == 0)
    {
        free(data);
        return NULL;
    }
    float scalew = srcW / (float)w;
    float scaleh = srcH / (float)h;
    CGDataProviderRef srcDataProvider = CGImageGetDataProvider(image);
    CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    
    const UInt8* srcDataPtr = CFDataGetBytePtr(srcData);

    for(int i = 0 ; i < h ; i++)
    {
        int srcY = i * scaleh;
        const UInt8* src = srcDataPtr + srcY * srcBytesPerRow;
        UInt8* dst = data + i * dstBytesPerRow;
        for(int j = 0 ; j < w; j++)
        {
            int srcX = j * scalew;
            const UInt8* srcBits = src + srcX * srcBpp;
            UInt8* dstBits = dst + j * 4;
            switch (srcBpp) {
                case 3:
                {
                    *dstBits = *srcBits;
                    *(dstBits + 1) = *(srcBits + 1);
                    *(dstBits + 2) = *(srcBits + 2);
                    *(dstBits + 3) = 255;
                }
                    break;
                case 4:
                {
                    *dstBits = *srcBits;
                    *(dstBits + 1) = *(srcBits + 1);
                    *(dstBits + 2) = *(srcBits + 2);
                    *(dstBits + 3) = *(srcBits + 3);

                }
                    break;
                default:
                    break;
            }
        }
    }
    CFDataRef dstData = CFDataCreateWithBytesNoCopy(NULL, data, dstBytesPerRow * h, kCFAllocatorNull);
    CGDataProviderRef dstDataProvider = CGDataProviderCreateWithCFData(dstData);
    CFRelease(dstData);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGImageRef dstImageRef = CGImageCreate(w, h, 8, 32, dstBytesPerRow, colorSpace, kCGImageAlphaNoneSkipLast, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dstDataProvider);
    CFRelease(srcData);
    return dstImageRef;
}
+ (CGSize) computeFitSize: (CGSize)src toDst: (CGSize) dst
{
    float ratio = src.width / src.height;
    if(src.width >= src.height)
    {
        CGFloat h = src.height;
        if(h > dst.height)
        {
            h = dst.height;
        }
        assert(h <= dst.height);
        CGFloat w = h * ratio;
        if(w > dst.width)
        {
            w = dst.width;
            h = w / ratio;
        }
        assert(w <= dst.width);
        assert(h <= dst.height);
        return CGSizeMake(w, h);
    }
    else
    {
        CGFloat w = src.width;
        if(w > dst.width)
        {
            w = dst.width;
        }
        CGFloat h = w / ratio;
        if(h > dst.height)
        {
            h = dst.height;
            w = h * ratio;
        }
        assert(w <= dst.width);
        assert(h <= dst.height);
        return CGSizeMake(w, h);
    }
}
+ (UIImage*) createUIImageFrom: (UIImage*)background withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points
{
    //CGImageRef newImage = [SEUtil copyImageRef:[background CGImage]];
    CGImageRef image = [background CGImage];
    //CGDataProviderRef srcDataProvider = CGImageGetDataProvider(image);
    //CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    //const UInt8* srcDataPtr = CFDataGetBytePtr(srcData);
    //int len = CFDataGetLength(srcData);
    size_t w = CGImageGetWidth(image);
    size_t h = CGImageGetHeight(image);
    CGColorSpaceRef space = CGColorSpaceCreateDeviceRGB();
    CGBitmapInfo bmpInfo = CGImageGetBitmapInfo(image);
    NSMutableArray* newPoints = [NSMutableArray array];
    for(NSArray* array in points)
    {
        NSMutableArray* newa = [NSMutableArray array];
        for(NSValue* v in array)
        {
            CGPoint p = [v CGPointValue];
            p.x = p.x * w;
            p.y = p.y * h;
            [newa addObject:[NSValue valueWithCGPoint:p]];
        }
        [newPoints addObject:newa];
    }
    /*
    UInt8* data = (UInt8*)malloc(len);
    memcpy(data, srcDataPtr, len);
    size_t w = CGImageGetWidth(image);
    size_t h = CGImageGetHeight(image);
    CGColorSpaceRef space = CGColorSpaceCreateDeviceRGB();
    CGBitmapInfo bmpInfo = CGImageGetBitmapInfo(image);
    size_t bytesPerRow = CGImageGetBytesPerRow(image);
    */
    CGContextRef context =  MyCreateBitmapContext(w, h);//CGBitmapContextCreate(data, w, h, 8, bytesPerRow, space, bmpInfo);
    CGContextDrawImage(context, CGRectMake(0, 0, w, h), image);
    for(NSMutableArray* p in newPoints)
    {
        [[UIColor blackColor] set];
        CGContextSetLineWidth(context, lineWidth);
        if(p.count < 2)
        {
            CGPoint pt = [[p objectAtIndex:0] CGPointValue];
            CGContextFillRect(context, CGRectMake(pt.x - lineWidth / 2, pt.y - lineWidth / 2, lineWidth, lineWidth));
        }
        else
        {
            for(int i = 0 ; i < (p.count - 1) ; i++)
            {
                CGPoint pt1 = [[p objectAtIndex:i] CGPointValue];
                CGPoint pt2 = [[p objectAtIndex:(i + 1)] CGPointValue];
                CGContextMoveToPoint(context, pt1.x, pt1.y);
                CGContextAddLineToPoint(context, pt2.x, pt2.y);
                CGContextStrokePath(context);
            }
        }
    }    
    CGImageRef retImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    //CFRelease(srcData);
    //free(data);
    return [UIImage imageWithCGImage:retImageRef];
}

@end
