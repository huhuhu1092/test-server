//
//  SEUtil.mm
//  PhotoFrame
//
//  Created by 陈勇 on 12-1-21.
//  Copyright 2012年 __MyCompanyName__. All rights reserved.
//

#import "SEUtil.h"
#import <stdarg.h>
#import <sys/socket.h>
#import <netinet/in.h>
#import <netinet6/in6.h>
#import <arpa/inet.h>
#import <ifaddrs.h>
#import <netdb.h>
#import <math.h>
#import <vector>
#import <SystemConfiguration/SystemConfiguration.h>
#import <AssetsLibrary/AssetsLibrary.h>
#import <MediaPlayer/MediaPlayer.h>
////
static void find_iir_constants (gdouble *n_p,
                    gdouble *n_m,
                    gdouble *d_p,
                    gdouble *d_m,
                    gdouble *bd_p,
                    gdouble *bd_m,
                    gdouble  std_dev)
{
    /*  The constants used in the implemenation of a casual sequence
     *  using a 4th order approximation of the gaussian operator
     */
    
    const gdouble div = sqrt (2 * G_PI) * std_dev;
    const gdouble x0 = -1.783 / std_dev;
    const gdouble x1 = -1.723 / std_dev;
    const gdouble x2 = 0.6318 / std_dev;
    const gdouble x3 = 1.997  / std_dev;
    const gdouble x4 = 1.6803 / div;
    const gdouble x5 = 3.735 / div;
    const gdouble x6 = -0.6803 / div;
    const gdouble x7 = -0.2598 / div;
    gint          i;
    
    n_p [0] = x4 + x6;
    n_p [1] = (exp(x1)*(x7*sin(x3)-(x6+2*x4)*cos(x3)) +
               exp(x0)*(x5*sin(x2) - (2*x6+x4)*cos (x2)));
    n_p [2] = (2 * exp(x0+x1) *
               ((x4+x6)*cos(x3)*cos(x2) - x5*cos(x3)*sin(x2) -
                x7*cos(x2)*sin(x3)) +
               x6*exp(2*x0) + x4*exp(2*x1));
    n_p [3] = (exp(x1+2*x0) * (x7*sin(x3) - x6*cos(x3)) +
               exp(x0+2*x1) * (x5*sin(x2) - x4*cos(x2)));
    n_p [4] = 0.0;
    
    d_p [0] = 0.0;
    d_p [1] = -2 * exp(x1) * cos(x3) -  2 * exp(x0) * cos (x2);
    d_p [2] = 4 * cos(x3) * cos(x2) * exp(x0 + x1) +  exp(2 * x1) + exp(2 * x0);
    d_p [3] = -2 * cos(x2) * exp(x0 + 2*x1) -  2*cos(x3) * exp(x1 + 2*x0);
    d_p [4] = exp(2*x0 + 2*x1);
    
    for (i = 0; i <= 4; i++)
        d_m[i] = d_p[i];
    
    n_m[0] = 0.0;
    
    for (i = 1; i <= 4; i++)
        n_m[i] = n_p[i] - d_p[i] * n_p[0];
    
    {
        gdouble sum_n_p, sum_n_m, sum_d;
        gdouble a, b;
        
        sum_n_p = 0.0;
        sum_n_m = 0.0;
        sum_d = 0.0;
        
        for (i = 0; i <= 4; i++)
        {
            sum_n_p += n_p[i];
            sum_n_m += n_m[i];
            sum_d += d_p[i];
        }
        
        a = sum_n_p / (1.0 + sum_d);
        b = sum_n_m / (1.0 + sum_d);
        
        for (i = 0; i <= 4; i++)
        {
            bd_p[i] = d_p[i] * a;
            bd_m[i] = d_m[i] * b;
        }
    }
}

static void transfer_pixels (const gdouble *src1,
                 const gdouble *src2,
                 guchar        *dest,
                 gint           bytes,
                 gint           width)
{
    gint    b;
    gint    bend = bytes * width;
    gdouble sum;
    
    for (b = 0; b < bend; b++)
    {
        sum = *src1++ + *src2++;
        
        if (sum > 255)
            sum = 255;
        else if (sum < 0)
            sum = 0;
        
        *dest++ = (guchar) sum;
    }
}


//////////////////////////////////////
CGContextRef MyCreateBitmapContext (int pixelsWide,
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
        CGColorSpaceRelease(colorSpace);
        fprintf (stderr, "Memory not allocated!");
        return NULL;
    }
    context = CGBitmapContextCreate (NULL,//bitmapData,// 4
                                     pixelsWide,
                                     pixelsHigh,
                                     8,      // bits per component
                                     bitmapBytesPerRow,
                                     colorSpace,
                                     kCGImageAlphaNoneSkipLast);
    if (context== NULL)
    {
        CGColorSpaceRelease(colorSpace);
        free (bitmapData);// 5
        fprintf (stderr, "Context not created!");
        return NULL;
    }
    CGColorSpaceRelease( colorSpace );// 6
    free(bitmapData);
    return context;// 7
}
@implementation SEPhotoLibData
@synthesize url;
@synthesize urlDate;
@synthesize width;
@synthesize height;
@synthesize orient;
- (void) dealloc
{
    [url release];
    [urlDate release];
    [super dealloc];
}


@end
////
@implementation SEMusicLibData
@synthesize title;
@synthesize artist;
@synthesize album;
- (void) dealloc
{
    [title release];
    [artist release];
    [album release];
    [super dealloc];
}

@end
////
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
+ (CGImageRef) copyImageRef:(CGImageRef)srcImage rect: (CGRect) frame
{
    /*
    CGContextRef context = MyCreateBitmapContext(frame.size.width, frame.size.height);
    CGRect rect = CGRectMake(-frame.origin.x, -frame.origin.y, frame.size.width, frame.size.height);
    CGContextDrawImage(context, rect, srcImage);
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    return newImageRef;
     */
    int w = frame.size.width;
    int h = frame.size.height;
    UInt8* data = (UInt8*)malloc(w * h * 4);
    if(data == NULL)
        return NULL;
    int dstBytesPerRow = w * 4;
    int srcW = CGImageGetWidth(srcImage);
    int srcH = CGImageGetHeight(srcImage);
    int srcBytesPerRow = CGImageGetBytesPerRow(srcImage);
    int srcBpp = CGImageGetBitsPerPixel(srcImage);
    CGBitmapInfo srcBitmapInfo = CGImageGetBitmapInfo(srcImage);
    CGBitmapInfo dstBitmapInfo = srcBitmapInfo;
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
    CGDataProviderRef srcDataProvider = CGImageGetDataProvider(srcImage);
    CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    
    const UInt8* srcDataPtr = CFDataGetBytePtr(srcData);
    
    for(int i = frame.origin.y, j = 0 ; i < (frame.origin.y + frame.size.height) && j < h ; i++, j++)
    {
        const UInt8* src = srcDataPtr + i * srcBytesPerRow;
        UInt8* dst = data + j * dstBytesPerRow;
        for(int k = 0 , n = frame.origin.x ; k < w && n < (frame.origin.x + frame.size.width); k++)
        {
            const UInt8* srcBits = src + n * srcBpp;
            UInt8* dstBits = dst + k * 4;
            switch (srcBpp) {
                case 3:
                {
                    *dstBits = *srcBits;
                    *(dstBits + 1) = *(srcBits + 1);
                    *(dstBits + 2) = *(srcBits + 2);
                    *(dstBits + 3) = 255;
                    dstBitmapInfo = kCGImageAlphaNoneSkipLast;
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
    //kCGImageAlphaNoneSkipLast
    CGImageRef dstImageRef = CGImageCreate(w, h, 8, 32, dstBytesPerRow, colorSpace,  dstBitmapInfo, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dstDataProvider);
    CFRelease(srcData);
    return dstImageRef;
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
    if(image.size.width == s.width && image.size.height == s.height)
        return image;
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
+ (UIImage*) createImageWithColorImage:(UIImage*)colorImage alphaImage: (UIImage*)alphaImage
{
    NSLog(@"color image size = %f, %f", colorImage.size.width, colorImage.size.height);
    NSLog(@"alpha image size = %f, %f", alphaImage.size.width, alphaImage.size.height);
    if(colorImage.size.width != alphaImage.size.width || colorImage.size.height != alphaImage.size.height)
        return nil;
    CGImageRef colorImageRef = [colorImage CGImage];
    CGImageRef alphaImageRef = [alphaImage CGImage];
    
    size_t width = CGImageGetWidth(colorImageRef);
    size_t height = CGImageGetHeight(colorImageRef);
    size_t bytesPerRow = 4 * width;
    
    size_t colorBpp = CGImageGetBitsPerPixel(colorImageRef);
    NSLog(@"color bpp = %lu", colorBpp);
    colorBpp /= 8;
    if(colorBpp != 3 && colorBpp != 4)
        return nil;
    
    UInt8* data = (UInt8*)malloc(height * bytesPerRow);
    memset(data, 255, height * bytesPerRow);
    
    CGDataProviderRef colorDataProvider = CGImageGetDataProvider(colorImageRef);
    CFDataRef colorData = CGDataProviderCopyData(colorDataProvider);
    const UInt8* colorDataPtr = CFDataGetBytePtr(colorData);
    size_t colorBytesPerRow = CGImageGetBytesPerRow(colorImageRef);
    
    
    CGDataProviderRef alphaDataProvider = CGImageGetDataProvider(alphaImageRef);
    CFDataRef alphaData = CGDataProviderCopyData(alphaDataProvider);
    const UInt8* alphaDataPtr = CFDataGetBytePtr(alphaData);
    size_t alphaBytesPerRow = CGImageGetBytesPerRow(alphaImageRef);
    size_t alphaBpp = CGImageGetBitsPerPixel(alphaImageRef);
    alphaBpp /= 8;
    CGBitmapInfo colorBitmapInfo  = CGImageGetBitmapInfo(colorImageRef);
    BOOL alphaInvert = NO;
    switch(colorBitmapInfo & kCGBitmapAlphaInfoMask)
    {
        case kCGImageAlphaPremultipliedFirst:
        case kCGImageAlphaFirst:
        case kCGImageAlphaNoneSkipFirst:
        {
            alphaInvert = YES;
        }
    }
    //size_t k = 0;
    for(size_t i = 0 ; i < height ; i++)
    {
        for(size_t j = 0 ; j < width ; j++)
        {
            UInt8* colorBits = (UInt8*)colorDataPtr + i * colorBytesPerRow + colorBpp * j;
            UInt8* alphaBits = (UInt8*)alphaDataPtr + i * alphaBytesPerRow + alphaBpp * j;
            //NSLog(@"colorBits = %p", colorBits);
            UInt8* dstBits = data + i * bytesPerRow + j * 4;
            //NSLog(@"|color %d, %d, %d, %d|", colorBits[2], colorBits[2], colorBits[2], colorBits[2]);
            switch (colorBpp) 
            {
                case 4:
                case 3:
                    if(alphaInvert)
                    {
                        dstBits[0] = colorBits[2];
                        dstBits[1] = colorBits[1];
                        dstBits[2] = colorBits[0];
                        dstBits[3] = *alphaBits;
                    }
                    else
                    {
                        *dstBits = *colorBits;
                        *(dstBits + 1) = *(colorBits + 1);
                        *(dstBits + 2) = *(colorBits + 2);
                        *(dstBits + 3) = *alphaBits;
                    }
                    break;
                default:
                    break;
            }

        }
    }
    /*
    for(int i = 0 ; i < height; i++)
    {
        for(int j = 0 ; j < width; j++)
        {
            UInt8* dstBits = data + i * bytesPerRow + j * 4;
            NSLog(@"|%d, %d, %d, %d|", dstBits[0], dstBits[1], dstBits[2], dstBits[3]);
        }
        NSLog(@"\n");
    }
     */
    CFRelease(colorData);
    CFRelease(alphaData);
    CFDataRef dstData = CFDataCreate(NULL, data, bytesPerRow * height);
    free(data);
    CGDataProviderRef dstDataProvider = CGDataProviderCreateWithCFData(dstData);
    CFRelease(dstData);
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGImageRef dstImageRef = CGImageCreate(width, height, 8, 32, bytesPerRow, colorSpace, kCGImageAlphaLast, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGDataProviderRelease(dstDataProvider);
    UIImage* uiImage = [UIImage imageWithCGImage:dstImageRef];
    CGImageRelease(dstImageRef);
    CGColorSpaceRelease(colorSpace);
    return uiImage;
}
+ (UIImage*) createImageWithColor: (float)r g: (float) g b: (float) b alphaImage:(UIImage*)image
{
    CGImageRef srcImageRef = [image CGImage];
    size_t width = CGImageGetWidth(srcImageRef);
    size_t height = CGImageGetHeight(srcImageRef);
    size_t bytesPerRow = 4 * width;
    UInt8* data = (UInt8*)malloc(height * bytesPerRow);
    
    CGDataProviderRef srcDataProvider = CGImageGetDataProvider(srcImageRef);
    CFDataRef srcData = CGDataProviderCopyData(srcDataProvider);
    const UInt8* srcDataPtr = CFDataGetBytePtr(srcData);
    size_t k = 0;
    for(size_t i = 0 ; i < height ; i++)
    {
        for(size_t j = 0 ; j < width ; j++)
        {
            int a = srcDataPtr[k];
            data[k] = r;
            k++;
            data[k] = g;
            k++;
            data[k] = b;
            k++;
            data[k] = a;
            k++;
        }
    }
    CFRelease(srcData);
    CFDataRef dstData = CFDataCreate(NULL, data, bytesPerRow * height);
    free(data);
    CGDataProviderRef dstDataProvider = CGDataProviderCreateWithCFData(dstData);
    CFRelease(dstData);
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGImageRef dstImageRef = CGImageCreate(width, height, 8, 32, bytesPerRow, colorSpace, kCGImageAlphaLast, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGDataProviderRelease(dstDataProvider);
    UIImage* uiImage = [UIImage imageWithCGImage:dstImageRef];
    CGImageRelease(dstImageRef);
    CGColorSpaceRelease(colorSpace);
    return uiImage;
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
    CFRelease(srcData);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGImageRef dstImageRef = CGImageCreate(width, height, 8, 32, bytesPerRow, colorSpace, kCGImageAlphaNoneSkipLast, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGDataProviderRelease(dstDataProvider);
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
        UIImage* srcImage = [UIImage imageWithCGImage:image];
        UIImage* dstImage = [SEUtil cropUIImage:srcImage withRect:CGSizeMake(1024, 768)];
        image = [dstImage CGImage];
        retImage = CGImageRetain(image);
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
+ (CGImageRef) getImageFromPhotoLib: (NSURL*) url withAssetLib: (ALAssetsLibrary*)assetLib fitSize:(CGSize)size
{
    __block CGImageRef retImage  = NULL;
    __block int accessError = 0;
    ALAssetsLibraryAssetForURLResultBlock getAsset = ^(ALAsset *asset)
    {
        NSThread* currentThread = [NSThread currentThread];
        NSLog(@"current thread = %@\n", currentThread);
        ALAssetRepresentation* rep = [asset defaultRepresentation];
        CGImageRef image = [rep fullResolutionImage];
        CGSize srcS = CGSizeMake(CGImageGetWidth(image), CGImageGetHeight(image));
        CGSize s = [SEUtil computeFitSize:srcS toDst:size];
        retImage = [SEUtil fastScale:image withRect:s];
        float width = CGImageGetWidth(retImage);
        float height = CGImageGetHeight(retImage);
        NSLog(@"## retImage with = %f, height = %f ##\n", width, height);
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
    CGBitmapInfo srcBitmapInfo = CGImageGetBitmapInfo(image);
    CGBitmapInfo dstBitmapInfo = srcBitmapInfo;
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
                    dstBitmapInfo = kCGImageAlphaNoneSkipLast;
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
    //CFDataRef dstData = CFDataCreateWithBytesNoCopy(NULL, data, dstBytesPerRow * h, kCFAllocatorNull);
    CFDataRef dstData = CFDataCreate(NULL, data, dstBytesPerRow * h);
    CGDataProviderRef dstDataProvider = CGDataProviderCreateWithCFData(dstData);
    CFRelease(dstData);
    free(data);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    //kCGImageAlphaNoneSkipLast
    CGImageRef dstImageRef = CGImageCreate(w, h, 8, 32, dstBytesPerRow, colorSpace,  dstBitmapInfo, dstDataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dstDataProvider);
    CFRelease(srcData);
    return dstImageRef;
}
+ (CGSize) computeFitSize2: (CGSize) src toDst: (CGSize) dst
{
    float ratio = src.width / src.height;
    if(src.height <= dst.height && src.width <= dst.width)
    {
        if(src.width < src.height)
        {
            float width = dst.width;
            float height = width / ratio;
            return CGSizeMake(width, height);
        }
        else
        {
            float height = dst.height;
            float width = height * ratio;
            return CGSizeMake(width, height);
        }
    }
    else if(src.width >= dst.width && src.height >= dst.height)
    {
        
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
    else if(src.width <= dst.width && src.height >= dst.height)
    {
        float ratio = dst.width  / dst.height;
        float h = dst.height;
        float w = h * ratio;
        return CGSizeMake(w, h);
    }
    else if(src.height <= dst.height && src.width >= dst.width)
    {
        float ratio = dst.width / dst.height;
        float w = dst.width;
        float h = w / ratio;
        return CGSizeMake(w, h);
    }
    else
    {
        return CGSizeMake(dst.width, dst.height);    
    }
    
}
+ (CGSize) computeFitSize: (CGSize)src toDst: (CGSize) dst
{
    float ratio = src.width / src.height;
    if(src.height <= dst.height && src.width <= dst.width)
    {
        return src;
    }
    else if(src.width >= dst.width && src.height >= dst.height)
    {
        
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
    else if(src.width <= dst.width && src.height >= dst.height)
    {
        float ratio = src.width  / src.height;
        float h = dst.height;
        float w = h * ratio;
        return CGSizeMake(w, h);
    }
    else if(src.height <= dst.height && src.width >= dst.width)
    {
        float ratio = src.width / src.height;
        float w = dst.width;
        float h = w / ratio;
        return CGSizeMake(w, h);
    }
    else
    {
        return CGSizeMake(dst.width, dst.height);    
    }
}
+ (UIImage*) createSignatureInMainThread: (UIImage*)background size: (CGSize)size withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points
{
    UIGraphicsBeginImageContext(size);
    int w = background.size.width;
    int h = background.size.height;
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextDrawImage(context, CGRectMake(0, 0, size.width, size.height), [background CGImage]);
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
    for(NSMutableArray* p in newPoints)
    {
        [[UIColor blackColor] set];
        CGContextSetLineWidth(context, lineWidth);
        CGContextSetLineCap(context, kCGLineCapRound);
        if(p.count < 2)
        {
            CGPoint pt = [[p objectAtIndex:0] CGPointValue];
            CGContextFillEllipseInRect(context, CGRectMake(pt.x - lineWidth / 2, pt.y - lineWidth / 2, lineWidth, lineWidth));
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
    UIImage* newImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return newImage;
}
+ (UIImage*) createUIImageFrom: (UIImage*)background withLineWidth: (CGFloat) lineWidth drawPoints:(NSMutableArray*)points
{
    CGImageRef image = [background CGImage];
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
            p.y = h - p.y * h;
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
        CGContextSetLineCap(context, kCGLineCapRound);
        if(p.count < 2)
        {
            CGPoint pt = [[p objectAtIndex:0] CGPointValue];
            CGContextFillEllipseInRect(context, CGRectMake(pt.x - lineWidth / 2, pt.y - lineWidth / 2, lineWidth, lineWidth));
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
+ (BOOL) isRectContain: (CGRect) rect point: (CGPoint)p
{
    CGFloat left = rect.origin.x;
    CGFloat right = rect.origin.x + rect.size.width;
    CGFloat top = rect.origin.y;
    CGFloat bottom = rect.origin.y + rect.size.height;
    if(p.x >= left && p.x <= right && p.y >= top && p.y <= bottom)
        return YES;
    else
        return NO;
}
+ (UIImage*) drawImage : (UIImage*)image inRect:(CGRect)rect
{
    if(image.size.width == rect.size.width && image.size.height == rect.size.height)
        return image;
    UIGraphicsBeginImageContext(CGSizeMake(rect.size.width, rect.size.height));
    [image drawAtPoint:CGPointMake(0, 0)];
    UIImage* i = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return i;    
}
+ (UIImage*) drawImage: (UIImage*)image toSize: (CGSize) size
{
    //if(image.size.width == size.width && image.size.height == size.height)
    //    return image;
    UIGraphicsBeginImageContext(CGSizeMake(size.width, size.height));
    [image drawInRect:CGRectMake(0, 0, size.width, size.height)];
    UIImage* i = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return i; 
}
+ (NSString*) urlToString: (NSURL*)url
{
    return [url absoluteString];
}
+ (CGImageRef) createEmptyCGImage: (CGSize) size
{
    CGContextRef context = MyCreateBitmapContext(size.width, size.height);
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    return newImageRef;
}
+ (CGImageRef) CGImageDrawInRect: (CGImageRef) image rect: (CGSize) size
{
    CGContextRef context = MyCreateBitmapContext(size.width, size.height);
    CGRect rect = CGRectMake(0, 0, size.width, size.height);
    CGContextDrawImage(context, rect, image);
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    return newImageRef;
}
+ (UIImage*) createThumbnail: (UIImage*)srcImage thumbnailSize: (CGSize) size
{
    CGImageRef srcCGImage = [srcImage CGImage];
    CGSize srcSize = CGSizeMake(CGImageGetWidth(srcCGImage), CGImageGetHeight(srcCGImage));
    CGSize dstSize = [SEUtil computeFitSize:srcSize toDst:size];
    CGContextRef context = MyCreateBitmapContext(dstSize.width, dstSize.height);
    CGContextDrawImage(context, CGRectMake(0, 0, dstSize.width, dstSize.height), srcCGImage);
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    UIImage* uiImage = [UIImage imageWithCGImage:newImageRef];
    CGImageRelease(newImageRef);
    return uiImage;
    //CGImageRef imageRef = [SEUtil fastScale:[srcImage CGImage] withRect:dstSize];
    /*
    CGContextRef context = MyCreateBitmapContext(size.width, size.height);
    //CGContextSetFillColorWithColor(context, [[UIColor whiteColor] CGColor]);
    //CGContextFillRect(context, CGRectMake(0, 0, size.width, size.height));
    CGRect rect = CGRectMake((size.width - dstSize.width) / 2, (size.height - dstSize.height) / 2, dstSize.width, dstSize.height);
    CGContextDrawImage(context, rect, srcCGImage);
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    //CGImageRelease(imageRef);
    UIImage* uiImage = [UIImage imageWithCGImage:newImageRef];
    CGImageRelease(newImageRef);
    return uiImage;
     */
}
+ (BOOL) reachabilityWithLocalWifi
{
    struct sockaddr_in localWifiAddress;
    bzero(&localWifiAddress, sizeof(localWifiAddress));
    localWifiAddress.sin_len = sizeof(localWifiAddress);
    localWifiAddress.sin_family = AF_INET;
    // IN_LINKLOCALNETNUM is defined in <netinet/in.h> as 169.254.0.0
    localWifiAddress.sin_addr.s_addr = htonl(IN_LINKLOCALNETNUM);
    SCNetworkReachabilityRef reachability = SCNetworkReachabilityCreateWithAddress(kCFAllocatorDefault, (const struct sockaddr*)&localWifiAddress);
    if(reachability != NULL)
    {
        return YES;
    }
    else {
        return NO;
    }
}
+ (BOOL) reachabilityForHostName: (NSString*)name
{
    SCNetworkReachabilityRef reachability = SCNetworkReachabilityCreateWithName(NULL, [name UTF8String]);
    if(reachability != NULL)
        return YES;
    else {
        return NO;
    }
}
+ (NSString*) dateToString:(NSDate *)date
{
    NSTimeInterval t = [date timeIntervalSince1970];
    unsigned long time = t;
    NSString* str = [NSString stringWithFormat:@"%lu", time];
    return str;
    /*
    NSDateFormatter *dateFormatter = [[NSDateFormatter alloc] init];
    [dateFormatter setTimeStyle:NSDateFormatterMediumStyle];
    [dateFormatter setDateStyle:NSDateFormatterMediumStyle];
    NSString* str = [dateFormatter stringFromDate:date];
    [dateFormatter release];
    return str;
     */
}
+ (NSString*) readDataFromDocumentDir:(NSString*) fileName
{
    NSArray* dirArray = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* dirPath = [dirArray objectAtIndex:0];
    NSString* filePath = [dirPath stringByAppendingFormat:@"/", fileName];
    NSString* dataContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    return dataContent;
}
+ (NSString*)readDataFromBundle:(NSString*)fileName
{
    NSArray* fileNameArray = [fileName componentsSeparatedByString:@"."];
    if([fileNameArray count] != 2)
    {
        return NO;
    }
    NSString* s = [fileNameArray objectAtIndex:0];
    NSString* ext = [fileNameArray objectAtIndex:1];
    NSString* filePath = [[NSBundle mainBundle] pathForResource:s ofType:ext];
    NSString* dataContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    return dataContent;
}
+ (BOOL)isWhitespaceLine: (NSString*)line
{
    NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
    NSUInteger i;
    for(i = 0 ; i < [line length]; i++)
    {
        unichar c = [line characterAtIndex:i];
        BOOL b = [cs characterIsMember:c];
        if(!b)
            return false;
    }
    return true;
}
+ (NSString*) stringTrim: (NSString*) str
{
    NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
    NSString* newStr = [str stringByTrimmingCharactersInSet:cs];
    return newStr;
}
+ (CGImageRef)createCGImage: (ppm_t) p
{
    CFDataRef data = CFDataCreateWithBytesNoCopy(NULL, p.col, p.width * p.height * 3, kCFAllocatorNull);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGDataProviderRef dataProvider = CGDataProviderCreateWithCFData(data);
    CFIndex retainCount = CFGetRetainCount(data);
    CFRelease(data);
    retainCount = CFGetRetainCount(data);
    CGImageRef cgImage = CGImageCreate(p.width, p.height, 8, 24, p.width * 3, colorSpace, kCGImageAlphaNoneSkipLast, dataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    retainCount = CFGetRetainCount(colorSpace);
    retainCount = CFGetRetainCount(dataProvider);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dataProvider);
    retainCount = CFGetRetainCount(colorSpace);
    retainCount = CFGetRetainCount(dataProvider);
    return cgImage;
}
+ (CGImageRef) create32BitsCGimageWithCopy: (ppm_t)p
{
    //CFDataRef data = CFDataCreate(NULL, p.col, p.width * p.height * 4);
    
    unsigned char* data = (unsigned char*)malloc(p.width * p.height * 4);
    for(int i = 0 ; i < p.height ; i++)
    {
        for(int j = 0 ; j < p.width ; j++)
        {
            unsigned char* dst = data + i * p.width * 4 + j * 4;
            unsigned char* src = p.col + i * p.width * 3 + j * 3;
            dst[0] = 0;//src[0];
            dst[1] = 0;//src[1];
            dst[2] = 0;//src[2];
            dst[3] = src[0];
        }
    }
    //CFDataRef cfData = CFDataCreateWithBytesNoCopy(NULL, data, p.width * p.height * 4, kCFAllocatorNull);
    CFDataRef cfData = CFDataCreate(NULL, data, p.width * p.height * 4);
    free(data);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGDataProviderRef dataProvider = CGDataProviderCreateWithCFData(cfData);
    CFRelease(cfData);
    CGImageRef cgImage = CGImageCreate(p.width, p.height, 8, 32, p.width * 4, colorSpace, kCGImageAlphaLast, dataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dataProvider);
    return cgImage;
}
+ (CGImageRef) createCGImageWithCopy:(ppm_t)p
{
    CFDataRef data = CFDataCreate(NULL, p.col, p.width * p.height * 3);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGDataProviderRef dataProvider = CGDataProviderCreateWithCFData(data);
    CFRelease(data);
    CGImageRef cgImage = CGImageCreate(p.width, p.height, 8, 24, p.width * 3, colorSpace, kCGImageAlphaNoneSkipLast, dataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dataProvider);
    return cgImage;   
}
+ (int) getSystemVersion
{
    NSString* version = [[UIDevice currentDevice] systemVersion];
    unichar  c = [version characterAtIndex:0];
    
    if(c == '4')
        return 4;
    if(c == '3')
        return 3;
    else if(c == 5)
    {
        return 5;
    }
    else 
    {
        return 5;
    }
}
+ (UIImage*) imageWithInsets: (UIImage*)image top: (float) top bottom: (float) bottom left: (float) left right: (float)right
{
    int version = [SEUtil getSystemVersion];
    if(version <= 4)
    {
        //NSLog(@"version ios4");
        float width = image.size.width;
        float height = image.size.height;
        float left = width * left;
        float top = height * top;
        UIImage* img =  [image stretchableImageWithLeftCapWidth:left topCapHeight:top];
        return img;
    }
    else 
    {

        float width = image.size.width;
        float height = image.size.height;
        float top = height * top;
        float bottom = height * bottom;
        float left = width * left;
        float right = width * right;
        UIImage* img = [image resizableImageWithCapInsets:UIEdgeInsetsMake(top, left, bottom, right)];
        return img;
    }
   
}
+ (UIImage*) imageWithCapInset: (UIImage*)image top:(float)top bottom: (float)bottom left: (float)left right: (float)right
{
    int version = [SEUtil getSystemVersion];
    if(version <= 4)
    {
        return [image stretchableImageWithLeftCapWidth:left topCapHeight:top];
    }
    else {
        return [image resizableImageWithCapInsets:UIEdgeInsetsMake(top, left, bottom, right)];
    }
}
+ (UIImage*) imageStretchForSlider: (UIImage*)image
{
    CGSize s = image.size;
    NSString* version = [[UIDevice currentDevice] systemVersion];
    unichar  c = [version characterAtIndex:0];
    if(c == '4')
    {
        UIImage* img =  [image stretchableImageWithLeftCapWidth:60  topCapHeight:0];
        return img;
    }
    else
    {
        UIImage* img = [image resizableImageWithCapInsets:UIEdgeInsetsMake(0, 60,  0 , 60)];
        return img;
    }
}
+ (UIImage*) imageWithCap:(UIImage*)image top: (float) top bottom: (float) bottom left: (float) left right: (float)right
{
    CGSize s = image.size;
    NSString* version = [[UIDevice currentDevice] systemVersion];
    unichar  c = [version characterAtIndex:0];
    if(c == '4')
    {
        //NSLog(@"version ios4");
        float width = image.size.width;
        float height = image.size.height;
        int midW = (width - 1) * 0.25;
        int midH = (height - 1) * 0.25;
        //UIImage* img =  [image stretchableImageWithLeftCapWidth:midW  topCapHeight:midH];
        UIImage* img =  [image stretchableImageWithLeftCapWidth:22  topCapHeight:22];
        return img;
    }
    else 
    {
        //NSLog(@"version ios5");
        float width = image.size.width;
        float height = image.size.height;
        //NSLog(@"width = %f , height = %f", width, height);
        float h = width / 4;
        float v = height / 4;
        UIImage* img = [image resizableImageWithCapInsets:UIEdgeInsetsMake(v, h, v  , h)];
        return img;
        //return [image resizableImageWithCapInsets:UIEdgeInsetsMake(top * s.height, left * s.width, bottom * s.height, right * s.width)];
    }
    
}
+ (void) savePNGImageToDocument: (UIImage*)image withName:(NSString*)fileName
{
    NSArray* strArray = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [strArray objectAtIndex:0];

    NSString* path = [docs stringByAppendingFormat:@"%@%@%@", @"/", fileName, @".png"];
    NSLog(@"## saved image path = %@ ##", path);
    NSData* imageData = UIImagePNGRepresentation(image);
    NSError* writeError = nil;
    BOOL ret = [imageData writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save image : %@", path);
    }
}
+ (NSString*) getFontName
{
    if([SEUtil getSystemVersion] <= 4)
    {
        return @"HelveticaNeue-Bold";
    }
    else 
    {
        return @"HelveticaNeue-CondensedBold";
    }
}
+ (void) calculateImageActualSizeWithWidth: (float)srcWidth height: (float)srcHeight orientation: (UIImageOrientation) orient : (float*)outWidth : (float*) outHeight
{
    switch (orient) 
    {
        case UIImageOrientationUp:
        case UIImageOrientationDown:
        case UIImageOrientationUpMirrored:
        case UIImageOrientationDownMirrored:
        {
            *outWidth = srcWidth;
            *outHeight = srcHeight;
        }
            break;
        case UIImageOrientationLeft:
        case UIImageOrientationRight:
        case UIImageOrientationLeftMirrored:
        case UIImageOrientationRightMirrored:
        {
            *outWidth = srcHeight;
            *outHeight = srcWidth;
        }
            break;
        default:
            *outHeight = 0;
            *outWidth = 0;
            break;
    }

}
+ (NSArray*) removeWhiteLineStringFrom: (NSArray*)tokens
{
    NSArray* newArray = [NSArray array];
    for(NSString* str in tokens)
    {
        if([SEUtil isWhitespaceLine:str] == NO)
        {
            newArray = [newArray arrayByAddingObject:str];
        }
    }
    return newArray;
}
+ (BOOL) isInPhotoAssetArray:(NSMutableArray*)photoArray: (SEPhotoLibData*)photoData
{
    for(SEPhotoLibData* data in photoArray)
    {
        if([data.url isEqual:photoData.url] && [data.urlDate isEqualToString:photoData.urlDate])
            return YES;
    }
    return NO;
}
+ (void) addPhotoByDateSequent: (NSMutableArray*) photoArray :(SEPhotoLibData*)url
{
    int index = 0;
    int count = photoArray.count;
    for(int j = 0 ; j < photoArray.count ; j++, index++)
    {
        SEPhotoLibData* urlInPhotoAsset = [photoArray objectAtIndex:j];
        NSString* tmpUrlDate = urlInPhotoAsset.urlDate;
        NSComparisonResult ret = [tmpUrlDate compare:url.urlDate];
        if(ret == NSOrderedAscending)
        {
            break;
        }
    }
    if(index < photoArray.count)
    {
        [photoArray insertObject:url atIndex:index];
    }
    else
    {
        [photoArray addObject:url];
    }
    assert(photoArray.count == (count + 1));
}
+ (void) getImagesFromPhotoLib: (ALAssetsLibrary*)lib assetType: (NSArray*)assetTypes photoContainer:(NSMutableArray*)photoArray finishedTarget:(id)target finishedAction: (SEL)action
{
    __block int currentAssetIndex = 0;
    __block int imageCount = 0;
    int typeCount = assetTypes.count;
    ALAssetsGroupEnumerationResultsBlock getPix =
    ^(ALAsset *result, NSUInteger index, BOOL* stop)
    {
        //NSLog(@"## reslult = %@ ###", result);
        if(!result)
            return;
        if(photoArray.count >= 5)
        {
            *stop = YES;
            return;
        }
        ALAssetRepresentation* rep = [result defaultRepresentation];
        NSDictionary* metadata = [rep metadata];
        NSNumber* metaWidth = [metadata objectForKey:@"PixelWidth"];
        NSNumber* metaHeight = [metadata objectForKey:@"PixelHeight"];
        ALAssetOrientation orient = [rep orientation];
        NSURL* assetURL = [rep url];
        //NSThread* thread = [NSThread currentThread];
        //NSLog(@"enumerate assert thread = %@", thread);
        //SEPageImageURL* imageURL = [[SEPageImageURL alloc] init];
        SEPhotoLibData* data = [[[SEPhotoLibData alloc] init] autorelease];
        data.url = assetURL;
        data.width = [metaWidth intValue];
        data.height = [metaHeight intValue];
        data.orient = orient;
        NSDate* date = [result valueForProperty: ALAssetPropertyDate];
        data.urlDate = [SEUtil dateToString: date];
        if([SEUtil isInPhotoAssetArray:photoArray :data] == NO)
        {
            [SEUtil addPhotoByDateSequent: photoArray: data];
        }
        *stop = NO;
    }   ;
    ALAssetsLibraryGroupsEnumerationResultsBlock getGroups =
    ^(ALAssetsGroup* group, BOOL* stop)
    {
        NSLog(@"## group = %@ ###", group);
        if(photoArray.count >= 5 && group)
        {
            *stop = YES;
            /*
            NSMutableArray* retObject = [[NSMutableArray array] retain];
            [retObject addObject:lib];
            [retObject addObject:photoArray];
            [target performSelectorOnMainThread:action withObject:retObject waitUntilDone:NO];
             */
            return;
        }
        if(!group)
        {
            currentAssetIndex++;
            if(currentAssetIndex == typeCount)//ASSET_NUM)
            {
                NSMutableArray* retObject = [[NSMutableArray array] retain];
                [retObject addObject:lib];
                [retObject addObject:photoArray];
                [target performSelectorOnMainThread:action withObject:retObject waitUntilDone:NO];
            }
            return;
        }
        [group setAssetsFilter:[ALAssetsFilter allPhotos]];
        NSString* title = [group valueForProperty: ALAssetsGroupPropertyName];
        NSLog(@"title = %@", title);
        [group enumerateAssetsUsingBlock: getPix];
        *stop = NO;
    };
    ALAssetsLibraryAccessFailureBlock oops =
    ^(NSError* error)
    {
        NSLog(@"oops ! %@", [error localizedDescription]);
    };
    for(int i = 0 ; i < typeCount; i++)
    {
        int type = [[assetTypes objectAtIndex:i] intValue];
        if(type != 0)
            [lib enumerateGroupsWithTypes:type usingBlock:getGroups failureBlock:oops];
    }
    NSLog(@"photo enumerate end");
}
+ (NSArray*) getMusicFromMusicLib
{
    MPMediaQuery* query = [MPMediaQuery songsQuery];
    NSArray* result = [query collections];
    NSArray* musicItemPropertyArray = [NSArray array];
    for(MPMediaItemCollection* collection in result)
    {
        for(MPMediaItem* item in [collection items])
        {
            NSString* title  = [item valueForKey:MPMediaItemPropertyTitle];
            NSString* artist = [item valueForKey:MPMediaItemPropertyAlbumTitle];
            NSString* artist1 = [item valueForKey:MPMediaItemPropertyArtist];
            //NSLog(@"title = %@\n", title);
            //NSLog(@"artist = %@\n", artist);
            //NSLog(@"artist1 = %@\n", artist1);
            SEMusicLibData* itemProperty = [[SEMusicLibData alloc] init];
            itemProperty.title = title;
            itemProperty.artist = artist1;
            itemProperty.album = artist;
            musicItemPropertyArray = [musicItemPropertyArray arrayByAddingObject:itemProperty];
            [itemProperty release];
        }
    }
    return musicItemPropertyArray;
}
+ (CGSize) calculateImageSizeByRatio: (CGSize) size dstSize: (CGSize) dstSize
{
    if(dstSize.width > dstSize.height)
    {
        float ratio = size.width / size.height;
        float width = ratio * dstSize.height;
        if(width < dstSize.width)
        {
            float height = dstSize.width / ratio;
            return CGSizeMake(dstSize.width, height);
        }
        else
        {
            return CGSizeMake(width, dstSize.height);    
        }
    }
    else
    {
        float ratio = size.width / size.height;
        float height = dstSize.width / ratio;
        if(height < dstSize.height)
        {
            float width = dstSize.height * ratio;
            return CGSizeMake(width, dstSize.height);
        }
        else 
        {
            return CGSizeMake(dstSize.width, height);
        }
    }
}
+ (void) pauseCurrentMusic
{
    MPMusicPlayerController* player = [MPMusicPlayerController applicationMusicPlayer];
    [player pause];
}
+ (NSArray*) findMediaItemByTitle: (NSString*)title aritst: (NSString*)artist album: (NSString*)album
{
    MPMediaPropertyPredicate *artistNamePredicate = nil;
    if(artist != nil)
    {
        artistNamePredicate =  [MPMediaPropertyPredicate predicateWithValue: artist
                                                                forProperty: MPMediaItemPropertyArtist];
    }
    
    MPMediaPropertyPredicate *albumNamePredicate = nil;
    if(album != nil)
    {
        albumNamePredicate =  [MPMediaPropertyPredicate predicateWithValue: album
                                                               forProperty: MPMediaItemPropertyAlbumTitle];
    }
    
    MPMediaPropertyPredicate* titlePredicate = [MPMediaPropertyPredicate predicateWithValue:title forProperty:MPMediaItemPropertyTitle];
    
    MPMediaQuery *myComplexQuery = [[MPMediaQuery alloc] init];
    
    if(artist)
    {
        [myComplexQuery addFilterPredicate: artistNamePredicate];
    }
    if(album)
    {
        [myComplexQuery addFilterPredicate: albumNamePredicate];
    }
    [myComplexQuery addFilterPredicate:titlePredicate];
    NSArray *items = [myComplexQuery items]; 
    [myComplexQuery release];
    return items;
}
+ (NSString*) createNextName: (NSString*) prefix: (NSArray*) currentNameArray: (int) count
{
    for(int i = 0 ; i < count ; i++)
    {
        NSString* str = [NSString stringWithFormat:@"%@%d", prefix, i];
        if([currentNameArray containsObject:str] == NO)
        {
            return str;
        }
    }
    return [NSString stringWithFormat:@"%@%d", prefix, count];
}
+ (UIImage*) drawTextOnImage: (UIImage*)currImage text: (NSString*)str color: (UIColor*)color textSize: (float)textSize  x: (float)x y: (float)y
{
    CGImageRef srcImage = [currImage CGImage];
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    size_t width = CGImageGetWidth(srcImage);
    size_t height = CGImageGetHeight(srcImage);
    NSLog(@"### image loader width = %lu, height = %lu", width, height);
    NSLog(@"### image orientation = %d ##", currImage.imageOrientation);
    size_t bytesPerRow = width * 4;
    UInt8* data = (UInt8*)malloc(height * bytesPerRow);
    CGContextRef context = CGBitmapContextCreate((void*)data, width, height, 8, bytesPerRow, colorSpace, kCGImageAlphaNoneSkipLast);
    CGContextDrawImage(context, CGRectMake(0, 0, width, height), srcImage);
    const char* drawText  = [str cStringUsingEncoding:NSASCIIStringEncoding];
    size_t strLen = strlen(drawText);
    CGContextSaveGState(context);
    CGColorRef colorRef = [color CGColor];
    CGColorSpaceRef colorSpaceRef = CGColorSpaceCreateDeviceRGB();
    CGContextSetFillColorSpace(context, colorSpaceRef);
    CGContextSetFillColorWithColor(context, colorRef);
    CGContextSetTextMatrix(context, CGAffineTransformIdentity);
      
    CGContextSetTextDrawingMode(context, kCGTextFill);
    UIImageOrientation currentImageOrient = currImage.imageOrientation;
    //currentImageOrient = UIImageOrientationDownMirrored;
    float fontSizeBase = 40;
    float textY = 0;
    switch (currentImageOrient) 
    {
        case UIImageOrientationLeft:
        {
            CGContextTranslateCTM(context, 0, height);
            CGContextRotateCTM(context, -M_PI / 2);
            textSize = width / fontSizeBase;
            textY = width - textSize;
        }
            break;
        case UIImageOrientationRight:
        {
            CGContextTranslateCTM(context, width, 0);
            CGContextRotateCTM(context, M_PI  / 2);
            textSize = width / fontSizeBase;
            textY = width - textSize;
        }
            break;
        case UIImageOrientationLeftMirrored:
        {
            CGContextTranslateCTM(context, width, height);
            CGContextRotateCTM(context, M_PI / 2);
            CGContextScaleCTM(context, -1, 1);
            textSize = width / fontSizeBase;
            textY = width - textSize;
        }
            break;
        case UIImageOrientationUp:
        {
            textSize = height / fontSizeBase;
            textY = height - textSize;
        }
            break;
        case UIImageOrientationDown:
        {
            CGContextTranslateCTM(context, width, height);
            CGContextScaleCTM(context, -1, -1);
            textSize = height / fontSizeBase;
            textY = height - textSize;
        }
            break;
        case UIImageOrientationUpMirrored:
        {
            CGContextTranslateCTM(context, width, 0);
            CGContextScaleCTM(context, -1, 1);
            textSize = height / fontSizeBase;
            textY = height - textSize;
        }
            break;
        case UIImageOrientationDownMirrored:
        {
            CGContextTranslateCTM(context, 0, height);
            CGContextScaleCTM(context, 1, -1);
            textSize = height / fontSizeBase;
            textY = height - textSize;
        }
            break;
        case UIImageOrientationRightMirrored:
        {
            CGContextRotateCTM(context, M_PI / 2);
            CGContextScaleCTM(context, 1, -1);
            textSize = width / fontSizeBase;
            textY = width - textSize;
        }
            break;
        default:
            break;
    }
    CGContextSelectFont(context, "Helvetica", textSize,  kCGEncodingMacRoman);
    CGContextShowTextAtPoint(context, x, textY, drawText, strLen);
    CGColorSpaceRelease(colorSpaceRef);
    CGContextRestoreGState(context);
    //end
    CGImageRef newImageRef = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    free(data);
    CGColorSpaceRelease(colorSpace);
    UIImage* newUIImage = [UIImage imageWithCGImage:newImageRef scale:currImage.scale orientation:currentImageOrient];
    CGImageRelease(newImageRef);
    return newUIImage;
}
+ (double) calculateGxy: (int)x : (int)y : (double) fi
{
    double tt = - (x * x + y * y) / (2 * fi * fi);
    tt = exp(tt);
    double gxy = tt / (2 * M_PI * fi * fi);
    return gxy;
}
+ (std::vector<double>) createBlurMatrix: (int)radius : (double) sig
{
    int num = 2 * radius + 1;
    std::vector<double> data(num * num);
    //float* data = (float*)malloc(num * num * sizeof(float));
    int k = 0;
    double sum = 0;
    sig = sqrt(radius * radius * 2);
    for(int y = radius ; y >= -radius ; y--)
    {
        for(int x = -radius ; x <= radius ; x++)
        {
            double v = [self calculateGxy:x :y :sig];
            data[k++] = v;
            sum += v;
        }
    }
    for(int i = 0 ; i < data.size() ; i++)
    {
        data[i] /= sum;
    }
    assert(k == num * num);
    return data;
}
+ (ppm_t) blurBrush: (ppm_t) srcBrush radius: (int) radius sig: (float)sig
{
    ppm_t newBrush;
    ppm_new(&newBrush, srcBrush.width, srcBrush.height);
    /*
    float blurMatrix[] = {0.0947416, 0.118318, 0.0947416, 
                          0.118318, 0.147761, 0.118318,
                          0.0947416, 0.118318, 0.0947416};
     */
    //int radius = 10;
    //int sig = 2.7;
    std::vector<double> blurMatrix1 = [self createBlurMatrix:radius : sig];
    double sum = 0;
    for(int i = 0 ; i < blurMatrix1.size() ; i++)
    {
        sum += blurMatrix1[i];
        NSLog(@"%d = %f", i , blurMatrix1[i]);
    }
    NSLog(@"sum = %f", sum);
    double (^mulMatrix) (std::vector<int>, std::vector<double>) = ^double(std::vector<int> colors, std::vector<double> matrix)
    {
        double sum = 0;
        assert(colors.size() == matrix.size());
        for(int i = 0 ; i < colors.size() ; i++)
        {
            sum += colors[i] * matrix[i];
        }
        return sum;
    };
    for(int i = 0 ; i < newBrush.height ; i++)
    {
        for(int j = 0 ; j < newBrush.width ; j++)
        {
            int num = 2 * radius + 1;
            std::vector<int> colorsR(num * num);
            std::vector<int> colorsG(num * num);
            std::vector<int> colorsB(num * num);
            int k = 0;
            for(int y = -radius ; y <= radius ; y++)
            {
                for(int x = -radius ; x <= radius ; x++)
                {
                    int indexX = j + x;
                    int indexY = i + y;
                    if(indexY < newBrush.height && indexY >= 0 && indexX < newBrush.width && indexX >= 0)
                    {
                        colorsR[k] = srcBrush.col[indexY * srcBrush.width * 3 + indexX * 3];
                        colorsG[k] = srcBrush.col[indexY * srcBrush.width * 3 + indexX * 3 + 1];
                        colorsB[k] = srcBrush.col[indexY * srcBrush.width * 3 + indexX * 3 + 2];
                    }
                    else {
                        colorsR[k] = colorsG[k] = colorsB[k] = 0;
                    }
                    k++;
                }
            }
            /*
            for(int mm = 0 ; mm < colorsR.size() ; mm++)
            {
                NSLog(@"r = %d, g = %d, b = %d", colorsR[mm], colorsG[mm], colorsB[mm]);
            }
             */
            assert(k == num * num);
            double r = mulMatrix(colorsR, blurMatrix1);
            double g = mulMatrix(colorsG, blurMatrix1);
            double b = mulMatrix(colorsB, blurMatrix1);
            //NSLog(@"final r = %f, g = %f, b = %f", r, g , b);
            newBrush.col[i * newBrush.width * 3 + j * 3 ] = (unsigned char)r;
            newBrush.col[i * newBrush.width * 3 + j * 3 + 1] = (unsigned char)g;
            newBrush.col[i * newBrush.width * 3 + j * 3 + 2] = (unsigned char)b;
        }
    }
    return newBrush;
}
+ (void) get_pixels_col: (ppm_t) srcData : (guchar*)buf :(int) col 
{
    int k = 0;
    for(int i = 0 ; i < srcData.height ; i++)
    {
        buf[k++] = srcData.col[i * srcData.width * 3 + col * 3];
        buf[k++] = srcData.col[i * srcData.width * 3 + col * 3 + 1];
        buf[k++] = srcData.col[i * srcData.width * 3 + col * 3 + 2];
    }
}
+ (void) set_pixels_col: (ppm_t)dstData :(guchar*)buf : (int)col
{
    int k = 0 ; 
    for(int i = 0 ; i < dstData.height ; i++)
    {
        dstData.col[i * dstData.width * 3 + col * 3] = buf[k++];
        dstData.col[i * dstData.width * 3 + col * 3 + 1] = buf[k++];
        dstData.col[i * dstData.width * 3 + col * 3 + 2] = buf[k++];
    }

}
+ (void) get_pixels_row: (ppm_t) srcData :(guchar*)buf : (int)row
{
    int k = 0;
    for(int j = 0 ; j < srcData.width ; j++)
    {
        buf[k++] = srcData.col[row * srcData.width * 3 + j * 3];
        buf[k++] = srcData.col[row * srcData.width * 3 + j * 3 + 1];
        buf[k++] = srcData.col[row * srcData.width * 3 + j * 3 + 2];
    }
}
+ (void) set_pixels_row: (ppm_t)dstData : (guchar*)buf : (int)row
{
    int k = 0;
    for(int j = 0 ; j < dstData.width ; j++)
    {
        dstData.col[row * dstData.width * 3 + j * 3] = buf[k++];
        dstData.col[row * dstData.width * 3 + j * 3 + 1] = buf[k++];
        dstData.col[row * dstData.width * 3 + j * 3 + 2] = buf[k++];
    }    
}
+ (ppm_t) blurBrush: (ppm_t) srcBrush horiz: (int) horz vert: (int)vert
{
    gint          bytes;
    gint          has_alpha;
    guchar       *dest;
    guchar       *src,  *sp_p, *sp_m;
    gdouble       n_p[5], n_m[5];
    gdouble       d_p[5], d_m[5];
    gdouble       bd_p[5], bd_m[5];
    gdouble      *val_p = NULL;
    gdouble      *val_m = NULL;
    gdouble      *vp, *vm;
    gint          i, j;
    gint          row, col, b;
    gint          terms;
    gdouble       progress, max_progress;
    gint          initial_p[4];
    gint          initial_m[4];
    gdouble       std_dev;
    gboolean      direct;
    gint          progress_step;
    int width , height;
    width = srcBrush.width;
    height = srcBrush.height;
    direct = 1;
    bytes = 3;
    has_alpha = false;
    val_p = g_new (gdouble, MAX (width, height) * bytes);
    val_m = g_new (gdouble, MAX (width, height) * bytes);
    
    src =  g_new (guchar, MAX (width, height) * bytes);
    dest = g_new (guchar, MAX (width, height) * bytes);
    ppm_t newBrush;
    ppm_new(&newBrush, srcBrush.width, srcBrush.height);
    ppm_t blurBrush = {0, 0, NULL};
    ppm_copy(&srcBrush, &blurBrush);

    //for test
    //ppm_copy(&blurBrush, &newBrush);
    //return newBrush;
    //end
    
    
    progress = 0.0;
    max_progress  = (horz <= 0.0) ? 0 : width * height * horz;
    max_progress += (vert <= 0.0) ? 0 : width * height * vert;
    
    
    /*  First the vertical pass  */
    if (vert > 0.0)
    {
        vert = fabs (vert) + 1.0;
        std_dev = sqrt (-(vert * vert) / (2 * log (1.0 / 255.0)));
        
        /* We do not want too many progress updates because they
         * can slow down the processing significantly for very
         * large images
         */
        progress_step = width / 16;
        
        if (progress_step < 5)
            progress_step = 5;
        
        /*  derive the constants for calculating the gaussian
         *  from the std dev
         */
        
        find_iir_constants (n_p, n_m, d_p, d_m, bd_p, bd_m, std_dev);
        
        for (col = 0; col < width; col++)
        {
            memset (val_p, 0, height * bytes * sizeof (gdouble));
            memset (val_m, 0, height * bytes * sizeof (gdouble));
            
            //gimp_pixel_rgn_get_col (&src_rgn, src, col, 0, height);
            [SEUtil get_pixels_col:blurBrush :src :col];
            
            sp_p = src;
            sp_m = src + (height - 1) * bytes;
            vp = val_p;
            vm = val_m + (height - 1) * bytes;
            
            /*  Set up the first vals  */
            for (i = 0; i < bytes; i++)
            {
                initial_p[i] = sp_p[i];
                initial_m[i] = sp_m[i];
            }
            
            for (row = 0; row < height; row++)
            {
                gdouble *vpptr, *vmptr;
                terms = (row < 4) ? row : 4;
                
                for (b = 0; b < bytes; b++)
                {
                    vpptr = vp + b; vmptr = vm + b;
                    for (i = 0; i <= terms; i++)
                    {
                        *vpptr += n_p[i] * sp_p[(-i * bytes) + b] - d_p[i] * vp[(-i * bytes) + b];
                        *vmptr += n_m[i] * sp_m[(i * bytes) + b] - d_m[i] * vm[(i * bytes) + b];
                    }
                    for (j = i; j <= 4; j++)
                    {
                        *vpptr += (n_p[j] - bd_p[j]) * initial_p[b];
                        *vmptr += (n_m[j] - bd_m[j]) * initial_m[b];
                    }
                }
                
                sp_p += bytes;
                sp_m -= bytes;
                vp += bytes;
                vm -= bytes;
            }
            
            transfer_pixels (val_p, val_m, dest, bytes, height);
            
            
            if (direct)
            {
                //gimp_pixel_rgn_set_col(&dest_rgn, dest, col + x1, y1, height);
                [SEUtil set_pixels_col:newBrush: dest :col];
                progress += height * vert;
                
            }
        }
        ppm_copy(&newBrush, &blurBrush);
    }
    
    /*  Now the horizontal pass  */
    if (horz > 0.0)
    {
        
        /* We do not want too many progress updates because they
         * can slow down the processing significantly for very
         * large images
         */
        progress_step = height / 16;
        
        if (progress_step < 5)
            progress_step = 5;
        
        horz = fabs (horz) + 1.0;
        
        if (horz != vert)
        {
            std_dev = sqrt (-(horz * horz) / (2 * log (1.0 / 255.0)));
            
            /*  derive the constants for calculating the gaussian
             *  from the std dev
             */
            find_iir_constants (n_p, n_m, d_p, d_m, bd_p, bd_m, std_dev);
            
        }
        
        
        for (row = 0; row < height; row++)
        {
            
            memset (val_p, 0, width * bytes * sizeof (gdouble));
            memset (val_m, 0, width * bytes * sizeof (gdouble));
            
            if (direct)
            {
                //gimp_pixel_rgn_get_row (&src_rgn, src, x1, row + y1, width);
                [SEUtil get_pixels_row:blurBrush :src :row];
            }
            
            sp_p = src;
            sp_m = src + (width - 1) * bytes;
            vp = val_p;
            vm = val_m + (width - 1) * bytes;
            
            /*  Set up the first vals  */
            for (i = 0; i < bytes; i++)
            {
                initial_p[i] = sp_p[i];
                initial_m[i] = sp_m[i];
            }
            
            for (col = 0; col < width; col++)
            {
                gdouble *vpptr, *vmptr;
                
                terms = (col < 4) ? col : 4;
                
                for (b = 0; b < bytes; b++)
                {
                    vpptr = vp + b; vmptr = vm + b;
                    
                    for (i = 0; i <= terms; i++)
                    {
                        *vpptr += n_p[i] * sp_p[(-i * bytes) + b] -
                        d_p[i] * vp[(-i * bytes) + b];
                        *vmptr += n_m[i] * sp_m[(i * bytes) + b] -
                        d_m[i] * vm[(i * bytes) + b];
                    }
                    for (j = i; j <= 4; j++)
                    {
                        *vpptr += (n_p[j] - bd_p[j]) * initial_p[b];
                        *vmptr += (n_m[j] - bd_m[j]) * initial_m[b];
                    }
                }
                
                sp_p += bytes;
                sp_m -= bytes;
                vp += bytes;
                vm -= bytes;
            }
            
            transfer_pixels (val_p, val_m, dest, bytes, width);
            
            
            if (direct)
            {
                //gimp_pixel_rgn_set_row (&dest_rgn, dest, x1, row + y1, width);
                [SEUtil set_pixels_row:newBrush :dest :row];
                progress += width * horz;
                

            }
        }
    }
    
    /*  free up buffers  */
    
    g_free (val_p);
    g_free (val_m);
    
    g_free (src);
    g_free (dest);
    ppm_kill(&blurBrush);
    return newBrush;
}
+ (NSArray*) getFeedChar
{
    NSArray* ret = [NSArray arrayWithObject:[NSNumber numberWithUnsignedChar:'.']];
    return ret;
}
+ (BOOL) isWifiConnectionOK
{
    struct sockaddr_in localWifiAddress;
    bzero(&localWifiAddress, sizeof(localWifiAddress));
    localWifiAddress.sin_len = sizeof(localWifiAddress);
    localWifiAddress.sin_family = AF_INET;
    // IN_LINKLOCALNETNUM is defined in <netinet/in.h> as 169.254.0.0
    localWifiAddress.sin_addr.s_addr = htonl(IN_LINKLOCALNETNUM);
    SCNetworkReachabilityRef reachabilityRef = SCNetworkReachabilityCreateWithAddress(kCFAllocatorDefault, (const struct sockaddr*)&localWifiAddress);
    if(reachabilityRef == NULL)
        return NO;
    SCNetworkReachabilityFlags flags = 0;

    if(SCNetworkReachabilityGetFlags(reachabilityRef, &flags))
    {
        // check we're reachable
        if((flags & kSCNetworkReachabilityFlagsReachable))
        {
            // check we're NOT on WWAN
            if((flags & kSCNetworkReachabilityFlagsIsWWAN))
            {
                CFRelease(reachabilityRef);
                return NO;
            }
            CFRelease(reachabilityRef);
            return YES;
        }
    }
    CFRelease(reachabilityRef);
    return NO;

}
@end
