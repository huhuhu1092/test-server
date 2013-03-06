//
//  PainterManager.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-13.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PainterManager.h"
#import "gimpressionist.h"
#import "PGMDataReader.h"
#import "random.h"
#import "SS_Model.h"
#import "SEUtil.h"
#import "PHImageView.h"
#import "SEImageAsyncLoader.h"
#import "SEViewNavigator.h"
#import "UserInfo.h"
#import "SelectedImage.h"
#import "SEResDefine.h"
#import "SESystemConfig.h"
#import "Signature.h"
#import "PhotoFrameAppDelegate.h"
#import "SEUserDefaultManager.h"
#import "SESystemDataManager.h"
#import <list>
#import <algorithm>
#import <sys/time.h>
typedef struct _Image
{
	int x;
	int y;
	int width;
	int height;
	int bpp; //bytes per pixel
	int rowstride;
    const unsigned char* data;
} Image;
/*
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
*/
#define IMAGE_HORIZON 1
#define IMAGE_VERTICAL 0
static PainterManager* sPainterManager;
static float calculateDensity(float brush_density_max, float brush_density_min)
{
    //float sliderMin = [SESystemConfig getMinDensityValue];
    float sliderMax = [SESystemConfig getMaxDensityValue];
    UserInfo* userInfo = [[PhotoFrameAppDelegate getViewNavigator] getUserInfo];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    float currentSelectDensity = [viewNav.mSystemDataManager.brushdensity intValue];
    float ratio = currentSelectDensity / sliderMax;
    float ret = brush_density_min + ratio * (brush_density_max - brush_density_min);
    NSLog(@"density value = %f", ret);
    return ret;
}
static float calculateEdgeDetectValue(float edgeDetectValue, float currentSelect)
{
    //float sliderMin = [SESystemConfig getMinEdgeDetectValue];
    float sliderMax = [SESystemConfig getMaxEdgeDetectValue];
    //UserInfo* userInfo = [[PhotoFrameAppDelegate getViewNavigator] getUserInfo];
    //float currentSelect = mCurrentEdgeDetectValue;//[userInfo.brushedgedetect intValue];
    //currentSelect = 10 - currentSelect;
    currentSelect = sliderMax - currentSelect;
    float ratio = 2 * currentSelect / sliderMax;
    float ret = edgeDetectValue * ratio;
    NSLog(@"edge detect value = %f", ret);
    return ret;
}
static ppm_t grabarea (Image drawable)
{
    Image  src_rgn;
    ppm_t        *p;
    gint          x1, y1, x2, y2;
    gint          x, y;
    gint          width, height;
    gint          row, col;
    gint          rowstride;
    ppm_t infile = {0, 0, NULL};
    BOOL colorRevert = sPainterManager.painterState.colorRevert;
    //NSLog(@"color revert = %d", colorRevert);
    //gimp_drawable_mask_bounds (drawable->drawable_id, &x1, &y1, &x2, &y2);
    x1 = drawable.x;
    y1 = drawable.y;
    x2 = x1 + drawable.width;
    y2= y1 + drawable.height;
    width  = x2 - x1;
    height = y2 - y1;
    
    ppm_new (&infile, width, height);
    p = &infile;
    
    rowstride = p->width * 3;
    
    src_rgn.x = x1;
    src_rgn.y = y1;
    src_rgn.width = width;
    src_rgn.height = height;
    src_rgn.bpp = drawable.bpp;
    src_rgn.data = drawable.data;
    src_rgn.rowstride = drawable.rowstride;
    const guchar *src = src_rgn.data;
    //debug
    /*
    for(int i = 0 ; i < src_rgn.rowstride * src_rgn.height ; i++)
    {
        if(src[i] != 0)
        {
            NSLog(@"## src = %d ##", src[i]);
        }
    }
     */
    //end
    switch (src_rgn.bpp)
    {
        case 1:
            for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
            {
                const guchar *s      = src;
                guchar       *tmprow = p->col + row * rowstride;
                
                for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
                {
                    gint k = col * 3;
                    
                    tmprow[k + 0] = s[0];
                    tmprow[k + 1] = s[0];
                    tmprow[k + 2] = s[0];
                    
                    s++;
                }
                
                src += src_rgn.rowstride;
            }
            break;
            
        case 2:
            for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
            {
                const guchar *s       = src;
                guchar       *tmprow  = p->col + row * rowstride;
                
                for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
                {
                    gint k = col * 3;
                    
                    tmprow[k + 0] = s[0];
                    tmprow[k + 1] = s[0];
                    tmprow[k + 2] = s[0];
                    
                    s += 2;
                }
                
                src += src_rgn.rowstride;
            }
            break;
            
        case 3:
            col = src_rgn.x - x1;
            
            for (y = 0, row = (src_rgn.y - y1); y < src_rgn.height; y++, row++)
            {
                memcpy (p->col + row * rowstride + col * 3, src, src_rgn.width * 3);
                
                src += src_rgn.rowstride;
            }
            break;
            
        case 4:
            for (y = 0, row = src_rgn.y - y1; y < src_rgn.height; y++, row++)
            {
                const guchar *s       = src;
                guchar       *tmprow  = p->col + row * rowstride;
                for (x = 0, col = src_rgn.x - x1; x < src_rgn.width; x++, col++)
                {
                    gint k = col * 3;
                    if(colorRevert == YES)
                    {
                        tmprow[k + 0] = s[2];
                        tmprow[k + 1] = s[1];
                        tmprow[k + 2] = s[0];
                    }
                    else
                    {
                        tmprow[k + 0] = s[0];
                        tmprow[k + 1] = s[1];
                        tmprow[k + 2] = s[2];
                    }
                    //assert(s[0] == 255);
                    //assert(s[1] == 255);
                    //assert(s[2] == 255);
                    s += 4;
                }
                
                src += src_rgn.rowstride;
            }
            break;
        default:
            break;
    }
    return infile;
}

static CFDataRef getImageData(CGImageRef inImage)
{
    CGDataProviderRef pv = CGImageGetDataProvider(inImage);
    CFDataRef dataRef = CGDataProviderCopyData(pv);
    return dataRef;
}
///////
@interface SEPainterManagerLoadPhoto : SEImageAsyncLoadHandler
{
    UIImage* currentImage;
    PainterManager* pm;
}
@property (nonatomic, assign) PainterManager* pm;
//@property (nonatomic, retain) SEImageAsyncLoader* imageLoader;
@end
@implementation SEPainterManagerLoadPhoto
@synthesize pm;
-(void) setImage:(UIImage *)image
{
    [currentImage release];
    currentImage = [image retain];
}
- (void)dealloc
{
    NSUInteger count2 = [currentImage retainCount];
    [currentImage release];
    [super dealloc];
}
- (void) preHandleImage
{}
/*
- (CGSize) calculateImageSizeByRatio: (CGSize) size dstSize: (CGSize) dstSize
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
*/
- (void)handleImage
{
    //// for test
    /*
    UIGraphicsBeginImageContext(CGSizeMake(1024, 768));
    [currentImage drawInRect:CGRectMake(0, 0, 1024, 768)];
    UIImage* retImage1 = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();

    //CGImageRef im = [currentImage CGImage];
    CGImageRef im = [retImage1 CGImage];
    CFDataRef imageData = getImageData(im);
    size_t bbb = CGImageGetBitsPerPixel(im);
    size_t bytesPerRow = CGImageGetBytesPerRow(im);
    CFIndex len = CFDataGetLength(imageData);
    size_t width1 = CGImageGetWidth(im);
    size_t height1 = CGImageGetHeight(im);
    NSLog(@"CFDataRef data size = %ld, width = %lu, height = %lu", len ,width1, height1);
    
    random_generator = g_rand_new ();
    Image srcImage;
    srcImage.x = 0;
    srcImage.y = 0;
    srcImage.width = width1;
    srcImage.height = height1;
    srcImage.bpp = bbb / 8;
    srcImage.rowstride = bytesPerRow;
    srcImage.data = CFDataGetBytePtr(imageData);
    ppm_t infile = grabarea(srcImage);
    ppm_t edgeD = edgeDetection(&infile, 128, 255);
    CGImageRef newImage = [PainterManager createCGImage:edgeD];
    ppm_kill(&infile);
    UIImage* uiImage = [UIImage imageWithCGImage:newImage];
    CGImageRelease(newImage);
    PHImageView* imageView = [[PainterManager painterManager] getImageView];
    imageView.image = uiImage;
    [imageView setNeedsDisplay];
    //NSDate* date = [NSDate date];
    //NSString* str = [date description];
    //NSLog(@"date = %@", str);
    struct timeval time;
    gettimeofday(&time, NULL);
    NSString* str = [NSString stringWithFormat:@"%s%ld%ld", "test-", time.tv_sec, time.tv_usec];
    [SEUtil savePNGImageToDocument:imageView.image withName:str];
     
    return;
    */
    
    const float pi = 3.1415926;
    CGImageRef imageRef = [currentImage CGImage];
    /*
    size_t currentImageWidth = CGImageGetWidth(imageRef);
    size_t currentImageHeight = CGImageGetHeight(imageRef);
    size_t currentBPP = CGImageGetBitsPerPixel(imageRef);
    currentBPP /= 8;
    CGDataProviderRef currentDataProvider = CGImageGetDataProvider(imageRef);
    CFDataRef currentDataRef = CGDataProviderCopyData(currentDataProvider);
    const uint8_t* currentData = CFDataGetBytePtr(currentDataRef);
    for(int i = 0 ; i < currentImageHeight ; i++)
    {
        for(int j = 0 ; j < currentImageWidth ; j++)
        {
            int r = currentData[i * currentBPP * currentImageWidth + j * currentBPP];
            int g = currentData[i * currentBPP * currentImageWidth + j * currentBPP + 1];
            int b = currentData[i * currentBPP * currentImageWidth + j * currentBPP + 2];
            if((r > 171 || r < 168) || (g > 171 || g < 168) || (b > 171 || b < 168))
            {
                NSLog(@"current r = %d, g = %d, b = %d", r, g, b);
            }
        }
    }
     CFRelease(currentDataRef);
     */
    
    /////////////////////////////////////////////
    int width = currentImage.size.width;//CGImageGetWidth(imageRef);
    int height = currentImage.size.height;//CGImageGetHeight(imageRef);
    UIImageOrientation orientation = currentImage.imageOrientation;
    NSLog(@"## %s : full image width = %d, height = %d, orientation = %d", __FUNCTION__, width, height, orientation);
    CGSize dsts = [pm currentPainterImageSize];
    CGSize tmpSize = dsts;
    if(width < height)
    {
        tmpSize.width = dsts.height;
        tmpSize.height = dsts.width;
    }
    CGSize actualSize = [SEUtil calculateImageSizeByRatio:CGSizeMake(width, height) dstSize:tmpSize];
    UIImage* newImage = [SEUtil drawImage:currentImage toSize:actualSize]; 
    imageRef = [newImage CGImage];//[SEUtil fastScale:[currentImage CGImage] withRect:actualSize ];//[newImage CGImage];
    //width = CGImageGetWidth(imageRef);
    //height = CGImageGetHeight(imageRef);
    orientation = newImage.imageOrientation;
    NSLog(@"## %s : new full image width = %d, height = %d, orientation = %d", __FUNCTION__, width, height, orientation);
    CGContextRef context = MyCreateBitmapContext(dsts.width, dsts.height);
    //CGRect drawRect = CGRectMake((dsts.width - actualSize.width) / 2, (dsts.height - actualSize.height) / 2, actualSize.width, actualSize.height);
    if(width >= height)
    {
        CGRect drawRect = CGRectMake((dsts.width - actualSize.width) / 2, (dsts.height - actualSize.height) / 2, actualSize.width, actualSize.height);
        switch (orientation) {
            case UIImageOrientationUp:
            {
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            case UIImageOrientationDown:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, pi);
                CGContextTranslateCTM(context, -dsts.width / 2, -dsts.height / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            case UIImageOrientationLeft:
            {
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationLeft;
            }
                break;
            case UIImageOrientationRight:
            {
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationRight;
            }
                break;
            case UIImageOrientationUpMirrored:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextScaleCTM(context, -1, 1);
                CGContextTranslateCTM(context, -dsts.width / 2, -dsts.height / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
                
            case UIImageOrientationDownMirrored:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextScaleCTM(context, 1, -1);
                CGContextTranslateCTM(context, -dsts.width / 2, -dsts.height / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            case UIImageOrientationLeftMirrored:
            {
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationLeftMirrored;
            }
                break;
            case UIImageOrientationRightMirrored:
            {
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationRightMirrored;
            }
                break;
            
            default:
                break;
        }
    }
    else 
    {
        CGRect drawRect = CGRectMake((dsts.height - actualSize.width) / 2, (dsts.width - actualSize.height) / 2, actualSize.width, actualSize.height);
        switch (orientation) 
        {
            case UIImageOrientationUp:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, pi / 2);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationRight;
            }
                break;
            case UIImageOrientationDown:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, -pi / 2);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationRight;
            }
                break;
            case UIImageOrientationLeft:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, pi / 2);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            case UIImageOrientationRight:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, -pi / 2);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            case UIImageOrientationUpMirrored:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, pi / 2);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationRight;
            }
                break;
            case UIImageOrientationDownMirrored:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, pi / 2);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationRight;
            }
                break;
            case UIImageOrientationLeftMirrored:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, pi / 2);
                CGContextScaleCTM(context, -1, 1);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            case UIImageOrientationRightMirrored:
            {
                CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
                CGContextRotateCTM(context, -pi / 2);
                CGContextScaleCTM(context, -1, 1);
                CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
                CGContextDrawImage(context, drawRect, imageRef);
                pm.mCurrentImageOrientation = UIImageOrientationUp;
            }
                break;
            default:
                break;
        }
    }
    /*
    pm.mCurrentImageOrientation = orientation;
    if(width < height)
    {
        CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
        CGContextRotateCTM(context, 90 * pi / 180);
        CGContextTranslateCTM(context, -dsts.height / 2, -dsts.width / 2);
        pm.mCurrentImageOrientation = 3;
    }
    if(orientation == 1)
    {
        CGContextTranslateCTM(context, dsts.width / 2, dsts.height / 2);
        CGContextScaleCTM(context, -1, -1);
        CGContextTranslateCTM(context, -dsts.width / 2, -dsts.height / 2);
    }
    if(width < height)
    {
        CGContextDrawImage(context, CGRectMake(0, 0, dsts.height , dsts.width), imageRef);
    }
    else 
    {
        
        CGContextDrawImage(context, CGRectMake(0, 0, dsts.width, dsts.height), imageRef);
    }
     */
    CGImageRef theCGImage = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    UIImage* retImage = [UIImage imageWithCGImage:theCGImage];
    CGImageRelease(theCGImage);
    retImage = [retImage retain];
    //for test
    /*
    CGDataProviderRef retDataProvider = CGImageGetDataProvider(theCGImage);
    size_t retBpp = CGImageGetBitsPerPixel(theCGImage);
    size_t retWidth = CGImageGetWidth(theCGImage);
    size_t retHeight = CGImageGetHeight(theCGImage);
    retBpp /= 8;
    CFDataRef retDataRef = CGDataProviderCopyData(retDataProvider);
    const uint8_t* retData = CFDataGetBytePtr(retDataRef);
    
    for(int i = 0 ; i < retHeight ; i++)
    {
        for(int j = 0 ; j < retWidth ; j++)
        {
            int r = retData[i * retBpp * retWidth + j * retBpp];
            int g = retData[i * retBpp * retWidth + j * retBpp + 1];
            int b = retData[i * retBpp * retWidth + j * retBpp  + 2];
            if((r > 171 || r < 168) || (g > 171 || g < 168) || (b > 171 || b < 168))
            {
                NSLog(@"r = %d, g = %d, b = %d", r, g, b);
            }
        }
    }
    
    CFRelease(retDataRef);
     */
    //end
    
    //for test
    //PHImageView* imageView = [[PainterManager painterManager] getImageView];
    //imageView.image = newImage;
    //[imageView setNeedsDisplay];
    //[SEUtil savePNGImageToDocument:retImage withName:@"inputImage"];
    //end
    
    
    [pm performSelectorInBackground:@selector(displayUIImageWithThread:) withObject:retImage];
    [currentImage release];
    currentImage = nil;
    
}
@end
///////////
@interface SS_3DData : NSObject {
@public
    SS_ModelManager* modelManager;
}
@end
@implementation SS_3DData

- (id) init
{
    self = [super init];
    if(self)
    {
        modelManager = new SS_ModelManager;
        modelManager->loadModel("photoframe.cbf");
    }
    return self;
}
- (void) dealloc
{
    delete modelManager;
    [super dealloc];
}
@end
////////////
@interface SS_ThreadShareData : NSObject {
@public
    SS_BrushListPool* brushListPool;
    SS_Canvas* brushCanvas;
    SS_PausePoint* pausePoint;
    SS_AtomicCounter* statusPoint;
}
- (id)init;
@end

@implementation SS_ThreadShareData

- (id) init
{
    self = [super init];
    if(self)
    {
        brushListPool = SS_BrushListPoolCreate();
        brushCanvas = SS_CanvasCreate();
        pausePoint = SS_PausePointCreate();
        statusPoint = SS_CreateAtomicConter();
    }
    return self;
}
- (void)dealloc
{
    if(brushListPool)
    {
        SS_BrushListPoolRelease(brushListPool);
    }
    if(brushCanvas)
    {
        SS_CanvasRelease(brushCanvas);
    }
    if(pausePoint)
    {
        SS_PausePointRelease(pausePoint);
    }
    if(statusPoint)
    {
        SS_ReleaseAtomicCounter(statusPoint);
    }
    [super dealloc];
}
@end

@implementation PainterParam
@synthesize state;
@synthesize paintid;
@synthesize width;
@synthesize height;
@synthesize orient_type;
@synthesize orient_num;
@synthesize orient_first;
@synthesize orient_last;
@synthesize size_num;
@synthesize size_first;
@synthesize size_last;
@synthesize size_type;
@synthesize bg_type;
@synthesize place_type;
@synthesize brush_density;
@synthesize paper_scale;
@synthesize paper_relief;
@synthesize brush_relief;
@synthesize color_type;
@synthesize drawing_speed;
@synthesize wait_time;
@synthesize brushName;
@synthesize brushName1;
@synthesize brushName2;
@synthesize paperName;
@synthesize brush_density_max;
@synthesize brush_density_min;
@synthesize edgeDetectLowValue;
- (void) print
{
    NSLog(@"#### param value #######");
    NSLog(@"state = %d", state );
    NSLog(@"paintid = %@", paintid);
    NSLog(@"sid = %d, %d, %d, %d", sid[0], sid[1], sid[2], sid[3]);
    NSLog(@"width = %d ", width);
    NSLog(@"height = %d ", height);
    NSLog(@"orient_type = %d ", orient_type);
    NSLog(@"orient_num = %d", orient_num);
    NSLog(@"orient_first = %f", orient_first);
    NSLog(@"orient_last = %f", orient_last);
    NSLog(@"size_num = %d", size_num);
    NSLog(@"size_first = %f", size_first);
    NSLog(@"size_last = %f", size_last);
    NSLog(@"size_type = %d", size_type);
    NSLog(@"bg_type = %d", bg_type);
    NSLog(@"place_type = %d", place_type);
    NSLog(@"brush_density = %f", brush_density);
    NSLog(@"paper_scale = %f", paper_scale);
    NSLog(@"paper_relief = %f", paper_relief);
    NSLog(@"brush_relief = %f", brush_relief);
    NSLog(@"color_type = %d", color_type);
    NSLog(@"drawing_speed = %d", drawing_speed);
    NSLog(@"wait_time = %d", wait_time);
    NSLog(@"brushName = %@", brushName);
    NSLog(@"brushName1 = %@", brushName1);
    NSLog(@"brushName2 = %@", brushName2);
    NSLog(@"paperName = %@", paperName);
    NSLog(@"edgeDetectLowValue = %f", edgeDetectLowValue);
    NSLog(@"#### end #####");
}
- (id)init
{
    self = [super init];
    if(self)
    {
        self.paper_scale = 30;
        self.brushName = [NSString stringWithString: @"defaultbrush.pgm"];
        self.brushName1 = [NSString stringWithString:@"paintbrush.pgm"];
        self.brushName2 = [NSString stringWithString:@"paintbrush02.pgm"];
        self.paperName = [NSString stringWithString:@"bricks.ppgm"];
    }
    return self;
}
- (void)setParam:(int)ot :(int)on :(float)of :(float)ol :(int)sn :(float)sf :(float)sl :(int)st
                :(int)bt :(int)pt :(float)bd :(float)ps :(float)pr :(float)br :(int)ct :(int)ds :(int)wt
{
    orient_type = ot;
    orient_num = on;
    orient_first = of;
    orient_last = ol;
    size_num = sn;
    size_first = sf;
    size_last = sl;
    size_type = st;
    bg_type = bt;
    place_type = pt;
    brush_density = bd;
    paper_scale = ps;
    paper_relief = pr;
    brush_relief = br;
    drawing_speed = ds;
    wait_time = wt;
    color_type = ct;
}
- (int) getSid: (int) index
{
    return sid[index];
}
- (void)setSid: (int*)d count:(int) count
{
    if(count > 4)
        return;
    for(int i = 0 ; i < count ; i++)
    {
        sid[i] = d[i];
    }
}
- (void)dealloc
{
    [brushName release];
    [brushName1 release];
    [brushName2 release];
    [paperName release];
    [paintid release];
    [super dealloc];
}
@end

@implementation BrushDefine
@synthesize brushGettingWay;
@synthesize brushID;
@synthesize brushGettingName;
@synthesize brushGettingLevel;
@synthesize brushNames;
@synthesize brushOutName;
- (id) init
{
    self = [super init];
    if(self)
    {
        brushNames = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    }
    return self;
}
- (void) dealloc
{
    [brushNames release];
    [brushOutName release];
    [super dealloc];
}
- (void) addBrushName: (NSString*) brushName
{
    [brushNames addObject:brushName];
}
- (NSArray*) getBrushNames
{
    return [NSArray arrayWithArray:brushNames];
}
@end
@implementation BrushPackage
@synthesize packageName;
- (id) init
{
    self = [super init];
    if(self)
    {
        brushDefineArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        brushNameArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        nameArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    }
    return self;
}
- (void) dealloc
{
    [brushDefineArray release];
    [brushNameArray release];
    [nameArray release];
    [packageName release];
    [super dealloc];
}
- (void) load:(NSString *)pn
{
    self.packageName = pn;
    NSString* data = [SEUtil readDataFromDocumentDir:pn];
    if(data == nil)
    {
        data = [SEUtil readDataFromBundle:pn];
    }
    if(data == nil)
    {
        NSLog(@"## can not find brush package : %@ ##\n", pn);
        return;
    }
    NSArray* dataLines = [data componentsSeparatedByString:@"\n"];
    enum BLOCK_TYPE {NO_BLOCK, BRUSH_TABLE_BLOCK, NAME_BLOCK, BRUSH_NAME_BLOCK};
    BLOCK_TYPE blockType = NO_BLOCK;
    for(int i = 0 ; i < dataLines.count ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([SEUtil isWhitespaceLine:line])
            continue;
        line = [SEUtil stringTrim:line];
        NSUInteger len = line.length;
        unichar firstC = [line characterAtIndex:0];
        unichar lastC = [line characterAtIndex:len - 1];
        if(firstC == '[' && lastC == ']')
        {
            if([line isEqualToString:@"[BrushTable]"])
            {
                blockType = BRUSH_TABLE_BLOCK;
            }
            else if([line isEqualToString:@"[Names]"])
            {
                blockType = NAME_BLOCK;
            }
            else if([line isEqualToString:@"[Brushes]"])
            {
                blockType = BRUSH_NAME_BLOCK;
            }
            continue;
        }
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        NSArray* newTokens = [NSArray array];
        for(NSString* str in tokens)
        {
            if([SEUtil isWhitespaceLine:str] == NO)
            {
                newTokens = [newTokens arrayByAddingObject:str];
            }
        }
        tokens = newTokens;
        NSString* s = nil;
        BrushDefine* bd = nil;
        NSMutableArray* data = nil;
        switch (blockType)
        {
            case BRUSH_NAME_BLOCK:
                data = [NSMutableArray array];
                s = [tokens objectAtIndex:0];
                [data addObject:s];
                s = [tokens objectAtIndex:1];
                [data addObject:[NSNumber numberWithInt:[s intValue]]];
                [brushNameArray addObject:data];
                break;
            case NAME_BLOCK:
                data = [NSMutableArray array];
                s = [tokens objectAtIndex:0];
                [data addObject:s];
                s = [tokens objectAtIndex:1];
                [data addObject:[NSNumber numberWithInt:[s intValue]]];
                [nameArray addObject:data];
                break;
            case BRUSH_TABLE_BLOCK:
                bd = [[BrushDefine alloc] init];
                s = [[tokens objectAtIndex:0] stringByTrimmingCharactersInSet:cs];
                bd.brushID = [s intValue];
                s = [[tokens objectAtIndex:1] stringByTrimmingCharactersInSet:cs];
                [bd.brushNames addObject:[NSNumber numberWithInt:[s intValue]]];
                s = [[tokens objectAtIndex:2] stringByTrimmingCharactersInSet:cs];
                [bd.brushNames addObject:[NSNumber numberWithInt:[s intValue]]];
                s = [[tokens objectAtIndex:3] stringByTrimmingCharactersInSet:cs];
                [bd.brushNames addObject:[NSNumber numberWithInt:[s intValue]]];
                s = [[tokens objectAtIndex:4] stringByTrimmingCharactersInSet:cs];
                bd.brushGettingWay = [s intValue];
                s = [[tokens objectAtIndex:5] stringByTrimmingCharactersInSet:cs];
                bd.brushGettingLevel = [s intValue];
                s = [[tokens objectAtIndex:6] stringByTrimmingCharactersInSet:cs];
                bd.brushGettingName = [s intValue];
                s = [[tokens objectAtIndex:7] stringByTrimmingCharactersInSet:cs];
                bd.brushOutName = s;
                NSLog(@"brush out name = %@", s);
                [brushDefineArray addObject:bd];
                [bd release];
                break;
            default:
                assert(0);
                break;
        }
        
    }
}
- (NSString*) getBrushString: (int) index
{
    for(int i = 0 ; i < brushNameArray.count ; i++)
    {
        NSArray* data = [brushNameArray objectAtIndex:i];
        int tmpIndex = [[data objectAtIndex:1] intValue];
        if(tmpIndex == index)
            return [data objectAtIndex:0];
    }
    return nil;
}
- (BrushDefine*) getBrushDefineByOutName: (NSString*)outName
{
    for(int i = 0 ;i < brushDefineArray.count ; i++)
    {
        BrushDefine* bd = [brushDefineArray objectAtIndex:i];
        if([bd.brushOutName isEqualToString:outName])
            return bd;
    }
    return nil;
}
- (BrushDefine*) getBrushDefine: (int) currentBrushId
{
    for(int i = 0 ;i < brushDefineArray.count ; i++)
    {
        BrushDefine* bd = [brushDefineArray objectAtIndex:i];
        if(bd.brushID == currentBrushId)
            return bd;
    }
    return nil;
}
- (NSArray*) getAllBrushID
{
    NSMutableArray* idArray = [NSMutableArray array];
    for(int i = 0 ; i < brushDefineArray.count ; i++)
    {
        BrushDefine* bd = [brushDefineArray objectAtIndex:i];
        [idArray addObject:[NSNumber numberWithInt:bd.brushID]];
    }
    return idArray;
}

- (NSArray*) getBrushes: (int) currentBrushID
{
    BrushDefine* bd = [self getBrushDefine:currentBrushID];
    if(bd == nil)
        return nil;
    NSMutableArray* brushes = bd.brushNames;
    NSMutableArray* retBrushes = [NSMutableArray array];
    for(NSNumber* n in brushes)
    {
        int i = [n intValue];
        NSString* str = [self getBrushString:i];
        assert(str != nil);
        if(str != nil)
            [retBrushes addObject: str];
    }
    return retBrushes;
}
@end
//////
struct ParamColumn
{
    char name[32];
    SSParamColumnType type;
};
/*
 the sequence in this array should be consistent with the table created by excel
 */
static struct ParamColumn paramColumnSet[] = {
    {"State", P_STATE},
    {"ID", P_ID},
    {"宽", P_WIDTH},
    {"高", P_HEIGHT},
    {"方向", P_ORIENT_TYPE},
    {"方向数", P_ORIENT_NUM},
    {"起始角度", P_ORIENT_FIRST},
    {"角度跨度", P_ORIENT_LAST},
    {"大小数", P_SIZE_NUM}, 
    {"最小大小", P_SIZE_FIRST},
    {"最大大小", P_SIZE_LAST},
    {"大小", P_SIZE_TYPE},
    {"背景", P_BG_TYPE},
    {"放置", P_PLACE_TYPE},
    {"最大比划密度", P_BRUSH_DENSITY_MAX},
    {"最小笔画密度", P_BRUSH_DENSITY_MIN},
    {"时间", P_WAITING_TIME},
    {"速度", P_DRAWING_SPEED}
};
/////////////////////////////
@implementation PainterQuality
@synthesize paintIDArray;
@synthesize percent;
@synthesize seq;
- (id)init
{
    if(self = [super init])
    {
        paintIDArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
    }
    return self;
}
- (void)dealloc
{
    [paintIDArray release];
    [super dealloc];
}
@end
////////
struct _BrushPiecesList
{
    std::list<BrushPiece> brushPieces;
    _BrushPiecesList()
    {
        NSLog(@"brush contructor");
    }
};
@implementation PainterState
@synthesize currentSeq;
@synthesize colorRevert;
@synthesize painterParamIDs;
@synthesize currentBrushSet;
@synthesize bSaveBrush;
@synthesize bShowSettingUI;
- (int) paintTimes
{
    return [painterParamIDs count];
}
- (id) init
{
    self = [super init];
    if(self)
    {
        brushPieces = new struct _BrushPiecesList;
        currentSeq = 0;
        bSaveBrush = YES;
        bShowSettingUI = YES;
        //currentBrushSet = [[NSArray alloc] initWithObjects:@"paintbrush.pgm", @"defaultbrush.pgm", @"paintbrush02.pgm", nil];
    }
    return self;
}
- (void)dealloc
{
    delete brushPieces;
    [painterParamIDs release];
    [currentBrushSet release];
    [super dealloc];
}
@end
/////////////////////////
@implementation PainterProperty

@synthesize percent;
@synthesize times;
@synthesize paper;
@end
////////////////////////////
@interface PainterManager (PrivateMethod)
- (BOOL)readParamFromDataFile: (NSString*)fileName;
- (BOOL)whitespaceLine: (NSString*)line;
- (int)indexInParamColumnSet: (NSString*) token;
- (PainterParam*) currentParam;
- (PainterParam*) getPainterParam:(NSString*)paramID;
- (void) displayCGImage:(CGImageRef)im;
- (void) setParam: (PainterParam*)p atType:(SSParamColumnType)index withContent:(NSString*)token;
- (void) computeFinished : (NSObject*) data;
- (void) saveCurrentPass: (BOOL) hasSignature;
- (BOOL) isSelectedImageValid: (NSString*) url : (NSString*) date;
- (PHImageView*)getImageView;
- (void) drawSignature;
- (void) readBrushDefine;
- (NSString*) readDataFromDocumentDir:(NSString*) fileName;
@end
@implementation PainterManager (PrivateMethod)
- (void) displayCGImage:(CGImageRef)im
{
    CGBitmapInfo bitmapInfo = CGImageGetBitmapInfo(im);
    switch (bitmapInfo) {
        case kCGBitmapAlphaInfoMask:
            NSLog(@"1");
            break;
        case kCGBitmapFloatComponents:
            NSLog(@"2");
            break;
    
        case kCGBitmapByteOrderMask:
            NSLog(@"3");
            break;
        case kCGBitmapByteOrderDefault:
            NSLog(@"4");
            break;
        case kCGBitmapByteOrder16Little:
            NSLog(@"5");
            break;
        case kCGBitmapByteOrder32Little:
            NSLog(@"6");
            break;
        case kCGBitmapByteOrder16Big:
            NSLog(@"7");
            break;
        case kCGBitmapByteOrder32Big:
            NSLog(@"8");
            break;
    }
    CFDataRef imageData = getImageData(im);
    size_t bbb = CGImageGetBitsPerPixel(im);
    size_t bytesPerRow = CGImageGetBytesPerRow(im);
    CFIndex len = CFDataGetLength(imageData);
    size_t width = CGImageGetWidth(im);
    size_t height = CGImageGetHeight(im);
    NSLog(@"CFDataRef data size = %ld", len );

    random_generator = g_rand_new ();
    Image srcImage;
    srcImage.x = 0;
    srcImage.y = 0;
    srcImage.width = width;
    srcImage.height = height;
    srcImage.bpp = bbb / 8;
    srcImage.rowstride = bytesPerRow;
    srcImage.data = CFDataGetBytePtr(imageData);
    ppm_t infile = grabarea(srcImage);
    CFRelease(imageData);
    CGImageRelease(im);
    static BOOL firstBg = YES;
    if(firstBg)
    {
        firstBg = NO;
        UIImage* image = [mViewNav.mResLoader getImage:@"MainDisplayBg"];
        //UIImage* image = [UIImage imageNamed:@"whitecolor.png"];
        CGImageRef imageRef = [image CGImage];
        CFDataRef bgData = getImageData(imageRef);
        size_t imageBits = CGImageGetBitsPerPixel(imageRef);
        size_t imageWidth = CGImageGetWidth(imageRef);
        size_t imageHeight = CGImageGetHeight(imageRef);
        CFIndex imagelen = CFDataGetLength(bgData);
        size_t imageBytesPerRow = CGImageGetBytesPerRow(imageRef);
        Image bgImage;
        bgImage.x = 0;
        bgImage.y = 0;
        bgImage.width = imageWidth;
        bgImage.height = imageHeight;
        bgImage.bpp = imageBits / 8;
        bgImage.rowstride = imageBytesPerRow;
        bgImage.data = CFDataGetBytePtr(bgData);
        ppm_t bgPPM = grabarea(bgImage);
        CFRelease(bgData);
        SS_Canvas* currentCanvas = SS_GetCurrentCanvas();
        SS_SetCanvasBackground(currentCanvas, bgPPM);
    }
    else
    {
            
    }
    //debug
    /*
    for(int i = 0 ; i < infile.height * infile.width * 3; i++)
    {
        if(infile.col[i] != 0)
        {
            NSLog(@"## c = %d ##", infile.col[i]);
        }
    }
     */
    //end
    //debug
    //SE_setBackground(infile);
    //end
    
    
    //CGImageRelease(im);
    
    NSLog(@"######################### get infile ####################\n");
    NSLog(@"## infile width = %d, height = %d ###\n", infile.width, infile.height);
    NSThread* mainThread = [NSThread mainThread];
    NSThread* currentThread = [NSThread currentThread];
    assert(currentThread != mainThread);
    startTime();
    RepaintData rd;
    PainterParam* myCurrentParam = [self currentParam];
    [myCurrentParam print];
    rd.pass = displayIndex;
    rd.adjustAngle = mAdjustAngle;
    rd.edgeDetectionEnd = 255;
    rd.edgeDetectionStart = 80;
    int times = [painterState paintTimes];
    static int edgeDetectionPassStartPos[] = {-1, -1, -1, 2, 2, 3, 4, 5, 6, 7, 7};
    rd.brushNum = 0;
    rd.currentGrayIndex = 0;
    rd.calculateOnEdge = [myCurrentParam getSid:3] == 2;
    NSLog(@"## mCurrentEdgeDetectValue = %d ##", mCurrentEdgeDetectValue);
    rd.edgeDetectionStart = calculateEdgeDetectValue(myCurrentParam.edgeDetectLowValue, mCurrentEdgeDetectValue);
    float minBrushTransparent = [SESystemConfig getMinBrushTransparentValue];
    float maxBrushTransparent = [SESystemConfig getMaxBrushTransparentValue];
    float radiusRatio = (mCurrentTransparentValue - minBrushTransparent) / (maxBrushTransparent - minBrushTransparent);
    float maxRadiusH = 100 * radiusRatio;
    float maxRadiusV = 100 * radiusRatio;
    float minRadius = 50 * radiusRatio;
    float maxSigma = 120;
    int radiusH = (int)(maxRadiusH + (minRadius - maxRadiusH) * displayIndex / (times - 1));
    int radiusV = (int)(maxRadiusV + (minRadius - maxRadiusV) * displayIndex / (times - 1));
    float sigma = (maxSigma + (1 - maxSigma) * displayIndex / (times - 1));
    if(displayIndex == (times - 1))
    {
        radiusH = 0;
        radiusV = 0;
    }
    rd.blurRadiusH = radiusH;
    rd.blurRadiusV = radiusV;
    rd.blurSigmaValue = sigma;
    rd.blurRatio = radiusRatio;
    rd.totalTimes = times;

    if(displayIndex == times - 1)
    {
        rd.lastTime = true;
    }
    else
    {
        rd.lastTime = false;
    }
    if(times % 2 == 0)
    {
        times = times / 2;
    }
    else
    {
        times = times / 2 + 1;
    }
    int step = 10 / times;
    if(displayIndex < times)
    {
        rd.brushSizeComp = 10 - displayIndex * step;
    }
    else 
    {
        rd.brushSizeComp = 0;
    }
    
    if(myCurrentParam.width == 1024 && myCurrentParam.height == 768)
    {
        rd.mostConcisePass = true;
    }
    else 
    {
        rd.mostConcisePass = false;
    }
        
    NSLog(@"### calculate edge = %d ###\n", rd.calculateOnEdge);
    //repaint4(&infile, NULL, rd);
    if(infile.width == 1024 && infile.height == 768)
    {
        SS_Canvas* currentCanvas = SS_GetCurrentCanvas();
        SS_SetCanvasOriginMap(currentCanvas, &infile);
    }
    repaint3(&infile, NULL, rd);
    endTime();
    double t = getTime();
    NSLog(@"### consume time is %f ###\n", t);
    ppm_kill(&infile);
    g_rand_free(random_generator);
    

    //[NSThread sleepForTimeInterval:120];
    [self performSelectorOnMainThread:@selector(computeFinished:) withObject:nil waitUntilDone:NO];
}
- (void) computeFinished : (NSObject*) data
{
    // make last piece added in main thread
    BrushPiece bp;
    bp.last_piece = 1;
    SS_BrushList* bl = SS_BrushListCreate();
    SS_AddBrushPiece(bl, bp);
    SS_BrushListPool* blp = SS_GetBrushListPool();
    SS_AddBrushList(blp, bl);
    
    if(mDrawFinishedArray[displayIndex] == NO)
    {
        SS_Pause((SS_PausePoint*)[self currentPausePoint]);
        
    }
    //SS_AddLog("###### compute end ########\n");
    NSLog(@"###### compute end ########\n");
    mComputeThreadEnd = YES;
    [mViewNav computeEnd];
    /*
    DRAW_IMAGE_STATE currentDrawImageState = [mViewNav getDrawImageState];
    if(currentDrawImageState == STOPPING_DRAWING_IMAGE)
    {
        [self releaseResourceForDraw];
        [mViewNav setDrawImageState:STOP_DRAW_IMAGE];
    }
    else if(currentDrawImageState == START_DRAW_IMAGE)
    {
        [self nextDisplayStage];
    }
    else if(currentDrawImageState == START_DRAW_IMAGE_PENDING)
    {
        [mViewNav setDrawImageState:START_DRAW_IMAGE];
        [self releaseResourceForDraw];
        [mViewNav displayNextImage];
    }
     */
}
- (PainterParam*) getPainterParam:(NSString*)paramID
{
    NSUInteger i;
    for(i = 0 ; i < [paramArrayFromFile count] ; i++)
    {
        PainterParam* p = [paramArrayFromFile objectAtIndex:i];
        if([p.paintid isEqualToString:paramID])
        {
            return p;
        }
    }
    return NULL;
}
- (PainterParam*) currentParam
{
    int index = painterState.currentSeq;
    NSArray* paramID = painterState.painterParamIDs;
    NSString* sid = [paramID objectAtIndex:index];
    
    PainterParam* p = [self getPainterParam:sid];
    return p;
}
- (BOOL)whitespaceLine: (NSString*)line
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
- (int)indexInParamColumnSet: (NSString*) token
{
    int size = sizeof(paramColumnSet) / sizeof(struct ParamColumn);
    for(int i = 0 ; i < size ; i++)
    {
        NSString* s = [NSString stringWithCString:paramColumnSet[i].name encoding:NSUTF8StringEncoding];
        if([s isEqualToString:token])
            return i;
    }
    return -1;
}
- (NSString*)readDataFile:(NSString*)fileName
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
- (BOOL) readPainterQuality : (NSString*)fileName
{
    NSString* dataContent = nil;
    dataContent = [self readDataFromDocumentDir:@"paramid_url.txt"];
    if(dataContent == nil)
    {
        dataContent = [self readDataFile:fileName];
    }
    if(dataContent)
    {
        NSArray* dataLines = [dataContent componentsSeparatedByString:@"\n"];
        NSUInteger i;
        for(i = 0 ; i < [dataLines count] ; i++)
        {
            NSString* line = [dataLines objectAtIndex:i];
            if([self whitespaceLine:line])
                continue;
            NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
            NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
            tokens = [SEUtil removeWhiteLineStringFrom:tokens];
            NSUInteger j;
            PainterQuality* pq = [[PainterQuality alloc] init];
            [pq autorelease];
            for(j = 0 ; j < [tokens count] ; j++)
            {
                NSString* s = [tokens objectAtIndex:j];
                NSString* tok = [s stringByTrimmingCharactersInSet:cs];
                int percent = -1;
                int seq = -1;
                int (^strToInt)(NSString*) = ^(NSString* tok) {
                    const char* str = [tok cStringUsingEncoding:NSASCIIStringEncoding];
                    char buf[3];
                    buf[0] = buf[1] = buf[2] = 0;
                    buf[0] = str[1];
                    buf[1] = str[2];
                    int i = atoi(buf);
                    return i;
                };
                if(j == 0)
                {
                    percent = strToInt(tok);
                    pq.percent = percent;
                }
                else if(j == 1)
                {
                    seq = strToInt(tok);
                    pq.seq = seq;
                }
                else if(j < 12)
                {
                    [pq.paintIDArray addObject: tok];
                }
            }
            if(pq.percent >= pq.seq)
            {
                [paramQualityArray addObject:pq];
            }
        }

        return YES;
    }
    else
        return NO;
}
- (void) readDataContent: (NSString*) dataContent
{
    NSArray* dataLines = [dataContent componentsSeparatedByString:@"\n"];
    NSUInteger i;
    //int paramCount = [dataLines count] - 1;
    //paramArrayFromFile = [NSMutableArray arrayWithCapacity:paramCount];
    //PainterParam** params = (PainterParam**)malloc(sizeof(PainterParam*) * paramCount);
    for(i = 0 ; i < [dataLines count] ; i++)
    {
        NSString* line = [dataLines objectAtIndex:i];
        if([self whitespaceLine:line])
            continue;
        if(i == 0)
        {
            continue;
        }
        NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
        NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
        tokens = [SEUtil removeWhiteLineStringFrom:tokens];
        NSUInteger j;
        PainterParam* p = [[PainterParam alloc] init];
        
        for(j = 0 ; j < [tokens count] ; j++)
        {
            NSString* s = [tokens objectAtIndex:j];
            NSString* tok = [s stringByTrimmingCharactersInSet:cs];
            if(![self whitespaceLine:tok])
            {
                [self setParam:p atType:(SSParamColumnType)j withContent:tok];
            }
        }
        [paramArrayFromFile addObject:p];
    }

}
- (NSString*) readDataFromDocumentDir:(NSString*) fileName
{
    NSArray* dirArray = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* dirPath = [dirArray objectAtIndex:0];
    NSString* filePath = [dirPath stringByAppendingFormat:@"/", fileName];
    NSString* dataContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    return dataContent;
}
/*
- (void) addBrushDefine : (NSArray*) tokens
{
    BrushDefine* bd = [[BrushDefine alloc] init];
    [brushDefineArray addObject:bd];
    [bd release];
    if(tokens.count == 3)
    {
        for(NSString* s in tokens)
        {
            [bd addBrushName:s];
        }
    }
    else
    {
        //TO-DO: add full brush define property
        assert(0);
    }
}
 */
- (void) readBrushDefine
{
    brushPackage = [[BrushPackage alloc] init];
    [brushPackage load:@"brushdefine.txt"];
}

- (BOOL) readParamFromDataFile: (NSString*)fileName;
{
    NSString* dataContent = [self readDataFile:fileName];
    if(dataContent)
    {
        NSArray* dataLines = [dataContent componentsSeparatedByString:@"\n"];
        NSUInteger i;
        //int paramCount = [dataLines count] - 1;
        //paramArrayFromFile = [NSMutableArray arrayWithCapacity:paramCount];
        //PainterParam** params = (PainterParam**)malloc(sizeof(PainterParam*) * paramCount);
        for(i = 0 ; i < [dataLines count] ; i++)
        {
            NSString* line = [dataLines objectAtIndex:i];
            if([self whitespaceLine:line])
                continue;
            if(i == 0)
            {
                continue;
            }
            NSCharacterSet* cs = [NSCharacterSet whitespaceAndNewlineCharacterSet];
            NSArray* tokens = [line componentsSeparatedByCharactersInSet:cs];
            tokens = [SEUtil removeWhiteLineStringFrom:tokens];
            NSUInteger j;
            PainterParam* p = [[PainterParam alloc] init];
            int ct = 0;
            for(j = 0 ; j < [tokens count] ; j++)
            {
                NSString* s = [tokens objectAtIndex:j];
                NSString* tok = [s stringByTrimmingCharactersInSet:cs];
                if(![self whitespaceLine:tok])
                {
                    ct++;
                    [self setParam:p atType:(SSParamColumnType)j withContent:tok];
                }
            }
            //assert(ct == 20);
            [paramArrayFromFile addObject:p];
        }
        
        return YES;
    }
    else
    {
        return NO;
    }
}
- (void) setParam: (PainterParam*)p atType:(SSParamColumnType)type withContent:(NSString*)token
{
    switch (type) 
    {
        case P_STATE:
        {
            if([token isEqualToString:@"Enable"])
            {
                p.state = 1;
            }
            else
            {
                p.state = 0;
            }
                
        }
        break;
        case P_ID:
        {
            const char* data = [token cStringUsingEncoding:NSUTF8StringEncoding];
            p.paintid = token;
            if(strlen(data) == 13)
            {
                char buf[3];
                memset(buf, 0, 3);
                buf[0] = data[1];
                buf[1] = data[2];
                int index = atoi(buf);
                buf[0] = data[4];
                buf[1] = data[5];
                int percent = atoi(buf);
                buf[0] = data[7];
                buf[1] = data[8];
                int times = atoi(buf);
                
                buf[0] = data[10];
                buf[1] = data[11];
                
                buf[0] = data[12];
                buf[1] = 0;
                int c = atoi(buf);
                int ret[4];
                ret[0] = index;
                ret[1] = percent;
                ret[2] = times;
                ret[3] = c;
                [p setSid:ret count:4];
            }
        }
        break;
        case P_WIDTH:
        {
            int w = [token intValue];
            p.width = w;
        }
            break;
        case P_HEIGHT:
        {
            int h = [token intValue];
            p.height = h;
        }
            break;
        case P_ORIENT_TYPE:
        {
            if([token isEqualToString:@"随机"])
            {
                p.orient_type = ORIENTATION_RANDOM;
            }
            else if([token isEqualToString:@"适应"])
            {
                p.orient_type = ORIENTATION_ADAPTIVE;
            }
        }
            break;
        case P_ORIENT_NUM:
        {
            int n = [token intValue];
            p.orient_num = n;
        }
            break;
        case P_ORIENT_FIRST:
        {
            float f = [token floatValue];
            p.orient_first = f;
        }
            break;
        case P_ORIENT_LAST:
        {
            float f = [token floatValue];
            p.orient_last = f;
        }
            break;
        case P_SIZE_NUM:
        {
            int n = [token intValue];
            p.size_num = n;
        }
            break;
        case P_SIZE_FIRST:
        {
            float f = [token floatValue];
            p.size_first = f;
        }
            break;
        case P_SIZE_LAST:
        {
            float f = [token floatValue];
            p.size_last = f;
        }
            break;
        case P_SIZE_TYPE:
        {
            if([token isEqualToString:@"随机"])
            {
                p.size_type = SIZE_TYPE_RANDOM;
            }
            else if([token isEqualToString:@"明度"])
            {
                p.size_type = SIZE_TYPE_VALUE;
            }
        }
            break;
        case P_BG_TYPE:
        {
            if([token isEqualToString:@"取自画纸"])
            {
                p.bg_type = BG_TYPE_FROM_PAPER;
            }
        }
            break;
        case P_PLACE_TYPE:
        {
            if([token isEqualToString:@"均匀分布"])
            {
                p.place_type = PLACEMENT_TYPE_EVEN_DIST;
            }
            else if([token isEqualToString:@"随机"])
            {
                p.place_type = PLACEMENT_TYPE_RANDOM;
            }
        }
            break;
        case P_BRUSH_DENSITY_MAX:
        {
            float f = [token floatValue];
            p.brush_density_max = f;
        }
            break;
        case P_BRUSH_DENSITY_MIN:
        {
            float f = [token floatValue];
            p.brush_density_min = f;
        }
            break;
            /*
        case P_BRUSH_DENSITY:
        {
            float f = [token floatValue];
            p.brush_density = f;
        }
             */
            break;
        case P_WAITING_TIME:
        {
            int i = [token intValue];
            p.wait_time = i;
        }
            break;
        case P_DRAWING_SPEED:
        {
            int i = [token intValue];
            p.drawing_speed = i;
        }
            break;
        case P_COLOR_TYPE:
        {
            if([token isEqualToString:@"笔画下面平均"])
            {
                p.color_type = 0;
            }
            else if([token isEqualToString:@"笔画下面中心"])
            {
                p.color_type = 1;
            }
        }
        case P_EDGEDETECT_LOWVALUE:
        {
            float v = [token floatValue];
            p.edgeDetectLowValue = v;
        }
            break;
        default:
            break;
    }
}
- (BOOL) isSelectedImageValid: (NSString*) url : (NSString*) date
{
    if(url == nil || date == nil)
        return NO;
    SelectedImage* si = [mViewNav getSelectedImageByUrl:url andDate:date];
    if(si == nil)
        return NO;
    else 
    {
        return YES;
    }
}
- (void) saveCurrentPass: (BOOL) hasSignature
{
    NSLog(@"save URL: %@", self.mSaveImageURL);
    if([self isSelectedImageValid:self.mSaveImageURL :self.mSaveImageDate])
    {
        if(hasSignature)
        {
            Signature* sig = [mViewNav getCurrentSignature];
            if(sig != nil)
            {
                NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
                SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
                CGImageRef image = [mViewNav createSignatureImageWithPoints:sd.points colors:sd.colors];
                CGRect dstRect = [mViewNav getSignatureViewRect];
                PHImageView* imageView = [self getImageView];
                CGImageRef totalImage = [imageView createSignatureImage:image frame:dstRect];
                UIImage* retImage = [UIImage imageWithCGImage:totalImage];
                CGImageRelease(image);
                CGImageRelease(totalImage);
                [self saveCurrentImage:retImage];
            }
        }
        else
        {
            PHImageView* imageView = [self getImageView];
           [self saveCurrentImage: imageView.image];
        }
    }
}
- (PHImageView*)getImageView
{
    UIWindow* window = [[UIApplication sharedApplication] keyWindow];
    UIViewController* viewController = window.rootViewController;
    UIView* rootView = viewController.view;
    UIView* v = [rootView viewWithTag:505];
    if(v)
    {
        PHImageView* imageView = (PHImageView*)v;
        return imageView;
    }
    else
        return nil;
}
- (void) drawSignature
{
    [mViewNav drawSignatureAnim];
}

@end

@implementation PainterManager
@synthesize paramArray;
@synthesize painterState;
@synthesize bgWidth = _bgWidth;
@synthesize bgHeight = _bgHeight;
@synthesize painterProperty;
@synthesize imageArray;
//@synthesize dateArray;
@synthesize isPause;
@synthesize currentImageIndex;
@synthesize mViewNav;
@synthesize mCurrentImageOrientation;
//@synthesize mStageDrawFinished;
@synthesize mBrushTransparent;
@synthesize mSaveImageURL;
@synthesize mSaveImageDate;
- (void)initParams
{
    PainterParam* p1 = [[PainterParam alloc] init];
    [p1 setParam:6 :8 :120 :60 :6 :179 :224 :4 :2 :1 :20 :30 :0 :0 :0 :100 :20];
    PainterParam* p2 = [[PainterParam alloc] init];
    [p2 setParam:6 :8 :45 :180 :6 :148 :184 :4 :2 :0 :10 :30 :0 :0 :0 :500 :20];
    
    PainterParam* p3 = [[PainterParam alloc] init];
    [p3 setParam:6 :8 :45 :180 :6 :120 :148 :4 :2 :0 :10 :30 :0 :0 :0 :500 :10];
    
    PainterParam* p4 = [[PainterParam alloc] init];
    [p4 setParam:6 :8 :45 :180 :6 :95 :116 :4 :2 :0 :10 :30 :0 :0 :0 :500 :10];
    
    PainterParam* p5 = [[PainterParam alloc] init];
    [p5 setParam:6 :8 :45 :180 :6 :73 :88 :4 :2 :0 :10 :30 :0 :0 :0 :500 :0];
    
    PainterParam* p6 = [[PainterParam alloc] init];
    [p6 setParam:6 :8 :45 :180 :6 :54 :64 :4 :2 :0 :10 :30 :0 :0 :0 :500 :0];
    
    PainterParam* p7 = [[PainterParam alloc] init];
    [p7 setParam:6 :8 :45 :180 :6 :38 :44 :4 :2 :0 :10 :30 :0 :0 :0 :1000 :0];
    
    PainterParam* p8 = [[PainterParam alloc] init];
    [p8 setParam:4 :8 :45 :180 :4 :25 :28 :4 :2 :0 :10 :30 :0 :0 :0 :1000 :0];
    
    PainterParam* p9 = [[PainterParam alloc] init];
    [p9 setParam:6 :8 :45 :180 :2 :15 :16 :4 :2 :0 :10 :30 :0 :0 :0 :1000 :0];
    
    PainterParam* p10 = [[PainterParam alloc] init];
    [p10 setParam:6 :8 :45 :180 :1 :8 :8 :4 :2 :0 :20 :30 :0 :0 :0 :1000 :0];
    
    paramArray = [[NSArray alloc] initWithObjects:p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, nil];
}
- (BOOL) readParamDataFileFromDocuments: (NSString*) str
{
    NSArray* dirArray = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* dirPath = [dirArray objectAtIndex:0];
    NSString* filePath = [dirPath stringByAppendingFormat:@"/paramset_url.txt"];
    NSString* dataContent = [NSString stringWithContentsOfFile:filePath encoding:NSUTF8StringEncoding error:nil];
    if(dataContent == nil)
        return NO;
    [self readDataContent:dataContent];
    return YES;
}
- (id)init
{
    self = [super init];
    if (self) 
    {
        // Initialization code here.
        [self initParams];
        for(int i = 0 ; i < PARAM_NUM ; i++)
        {
            drawingState[i] = 1;
        }
        painterState = [[PainterState alloc] init];
        paramArrayFromFile = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        paramQualityArray = [[NSMutableArray alloc] initWithArray:[NSArray array]];
        [self readBrushDefine];
        mCurrentBrush = 13;
        BOOL ret = [self readParamDataFileFromDocuments:@"paramset_url.txt"];
        if(ret == NO)
            [self readParamFromDataFile:@"paramset.txt"];
        [self readPainterQuality:@"paintiddefine.txt"];
        painterProperty = [[PainterProperty alloc] init];
        painterProperty.percent = 9;
        painterProperty.times = 5;
        painterProperty.paper = [NSString stringWithString:@"bricks.ppgm"];
        threadShareData = [[SS_ThreadShareData alloc] init];
        drawOnePicture = YES;
        displayIndex = -1;
        isPause = YES;
        mChangedDrawingIndex = -1;
        mFirstTimeDraw = YES;
        //mStageDrawFinished = YES;
        /////
        //data3D = [[SS_3DData alloc] init];
        data3D = nil;
    }
    
    return self;
}
- (void*) currentBrushListPool
{
    return threadShareData->brushListPool;
}
- (void*) currentBrushCanvas
{
    return threadShareData->brushCanvas;
}
- (void*) currentPausePoint
{
    return threadShareData->pausePoint;
}
- (void*) currentStatusPoint
{
    return threadShareData->statusPoint;
}
- (void)dealloc
{
    [painterState release];
    [paramArrayFromFile release];
    [paramArray release];
    [paramQualityArray release];
    [painterProperty release];
    [threadShareData release];
    [imageArray release];
    [mSaveImageDate release];
    [mSaveImageURL release];
    //[dateArray release];
    [brushPackage release];
    ///
    [data3D release];
    [super dealloc];
}
- (void)setDrawingState:(int)s index:(int)i
{
    if(i >= 0 && i < PARAM_NUM)
    {
        drawingState[i] = s;
    }
}
- (int)drawingState:(int)index
{
    return drawingState[index];
}

- (PainterParam*)currentParam: (int)index
{
    PainterParam* p = [paramArray objectAtIndex:index];
    return p;
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

- (CGSize) currentPainterImageSize
{
    int index = painterState.currentSeq;
    NSArray* paramID = painterState.painterParamIDs;
    NSString* sid = [paramID objectAtIndex:index];
    
    PainterParam* p = [self getPainterParam:sid];
    return CGSizeMake(p.width, p.height);
}
- (NSArray*) getAllGottenBrush
{
    NSArray* currentLevelBrushIDArray = [[mViewNav getUserUpgrade] getAllBrushIDByUserCurrentLevel];
    NSArray* currentAchieveBrushIDArray = [[mViewNav getUserUpgrade] getAllBrushIDByUserCurrentAchieve];
    NSArray* allBrushID = [brushPackage getAllBrushID];
    NSMutableArray* retArray = [NSMutableArray array];
    for(int i = 0 ; i < allBrushID.count ; i++)
    {
        int brushID = [[allBrushID objectAtIndex:i] intValue];
        BrushDefine* bd = [self getBrushDefine:brushID];
        switch (bd.brushGettingWay)
        {
            case ITEM_DEFAULT:
            {
                [retArray addObject:[NSNumber numberWithInt:brushID]];
            }
                break;
            case ITEM_LEVELUP:
            {
                NSUInteger index = [currentLevelBrushIDArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) 
                                    {
                                        NSNumber* num = (NSNumber*)obj;
                                        *stop = NO;
                                        if([num intValue] == brushID)
                                        {
                                            return YES;
                                        }
                                        else 
                                        {
                                            return NO;
                                        }
                                        
                                    }];
                if(index != NSNotFound)
                {
                    [retArray addObject:[NSNumber numberWithInt:brushID]];
                }
            }
                break;
            case ITEM_ARCHIEVE:
            {
                NSUInteger index = [currentAchieveBrushIDArray indexOfObjectPassingTest:^BOOL(id obj, NSUInteger idx, BOOL *stop) 
                                    {
                                        NSNumber* num = (NSNumber*)obj;
                                        *stop = NO;
                                        if([num intValue] == brushID)
                                        {
                                            return YES;
                                        }
                                        else 
                                        {
                                            return NO;
                                        }
                                        
                                    }];
                if(index != NSNotFound)
                {
                    [retArray addObject:[NSNumber numberWithInt:brushID]];
                }
            }
                break;
            case ITEM_BUY:
            {
                NSString* brushOutName = bd.brushOutName;
                NSString* productId = [[PhotoFrameAppDelegate getProductManager] getProductIdByBrushOutName:brushOutName];
                NSString* discountId = [[PhotoFrameAppDelegate getProductManager] getDiscountIdByBrushOutName:brushOutName];
                BOOL isBuied = [SEUserDefaultManager isProductBuied:productId] || [SEUserDefaultManager isProductBuied:discountId];
                if(isBuied == YES)
                {
                    [retArray addObject:[NSNumber numberWithInt:brushID]];
                }
            }
                break;
        }
    }
    return retArray;
}
- (void)setCurrentParamToGlobal
{
    int index = painterState.currentSeq;
    NSArray* paramID = painterState.painterParamIDs;
    NSString* sid = [paramID objectAtIndex:index];
    
    PainterParam* p = [self getPainterParam:sid];
    pcvals.orient_type = p.orient_type;
    pcvals.orient_num = p.orient_num;
    pcvals.orient_first = p.orient_first;
    pcvals.orient_last = p.orient_last;
    pcvals.size_num = p.size_num;
    pcvals.size_first = p.size_first;
    pcvals.size_last = p.size_last;
    pcvals.size_type = p.size_type;
    pcvals.general_background_type = p.bg_type;
    pcvals.place_type = p.place_type;
    //just for debug
    //pcvals.place_type = PLACEMENT_TYPE_EVEN_DIST;
    //end
    pcvals.brush_density = calculateDensity(p.brush_density_max, p.brush_density_min);
    
    pcvals.paper_scale = p.paper_scale;
    pcvals.paper_relief = p.paper_relief;
    pcvals.brush_relief = p.brush_relief;
    pcvals.color_type = p.color_type;
    const char* brush = [p.brushName cStringUsingEncoding:NSASCIIStringEncoding];
    const char* paper = [painterProperty.paper cStringUsingEncoding:NSASCIIStringEncoding];
    memset(pcvals.selected_brush, 0, sizeof(pcvals.selected_brush));
	strncpy(pcvals.selected_brush, brush, sizeof(pcvals.selected_brush) - 1);
	memset(pcvals.selected_paper, 0 , sizeof(pcvals.selected_paper));
	strncpy(pcvals.selected_paper, paper, sizeof(pcvals.selected_paper)  -1);
    UserInfo* userInfo = [mViewNav getUserInfo];
    int currentBrushID = [mViewNav.mSystemDataManager.currentbrushid intValue];
    int drawBrushMode = [[[mViewNav getUserInfo] drawbrushmode] intValue];
    if(drawBrushMode != 0)
    {
        if(drawBrushMode == 2)
        {
            NSArray* allBrushID = [self getAllGottenBrush];
            int index = abs(rand() % allBrushID.count);
            currentBrushID = [[allBrushID objectAtIndex:index] intValue];
        }
        else
        {
            NSArray* allBrushID = [self  getAllGottenBrush];
            if(displayIndex == 0)
            {
                int index = abs(rand() % allBrushID.count);
                mCurrentBrushIndex = index;
            }
            if(mCurrentBrushIndex < allBrushID.count)
            {
                currentBrushID = [[allBrushID objectAtIndex:mCurrentBrushIndex] intValue];
            }
        }
    }
    NSArray* currentBrushArray = [brushPackage getBrushes:currentBrushID];
    NSMutableArray* newBrushArray = [NSMutableArray array];
    for(int i = 0 ; i < currentBrushArray.count ; i++)
    {
        [newBrushArray addObject:[currentBrushArray objectAtIndex:0]];
        NSLog(@"brush %d = %@", i, [currentBrushArray objectAtIndex:i]);
    }
    assert(newBrushArray.count == 3);
    /*
    for(int i = 0 ; i < currentBrushArray.count ; i++)
    {
        NSString* s = [currentBrushArray objectAtIndex:i];
        NSLog(@"brush %d = %@", i, s);
    }
    NSString* s1 = [currentBrushArray objectAtIndex:0];
    NSArray* newArray = [NSArray array];
    for(int i = 0 ; i < currentBrushArray.count ; i++)
    {
        newArray = [newArray arrayByAddingObject:s1];
    }
     */
    painterState.currentBrushSet = newBrushArray;//currentBrushArray;//[[NSArray alloc] initWithObjects:p.brushName, p.brushName1, p.brushName2, nil];
    //painterState->wait_time = p.wait_time;
}
- (void) setCurrentPainterParamID:(NSArray*)paramIDArray
{
    painterState.painterParamIDs = paramIDArray;
}
- (PainterParam*)painterParam: (NSString*)sid
{
    return [self getPainterParam:sid];
}
- (void)setParam: (PainterParam*)p withID: (NSString*)sid
{
    PainterParam* dstp = [self painterParam:sid];
    dstp.orient_type = p.orient_type;
    dstp.orient_num = p.orient_num;
    dstp.orient_first = p.orient_first;
    dstp.orient_last = p.orient_last;
    dstp.size_num = p.size_num;
    dstp.size_first = p.size_first;
    dstp.size_last = p.size_last;
    dstp.size_type = p.size_type;
    dstp.bg_type = p.bg_type;
    dstp.place_type = p.place_type;
    dstp.brush_density = p.brush_density;
    dstp.paper_scale = p.paper_scale;
    dstp.paper_relief = p.paper_relief;
    dstp.brush_relief = p.brush_relief;
    dstp.color_type = p.color_type;
    dstp.brushName = p.brushName;
}
- (void)clearPainterState
{
    [painterState release];
    painterState = [[PainterState alloc] init];
}
+ (PainterManager*) painterManager
{
    if(!sPainterManager)
    {
        sPainterManager = [[PainterManager alloc] init];
    }
    return sPainterManager;
}
- (NSArray*) painterParamsByQuality: (int) percent withTimes: (int) times
{
    NSUInteger i;
    for(i = 0 ; i < [paramQualityArray count] ; i++)
    {
        PainterQuality* pq = [paramQualityArray objectAtIndex:i];
        if(pq.percent == percent && pq.seq == times)
        {
            int count = [pq.paintIDArray count];
            NSUInteger j;
            int validCount = 0;
            int (^stateFromStr)(NSString*) = ^(NSString* str){
                const char* ids = [str cStringUsingEncoding:NSASCIIStringEncoding];
                char buf[2];
                buf[0] = buf[1] = 0;
                buf[0] = ids[strlen(ids) - 1];
                int state = atoi(buf);
                return state;
            };
            for(j = 0 ; j < count ; j++)
            {
                NSString* s = [pq.paintIDArray objectAtIndex:j];
                int state = stateFromStr(s);
                if(state > 0)
                {
                    validCount++;
                }
            }
            NSString** pqArray = (NSString**)malloc( validCount * sizeof(NSString*));
            NSUInteger k = 0;
            for(j = 0; j < count ; j++)
            {
                NSString* s = [pq.paintIDArray objectAtIndex:j];
                int state = stateFromStr(s);
                if(state > 0)
                {
                    pqArray[k] = s;
                    k++;
                }
            }
            NSArray* retArray = [NSArray arrayWithObjects:pqArray count:validCount];
            free(pqArray);
            return retArray;
        }
    }
    return NULL;
}
- (CGImageRef) getCGImageRef : (NSString*)fileName
{
    //UIImage* uiImage = [UIImage imageNamed:fileName];
    //CGImageRef imageRef = [uiImage CGImage];
    NSURL* url = [NSURL URLWithString:fileName];
    ALAssetsLibrary* assetLib = [[ALAssetsLibrary alloc] init];
    CGImageRef imageRef = [SEUtil getFullRepresentationImageFromPhotoLib:url withAssetLib:assetLib];
    [assetLib release];
    CGSize dsts = [self currentPainterImageSize];
    CGContextRef context = MyCreateBitmapContext(dsts.width, dsts.height);
    CGContextDrawImage(context, CGRectMake(0, 0, dsts.width, dsts.height), imageRef);
    CGImageRef theCGImage = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    CGImageRelease(imageRef);
    return theCGImage;
}
/*
- (UIImage*) getImage : (NSString*)fileName
{
    UIImage* uiImage = [UIImage imageNamed:fileName];
    UIImageOrientation io = uiImage.imageOrientation;
    CGImageRef imageRef = [uiImage CGImage];
    PainterManager* painterManager = [PainterManager painterManager];
    int width = CGImageGetWidth(imageRef);
    int height = CGImageGetHeight(imageRef);
    CGSize dsts = [painterManager currentPainterImageSize];//CGSizeMake(1024, 768);
    CGSize s = [PainterManager computeFitSize:CGSizeMake(width, height) toDst:dsts];
    s = dsts;
    if(fabsf(s.width - width) < 3 && fabsf(s.height - height) < 3)
    {
        painterState.colorRevert = NO;
        return uiImage;
    }
    else
    {
        if(&UIGraphicsBeginImageContextWithOptions)
        {
            UIGraphicsBeginImageContextWithOptions(s, YES, 0.0);
        }
        else
            UIGraphicsBeginImageContext(s);
        
        [uiImage drawInRect:CGRectMake(0, 0, s.width, s.height)];
        UIImage* image = UIGraphicsGetImageFromCurrentImageContext();
        CGImageRef imageRef1 = [image CGImage];
        //[self compareImageRef: imageRef second: imageRef1];
        UIGraphicsEndImageContext();
        io = uiImage.imageOrientation;
        painterState.colorRevert = YES;
        //setImageRGBExchange(1);
        return image;
    }
}
*/
- (int)currentDrawingSpeed
{
    PainterParam* p = [self currentParam];
    return p.drawing_speed;
}
- (void) displayUIImageWithThread: (UIImage*)image
{
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    CGImageRef imageRef = [image CGImage];
    imageRef = CGImageRetain(imageRef);
    [image release];
    [self displayCGImage:imageRef];
    [pool release];
}
- (void) displayCGImageWithName:(NSString*) name
{
    NSLog(@"### create image ####");
    SEPainterManagerLoadPhoto* loadPhotoHandler = [[SEPainterManagerLoadPhoto alloc] init];
    ALAssetsLibrary* assetLib = [[ALAssetsLibrary alloc] init];
    loadPhotoHandler.pm = self;
    [loadPhotoHandler setAssetLibOwn:assetLib];
    //SEImageAsyncLoader* imageLoader =  [[SEImageAsyncLoader alloc] init];
    //[imageLoader setAssetLibOwn:assetLib];
    [assetLib release];
    //loadPhotoHandler.imageLoader = imageLoader;
    //[imageLoader release];
    NSURL* url = [NSURL URLWithString:name];
    [loadPhotoHandler loadFullRepresentation:url];
    //[imageLoader loadFullRepresentation:url withHandler:loadPhotoHandler];

}


- (void) updateImageView:(NSArray*)rectArray
{
    PHImageView* imageView = [self getImageView];
    for(int i = 0 ; i < rectArray.count; i++)
    {
        NSValue* v = [rectArray objectAtIndex:i];
        CGRect r = [v CGRectValue];
        [imageView setNeedsDisplayInRect:r];
    }
}
- (void)timerUpdate:(NSTimer*)theTimer
{
    SS_Canvas* currentCanvas = SS_GetCurrentCanvas();
    SS_DrawCanvas(currentCanvas);
    /*
    static int i = 0;
    //if(i == 0)
    {
        SS_UpdateImageView();
        i = 1;
    }
     
     */
}
- (void) setTimer
{
    [displayTimer invalidate];
    [displayTimer release];
    displayTimer = nil;
    NSTimeInterval t = ((double)painterState->wait_time) / 1000;
    NSLog(@"timer interval = %d ", painterState->wait_time);
    if(painterState->wait_time == 0)
        t = 0.01;
    displayTimer = [[NSTimer timerWithTimeInterval:t target:self selector:@selector(timerUpdate:) userInfo:nil repeats:YES] retain];
    [[NSRunLoop currentRunLoop] addTimer:displayTimer forMode:NSDefaultRunLoopMode];
    
}

- (void) initPcVals
{
    setDefaultPcvals();
    pcvals.general_dark_edge = 0.0f;
    pcvals.general_shadow_darkness = 0.0f;
    pcvals.general_shadow_depth = 0.0f;
    pcvals.general_shadow_blur = 0.0f;

}
- (BOOL) isHorizonPhoto: (float) width : (float)height : (int) o
{
    if(width > height)
    {
        if(o == UIImageOrientationDown || o == UIImageOrientationUp || o == UIImageOrientationUpMirrored || o == UIImageOrientationDownMirrored)
        {
            return YES;
        }
        else
        {
            return NO;        
        }
    }
    else
    {
        if(o == UIImageOrientationUp || o == UIImageOrientationDown || o == UIImageOrientationUpMirrored || o == UIImageOrientationDownMirrored)
        {
            return NO;
        }
        else 
        {
            return YES;
        }
    }    
}
- (NSArray*) getImageIndexByOrientation: (int) orient
{
    NSMutableArray* horizonArray = [NSMutableArray array];
    NSMutableArray* verticalArray = [NSMutableArray array];
    for(int i = 0 ; i < imageArray.count ; i++)
    {
        SelectedImage* si = [imageArray objectAtIndex:i];
        BOOL isH = [self isHorizonPhoto: [si.width intValue]: [si.height intValue]: [si.orientation intValue]];
        if(isH)
        {
            [horizonArray addObject:[NSNumber numberWithInt:i]];
        }
        else
        {
            [verticalArray addObject:[NSNumber numberWithInt:i]];
        }
    }
    if(orient == IMAGE_HORIZON)
    {
        return horizonArray;
    }
    else if(orient == IMAGE_VERTICAL)
    {
        return verticalArray;
    }
    else 
    {
        return nil;
    }
}
- (int) findNextImageIndex: (int)fromIndex withOrientation: (int)hv
{
    
}
- (int) advanceImageIndex : (int) inputIndex
{
    UserInfo* userInfo = [[PhotoFrameAppDelegate getViewNavigator] getUserInfo];
    int sequenceMode = [userInfo.imageplaymode intValue];
    BOOL isImageFilter = [userInfo.imagesizefilter boolValue] == NO;
    if(sequenceMode == 0)//SEQUENCE
    {
        if(isImageFilter == NO)
        {
            inputIndex++;
            int imageCount = [imageArray count];
            if(inputIndex >= imageCount)
            {
                inputIndex = 0;
            }
            return inputIndex;
        }
        else
        {
            if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
            {
                
            }
            else 
            {
                
            }
        }
    }
    else if(sequenceMode == 1)//random
    {
        int imageCount = [imageArray count];
        int random = rand();
        if(random < 0)
            random = -random;
        int index = random % imageCount;
        if(isImageFilter)
        {
            return index;
        }
        NSArray* horizontalArray = [self getImageIndexByOrientation:IMAGE_HORIZON];
        NSArray* verticalArray = [self getImageIndexByOrientation:IMAGE_VERTICAL];
        if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
        {
            if(verticalArray.count == 0)
            {
                return imageCount - 1;
            }
            else
            {
                index = random % verticalArray.count;
                return index;
            }
        }
        else
        {
            if(horizontalArray.count == 0)
            {
                return imageCount - 1;
            }
            else
            {
                index = random % horizontalArray.count;
                return index;
            }
        }
    }
    else if(sequenceMode == 2)
    {
        return inputIndex;
        /*
        NSArray* horizontalArray = [self getImageIndexByOrientation:IMAGE_HORIZON];
        NSArray* verticalArray = [self getImageIndexByOrientation:IMAGE_VERTICAL];
        if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
        {
            if([userInfo.imagesizefilter boolValue] == NO)
            {
                
            }
        }
        else
        {
            
        }
         */
    }
    else 
    {
        NSLog(@"## error sequence mode ##\n");
        return inputIndex;
    }
}
- (BOOL) findNextProperImageIndex
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([userInfo.imagesizefilter boolValue] == NO)
    {
        return YES;
    }
    int inputIndex = currentImageIndex;
    SelectedImage* si = [imageArray objectAtIndex:inputIndex];
    int imageCount = [imageArray count];
    if(UIInterfaceOrientationIsPortrait(mViewNav.interfaceOrientation))
    {
        int count = 0;
        while([self isHorizonPhoto: [si.width intValue]: [si.height intValue]: [si.orientation intValue]] && count < imageCount)
        {
            inputIndex = [self advanceImageIndex:inputIndex];
            si = [imageArray objectAtIndex:inputIndex];
            count++;
        }
        if(count == imageCount)
            return NO;
        currentImageIndex = inputIndex;
    }
    else
    {
        int count = 0;
        while([self isHorizonPhoto: [si.width intValue]: [si.height intValue]: [si.orientation intValue]] == NO && count < imageCount)
        {
            inputIndex = [self advanceImageIndex:inputIndex];
            si = [imageArray objectAtIndex:inputIndex];
            count++;
        }
        if(count == imageCount)
            return NO;
        currentImageIndex = inputIndex;
    }
    return YES;
}
- (void) displayNextImage
{
    SelectedImage* si = nil;
    if(imageArray.count > 0)
    {
        si = [imageArray objectAtIndex:currentImageIndex];
    }
    self.mSaveImageURL = si.url;
    self.mSaveImageDate = si.urldate;
    for(int i = 0 ; i < PARAM_NUM ; i++)
    {
        mDrawFinishedArray[i] = NO;
    }
    mChangedDrawingIndex = displayIndex - 1;
    mSaveOrientation = mCurrentImageOrientation;
    displayIndex = -1;
    //currentImageIndex = [self advanceImageIndex:currentImageIndex];
    [mViewNav performSelectorOnMainThread:@selector(displayNextImage) withObject:nil waitUntilDone:NO];
}

- (void) realNextDisplayStage: (NSMutableArray*)data
{
    if([mViewNav isInMainDisplay] == NO)
    {
        [data release];
        return;
    }
    SelectedImage* si = [data objectAtIndex:1];
    BOOL isExist = [[data objectAtIndex:2] boolValue];
    NSString* currentImageName = si.url;
    BOOL isDefaultImage = [SESystemConfig isDefaultSelectedImageURL:currentImageName];
    if(isExist == NO && isDefaultImage == NO)
    {
        NSLog(@"image is not in selected image list: %@, %@", si.url, si.urldate);
        si.width = [NSNumber numberWithInt:0];
        si.height = [NSNumber numberWithInt:0];
        [data release];
        [self displayNextImage];
        return;
    }
    displayIndex++;
    //for test
    //[self setFirstNum:[NSNumber numberWithInt:displayIndex]];
    //end
    if(mViewNav.mNewConfig)
    {
        mViewNav.mNewConfig = NO;
        [paramArrayFromFile release];
        paramArrayFromFile = [NSMutableArray array];
        [paramArrayFromFile retain];
        BOOL ret = [self readParamDataFileFromDocuments:@"paramset_url.txt"];
        if(ret == NO)
        {
            [self readParamFromDataFile:@"paramset.txt"];
        }
    }
    if(displayIndex >= [painterState paintTimes])
    {
        NSLog(@"## all compute pass end ###");
        [data release];
        [self displayNextImage];
        return;
    }
    painterState.currentSeq = displayIndex;
    NSArray* paramID = painterState.painterParamIDs;
    NSString* sid = [paramID objectAtIndex:displayIndex];
    NSLog(@"sid = %@", sid);
    PainterParam* p = [self getPainterParam:sid];
    [p print];
    painterState->wait_time = p.wait_time;
    [self initPcVals];
    [self setCurrentParamToGlobal];
    NSLog(@"## current image index: %d , pass : %d ##\n", currentImageIndex, displayIndex);
    UserInfo* userInfo = [mViewNav getUserInfo];
    mAdjustAngle = [mViewNav.mSystemDataManager.currentangle intValue];
    mBrushTransparent = [mViewNav.mSystemDataManager.currentbrushtransparent intValue];
    //mStageDrawFinished = NO;
    mComputeReady = NO;
    mComputeThreadEnd = NO;
    mCurrentEdgeDetectValue = [mViewNav.mSystemDataManager.brushedgedetect intValue];
    mCurrentTransparentValue = [mViewNav.mSystemDataManager.currentbrushtransparent intValue];
    [self displayCGImageWithName:currentImageName];
    if(displayIndex == 0)
    {
        [self startDrawing];
    }
    if(mFirstTimeDraw)
    {
        mFirstTimeDraw = NO;
        [self showLoadingView:nil];
    }
    [data release];
}
- (void) checkSelectedImageValid: (SelectedImage*)si
{
    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    NSString* urlStr = si.url;
    NSURL* url = [NSURL URLWithString:urlStr];
    NSMutableArray* data = [[NSMutableArray array] retain];
    [data addObject:lib];
    [lib release];
    [data addObject:si];
    ALAssetsLibraryAssetForURLResultBlock result = ^(ALAsset* asset)
    {
        BOOL isExist = NO;
        if(asset != NULL)
        {
            isExist = YES;
        }
        if(isExist)
        {
            isExist = [self isSelectedImageValid: si.url : si.urldate];
        }
        [data addObject:[NSNumber numberWithBool:isExist]];
        [self performSelectorOnMainThread:@selector(realNextDisplayStage:) withObject:data waitUntilDone:NO];
    };
    ALAssetsLibraryAccessFailureBlock fail = ^(NSError* error)
    {
        NSLog(@"error");
        [data addObject:[NSNumber numberWithBool:NO]];
        [self performSelectorOnMainThread:@selector(realNextDisplayStage:) withObject:data waitUntilDone:NO];
    };
    [lib assetForURL:url resultBlock:result failureBlock:fail];
}

- (void) nextDisplayStage
{
    if(imageArray == nil || imageArray.count == 0)
        return;
    SelectedImage* si = [imageArray objectAtIndex:currentImageIndex];
    [self checkSelectedImageValid:si];
    return;
    NSString* currentImageName = si.url;
    if([self isSelectedImageValid:si.url :si.urldate] == NO && [SESystemConfig isDefaultSelectedImageURL:si.url] == NO)
    {
        NSLog(@"image is not in selected image list: %@, %@", si.url, si.urldate);
        [self displayNextImage];
        return;
    }
    displayIndex++;
    //for test
    [self setFirstNum:[NSNumber numberWithInt:displayIndex]];
    //end
    if(mViewNav.mNewConfig)
    {
        mViewNav.mNewConfig = NO;
        [paramArrayFromFile release];
        paramArrayFromFile = [NSMutableArray array];
        [paramArrayFromFile retain];
        BOOL ret = [self readParamDataFileFromDocuments:@"paramset_url.txt"];
        if(ret == NO)
        {
            [self readParamFromDataFile:@"paramset.txt"];
        }
    }
    if(displayIndex >= [painterState paintTimes])
    {
        NSLog(@"## all compute pass end ###");
        [self displayNextImage];
        return;
    }
    painterState.currentSeq = displayIndex;
    NSArray* paramID = painterState.painterParamIDs;
    NSString* sid = [paramID objectAtIndex:displayIndex];
    NSLog(@"sid = %@", sid);
    PainterParam* p = [self getPainterParam:sid];
    [p print];
    painterState->wait_time = p.wait_time;
    [self initPcVals];
    [self setCurrentParamToGlobal];
    NSLog(@"## current image index: %d , pass : %d ##\n", currentImageIndex, displayIndex);
    UserInfo* userInfo = [mViewNav getUserInfo];
    mAdjustAngle = [mViewNav.mSystemDataManager.currentangle intValue];
    mBrushTransparent = [mViewNav.mSystemDataManager.currentbrushtransparent intValue];
    //mStageDrawFinished = NO;
    mComputeReady = NO;
    mComputeThreadEnd = NO;
    [self displayCGImageWithName:currentImageName];
    if(displayIndex == 0)
    {
        [self startDrawing];
    }
    if(mFirstTimeDraw)
    {
        mFirstTimeDraw = NO;
        [self showLoadingView:nil];
    }
}

- (void)initPainterState: (int)quality withTimes:(int)times
{
    NSArray* tmpParamArray = [self painterParamsByQuality:quality withTimes:times];
    [self setCurrentPainterParamID:tmpParamArray];
    painterState.currentSeq = 0;
}
- (void) getMinMaxTimesValue: (int)percent outMin:(int*)outMin outMax:(int*)outMax
{
    NSUInteger i;
    std::list<int> timesList;
    for(i = 0 ; i < [paramQualityArray count] ; i++)
    {
        PainterQuality* pq = [paramQualityArray objectAtIndex:i];
        if(pq.percent == percent)
        {
            timesList.push_back(pq.seq);
        }
    }
    timesList.sort();
    for(std::list<int>::iterator it = timesList.begin(); it != timesList.end(); it++)
    {
        NSLog(@"times = %d", *it);
    }
    std::list<int>::iterator min = std::min_element(timesList.begin(), timesList.end());
    *outMin = *min;
    std::list<int>::iterator max = std::max_element(timesList.begin(), timesList.end());
    *outMax = *max;
}
- (void*) detectModelManager
{
    if(data3D == nil)
        return nil;
    else {
        return data3D->modelManager;
    }
}
- (void*) modelManager
{
    if(data3D == nil)
    {
        data3D = [[SS_3DData alloc] init];
    }
    return data3D->modelManager;
}
- (void) releaseModelManager
{
    if(data3D != nil)
    {
        [data3D release];
        data3D = nil;
    }
}
- (NSArray*) currentBrushSet
{
    return painterState.currentBrushSet;
}
- (void) setCurrentBrushSet: (NSArray*) brushArray
{
    NSArray* newArray = [NSArray arrayWithArray:brushArray];
    painterState.currentBrushSet = newArray;
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
+ (CGImageRef) createCGImageWithCopy:(ppm_t)p
{
    CFDataRef data = CFDataCreate(NULL, p.col
                                  , p.width * p.height * 3);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGDataProviderRef dataProvider = CGDataProviderCreateWithCFData(data);
    CFRelease(data);
    CGImageRef cgImage = CGImageCreate(p.width, p.height, 8, 24, p.width * 3, colorSpace, kCGImageAlphaNoneSkipLast, dataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dataProvider);
    return cgImage;   
}
- (void) saveCurrentImage: (UIImage*)image
{
    NSString* url = self.mSaveImageURL;
    NSString* urlDate = self.mSaveImageDate;
    SelectedImage* si = [mViewNav getSelectedImageByUrl:url andDate:urlDate];
    if(si != nil)
    {
        NSLog(@"## save orientation = %d ##", mSaveOrientation);
        [mViewNav setSelectedImageProperty: url urlDate: urlDate orientation: mSaveOrientation image:image];
        /*
        BOOL isHoriz = [si.width intValue] > [si.height intValue];
        int orient = [si.orientation intValue];
        if(isHoriz == NO)
        {
            orient = UIImageOrientationRight;
        }
         */
        [mViewNav saveImageAndThumbnailToCoreData:image urlName:url urlDate:urlDate orientation: [si.orientation intValue] index:0];    
    }
}
- (void) pauseDrawing
{
    [displayTimer invalidate];
    [displayTimer release];
    displayTimer = nil;
    isPause = YES;
}
- (void) startDrawing
{
    [self setTimer];
    isPause = NO;
}
- (void) displayDrawFinishAnim
{
    
}
- (void) promptNextImageDraw
{
    [self displayComputeLoadingView];
    [mViewNav playDrawFrameHideAnim];
    //[self startDrawing];
    SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
}
- (void) calculateSignatureAutoColor
{
    UserInfo* userInfo = [mViewNav getUserInfo];
    if([[userInfo signatureautocolor] boolValue])
    {
        PHImageView* imageView = [self getImageView];
        CGRect rect = [mViewNav getSignatureViewRect];
        int startx = rect.origin.x;
        int starty = rect.origin.y;
        int endx = rect.origin.x + rect.size.width;
        int endy = rect.origin.y + rect.size.height;
        UIImage* image = imageView.image;
        CGImageRef imageRef = image.CGImage;
        CGDataProviderRef pv = CGImageGetDataProvider(imageRef);
        CFDataRef imageData = CGDataProviderCopyData(pv);;
        size_t bbb = CGImageGetBitsPerPixel(imageRef);
        size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
        //CFIndex len = CFDataGetLength(imageData);
        size_t width = CGImageGetWidth(imageRef);
        size_t height = CGImageGetHeight(imageRef);
        assert(width == 1024 && height == 768);
        bbb /= 8;
        assert(bbb == 3);
        const UInt8* data = CFDataGetBytePtr(imageData);
        double gray = 0;
        int count = 0;
        float cr = 0, cg = 0, cb = 0;
        for(int i = starty ; i < endy ; i++)
        {
            for(int j = startx ; j < endx ; j++)
            {
                const UInt8* src = data + i * bytesPerRow + j * 3;
                cr += src[0];
                cg += src[1];
                cb += src[2];
                double g = src[0] * 0.2126f + src[1] * 0.7152f + src[2] * 0.0722f;
                gray += g;
                count++;
            }
        }
        float avgGray = gray / count;
        avgGray /= 255;
        avgGray = 1.0 - avgGray;
        float avgR = cr / count;
        float avgG = cg / count;
        float avgB = cb / count;
        avgR = 1.0 - avgR / 255;
        avgG = 1.0 - avgG / 255;
        avgB = 1.0 - avgB / 255;
        NSLog(@"avgR = %f, avgG = %f, avgB = %f", avgR, avgG, avgB);
        //mViewNav.mSignatureAutoColor = [UIColor colorWithRed:avgGray green:avgGray blue:avgGray alpha:1.0];
        mViewNav.mSignatureAutoColor = [UIColor colorWithRed:avgR green:avgG blue:avgB alpha:1.0];
        CFRelease(imageData);
        //[SEUtil savePNGImageToDocument:image withName:@"autocolorimage"];
    }
    else
    {
        mViewNav.mSignatureAutoColor = nil;
    }
    
}
- (BOOL) hasSignature
{
    BOOL hasSigature = NO;
    Signature* sig = [mViewNav getCurrentSignature];
    if(sig != nil)
    {
        NSString* str = [NSString stringWithFormat:@"%d", [sig.seq intValue]];
        SignaturePointData sd = [mViewNav getSignaturePointsWithSeqName:str];
        if(sd.points.count > 0)
        {
            hasSigature = YES;
        }
    }
    return hasSigature;
}
- (void)drawFinishWhenPause
{
    BOOL drawpassFinish = NO;
    if(mDrawingIndex == 0)
    {
        mDrawFinishedArrayNum = [painterState paintTimes];
        NSLog(@"## %s : drawing times = %d", __FUNCTION__, mDrawFinishedArrayNum);
    }
    SS_BrushListPool* brushListPool = SS_GetBrushListPool();
    //brushListPool->clearDrawingBrushes();
    SS_ClearDrawingBrushes(brushListPool, mDrawingIndex);
    if(mChangedDrawingIndex == -1)
    {
        mDrawFinishedArray[mDrawingIndex] = YES;
        mDrawingIndex++;
        NSLog(@"## %s: next drawing index = %d, all drawing num = %d ##", __FUNCTION__, mDrawingIndex, mDrawFinishedArrayNum);
        if(mDrawingIndex == mDrawFinishedArrayNum)
        {
            SelectedImage* si = [imageArray objectAtIndex:currentImageIndex];
            self.mSaveImageURL = si.url;
            self.mSaveImageDate = si.urldate;
            mSaveOrientation = mCurrentImageOrientation;
            /*
            if([si.width intValue] > 0 && [si.height intValue] > 0)
            {
                [self saveCurrentPass];
            }
             */
            [self calculateSignatureAutoColor];
            [mViewNav finishOneImageDrawing];
            mDrawingIndex = 0;
            mDrawFinishedArrayNum = 0;
            drawpassFinish = YES;
        }
        else
        {
            [self startDrawing];
        }
    }
    else 
    {
        NSLog(@"# %s: mDrawingIndex = %d, mChangeDrawingIndex = %d ##", __FUNCTION__,  mDrawingIndex, mChangedDrawingIndex);
        /*
        if([self isSelectedImageValid:mSaveImageURL :mSaveImageDate] == YES)
        {
            assert(mDrawingIndex == mChangedDrawingIndex);
        }
         */
        mDrawingIndex = 0;
        mChangedDrawingIndex = -1; 
        mDrawFinishedArrayNum = 0;
        /*
        if([self isSelectedImageValid:mSaveImageURL :mSaveImageDate])
        {
            [self saveCurrentPass];
        }
         */
        [self calculateSignatureAutoColor];
        [mViewNav finishOneImageDrawing];
        drawpassFinish = YES;
    }
    if(drawpassFinish == YES)
    {
        [mViewNav playDrawFrameShowAnim];
        UserInfo* userInfo = [mViewNav getUserInfo];
        BOOL hasSig = [self hasSignature];
        if([userInfo.showsignatureview boolValue] == YES && hasSig)
        {
            [self saveCurrentPass : YES];
            //[self pauseDrawing];
            [self drawSignature];
        }
        else 
        {
            [self saveCurrentPass: NO];
            UserInfo* userInfo = [mViewNav getUserInfo];
            int waitTime = [userInfo.imageplaywaitingtime intValue];
            NSLog(@"wait time = %d", waitTime);
            if(waitTime == 0)
                waitTime = 5;
            [self performSelector:@selector(promptNextImageDraw) withObject:nil afterDelay:waitTime];
        }
    }
    else
    {
        SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
    }
}
- (void) drawFinishAction
{
    BOOL drawpassFinish = NO;
    if(mDrawingIndex == 0)
    {
        mDrawFinishedArrayNum = [painterState paintTimes];
        NSLog(@"## %s : drawing times = %d", __FUNCTION__, mDrawFinishedArrayNum);
    }
    SS_BrushListPool* brushListPool = SS_GetBrushListPool();
    SS_ClearDrawingBrushes(brushListPool, mDrawingIndex);
    if(mChangedDrawingIndex == -1)
    {
        mDrawFinishedArray[mDrawingIndex] = YES;
        mDrawingIndex++;
        NSLog(@"## %s: next drawing index = %d, all drawing num = %d ##", __FUNCTION__, mDrawingIndex, mDrawFinishedArrayNum);
        if(mDrawingIndex == mDrawFinishedArrayNum)
        {
            SelectedImage* si = [imageArray objectAtIndex:currentImageIndex];
            self.mSaveImageURL = si.url;
            self.mSaveImageDate = si.urldate;
            mSaveOrientation = mCurrentImageOrientation;
            /*
            if([self isSelectedImageValid:mSaveImageURL :mSaveImageDate])
            {
                [self saveCurrentPass];
            }
             */
            [self calculateSignatureAutoColor];
            [mViewNav finishOneImageDrawing];
            mDrawingIndex = 0;
            mDrawFinishedArrayNum = 0;
            drawpassFinish = YES;
        }
        else 
        {
            [self startDrawing];
        }
    }
    else 
    {
        NSLog(@"# %s: mDrawingIndex = %d, mChangeDrawingIndex = %d ##", __FUNCTION__,  mDrawingIndex, mChangedDrawingIndex);
        /*
        if([self isSelectedImageValid:mSaveImageURL :mSaveImageDate] == YES)
        {
            assert(mDrawingIndex == mChangedDrawingIndex);
        }
         */
        mDrawingIndex = 0;
        mChangedDrawingIndex = -1; 
        mDrawFinishedArrayNum = 0;
        /*
        if([self isSelectedImageValid:mSaveImageURL :mSaveImageDate])
        {
            [self saveCurrentPass];
        }
         */
        [self calculateSignatureAutoColor];
        [mViewNav finishOneImageDrawing];
        drawpassFinish = YES;
    }
    if(drawpassFinish == YES)
    {
        [mViewNav playDrawFrameShowAnim];
        UserInfo* userInfo = [mViewNav getUserInfo];
        BOOL hasSig = [self hasSignature];
        if([userInfo.showsignatureview boolValue] == YES && hasSig)
        {
            [self saveCurrentPass: YES];
            [self drawSignature];
        }
        else 
        {
            [self saveCurrentPass: NO];
            UserInfo* userInfo = [mViewNav getUserInfo];
            int waitTime = [userInfo.imageplaywaitingtime intValue];
            NSLog(@"wait time = %d", waitTime);
            if(waitTime == 0)
                waitTime = 5;
            [self performSelector:@selector(promptNextImageDraw) withObject:nil afterDelay:waitTime];
        }
    }
    else
    {
        SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
    }
    if(drawpassFinish == NO)
    {
        [self displayComputeLoadingView];
    }
}
- (void) drawFinished
{
    NSLog(@"## draw finished ##");
    //[self drawFinishAction];
    [mViewNav drawFinished];
}
- (void) displayComputeLoadingView
{
    if(mComputeReady == NO)
    {
        [self showLoadingView:nil];
    }
}
- (void) promptNextImageDrawAfterAnimation
{
    //[self saveCurrentPass];
    UserInfo* userInfo = [mViewNav getUserInfo];
    int waitTime = [userInfo.imageplaywaitingtime intValue];
    NSLog(@"wait time = %d", waitTime);
    if(waitTime == 0)
        waitTime = 5;
    [self performSelector:@selector(promptNextImageDraw) withObject:nil afterDelay:waitTime];
}
/*
- (void) changeDisplayIndex:(int) i
{
    displayIndex = i;
}
 */
- (NSArray*) getAllBrushID
{
    return [brushPackage getAllBrushID];
}
- (NSArray*) getBrushesById: (int) brushID
{
    return [brushPackage getBrushes:brushID];
}
- (BrushDefine*) getBrushDefine: (int) brushID
{
    return [brushPackage getBrushDefine:brushID];
}
- (BrushDefine*) getBrushDefineByOutName: (NSString*) outName
{
    return [brushPackage getBrushDefineByOutName:outName];
}
/*
- (int) currentBrushID
{
    return mCurrentBrush;
}
- (void) setCurrentBrushID: (int) brushID
{
    mCurrentBrush = brushID;
    
}
 */
- (void) addLog: (NSString*) text
{
    [mViewNav addLog:text];
}
- (void) setFirstNum: (NSNumber*) num
{
    [mViewNav setFirstViewNum:[num intValue]];
}
- (void) setSecondNum: (NSNumber*)num
{
    [mViewNav setSecondViewNum:[num intValue]];
}
- (void) computeReady: (id)param
{
    mComputeReady = YES;
    NSLog(@"### compte ready ####");
    [self hideLoadingView:nil];
}
- (void) showLoadingView: (id)param
{
    [mViewNav showDrawingWaitingView];
}
- (void) hideLoadingView: (id)param
{
    [mViewNav hideDrawingWaitingView];
}
- (void) paintSignatureToImage: (CGImageRef) imageRef frame:(CGRect)frame
{
    PHImageView* imageView = [self getImageView];
    [imageView paintSignatureImage:imageRef frame:frame];
}
- (void) releaseResourceForDraw
{
    SS_ClearBrushListPool(threadShareData->brushListPool);
    [displayTimer invalidate];
    [displayTimer release];
    displayTimer = nil;
    SS_AtomicCounter* statusPoint = threadShareData->statusPoint;
    SS_SetAtomicCounterValue(statusPoint, 1);
    displayIndex = -1;
    self.mSaveImageURL = nil;
    self.mSaveImageDate = nil;
    mChangedDrawingIndex = -1;
    mDrawingIndex = 0;
    for(int i = 0 ; i < PARAM_NUM ; i++)
    {
        mDrawFinishedArray[i] = NO;
    }
    mDrawFinishedArrayNum = 0;
}
- (void) stopDrawImage
{
    DRAW_IMAGE_STATE currentDrawImageState = [mViewNav getDrawImageState];
    if(currentDrawImageState == START_DRAW_IMAGE || currentDrawImageState == PAUSE_DRAW_IMAGE)
    {
        if(currentDrawImageState == START_DRAW_IMAGE)
        {
            mFromStartDrawImageState = YES;
        }
        else
        {
            mFromStartDrawImageState = NO;    
        }
        if(mComputeThreadEnd == NO)
        {
            SS_AtomicCounter* statusPoint = threadShareData->statusPoint;
            SS_SetAtomicCounterValue(statusPoint, 0);
            SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
            [mViewNav setDrawImageState:STOPPING_DRAWING_IMAGE];
        }
        else
        {
            [mViewNav setDrawImageState:STOP_DRAW_IMAGE];
            [self releaseResourceForDraw];
        }
    }
    else if(currentDrawImageState == START_DRAW_IMAGE_PENDING)
    {
        if(mComputeThreadEnd == NO)
        {
            [mViewNav setDrawImageState:STOPPING_DRAWING_IMAGE];
        }
        else
        {
            [mViewNav setDrawImageState:STOP_DRAW_IMAGE];
            [self releaseResourceForDraw];
        }
    }
}
- (void) startDrawImage
{
    DRAW_IMAGE_STATE currentDrawImageState = [mViewNav getDrawImageState];
    if(currentDrawImageState == INIT_DRAW_IMAGE_STATE)
    {
        [mViewNav setDrawImageState:START_DRAW_IMAGE];
        SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
        [mViewNav displayNextImage];
    }
    else if(currentDrawImageState == STOPPING_DRAWING_IMAGE)
    {
        if(mComputeThreadEnd == NO)
        {
            [mViewNav setDrawImageState:START_DRAW_IMAGE_PENDING];
        }
        else 
        {
            if(mFromStartDrawImageState)
            {
                [mViewNav setDrawImageState:START_DRAW_IMAGE];
                SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
                [mViewNav displayNextImage];
                //set the interface with start state
            }
            else
            {
                // set interface with start state
            }
        }
    }
    else if(currentDrawImageState == STOP_DRAW_IMAGE)
    {
        if(mFromStartDrawImageState)
        {
            [mViewNav setDrawImageState:START_DRAW_IMAGE];
            SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
            [mViewNav displayNextImage];
            //set interface with start state
        }
        else
        {
            
        }
    }
    else if(currentDrawImageState == PAUSE_DRAW_IMAGE)
    {
        [mViewNav setDrawImageState:START_DRAW_IMAGE];
        mFromStartDrawImageState = YES;
        [self startDrawImage];
    }
    
}
- (void) pauseDrawImage
{
    DRAW_IMAGE_STATE currentDrawImageState = [mViewNav getDrawImageState];
    if(currentDrawImageState == START_DRAW_IMAGE)
    {
        [mViewNav setDrawImageState:PAUSE_DRAW_IMAGE];
        mFromStartDrawImageState = NO;
        [self pauseDrawing];
    }
    else
    {
            
    }
}
- (void) setFirstTimeDraw: (BOOL) b
{
    mFirstTimeDraw = b;
}
- (void) doAction: (int)fromState toState: (int)toState
{

    switch (fromState) {
        case INIT_DRAW_IMAGE_STATE:
        {
            switch (toState) 
            {
                case INIT_DRAW_IMAGE_STATE:
                {}
                    break;
                case START_DRAW_IMAGE:
                {
                    [mViewNav displayNextImage];
                }
                    break;
                case STOPPING_DRAWING_IMAGE:
                {}
                    break;
                case STOP_DRAW_IMAGE:
                {}
                    break;
                case START_DRAW_IMAGE_PENDING:
                {}
                    break;
                case PAUSE_DRAW_IMAGE:
                {}
                    break;
                default:
                    break;
            }
        }
            break;
        case START_DRAW_IMAGE:
        {
            switch (toState) 
            {
                case INIT_DRAW_IMAGE_STATE:
                {}
                    break;
                case START_DRAW_IMAGE:
                {}
                    break;
                case STOPPING_DRAWING_IMAGE:
                {
                    
                }
                    break;
                case STOP_DRAW_IMAGE:
                {}
                    break;
                case START_DRAW_IMAGE_PENDING:
                {}
                    break;
                case PAUSE_DRAW_IMAGE:
                {}
                    break;
                default:
                    break;
            }
        }
            break;
        case STOPPING_DRAWING_IMAGE:
        {
            switch (toState) 
            {
                case INIT_DRAW_IMAGE_STATE:
                {}
                    break;
                case START_DRAW_IMAGE:
                {}
                    break;
                case STOPPING_DRAWING_IMAGE:
                {}
                    break;
                case STOP_DRAW_IMAGE:
                {}
                    break;
                case START_DRAW_IMAGE_PENDING:
                {}
                    break;
                case PAUSE_DRAW_IMAGE:
                {}
                    break;
                default:
                    break;
            }
        }
            break;
        case STOP_DRAW_IMAGE:
        {
            switch (toState) 
            {
                case INIT_DRAW_IMAGE_STATE:
                {}
                    break;
                case START_DRAW_IMAGE:
                {}
                    break;
                case STOPPING_DRAWING_IMAGE:
                {}
                    break;
                case STOP_DRAW_IMAGE:
                {}
                    break;
                case START_DRAW_IMAGE_PENDING:
                {}
                    break;
                case PAUSE_DRAW_IMAGE:
                {}
                    break;
                default:
                    break;
            }
        }
            break;
        case START_DRAW_IMAGE_PENDING:
        {
            switch (toState) 
            {
                case INIT_DRAW_IMAGE_STATE:
                {}
                    break;
                case START_DRAW_IMAGE:
                {}
                    break;
                case STOPPING_DRAWING_IMAGE:
                {}
                    break;
                case STOP_DRAW_IMAGE:
                {}
                    break;
                case START_DRAW_IMAGE_PENDING:
                {}
                    break;
                case PAUSE_DRAW_IMAGE:
                {}
                    break;
                default:
                    break;
            }
        }
            break;
        case PAUSE_DRAW_IMAGE:
        {
            switch (toState) 
            {
                case INIT_DRAW_IMAGE_STATE:
                {}
                    break;
                case START_DRAW_IMAGE:
                {}
                    break;
                case STOPPING_DRAWING_IMAGE:
                {}
                    break;
                case STOP_DRAW_IMAGE:
                {}
                    break;
                case START_DRAW_IMAGE_PENDING:
                {}
                    break;
                case PAUSE_DRAW_IMAGE:
                {}
                    break;
                default:
                    break;
            }
        }
            break;
        default:
            break;
    }
}
- (BOOL) isComputeEnd
{
    return mComputeThreadEnd;
}
@end
