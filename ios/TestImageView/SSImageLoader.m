//
//  SSImageLoader.m
//  TestImageView
//
//  Created by 陈勇 on 11-9-27.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "SSImageLoader.h"
#import <QuartzCore/QuartzCore.h>
#import <AssetsLibrary/AssetsLibrary.h>
#import "gimpressionist.h"
#import "ppmtool.h"
#import "random.h"
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

void initPainterProperty(struct PainterProperty* p)
{
    memset(p, 0, sizeof(struct PainterProperty));
}

/////////
ppm_t grabarea (Image drawable)
{
    Image  src_rgn;
    ppm_t        *p;
    gint          x1, y1, x2, y2;
    gint          x, y;
    gint          width, height;
    gint          row, col;
    gint          rowstride;
    ppm_t infile = {0, 0, NULL};
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
                    
                    tmprow[k + 0] = s[0];
                    tmprow[k + 1] = s[1];
                    tmprow[k + 2] = s[2];
                    
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
    return CGDataProviderCopyData(CGImageGetDataProvider(inImage));
}
static CGContextRef createRGBABitmapContext(CGImageRef inImage)
{
    CGContextRef context = NULL;
    CGColorSpaceRef colorSpace;
    void* bitmapData;
    int bitmapByteCount;
    int bitmapBytesPerRow;
    size_t pixelsWidth = CGImageGetWidth(inImage);
    size_t pixelsHeight = CGImageGetHeight(inImage);
    bitmapBytesPerRow = pixelsWidth * 4;
    bitmapByteCount = pixelsHeight * bitmapBytesPerRow;
    colorSpace = CGColorSpaceCreateDeviceRGB();
    
    if(colorSpace == NULL)
    {
        NSLog(@"error allocating color space\n");
        return NULL;
    }
    bitmapData = malloc(bitmapByteCount);
    if(bitmapData == NULL)
    {
        NSLog(@"Memory not allocated\n");
        CGColorSpaceRelease(colorSpace);
        return NULL;
    }
    context = CGBitmapContextCreate(bitmapData, pixelsWidth, pixelsHeight,
                                    8, bitmapBytesPerRow, colorSpace, kCGImageAlphaPremultipliedFirst);
    if(context == NULL)
    {
        free(bitmapData);
        NSLog(@"context not create \n");
    }
    CGColorSpaceRelease(colorSpace);
    return context;
    
}
static void handleImageData(unsigned char* data)
{
    
}
static void manipulateImagePixelData(CGImageRef inImage)
{
    CGContextRef cgctx = createRGBABitmapContext(inImage);
    if(cgctx == NULL)
        return;
    size_t w = CGImageGetWidth(inImage);
    size_t h = CGImageGetHeight(inImage);
    CGRect rect = {{0, 0} , {w, h}};
    CGContextDrawImage(cgctx, rect, inImage);
    void* data = CGBitmapContextGetData(cgctx);
    if(data != NULL)
    {
        handleImageData((unsigned char*)data);
    }
    CGContextRelease(cgctx);
    if(data != NULL)
        free(data);
}

@interface  SSTestAutoRelease : NSObject
- (void) dealloc;
+ (id) create;
@end
@implementation SSTestAutoRelease

- (void)dealloc
{
    NSUInteger count = [self retainCount];
    NSLog(@"string retain count = %u\n", count);
    [super dealloc];
}
+ (id) create
{
    SSTestAutoRelease* t = [[SSTestAutoRelease alloc] init];
    [t autorelease];
    return t;
}
@end

@interface SSImageLoader (PrivateMethod)
- (void) displayImage:(CGImageRef)im;
- (UIImage*) createUIImage:(CFDataRef)imageData :(size_t)width : (size_t)height;
- (void)loadImageFile:(NSString*)fileName;
@end
@implementation SSImageLoader (PrivateMethod)
- (void) displayImage:(CGImageRef)im
{
    CFDataRef imageData = getImageData(im);
    CFIndex len = CFDataGetLength(imageData);
    //UIImage* im2 = [UIImage imageWithCGImage:im];
    UIImage* im2 = [self createUIImage:imageData:CGImageGetWidth(im):CGImageGetHeight(im)];
    NSLog(@"CFDataRef data size = %ld", len );
    NSLog(@"image width = %f, height = %f", im2.size.width, im2.size.height);
    /*
    if(setImageCallback)
    {
        [setImageCallback displayImage: im2];
    }
     */
}
- (UIImage*) createUIImage:(CFDataRef)imageData :(size_t)width : (size_t)height
{
    char brush[] = "defaultbrush.pgm";
    char paper[] = "bricks.pgm";
    CFIndex len = CFDataGetLength(imageData);
	memset(pcvals.selected_brush, 0, sizeof(pcvals.selected_brush));
	strncpy(pcvals.selected_brush, brush, sizeof(pcvals.selected_brush) - 1);
	memset(pcvals.selected_paper, 0 , sizeof(pcvals.selected_paper));
	strncpy(pcvals.selected_paper, paper, sizeof(pcvals.selected_paper)  -1);
    random_generator = g_rand_new ();
    Image srcImage;
    srcImage.x = 0;
    srcImage.y = 0;
    srcImage.width = width;
    srcImage.height = height;
    srcImage.bpp = len / (width * height);
    srcImage.rowstride = width * srcImage.bpp;
    srcImage.data = CFDataGetBytePtr(imageData);
    ppm_t infile = grabarea(srcImage);
    
    NSLog(@"######################### get infile ####################");
    NSLog(@"## infile width = %d, height = %d ###", infile.width, infile.height);
    
    startTime();
    repaint(&infile, NULL);
    endTime();
    int t = getTime();
    NSLog(@"### consume time is %d ###\n", t);
    CGImageRef cgImage = [SSImageLoader createCGImage:&infile];
    UIImage* im = [UIImage imageWithCGImage:cgImage];
    if(setImageCallback)
    {
        //[setImageCallback displayImage:im];
    }
    NSLog(@"repaint time = %d", t);
    g_rand_free(random_generator);
    return im;
}
- (void)loadImageFile:(NSString*)fileName
{
    UIImage* uiImage = [UIImage imageNamed:fileName];
    CGImageRef imageRef = [uiImage CGImage];
    [self displayImage:imageRef];
}

@end

@implementation SSImageLoader
@synthesize setImageCallback;
- (void)createFromPhotoLib:(NSString*)photoName
{
    dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    dispatch_async(queue, ^(void){
        NSLog(@"### create image ####");
        [self loadPhotoLib];
    });
     
}
- (void)createFromFileName:(NSString*)fileName
{
    dispatch_queue_t queue = dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0);
    dispatch_async(queue, ^(void){
        NSLog(@"### create image ####");
        [self loadImageFile:fileName];
    });
    
}
+ (CGImageRef)createCGImage:(struct ppm*) p
{
    CFDataRef data = CFDataCreateWithBytesNoCopy(NULL, p->col, p->width * p->height * 3, NULL);
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    CGDataProviderRef dataProvider = CGDataProviderCreateWithCFData(data);
    CGImageRef cgImage = CGImageCreate(p->width, p->height, 8, 24, p->width * 3, colorSpace, kCGImageAlphaNoneSkipLast, dataProvider, NULL, TRUE, kCGRenderingIntentDefault);
    CGColorSpaceRelease(colorSpace);
    CGDataProviderRelease(dataProvider);
    return cgImage;
}
- (void)dealloc
{
    [setImageCallback release];
    [super dealloc];
}
- (void)loadPhotoLib
{
    ALAssetsGroupEnumerationResultsBlock getPix =
    ^(ALAsset *result, NSUInteger index, BOOL* stop)
    {
        NSLog(@"## reslult = %@ ###", result);
        if(!result)
            return;
        ALAssetRepresentation* rep = [result defaultRepresentation];
        CGImageRef im = [rep fullResolutionImage];
        [self displayImage:im];
        *stop = YES;
    }   ;
    ALAssetsLibraryGroupsEnumerationResultsBlock getGroups =
    ^(ALAssetsGroup* group, BOOL* stop)
    {
        NSLog(@"## group= %@ ###", group);
        if(!group)
            return;
        NSString* title = [group valueForProperty: ALAssetsGroupPropertyName];
        NSLog(@"title = %@", title);
        [group enumerateAssetsUsingBlock: getPix];
        *stop = YES;
    };
    ALAssetsLibraryAccessFailureBlock oops =
    ^(NSError* error)
    {
        NSLog(@"oops ! %@", [error localizedDescription]);
    };
    ALAssetsLibrary* library = [[ALAssetsLibrary alloc] init];
    NSLog(@"#### library = %@ ###", library);
    //[library enumerateGroupsWithTypes:ALAssetsGroupAlbum usingBlock:getGroups faulureBlock:oops];
    [library enumerateGroupsWithTypes:ALAssetsGroupAlbum usingBlock:getGroups failureBlock:oops];
    [library release];
}
- (void)setPainterProperty:(struct PainterProperty)p
{
    painterProperty = p;
}
- (struct PainterProperty)painterProperty
{
    return painterProperty;
}
- (id)init
{
    self = [super init];
    if(self)
    {
        setImageCallback = nil;
        initPainterProperty(&painterProperty);
        setDefaultPcvals();
        pcvals.size_first = 47;
        pcvals.size_last = 151;
        pcvals.size_num = 8;
        pcvals.size_type = 0;
        pcvals.orient_num = 6;
        pcvals.orient_first = 35;
        pcvals.orient_last = 96;
        pcvals.orient_type = 4;
        pcvals.general_background_type = BG_TYPE_FROM_PAPER;
        pcvals.general_dark_edge = 0.0f;
        pcvals.general_shadow_darkness = 0.0f;
        pcvals.general_shadow_depth = 0.0f;
        pcvals.general_shadow_blur = 0.0f;
    }
    return self;
}
@end
