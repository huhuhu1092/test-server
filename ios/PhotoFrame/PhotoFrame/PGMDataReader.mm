//
//  PGMDataReader.m
//  TestImageView
//
//  Created by 陈勇 on 11-10-7.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import "PGMDataReader.h"
#import <Foundation/Foundation.h>
#include "ppmtool.h"
#include "gimpressionist.h"
#import "PainterManager.h"
#import "PHImageView.h"
#import "SEViewNavigator.h"
#import "SelectedImage.h"
#include "SS_Model.h"
#import "SEImageAsyncLoader.h"
#import <list>
#import "SS_Model.h"
#import "SEUtil.h"
//#import <OpenGLES/ES1/gl.h>
//#import <OpenGLES/ES1/glext.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>
static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}

struct _PGMRetData
{
    const char* data;
    int len;
};
@interface PGMDataReader : NSObject
- (struct _PGMRetData) read:(NSString*)fileName;
+ (PHImageView*)getImageView;
+ (void) setToImageView: (ppm_t)ppm_image withClipRect:(const MyClipRect**)clipRectList count:(int)size;
+ (UIImage*) createUIImage:(ppm_t) ppm_image;
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
+ (void) setToImageView: (ppm_t)ppm_image withClipRect:(const MyClipRect**)clipRectList count:(int)size
{
    PHImageView* imageView = [PGMDataReader getImageView];
    if(imageView)
    {
        CGImageRef cgImage = [PainterManager createCGImage:ppm_image];
        CFIndex retainCount = CFGetRetainCount(cgImage);
        UIImage* image = [UIImage imageWithCGImage:cgImage];
        retainCount = CFGetRetainCount(cgImage);
        imageView.image = image;
        [imageView setClipRectList:clipRectList count:size];
        for(int i = 0 ; i < size ; i++)
        {
            [imageView setNeedsDisplayInRect:clipRectList[i]->clipRect];
        }
        //[imageView setNeedsDisplay];
        CGImageRelease(cgImage);
        retainCount = CFGetRetainCount(cgImage);
    }

}
- (struct _PGMRetData) read:(NSString*)fileName
{
    NSArray* fileNameArray = [fileName componentsSeparatedByString:@"."];
    assert([fileNameArray count] == 2);
    NSString* s = [fileNameArray objectAtIndex:0];
    NSString* ext = [fileNameArray objectAtIndex:1];
    NSString* filePath = [[NSBundle mainBundle] pathForResource:s ofType:ext];
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
+ (PHImageView*)getImageView
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
+ (UIImage*) createUIImage:(ppm_t) ppm_image
{
    CGImageRef imageRef = [PainterManager createCGImage:ppm_image];
    UIImage* uiImage = [UIImage imageWithCGImage:imageRef];
    CGImageRelease(imageRef);
    return uiImage;
}
@end
////////////
void SS_SaveImage(const char* name)
{
    
}
void SS_UpdateImageView()
{
    PHImageView* imageView = [PGMDataReader getImageView];
    [imageView setNeedsDisplay];
}
class SS_BrushList
{
public:
    std::list<BrushPiece> brushList;
};
SS_BrushList* SS_BrushListCreate()
{
    SS_BrushList* bl = new SS_BrushList;
    return bl;
}
void SS_BrushListRelease(SS_BrushList* brushList)
{
    if(brushList != NULL)
        delete brushList;
}
void SS_AddBrushPiece(SS_BrushList* brushList, BrushPiece bp)
{
    if(brushList)
    {
        brushList->brushList.push_back(bp);
    }
}
void SS_GetBrushListRects(SS_BrushList* brushList, SS_MyRect** outRects, int* outCount)
{
    std::list<BrushPiece>::iterator it = brushList->brushList.begin();
    *outRects = (SS_MyRect*)malloc(sizeof(SS_MyRect) * brushList->brushList.size());
    *outCount = brushList->brushList.size();
    int i = 0;
    for(; it != brushList->brushList.end() ; it++)
    {
        SS_MyRect r;
        r.x = it->x;
        r.y = it->y;
        r.w = it->w;
        r.h = it->h;
        (*outRects)[i] = r;
    }
}
void SS_DrawBrushList(SS_BrushList* brushList, int index)
{
    SS_BrushList* bl = brushList;
    double delayInSecond = 1;
    dispatch_time_t delayInNanoSeconds = dispatch_time(DISPATCH_TIME_NOW, delayInSecond * NSEC_PER_SEC);
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^(void)
                   {
                       std::list<BrushPiece>::iterator it;
                       for(it = bl->brushList.begin() ; it != bl->brushList.end() ; it++)
                       {
                           BrushPiece bp = *it;
                           apply_brush_area(&bp.data, NULL, &gBackground,NULL, bp.x, bp.y, bp.r, bp.g, bp.b);
                           ppm_kill(&bp.data);
                       }
                       SS_BrushListRelease(bl);
                       [PGMDataReader setToImageView:gBackground withClipRect:nil count:0];
                   });

    /*
    dispatch_after(delayInNanoSeconds, mainQueue, ^(void)
                   {
                       std::list<BrushPiece>::iterator it;
                       for(it = bl->brushList.begin() ; it != bl->brushList.end() ; it++)
                       {
                           BrushPiece bp = *it;
                           apply_brush_area(&bp.data, NULL, &gBackground,NULL, bp.x, bp.y, bp.r, bp.g, bp.b);
                           ppm_kill(&bp.data);
                       }
                       SS_BrushListRelease(bl);
                       [PGMDataReader setToImageView:gBackground];
                   });
    */
}
/////////////
class SS_BrushListPool
{
public:
    SS_BrushListPool()
    {
        lock = [[NSLock alloc] init];
        
    }
    ~SS_BrushListPool()
    {
        [lock release];
    }
    void addBrushList(SS_BrushList* bl)
    {
        [lock lock];
        brushListPool.push_back(bl);
        [lock unlock];
    }
    SS_BrushList* getBrushList()
    {
        SS_BrushList* ret = NULL;
        [lock lock];
        if(!brushListPool.empty())
        {
            ret = brushListPool.front();
            brushListPool.pop_front();
        }
        [lock unlock];
        return ret;
    }
    bool isEmpty()
    {
        return brushListPool.empty();
    }
private:
    NSLock* lock;
    std::list<SS_BrushList*> brushListPool;
};
SS_BrushListPool* SS_BrushListPoolCreate()
{
    return new SS_BrushListPool;
}
void SS_BrushListPoolRelease(SS_BrushListPool* blp)
{
    assert(blp && blp->isEmpty());
    delete blp;
}
void SS_AddBrushList(SS_BrushListPool* blp, SS_BrushList* bl)
{
    blp->addBrushList(bl);
}
SS_BrushList* SS_GetNextBrushList(SS_BrushListPool* blp)
{
    return blp->getBrushList();
}

SS_BrushListPool* SS_GetBrushListPool()
{
    PainterManager* pm = [PainterManager painterManager];
    return (SS_BrushListPool*)[pm currentBrushListPool];
}
//////////
class SS_PausePoint
{
public:
    SS_PausePoint()
    {
        condition = [[NSCondition alloc] init];
        pause = 0;
    }
    ~SS_PausePoint()
    {
        [condition release];
    }
    void setPause(int p)
    {
        [condition lock];
        pause = p;
        if(pause == 0)
        {
            [condition signal];
        }
        [condition unlock];
    }
    void pausePoint()
    {
        [condition lock];
        while(pause == 1)
        {
            [condition wait];
        }
        [condition unlock];
    }
private:
    NSCondition* condition;
    int pause;
};
SS_PausePoint* SS_PausePointCreate()
{
    return new SS_PausePoint;
}
void SS_PausePointRelease(SS_PausePoint* pp)
{
    delete pp;
}
void SS_SetComputationPausePoint(SS_PausePoint* pp)
{
    pp->pausePoint();
}
void SS_Pause(SS_PausePoint* pp)
{
    pp->setPause(1);
}
void SS_NoPause(SS_PausePoint* pp)
{
    pp->setPause(0);
}
SS_PausePoint* SS_GetCurrentPausePoint()
{
    PainterManager* pm = [PainterManager painterManager];
    return (SS_PausePoint*)[pm currentPausePoint];
}
////////////
class SS_Canvas
{
public:
    SS_Canvas()
    {
        canvas.width = 0;
        canvas.height = 0;
        canvas.col = NULL;
        lock = [[NSLock alloc] init];
    }
    ~SS_Canvas()
    {
        [lock release];
    }
    void setCanvasNoCopy(ppm_t c)
    {
        [lock lock];
        ppm_kill(&canvas);
        canvas = c;
        [lock unlock];
    }
    void setCanvasCopy(ppm_t c)
    {
        [lock lock];
        ppm_copy(&c , &canvas);
        [lock unlock];
    }
    void drawCanvas(SS_BrushList* bl)
    {
        [lock lock];
        if(!ppm_empty(&canvas))
        {
            std::list<BrushPiece>::iterator it;
            size_t i = 0;
            size_t count = bl->brushList.size();
            MyClipRect** rectArray = (MyClipRect**)malloc(sizeof(MyClipRect*) * bl->brushList.size());
            for(i = 0 ;i < count ; i++)
            {
                rectArray[i] = [[MyClipRect alloc] init];
                [rectArray[i] autorelease];
            }
            i = 0;
            for(it = bl->brushList.begin() ; it != bl->brushList.end() ; it++)
            {
                BrushPiece bp = *it;
                if(bp.last_piece == 1)
                {
                    PainterManager* pm = [PainterManager painterManager];
                    //[pm nextDisplayStage];
                    [pm drawFinished];
                    break;
                }
                apply_brush_area(&bp.data, NULL, &canvas, NULL, bp.x, bp.y, bp.r, bp.g, bp.b);
                rectArray[i]->clipRect.origin.x = bp.x;
                rectArray[i]->clipRect.origin.y = bp.y;
                rectArray[i]->clipRect.size.width = bp.data.width;
                rectArray[i]->clipRect.size.height = bp.data.height;
                i++;
                ppm_kill(&bp.data);
            }
            SS_BrushListRelease(bl);
            [PGMDataReader setToImageView:canvas withClipRect: (const MyClipRect**)rectArray count:count];
            free(rectArray);
        }
        [lock unlock];
    }
private:
    NSLock* lock;
    ppm_t canvas;
};
SS_Canvas* SS_CanvasCreate()
{
    return new SS_Canvas;     
}
void SS_CanvasRelease(SS_Canvas* canvas)
{
    delete canvas;
}
int SS_DrawCanvas(SS_Canvas* canvas)
{
    SS_BrushListPool* pool = SS_GetBrushListPool();
    SS_BrushList* bl = SS_GetNextBrushList(pool);
    if(bl == NULL)
        return 0;
    PHImageView* imageView = [PGMDataReader getImageView];
    if(imageView)
    {
        [imageView setPoints:nil];
    }
    canvas->drawCanvas(bl);
    return 1;
}
void SS_SetCanvasBackground(SS_Canvas* canvas, ppm_t background)
{
    canvas->setCanvasCopy(background);
}
SS_Canvas* SS_GetCurrentCanvas()
{
    PainterManager* pm = [PainterManager painterManager];
    return (SS_Canvas*)[pm currentBrushCanvas];
}
////////////////////////////////
void SS_GetPgmData(const char* fn, const char** outData, int* outLen)
{
    PGMDataReader* pgmData = [[PGMDataReader alloc] init];
    NSString* str = [NSString stringWithFormat:@"%s", fn];
    struct _PGMRetData ret = [pgmData read:str];
    *outData = ret.data;
    *outLen = ret.len;
    [pgmData release];
}
void SS_SetBackground(ppm_t background)
{
    ppm_t bb = background;
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^(void)
                   {
                       [PGMDataReader setToImageView:bb];
                   });
}
extern void SS_DrawBackgroundImageSync()
{
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_sync(mainQueue, ^(void)
                   {
                       
                       [PGMDataReader setToImageView:gBackground];
                       /*
                       UIImageView* imageView = [PGMDataReader getImageView];
                       if(imageView)
                       {
                           CGImageRef cgImage = [PainterManager createCGImage:gBackground];
                           UIImage* image = [UIImage imageWithCGImage:cgImage];
                           imageView.image = image;
                           CGImageRelease(cgImage);
                       }
                        */
                   });
    
}
void SS_StartBrushPaintInMainQueue(BrushPiece inputbp)
{
    __block BrushPiece bp = inputbp;
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^(void)
                   {
                       ppm_kill(&bp.data);
                       ppm_kill(&bp.alpha);
                       [PGMDataReader setToImageView:gBackground];
                   });
}
void SS_StartBrushPaint()
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
        SS_StartBrushPaint();
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
                       [PGMDataReader setToImageView:gBackground];
                       /*
                       UIImageView* imageView = [PGMDataReader getImageView];
                       if(imageView)
                       {
                           CGImageRef cgImage = [PainterManager createCGImage:gBackground];
                           UIImage* image = [UIImage imageWithCGImage:cgImage];
                           imageView.image = image;
                           CGImageRelease(cgImage);
                           SE_startBrushPaint();
                       }
                        */
                   });
}
int SS_GetDrawingSpeed()
{
    PainterManager* pm = [PainterManager painterManager];
    return [pm currentDrawingSpeed];
}
void SS_GetCurrentPaintSize(int* outWidth, int* outHeight)
{
    PainterManager* pm = [PainterManager painterManager];
    CGSize s = [pm currentPainterImageSize];
    *outWidth = (int)s.width;
    *outHeight = (int)s.height;
}
void SS_GetSettingBrush(SE_BrushSet* outBrushSet)
{
    assert(outBrushSet);
    memset(outBrushSet->brush, 0, 3 * 64);
    PainterManager* pm = [PainterManager painterManager];
    NSArray* brushes = [pm currentBrushSet];
    NSUInteger i;
    assert([brushes count] == 3);
    for(i = 0 ; i < [brushes count] ; i++)
    {
        NSString* str = [brushes objectAtIndex:i];
        const char* b = [str cStringUsingEncoding:NSASCIIStringEncoding];
        strncpy(outBrushSet->brush[i], b, 63);
    }
}
void SS_GetDestSize(int* dstWidth, int* dstHeight)
{
    PainterManager* pm = [PainterManager painterManager];
    *dstWidth = pm.bgWidth;
    *dstHeight = pm.bgHeight;
}
void SS_SaveByPNGFormat(const char* fileName, ppm_t ppm_image)
{
    NSString* fileNameStr = [NSString stringWithFormat:@"%s", fileName];
    UIImage* uiImage = [PGMDataReader createUIImage:ppm_image];
    NSData* imageData = UIImagePNGRepresentation(uiImage);
    NSArray* paths = NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES);
    NSString* docs = [paths objectAtIndex:0];
    NSString* path = [docs stringByAppendingFormat:@"%@%@%@", @"/", fileNameStr, @".png"];
    const char* cpath = [path cStringUsingEncoding:NSASCIIStringEncoding];
    NSError* writeError = nil;
    BOOL ret = [imageData writeToFile:path options:NSDataWritingAtomic error:&writeError];
    if(ret == NO && writeError != nil)
    {
        NSLog(@" Error save image : %@", path);
    }
}
void SS_SaveBrush(const char* prefix, int brushIndex, ppm_t ppm_image)
{
    PainterManager* pm = [PainterManager painterManager];
    if(pm.painterState.bSaveBrush == YES)
    {
        NSString* brushName = [NSString stringWithFormat:@"%sbrush%d", prefix, brushIndex];
        SS_SaveByPNGFormat([brushName cStringUsingEncoding:NSASCIIStringEncoding], ppm_image);
    }
}
SS_ModelManager* SS_GetModelManager()
{
    PainterManager* pm = [PainterManager painterManager];
    return (SS_ModelManager*)[pm modelManager];
}
void SS_DrawInMainThread(SS_MyRect* rectList, int count)
{
    PainterManager* pm = [PainterManager painterManager];
    NSArray* rectArray = [NSArray array];
    for(int i = 0 ; i < count ; i ++)
    {
        SS_MyRect r = rectList[i];
        CGRect rect = CGRectMake(r.x, r.y, r.w, r.h);
        NSValue* v = [NSValue valueWithCGRect:rect];
        rectArray = [rectArray arrayByAddingObject:v];
    }
    [rectArray retain];
    [pm performSelectorOnMainThread:@selector(updateImageView:) withObject:rectArray waitUntilDone:NO];
    free(rectList);
}
/////////
static unsigned int nextPOT(unsigned int x)
{
    x = x - 1;
    x = x | (x >> 1);
    x = x | (x >> 2);
    x = x | (x >> 4);
    x = x | (x >> 8);
    x = x | (x >>16);
    return x + 1;
}
#define MAX_FILTER_RADIUS 2
typedef struct {
    GLuint texID;
    GLsizei wide, high; // Texture dimensions
    GLfloat s, t;       // Texcoords to show full image, taking any POT padding into account
} Image;

enum {
    APPLE_texture_2D_limited_npot,
    IMG_texture_format_BGRA8888,
    NUM_EXTENSIONS
};

// Renderer capabilities that affect this application
typedef struct {
    GLboolean extension[NUM_EXTENSIONS];
    GLint     maxTextureSize;
    bool inited;
} RendererInfo;

static RendererInfo renderer;
static void loadTextureLocal(CGImageRef CGImage, SE_Texture* texture)
{
    GLuint texID = 0, components, x, y;
    GLuint imgWide, imgHigh;      // Real image size
    GLuint rowBytes, rowPixels;   // Image size padded by CGImage
    GLuint POTWide, POTHigh;      // Image size padded to next power of two
    CGBitmapInfo info;            // CGImage component layout info
    CGColorSpaceModel colormodel; // CGImage colormodel (RGB, CMYK, paletted, etc)
    GLenum internal, format;
    GLubyte *pixels, *temp = NULL;
    Image tmpImage;
    Image* img = &tmpImage;
    if(!renderer.inited)
    {
        renderer.extension[APPLE_texture_2D_limited_npot] =
        (0 != strstr((char *)glGetString(GL_EXTENSIONS), "GL_APPLE_texture_2D_limited_npot"));
        checkGLError();
        renderer.extension[IMG_texture_format_BGRA8888] =
        (0 != strstr((char *)glGetString(GL_EXTENSIONS), "GL_IMG_texture_format_BGRA8888"));
        checkGLError();
        glGetIntegerv(GL_MAX_TEXTURE_SIZE, &renderer.maxTextureSize);
        checkGLError();
        renderer.inited = true;
    }
    // Parse CGImage info
    info  = CGImageGetBitmapInfo(CGImage); // CGImage may return pixels in RGBA, BGRA, or ARGB order
    colormodel = CGColorSpaceGetModel(CGImageGetColorSpace(CGImage));
    size_t bpp = CGImageGetBitsPerPixel(CGImage);
    if (bpp < 8 || bpp > 32 || (colormodel != kCGColorSpaceModelMonochrome && colormodel != kCGColorSpaceModelRGB))
    {
        // This loader does not support all possible CGImage types, such as paletted images
        return;
    }
    components = bpp>>3;
    rowBytes   = CGImageGetBytesPerRow(CGImage);    // CGImage may pad rows
    rowPixels  = rowBytes / components;
    imgWide    = CGImageGetWidth(CGImage);
    imgHigh    = CGImageGetHeight(CGImage);
    img->wide  = rowPixels;
    img->high  = imgHigh;
    img->s     = (float)imgWide / rowPixels;
    img->t     = 1.0;
    
    // Choose OpenGL format
    switch(bpp)
    {
        default:
            SE_ASSERT(0 && "Unknown CGImage bpp");
        case 32:
        {
            internal = GL_RGBA;
            switch(info & kCGBitmapAlphaInfoMask)
            {
                case kCGImageAlphaPremultipliedFirst:
                case kCGImageAlphaFirst:
                case kCGImageAlphaNoneSkipFirst:
                    format = GL_BGRA;
                    break;
                default:
                    format = GL_RGBA;
            }
            break;
        }
        case 24:
            internal = format = GL_RGB;
            break;
        case 16:
            internal = format = GL_LUMINANCE_ALPHA;
            break;
        case 8:
            internal = format = GL_LUMINANCE;
            break;
    }
    
    // Get a pointer to the uncompressed image data.
    //
    // This allows access to the original (possibly unpremultiplied) data, but any manipulation
    // (such as scaling) has to be done manually. Contrast this with drawing the image
    // into a CGBitmapContext, which allows scaling, but always forces premultiplication.
    CFDataRef data = CGDataProviderCopyData(CGImageGetDataProvider(CGImage));
    SE_ASSERT(data);
    pixels = (GLubyte *)CFDataGetBytePtr(data);
    SE_ASSERT(pixels);
    
    // If the CGImage component layout isn't compatible with OpenGL, fix it.
    // On the device, CGImage will generally return BGRA or RGBA.
    // On the simulator, CGImage may return ARGB, depending on the file format.
    if (format == GL_BGRA)
    {
        uint32_t *p = (uint32_t *)pixels;
        int i, num = img->wide * img->high;
        
        if ((info & kCGBitmapByteOrderMask) != kCGBitmapByteOrder32Host)
        {
            // Convert from ARGB to BGRA
            for (i = 0; i < num; i++)
                p[i] = (p[i] << 24) | ((p[i] & 0xFF00) << 8) | ((p[i] >> 8) & 0xFF00) | (p[i] >> 24);
        }
        
        // All current iPhoneOS devices support BGRA via an extension.
        if (!renderer.extension[IMG_texture_format_BGRA8888])
        {
            format = GL_RGBA;
            
            // Convert from BGRA to RGBA
            for (i = 0; i < num; i++)
#if __LITTLE_ENDIAN__
                p[i] = ((p[i] >> 16) & 0xFF) | (p[i] & 0xFF00FF00) | ((p[i] & 0xFF) << 16);
#else
            p[i] = ((p[i] & 0xFF00) << 16) | (p[i] & 0xFF00FF) | ((p[i] >> 16) & 0xFF00);
#endif
        }
    }
    
    // Determine if we need to pad this image to a power of two.
    // There are multiple ways to deal with NPOT images on renderers that only support POT:
    // 1) scale down the image to POT size. Loses quality.
    // 2) pad up the image to POT size. Wastes memory.
    // 3) slice the image into multiple POT textures. Requires more rendering logic.
    //
    // We are only dealing with a single image here, and pick 2) for simplicity.
    //
    // If you prefer 1), you can use CoreGraphics to scale the image into a CGBitmapContext.
    POTWide = nextPOT(img->wide);
    POTHigh = nextPOT(img->high);
    
    if (!renderer.extension[APPLE_texture_2D_limited_npot] && (img->wide != POTWide || img->high != POTHigh))
    {
        GLuint dstBytes = POTWide * components;
        GLubyte *temp = (GLubyte *)malloc(dstBytes * POTHigh);
        
        for (y = 0; y < img->high; y++)
            memcpy(&temp[y*dstBytes], &pixels[y*rowBytes], rowBytes);
        
        img->s *= (float)img->wide/POTWide;
        img->t *= (float)img->high/POTHigh;
        img->wide = POTWide;
        img->high = POTHigh;
        pixels = temp;
        rowBytes = dstBytes;
    }
    
    // For filters that sample texel neighborhoods (like blur), we must replicate
    // the edge texels of the original input, to simulate CLAMP_TO_EDGE.
    GLubyte* tmpBuffer = NULL;
    {
        GLuint replicatew = MIN(MAX_FILTER_RADIUS, img->wide-imgWide);
        GLuint replicateh = MIN(MAX_FILTER_RADIUS, img->high-imgHigh);
        GLuint imgRow = imgWide * components;
        
        for (y = 0; y < imgHigh; y++)
            for (x = 0; x < replicatew; x++)
                memcpy(&pixels[y*rowBytes+imgRow+x*components], &pixels[y*rowBytes+imgRow-components], components);
        for (y = imgHigh; y < imgHigh+replicateh; y++)
            memcpy(&pixels[y*rowBytes], &pixels[(imgHigh-1)*rowBytes], imgRow+replicatew*components);
        
        GLuint dstBytes = POTWide * components;
        tmpBuffer = (GLubyte *)malloc(dstBytes * POTHigh);
        SE_ASSERT(dstBytes == rowBytes);
        for(int i = POTHigh - 1 ; i >= 0; i--)
        {
            GLubyte* srcData = &pixels[i * rowBytes];
            GLubyte* dstData = &tmpBuffer[((POTHigh - 1) - i) * rowBytes];
            memcpy(dstData, srcData, rowBytes);
        }
        
    }
    
    if (img->wide <= renderer.maxTextureSize && img->high <= renderer.maxTextureSize)
    {
        glGenTextures(1, &texID);
        checkGLError();
        glBindTexture(GL_TEXTURE_2D, texID);
        checkGLError();
        // Set filtering parameters appropriate for this application (image processing on screen-aligned quads.)
        // Depending on your needs, you may prefer linear filtering, or mipmap generation.

        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        checkGLError();

        /*
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
        checkGLError();
         */
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        checkGLError();
        //glTexImage2D(GL_TEXTURE_2D, 0, internal, img->wide, img->high, 0, format, GL_UNSIGNED_BYTE, pixels);
        LOGI("## load texture width = %d, height = %d ##\n", img->wide, img->high);
        glTexImage2D(GL_TEXTURE_2D, 0, internal, img->wide, img->high, 0, format, GL_UNSIGNED_BYTE, tmpBuffer);
        checkGLError();
        //glGenerateMipmap(GL_TEXTURE_2D);
        checkGLError();
    }
    
    if (temp) free(temp);
    if(tmpBuffer) free(tmpBuffer);
    CFRelease(data);
    //CGImageRelease(CGImage);
    img->texID = texID;   
    texture->texture = texID;   
}
void SS_LoadTextureForImage(void* viewNav, const char* imageName, const char* imageDate,SE_Texture* texture)
{
    if(texture->texture > 0)
        return;
    SEViewNavigator* vn = (SEViewNavigator*)viewNav;
    NSString* urlStr = [NSString stringWithCString:imageName encoding:NSUTF8StringEncoding];
    NSString* urlDate =[NSString stringWithCString:imageDate encoding:NSUTF8StringEncoding];
    //UIImage* uiImage = [vn getImageFromCoreData:urlStr urlDate:urlDate];
    UIImage* uiImage = [vn getThumbnailFromCoreData:urlStr urlDate:urlDate];
    CGImageRef CGImage = uiImage.CGImage;
    if (CGImage == NULL)
    {
        NSLog(@"url = %@, date = %@", urlStr, urlDate);
        assert(CGImage != NULL);
        return;
    }
    loadTextureLocal(CGImage, texture);
}

void loadTexture(const char* texturename, SE_Texture* texture)
{
    if(texture->texture > 0)
        return;
    UIImage* image = [UIImage imageNamed:[NSString stringWithUTF8String:texturename]];
    CGImageRef CGImage = image.CGImage;
    if (CGImage == NULL)
        return;
    loadTextureLocal(CGImage, texture);
}
void createMeshVBO(SE_Mesh* mesh)
{
    if(mesh->vboID == 0)
    {
        glGenBuffers(1, &mesh->vboID);
        GLsizei size = mesh->mFloatSize * sizeof(float);
        glBindBuffer(GL_ARRAY_BUFFER, mesh->vboID);
        glBufferData(GL_ARRAY_BUFFER, size, mesh->mDrawingVertex, GL_STATIC_DRAW);
        
        glGenBuffers(1, &mesh->indexVboID);
        size = mesh->getIndexBufferNum(SE_Mesh::XYZ_UV) * sizeof(unsigned short);
        glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, mesh->indexVboID);
        glBufferData(GL_ELEMENT_ARRAY_BUFFER, size, mesh->getIndexBuffer(SE_Mesh::XYZ_UV), GL_STATIC_DRAW);
    }
    else
        return;
}
void removeVBOFromGL(SE_Mesh* mesh)
{
    /*
    if(mesh->vboID != 0)
    {
        glDeleteBuffers(1, &mesh->vboID);
    }
    mesh->vboID = 0;
     */
}

SS_OutputVertexData* SS_CreateOutputVerteData()
{
    return NULL;
}
void SS_GetImageSize(void* viewNav, const char* imageName, const char* imageDate, int* width, int* height)
{
    SEViewNavigator* vn = (SEViewNavigator*)viewNav;
    NSString* urlStr = [NSString stringWithCString:imageName encoding:NSUTF8StringEncoding];
    NSString* urlDate =[NSString stringWithCString: imageDate encoding: NSUTF8StringEncoding];
    SelectedImage* si = [vn getSelectedImageByUrl:urlStr andDate:urlDate];
    if(si)
    {
        *width = [si.width intValue];
        *height = [si.height intValue];
    }
    /*
    UIImage* image = [vn getImageFromCoreData:urlStr urlDate:urlDate];
    //UIImage* image = [UIImage imageNamed:[NSString stringWithCString:imageName encoding:NSUTF8StringEncoding]];
    CGSize s = image.size;
     */
    /*
    *width = (int)s.width;
    *height = (int)s.height;
     */
}
void SS_OutputVertexData_Output(SS_OutputVertexData* vp)
{}
void SS_OutputVertexData_AddVertex(SS_OutputVertexData* vp, float x, float y , float z)
{}
void SS_OutputVertexData_AddTexVertex(SS_OutputVertexData* vp, float x, float y)
{}
void SS_OutputVertexData_AddFace(SS_OutputVertexData* vp, int v0 , int v1, int v2)
{}
void SS_OutputVertexData_AddTexFace(SS_OutputVertexData* vp, int v0 , int v1, int v2)
{}
@interface SEFullSizeTextureLoader : NSObject<SELoadedImageHandler>
{
    UIImage* currentImage;
    //CGImageRef texture;
    NSString* textureID;
    NSString* imageName;
    SS_ModelManager* modelManager;
    //
    unsigned char* data;
    int width, height;
}
@property (nonatomic, retain) UIImage* currentImage;
@property (nonatomic, retain) NSString* textureID;
@property (nonatomic, retain) NSString* imageName;
@property (nonatomic, assign) SS_ModelManager* modelManager;
@end
@implementation SEFullSizeTextureLoader
@synthesize modelManager;
@synthesize currentImage;
@synthesize textureID;
@synthesize imageName;
- (void) dealloc
{
    [textureID release];
    [imageName release];
    [currentImage release];
    if(data)
        free(data);
    //CGImageRelease(texture);
    [super dealloc];
}
- (void) setImage:(UIImage *)image
{
    self.currentImage = image;
}
- (void) createTexture: (UIImage*)image
{
    CGImageRef imageRef = [image CGImage];
    CGSize dstS = CGSizeMake(1024, 1024);
    CGSize srcS = CGSizeMake(CGImageGetWidth(imageRef), CGImageGetHeight(imageRef));
    CGSize fitSize = [SEUtil computeFitSize:srcS toDst:dstS];
    CGContextRef context = MyCreateBitmapContext(dstS.width, dstS.height);
    CGRect rect = CGRectMake((dstS.width - fitSize.width) / 2, (dstS.height - fitSize.height) / 2, fitSize.width, fitSize.height);
    CGContextDrawImage(context, rect, imageRef);
    CGImageRef retImage = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    width = CGImageGetWidth(retImage);
    height = CGImageGetHeight(retImage);
    CGDataProviderRef dataProvider = CGImageGetDataProvider(retImage);
    CFDataRef dataRef = CGDataProviderCopyData(dataProvider);
    const uint8_t* tmpBuffer = CFDataGetBytePtr(dataRef);
    data = (unsigned char*)malloc(width * height * 4);
    int j = 0;
    int rowBytes = width * 4;
    for(int i = height - 1 ; i >= 0 ; i--, j++)
    {
        const uint8_t* src = tmpBuffer + i * rowBytes;
        unsigned char* dst = data + j * rowBytes;
        memcpy(dst, src, rowBytes);
    }
    CFRelease(dataRef);
    CGImageRelease(retImage);
}
- (void) preHandleImage
{
    [self createTexture:currentImage];
}
- (void) handleImage
{
    GLuint texID;
    glGenTextures(1, &texID);
    checkGLError();
    glBindTexture(GL_TEXTURE_2D, texID);
    checkGLError();
    // Set filtering parameters appropriate for this application (image processing on screen-aligned quads.)
    // Depending on your needs, you may prefer linear filtering, or mipmap generation.
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    checkGLError();
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
    //CFRelease(dataRef);
    SE_Texture* newTexture = new SE_Texture;
    newTexture->texture = texID;
    newTexture->width = width;
    newTexture->height = height;
    newTexture->loadstate = SE_Texture::TEXTURE_LOADED;
    const char* textureName = [textureID cStringUsingEncoding:NSASCIIStringEncoding];
    modelManager->setTexture(textureName, newTexture);
    assert(modelManager->getFullTextureCount() == 1);
    NSLog(@"## load texture = %s , width = %d, height = %d ## ", textureName, width, height);
}

@end
void SS_LoadFullImageAsync(const char* imageName, const char* imageDate, const char* textureID, SS_ModelManager* modelManager, void* viewNav)
{
    SEFullSizeTextureLoader* loader = [[SEFullSizeTextureLoader alloc] init];
    loader.textureID = [NSString stringWithCString:textureID encoding:NSASCIIStringEncoding];
    loader.modelManager = modelManager;
    SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
    asyncLoader.mViewNav = (SEViewNavigator*)viewNav;
    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    [asyncLoader setAssetLibOwn:lib];
    NSString* imageStr = [NSString stringWithCString:imageName encoding:NSUTF8StringEncoding];
    NSURL* url = [NSURL URLWithString:imageStr];
    NSString* date = [NSString stringWithCString:imageDate encoding:NSUTF8StringEncoding];
    [asyncLoader loadCoreDataFullRepresentation:url date: date withHandler:loader];
    [lib release];
}
