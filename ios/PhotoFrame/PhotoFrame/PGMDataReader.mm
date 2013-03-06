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
#import "PhotoFrameAppDelegate.h"
#import "SEImageAsyncLoader.h"
#import "SE3DPreview.h"
#import "SS_Scene.h"
#import "SESystemConfig.h"
#import <list>
#import "SS_Model.h"
#import "SEUtil.h"
#import "SS_OpenGL.h"
//#import <OpenGLES/ES1/gl.h>
//#import <OpenGLES/ES1/glext.h>
#import <OpenGLES/ES2/gl.h>
#import <OpenGLES/ES2/glext.h>
/*
static void checkGLError()
{
    
    GLenum error = glGetError();
    if(error != GL_NO_ERROR)
    {
        LOGI("### gl error = %d ####\n", error);
        SE_ASSERT(0);
    }
    
}
*/
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
        if(clipRectList)
        {
            [imageView setClipRectList:clipRectList count:size];
            for(int i = 0 ; i < size ; i++)
            {
                [imageView setNeedsDisplayInRect:clipRectList[i]->clipRect];
            }
        }
        else 
        {
            [imageView setNeedsDisplay];
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
    bool precise;
    std::list<BrushPiece> brushList;
    SS_BrushList()
    {
        precise = false;
    }
};
SS_BrushList* SS_BrushListCreate(bool p)
{
    SS_BrushList* bl = new SS_BrushList;
    bl->precise = p;
    return bl;
}
void SS_BrushListRelease(SS_BrushList* brushList)
{
    if(brushList != NULL)
    {
        brushList->brushList.clear();
        delete brushList;
    }
}
void SS_AddBrushPiece(SS_BrushList* brushList, BrushPiece bp)
{
    if(brushList)
    {
        brushList->brushList.push_back(bp);
    }
}
void SS_ClearBrushList(SS_BrushList* brushList)
{
    if(brushList)
    {
        
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
        r.x = it->p1.x;
        r.y = it->p1.y;
        //r.w = it->w;
        //r.h = it->h;
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
                           //apply_brush_area(&bp.data, NULL, &gBackground,NULL, bp.x, bp.y, bp.r, bp.g, bp.b, 0);
                           //ppm_kill(&bp.data);
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
class SS_AtomicCounter
{
public:
    SS_AtomicCounter(int v = 1)
    {
        lock = [[NSLock alloc] init];
        mValue = v;
    }
    ~SS_AtomicCounter()
    {
        [lock release];
    }
    void setValue(int v)
    {
        [lock lock];
        mValue = v;
        [lock unlock];
    }
    int getValue()
    {
        int v = 0;
        [lock lock];
        v = mValue;
        [lock unlock];
        return v;
    }
private:
    NSLock* lock;
    volatile int mValue;
};
SS_AtomicCounter* SS_CreateAtomicConter()
{
    return new SS_AtomicCounter;
}
void SS_SetAtomicCounterValue(SS_AtomicCounter* c, int v)
{
    c->setValue(v);
}
int SS_GetAtomicCounterValue(SS_AtomicCounter* c)
{
    return c->getValue();
}
void SS_ReleaseAtomicCounter(SS_AtomicCounter* c)
{
    delete c;
}
///////////////
class SS_BrushListPool
{
public:
    struct PassBrush
    {
        int pass;
        ppm_t brush;
        PassBrush()
        {
            pass = -1;
            brush.col = NULL;
            brush.width = 0;
            brush.height = 0;
        }
        PassBrush(ppm_t brush, int pass)
        {
            this->brush = brush;
            this->pass = pass;
        }
    };
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
    bool addBrush(ppm_t brush, int pass)
    {
        [lock lock];
        std::list<PassBrush>::iterator it;
        bool found = false;
        for(it = brushes.begin(); it != brushes.end(); it++)
        {
            //assert(it->pass == pass);
            if(it->brush.col == brush.col && it->pass == pass)
            {
                found = true;
                break;
            }
        }
        if(!found )
        {
            brushes.push_back(PassBrush(brush, pass));
        }
        [lock unlock];
        return true;
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
        bool ret = false;
        [lock lock];
        ret = brushListPool.empty();
        [lock unlock];
        return ret;
    }
    class _CompareFun
    {
    public:
        int pass;
        bool operator() (PassBrush& b)
        {
            if(b.pass == pass)
                return true;
            else {
                return false;
            }
        }
    };
    void clearDrawingBrushes(int pass = -1)
    {
        [lock lock];
        bool hasOtherPassBrush = false;
        for(std::list<PassBrush>::iterator itBrush = brushes.begin() ; 
            itBrush != brushes.end(); itBrush++)
        {
            if(pass == -1)
            {
                ppm_kill(&itBrush->brush);
            }
            else
            {
                if(pass == itBrush->pass)
                {
                    ppm_kill(&itBrush->brush);
                }
                else 
                {
                    hasOtherPassBrush = true;
                }
            }
        }
        if(hasOtherPassBrush == false)
        {
            brushes.clear();
        }
        else
        {
            _CompareFun ff;
            ff.pass = pass;
            brushes.remove_if(ff);
        }
        [lock unlock];
    }
    
    void clear()
    {
        [lock lock];
        std::list<SS_BrushList*>::iterator it;
        for(it = brushListPool.begin() ; it != brushListPool.end() ; it++)
        {
            SS_BrushList* bl = *it;
            std::list<BrushPiece>::iterator bpIt;
            for(bpIt = bl->brushList.begin() ; bpIt != bl->brushList.end() ; bpIt++)
            {
            }
            bl->brushList.clear();
            delete bl;
        }
        for(std::list<PassBrush>::iterator itBrush = brushes.begin() ; 
            itBrush != brushes.end(); itBrush++)
        {
            ppm_kill(&itBrush->brush);
        }
        brushes.clear();
        brushListPool.clear();
        [lock unlock];
    }
private:
    NSLock* lock;
    std::list<SS_BrushList*> brushListPool;
    std::list<PassBrush> brushes;
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
bool SS_AddBrush(SS_BrushListPool* blp, ppm_t brush, int pass)
{
    return blp->addBrush(brush, pass);
}
SS_BrushList* SS_GetNextBrushList(SS_BrushListPool* blp)
{
    return blp->getBrushList();
}
void SS_ClearBrushListPool(SS_BrushListPool* blp)
{
    if(blp)
    {
        blp->clear();
    }
}
void SS_ClearDrawingBrushes(SS_BrushListPool* blp, int pass)
{
    blp->clearDrawingBrushes(pass);
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
    if(pp == NULL)
        return;
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
SS_AtomicCounter* SS_GetCurrentStatusPoint()
{
    PainterManager* pm = [PainterManager painterManager];
    return (SS_AtomicCounter*)[pm currentStatusPoint];
}
////////////

class SS_Canvas
{
public:
    SS_Canvas()
    {
        canvas.width = 1024;
        canvas.height = 768;
        //UIImage* image = [UIImage imageNamed:@"background_001.jpg"];
        //CGImageRef imageRef = [image CGImage];
        canvas.col = (unsigned char* )g_malloc(canvas.width * canvas.height * 3);
        memset(canvas.col, 255, canvas.width * canvas.height * 3);
        originMap.width = 0;
        originMap.height = 0;
        originMap.col = NULL;
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
    void setOriginMap(ppm_t* o)
    {
        ppm_copy(o, &originMap);
    }
    void clearColormap()
    {
        memset(colorMap, 0, sizeof(char) * 1024 * 768);
    }
    void drawCanvas(SS_BrushList* bl)
    {
        bool bGetLastPiece = false;
        [lock lock];
        if(!ppm_empty(&canvas))
        {
            std::list<BrushPiece>::iterator it;
            size_t i = 0;
            size_t count = bl->brushList.size();
            if(count > 0)
            {
                MyClipRect** rectArray = NULL;
                //if(bl->precise)
                //{
                //}
                //else
                {
                    rectArray = (MyClipRect**)malloc(sizeof(MyClipRect*) * bl->brushList.size());
                    for(i = 0 ;i < count ; i++)
                    {
                        rectArray[i] = [[MyClipRect alloc] init];
                        [rectArray[i] autorelease];
                    }
                }
                i = 0;
                bool drawFinished = false;
                for(it = bl->brushList.begin() ; it != bl->brushList.end() ; it++)
                {
                    BrushPiece bp = *it;
                    if(bp.last_piece == 1)
                    {
                        drawFinished = true;
                        bGetLastPiece = true;
                        break;
                    }
                    else
                    {
                        //PainterManager* pm = [PainterManager painterManager];
                        //pm.mStageDrawFinished = NO;
                    }
                    if(bp.precise)
                    {
                        rectArray[i]->clipRect.origin.x = bp.p2.startx;
                        rectArray[i]->clipRect.origin.y = bp.p2.starty;
                        rectArray[i]->clipRect.size.width = bp.p2.endx - bp.p2.startx;
                        rectArray[i]->clipRect.size.height = bp.p2.endy - bp.p2.starty;
                        for(int y = bp.p2.starty ; y < bp.p2.endy ; y++)
                        {
                            for(int x = bp.p2.startx ; x < bp.p2.endx ; x++)
                            {
                                /*
                                if(x > 768 && x >= 0 && x < 1024 && y >= 0 && y < 768)
                                {
                                    LOGI("x = %d, colorMap[] = %d\n", x, colorMap[y][x]);
                                    
                                }
                                */
                                if(x >= 0 && x < 1024 && y >= 0 && y < 768 && colorMap[y][x] == 0)
                                {
                                    guchar* data = originMap.col + y * originMap.width * 3 + x * 3;
                                    guchar* dstData = canvas.col + y * canvas.width * 3 + x * 3;
                                    int r = data[0];
                                    int g = data[1];
                                    int b = data[2];
                                    //NSLog(@"## dst brush rgb = %d, %d, %d ##", r, g, b);
                                    int r1 = dstData[0];
                                    int g1 = dstData[1];
                                    int b1 = dstData[2];
                                    //NSLog(@"r = %d, g = %d, b = %d", r, g , b);
                                    //NSLog(@"r1 = %d, g1 = %d, b1 = %d", r1, g1, b1);
                                    dstData[0] = r * 0.3 + r1 * 0.7;
                                    dstData[1] = g * 0.3 + g1 * 0.7;
                                    dstData[2] = b * 0.3 + b1 * 0.7;
                                    colorMap[y][x] = 1;
                                    //NSLog(@"dst = %d, %d, %d", dstData[0], dstData[1], dstData[2]);
                                }
                            }
                        }

                        //NSLog(@"brush : %d, %d, %d, %d", bp.startx, bp.starty, bp.endx - bp.startx, bp.endy - bp.starty);
                        /*
                        std::vector<BrushPiece::Point>::iterator itPoint;
                        for(itPoint = bp.pointVector.begin() ; itPoint != bp.pointVector.end() ; itPoint++)
                        {
                            BrushPiece::Point p = *itPoint;
                            guchar* dstData = canvas.col + p.y * canvas.width * 3 + p.x * 3;
                            //NSLog(@"p = %d, %d, %d, %d, %d", p.x, p.y, p.r, p.g, p.b);
                            dstData[0] = p.r * 0.5 + dstData[0] * 0.5;
                            dstData[1] = p.g * 0.5 + dstData[1] * 0.5;
                            dstData[2] = p.b * 0.5 + dstData[2] * 0.5;
                        }
                         */
                    }
                    else 
                    {
                        PainterManager* pm = [PainterManager painterManager];
                        int transparent = pm.mBrushTransparent;
                        //NSLog(@"## dst brush rgb = %d, %d, %d ##", bp.r, bp.g, bp.b);
                        //NSLog(@"## dst brush pos = %d, %d ##", bp.x, bp.y);
                        if(bp.p1.destData.col != NULL)
                        {
                            apply_brush_area(&bp.p1.destData, NULL, &canvas, NULL, bp.p1.x, bp.p1.y, bp.p1.r, bp.p1.g, bp.p1.b, transparent);
                        }
                        rectArray[i]->clipRect.origin.x = bp.p1.x;
                        rectArray[i]->clipRect.origin.y = bp.p1.y;
                        rectArray[i]->clipRect.size.width = bp.p1.destData.width;
                        rectArray[i]->clipRect.size.height = bp.p1.destData.height;
                        //ppm_kill(&bp.destData);
                    }
                    i++;
                
                }
                [PGMDataReader setToImageView:canvas withClipRect: (const MyClipRect**)rectArray count:count];
                free(rectArray);
            }
            SS_BrushListRelease(bl);
            
        }
        [lock unlock];
        if(bGetLastPiece == true)
        {
            PainterManager* pm = [PainterManager painterManager];
            NSLog(@"## get last brush piece ##");
            [pm drawFinished];
        }
    }
    void drawImage(void* pImageRef, SS_MyRect r)
    {
        CGImageRef imageRef = (CGImageRef)pImageRef;
        CGRect frame = CGRectMake(r.x, r.y, r.w, r.h);
        CGDataProviderRef dataProviderRef = CGImageGetDataProvider(imageRef);
        //size_t width = CGImageGetWidth(imageRef);
        //size_t height = CGImageGetHeight(imageRef);
        size_t bitsPerPixel = CGImageGetBitsPerPixel(imageRef);
        bitsPerPixel /= 8;
        CFDataRef dataRef = CGDataProviderCopyData(dataProviderRef);
        const UInt8* bits = CFDataGetBytePtr(dataRef);
        size_t bytesPerRow = CGImageGetBytesPerRow(imageRef);
        for(int i = frame.origin.y , j = frame.origin.y ; i < (frame.origin.y + frame.size.height) && j < (frame.origin.y + frame.size.height) ; i++, j++)
        {
            const UInt8* src = bits + i * bytesPerRow;
            unsigned char* dst = canvas.col + j * canvas.width * 3;
            for(int m = frame.origin.x , n = frame.origin.x ; m < (frame.origin.x + frame.size.width) && n < (frame.origin.x + frame.size.width) ; m++, n++)
            {
                const UInt8* srcBits = src + m * bitsPerPixel;
                unsigned char* dstBits = dst + n * 3;
                dstBits[0] = srcBits[0];
                dstBits[1] = srcBits[1];
                dstBits[2] = srcBits[2];
            }
            
        }
        CFRelease(dataRef);
    }
private:
    NSLock* lock;
    ppm_t canvas;
    ppm_t originMap;
    char colorMap[768][1024];
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
void SS_CanvasDrawImage(SS_Canvas* canvas, void* pImageRef, SS_MyRect r)
{
    canvas->drawImage(pImageRef, r);
}

void SS_SetCanvasBackground(SS_Canvas* canvas, ppm_t background)
{
    canvas->setCanvasCopy(background);
}
void SS_SetCanvasOriginMap(SS_Canvas* canvas, ppm_t* o)
{
    canvas->setOriginMap(o);
    canvas->clearColormap();
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

void SS_AddLog(const char* fmt, ...)
{
    va_list ap;
    char buf[4096];
    va_start(ap, fmt);
    memset(buf, 0, 4096);
    vsnprintf(buf, 4096, fmt, ap);
    buf[4096 - 1] = '\0';
    va_end(ap);

    NSString* str = [NSString stringWithCString:buf encoding:NSASCIIStringEncoding];
    [str retain];
    dispatch_queue_t mainQueue = dispatch_get_main_queue();
    dispatch_async(mainQueue, ^{
        PainterManager* pm =[PainterManager painterManager];
        [pm addLog:str];
        [str release];
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
    PainterManager* pm = [PainterManager painterManager];
    NSArray* brushes = [pm currentBrushSet];
    NSUInteger i;
    outBrushSet->brush.resize(brushes.count);
    for(i = 0 ; i < [brushes count] ; i++)
    {
        NSString* str = [brushes objectAtIndex:i];
        const char* b = [str cStringUsingEncoding:NSASCIIStringEncoding];
        outBrushSet->brush[i] = b;
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
        /*
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        checkGLError();
        */
        
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        checkGLError();
        
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        checkGLError();
        
        //glTexImage2D(GL_TEXTURE_2D, 0, internal, img->wide, img->high, 0, format, GL_UNSIGNED_BYTE, pixels);
        LOGI("## load texture width = %d, height = %d ##\n", img->wide, img->high);
        glTexImage2D(GL_TEXTURE_2D, 0, internal, img->wide, img->high, 0, format, GL_UNSIGNED_BYTE, tmpBuffer);
        checkGLError();
        
        //glHint(GL_GENERATE_MIPMAP_HINT, GL_NICEST);
        //checkGLError();
        glGenerateMipmap(GL_TEXTURE_2D);
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
void loadGrayTexture(const char* texturename, SE_Texture* texture)
{
    if(texture->texture > 0)
        return;
    UIImage* image = [UIImage imageNamed:[NSString stringWithUTF8String:texturename]];
    CGImageRef CGImage = image.CGImage;
    if (CGImage == NULL)
        return;
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
    GLuint destComponent = 1;
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
        
        GLuint dstBytes = POTWide * destComponent;
        tmpBuffer = (GLubyte *)malloc(dstBytes * POTHigh);
        SE_ASSERT(dstBytes == (rowBytes / 4));
        for(int i = POTHigh - 1 ; i >= 0; i--)
        {
            GLubyte* srcData = &pixels[i * rowBytes];
            GLubyte* dstData = &tmpBuffer[((POTHigh - 1) - i) * dstBytes];
            for(int j = 0 ; j < POTWide ; j++)
            {
                //memcpy(dstData, srcData, rowBytes);
                dstData[j] = srcData[4 * j + 2];
            }
        }
        
    }
    
    if (img->wide <= renderer.maxTextureSize && img->high <= renderer.maxTextureSize)
    {
        glGenTextures(1, &texID);
        checkGLError();
        glBindTexture(GL_TEXTURE_2D, texID);
        checkGLError();        
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        checkGLError();
        
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        checkGLError();
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        checkGLError();
        
        //glTexImage2D(GL_TEXTURE_2D, 0, internal, img->wide, img->high, 0, format, GL_UNSIGNED_BYTE, pixels);
        LOGI("## load texture width = %d, height = %d ##\n", img->wide, img->high);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_LUMINANCE, img->wide, img->high, 0, GL_LUMINANCE, GL_UNSIGNED_BYTE, tmpBuffer);
        checkGLError();
        
        //glHint(GL_GENERATE_MIPMAP_HINT, GL_NICEST);
        //checkGLError();
        glGenerateMipmap(GL_TEXTURE_2D);
        checkGLError();
        
    }
    
    if (temp) 
        free(temp);
    if(tmpBuffer) free(tmpBuffer);
    CFRelease(data);
    //CGImageRelease(CGImage);
    img->texID = texID;   
    texture->texture = texID;
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
void SS_SetLoadingStage(int stage)
{

    //[loadingView setStage:(LOADING_STAGE_TYPE)stage];
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    [viewNav performSelectorOnMainThread:@selector(setDrawingLoaingStage:) withObject:[NSNumber numberWithInt:stage] waitUntilDone:NO];
}
int SS_GetLoadingStage()
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    return [viewNav getDrawingLoadingStage]; 
}
void SS_SetFirstNum(int n)
{
    PainterManager* pm = [PainterManager painterManager];
    [pm performSelectorOnMainThread:@selector(setFirstNum:) withObject:[NSNumber numberWithInt:n] waitUntilDone:NO];
}
void SS_SetSecondNum(int n)
{
    PainterManager* pm = [PainterManager painterManager];
    [pm performSelectorOnMainThread:@selector(setSecondNum:) withObject:[NSNumber numberWithInt:n] waitUntilDone:NO];
}
void SS_ShowLoadingView()
{
    PainterManager* pm = [PainterManager painterManager];
    [pm performSelectorOnMainThread:@selector(showLoadingView:) withObject:nil waitUntilDone:NO];
    
}
void SS_HideLoadingView()
{
    PainterManager* pm = [PainterManager painterManager];
    [pm performSelectorOnMainThread:@selector(hideLoadingView:) withObject:nil waitUntilDone:NO];

}
void SS_ComputeReady()
{
    PainterManager* pm = [PainterManager painterManager];
    [pm performSelectorOnMainThread:@selector(computeReady:) withObject:nil waitUntilDone:NO];
}
SS_OutputVertexData* SS_CreateOutputVerteData()
{
    return NULL;
}
void SS_GetImageAcualSize(float width, float height, int orientation, float* outWidth, float* outHeight)
{
    [SEUtil calculateImageActualSizeWithWidth:width height:height orientation:(UIImageOrientation)orientation :outWidth :outHeight];
}
bool SS_IsDefaultSelectedImage(const std::string& name)
{
    NSString* str = [NSString stringWithCString:name.c_str() encoding:NSASCIIStringEncoding];
    return [SESystemConfig isDefaultSelectedImageURL:str] == YES;
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
struct V2
{
    float x;
    float y;
    V2()
    {
        x = 0;
        y = 0;
    }
    V2(float x, float y)
    {
        this->x = x;
        this->y = y;
    }
};
@interface SETextureLoader : SEImageAsyncLoadHandler
{
    UIImage* currentImage;
    NSString* textureID;
    NSString* imageName;
    int orientation;
    
    SS_ModelManager* modelManager;
    BOOL canCreateTexture;
    id mTarget;
    SEL mAction;
    //
    unsigned char* data;
    int width, height;
    V2 uv[4];
    V2 uvMirror[4];
    BOOL bFullSize;
}
@property (nonatomic, assign) BOOL canCreateTexture;
@property (nonatomic, assign) BOOL bFullSize;
@property (nonatomic, retain) UIImage* currentImage;
@property (nonatomic, retain) NSString* textureID;
@property (nonatomic, retain) NSString* imageName;
@property (nonatomic, assign) int orientation;
@property (nonatomic, assign) SS_ModelManager* modelManager;
- (void) setHandlerAfterImageLoad: (id)target action: (SEL)action;
@end

@implementation SETextureLoader
@synthesize bFullSize;
@synthesize modelManager;
@synthesize currentImage;
@synthesize textureID;
@synthesize imageName;
@synthesize orientation;
@synthesize canCreateTexture;
- (void) setHandlerAfterImageLoad: (id)target action: (SEL)action
{
    mTarget = target;
    mAction = action;
}
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
- (int) findHighBit: (int)v
{
    unsigned int bit = 0;
    while(v > 1)
    {
        bit++;
        v >>= 1;
    }
    return bit;
}
- (int) higherPower2:(int) v
{
    if([self isPower2:v])
        return v;
    return 1 << ([self findHighBit:v] + 1);
}

-(BOOL) isPower2:(int) v
{
    if(v >= 0)
        return (v & (v - 1)) == 0;
    else
        return 0;
}

- (void) createTexture: (UIImage*)image
{
    bool needSmallImage = false;
    if(image == nil)
    {
        image = [UIImage imageNamed:@"picturenotfound.png"];
        needSmallImage = true;
    }
    CGImageRef imageRef = [image CGImage];
    UIImageOrientation tmpOrientation = image.imageOrientation;
    int maxWidth = 1024;
    int maxHeight = 1024;
    if(bFullSize == false || needSmallImage)
    {
        maxWidth = 256;
        maxHeight = 256;
    }
    int srcWidth = CGImageGetWidth(imageRef);
    int srcHeight = CGImageGetHeight(imageRef);
    NSLog(@"srcWidth = %d, srcHeight = %d", srcWidth, srcHeight);
    //for test
    if(srcWidth == 800 && srcHeight == 500)
    {
        NSLog(@"test image");
    }
    //end
    CGSize dstS = CGSizeMake(maxWidth, maxHeight);
    CGSize srcS = CGSizeMake(CGImageGetWidth(imageRef), CGImageGetHeight(imageRef));
    if(srcWidth <= maxWidth)
        dstS.width = [self higherPower2:srcWidth];
    if(srcHeight <= maxHeight)
        dstS.height = [self higherPower2:srcHeight];
    
    CGSize origSize = dstS;
    CGSize fitSize = [SEUtil computeFitSize:srcS toDst:origSize];
    NSLog(@"fit size = %f, %f", fitSize.width, fitSize.height);
    assert(fitSize.width <= dstS.width && fitSize.height <= dstS.height);
    CGContextRef context = MyCreateBitmapContext(dstS.width, dstS.height);
    CGRect rect;
    rect = CGRectMake((dstS.width - fitSize.width) / 2, (dstS.height - fitSize.height) / 2, fitSize.width, fitSize.height);
    /*
    if(fitSize.width > dstS.width || fitSize.height > dstS.height)
    {
        //CGRectMake((dstS.width - fitSize.width) / 2, (dstS.height - fitSize.height) / 2), );
        rect = CGRectMake((dstS.width - fitSize.width) / 2, (dstS.height - fitSize.height) / 2, fitSize.width, fitSize.height);
    }
    else 
    {
        rect = CGRectMake((dstS.width - fitSize.width) / 2, (dstS.height - fitSize.height) / 2, fitSize.width, fitSize.height);
    }
     */
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
    float startx = ((dstS.width - fitSize.width) / 2) / dstS.width;
    float starty = ((dstS.height - fitSize.height)  / 2) / dstS.height;
    V2 v0(startx, starty);
    V2 v1(1 - startx, starty);
    V2 v2(1 - startx, 1 - starty);
    V2 v3(startx, 1 - starty);
    //float actualWidth = image.size.width;
    //float actualHeight = image.size.height;
    //BOOL bHorizon = actualWidth >= actualHeight;
    BOOL bHorizon = orientation == SE_Scene::PHOTOH;
    if(bHorizon)
    {
        switch (tmpOrientation) 
        {
            case UIImageOrientationUp:
            {
                NSLog(@"UIImageOrientationUp");
                //0
                uv[0] = v3;
                //1
                uv[1] = v0;
                //2
                uv[2] = v1;
                //3
                uv[3] = v2;
                
                //0
                uvMirror[0] = v2;
                
                //1
                uvMirror[1] = v1;
                
                
                //2
                uvMirror[2] = v0;
                
                
                //3
                uvMirror[3] = v3;
            }
                break;
            case UIImageOrientationDown:
            {
                NSLog(@"UIImageOrientationDown");
                //0
                uv[0] = v1;
                
                //1
                uv[1] = v2;
                
                //2
                uv[2] = v3;

                //3
                uv[3] = v0;
                
                //0
                uvMirror[0] = v0;
                //1
                uvMirror[1] = v3;

                //2
                uvMirror[2] = v2;

                //3
                uvMirror[3] = v1;

            }
                break;
            case UIImageOrientationLeft:
            {
                NSLog(@"UIImageOrientationLeft");

                uv[0] = v2;
                
                uv[1] = v3;
                
                uv[2] = v0;
                
                uv[3] = v1;
                
                uvMirror[0] = v1;
                
                uvMirror[1] = v0;
                
                uvMirror[2] = v3;
                
                uvMirror[3] = v2;
                
            }
                break;
            case UIImageOrientationRight:
            {
                NSLog(@"UIImageOrientationRight");
                uv[0] = v0;
                uv[1] = v1;
                uv[2] = v2;
                uv[3] = v3;
                
                uvMirror[0] = v3;
                uvMirror[1] = v2;
                uvMirror[2] = v1;
                uvMirror[3] = v0;
            }
                break;
            case UIImageOrientationUpMirrored:
            {
                NSLog(@"UIImageOrientationUpMirrored");
                uv[0] = v2;
                uv[1] = v1;
                uv[2] = v0;
                uv[3] = v3;
                
                uvMirror[0] = v3;
                uvMirror[1] = v0;
                uvMirror[2] = v1;
                uvMirror[3] = v2;
            } 
                break;
            case UIImageOrientationDownMirrored:
            {
                NSLog(@"UIImageOrientationDownMirrored");
                uv[0] = v0;
                uv[1] = v3;
                uv[2] = v2;
                uv[3] = v1;
                
                uvMirror[0] = v1;
                uvMirror[1] = v2;
                uvMirror[2] = v3;
                uvMirror[3] = v0;
            }
                break;
            case UIImageOrientationLeftMirrored:
            {
                NSLog(@"UIImageOrientationLeftMirrored");

                uv[0] = v3;
                uv[1] = v2;
                uv[2] = v1;
                uv[3] = v0;
                
                uvMirror[0] = v0;
                uvMirror[1] = v1;
                uvMirror[2] = v2;
                uvMirror[3] = v3;
            }
                break;
            case UIImageOrientationRightMirrored:
            {
                NSLog(@"UIImageOrientationRightMirrored");
                uv[0] = v1;
                uv[1] = v0;
                uv[2] = v3;
                uv[3] = v2;
                
                uvMirror[0] = v2;
                uvMirror[1] = v3;
                uvMirror[2] = v0;
                uvMirror[3] = v1;
            }
                break;
            default:
                break;
        }
    }
    else 
    {
        
        switch (tmpOrientation) {
            case UIImageOrientationUp:
            {
                NSLog(@"UIImageOrientationUp");
                //0
                uv[0] = v0;
                //1
                uv[1] = v1;
                //2
                uv[2] = v2;
                //3
                uv[3] = v3;
                
                //0
                uvMirror[0] = v1;
                
                //1
                uvMirror[1] = v0;
                
                
                //2
                uvMirror[2] = v3;
                
                
                //3
                uvMirror[3] = v2;
            }
                break;
            case UIImageOrientationDown:
            {
                NSLog(@"UIImageOrientationDown");
                //0
                uv[0] = v2;
                
                //1
                uv[1] = v3;
                
                //2
                uv[2] = v0;
                
                //3
                uv[3] = v1;
                
                //0
                uvMirror[0] = v3;
                //1
                uvMirror[1] = v2;
                
                //2
                uvMirror[2] = v1;
                
                //3
                uvMirror[3] = v0;
                
            }
                break;
            case UIImageOrientationLeft:
            {
                NSLog(@"UIImageOrientationLeft");
                uv[0] = v3;
                
                uv[1] = v0;
                
                uv[2] = v1;
                
                uv[3] = v2;
                
                uvMirror[0] = v0;
                
                uvMirror[1] = v3;
                
                uvMirror[2] = v2;
                
                uvMirror[3] = v1;
                
            }
                break;
            case UIImageOrientationRight:
            {
                NSLog(@"UIImageOrientationRight");
                uv[0] = v1;
                uv[1] = v2;
                uv[2] = v3;
                uv[3] = v0;
                
                uvMirror[0] = v2;
                uvMirror[1] = v1;
                uvMirror[2] = v0;
                uvMirror[3] = v3;
            }
                break;
            case UIImageOrientationUpMirrored:
            {
                NSLog(@"UIImageOrientationUpMirrored");
                uv[0] = v1;
                uv[1] = v0;
                uv[2] = v3;
                uv[3] = v2;
                
                uvMirror[0] = v0;
                uvMirror[1] = v1;
                uvMirror[2] = v2;
                uvMirror[3] = v3;
            } 
                break;
            case UIImageOrientationDownMirrored:
            {
                NSLog(@"UIImageOrientationDownMirrored");
                uv[0] = v3;
                uv[1] = v2;
                uv[2] = v1;
                uv[3] = v0;
                
                uvMirror[0] = v2;
                uvMirror[1] = v3;
                uvMirror[2] = v0;
                uvMirror[3] = v1;
            }
                break;
            case UIImageOrientationLeftMirrored:
            {
                NSLog(@"UIImageOrientationLeftMirrored");
                V2 v0(startx, starty);
                V2 v1(1 - startx, starty);
                V2 v2(1 - startx, 1 - starty);
                V2 v3(startx, 1 - starty);
                uv[0] = v2;
                uv[1] = v1;
                uv[2] = v0;
                uv[3] = v3;
                
                uvMirror[0] = v1;
                uvMirror[1] = v2;
                uvMirror[2] = v3;
                uvMirror[3] = v0;
            }
                break;
            case UIImageOrientationRightMirrored:
            {
                NSLog(@"UIImageOrientationRightMirrored");
                uv[0] = v0;
                uv[1] = v3;
                uv[2] = v2;
                uv[3] = v1;
                
                uvMirror[0] = v3;
                uvMirror[1] = v0;
                uvMirror[2] = v1;
                uvMirror[3] = v2;
            }
                break;
            default:
                break;
        }

    }
}
- (void) preHandleImage
{
    if(self.canCreateTexture)
    {
        [self createTexture:currentImage];
    }
    self.currentImage = nil;
}
- (void) handleImage
{
    if(self.canCreateTexture == NO)
        return;
    void* currentModelManager = [[PainterManager painterManager] detectModelManager];
    if(currentModelManager == nil)
        return;
    if(currentModelManager != modelManager)
        return;
    NSThread* currentThread = [NSThread currentThread];
    NSThread* mainThread = [NSThread mainThread];
    assert(currentThread == mainThread);
    GLuint texID;
    glGenTextures(1, &texID);
    checkGLError();
    glBindTexture(GL_TEXTURE_2D, texID);
    checkGLError();
    // Set filtering parameters appropriate for this application (image processing on screen-aligned quads.)
    // Depending on your needs, you may prefer linear filtering, or mipmap generation.
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    checkGLError();
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    checkGLError();
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, width, height, 0, GL_RGBA, GL_UNSIGNED_BYTE, data);
    checkGLError();
    free(data);
    data = NULL;
    SE_Texture* newTexture = new SE_Texture;
    newTexture->texture = texID;
    newTexture->width = width;
    newTexture->height = height;
    newTexture->loadstate = SE_Texture::TEXTURE_LOADED;
    memcpy(newTexture->uv, uv, sizeof(float) * 8);
    memcpy(newTexture->uvMirror, uvMirror, sizeof(float) * 8);
    const char* textureName = [textureID cStringUsingEncoding:NSASCIIStringEncoding];
    if(bFullSize)
    {
        modelManager->setFullImageTexture(textureName, newTexture);
        int fullTextureCount = modelManager->getFullTextureCount();
        assert(fullTextureCount <= 3);
        NSLog(@"## load texture = %s , width = %d, height = %d ## ", textureName, width, height);
    }
    else
    {
        NSLog(@"## load thumbnail texture = %s , width = %d, height = %d ## ", textureName, width, height);
        modelManager->setTexture(textureName, newTexture);
    }
    [mTarget performSelectorOnMainThread:mAction withObject:nil waitUntilDone:NO];
}
@end
///////////////////////
@interface SETextureListLoader : NSObject
{
    std::vector<SE_TextureLoadInfo> mTextureInfoList;
    int mCurrentTextureInfoIndex;
    SS_ModelManager* modelManager;
    BOOL canCreateTexture;
    BOOL bFullSize;
}
@property (nonatomic, assign) SS_ModelManager* modelManager;
@property (nonatomic, assign) BOOL bFullSize;
@property (nonatomic, assign) BOOL canCreateTexture;
- (void) setTextureLoadInfoList: (std::list<SE_TextureLoadInfo>&) textureLoadList;
- (void) load;
@end
@implementation SETextureListLoader
@synthesize bFullSize;
@synthesize modelManager;
@synthesize canCreateTexture;
- (void) setTextureLoadInfoList: (std::list<SE_TextureLoadInfo>&) textureLoadList
{
    if(textureLoadList.size() > 0)
    {
        mTextureInfoList.resize(textureLoadList.size());
        std::copy(textureLoadList.begin(), textureLoadList.end(), mTextureInfoList.begin());
    }
}
- (void) afterImageLoad
{
    mCurrentTextureInfoIndex++;
    [self load];
}
- (void) load
{
    if(mCurrentTextureInfoIndex < mTextureInfoList.size())
    {
        SETextureLoader* loader = [[SETextureLoader alloc] init];
        SE_TextureLoadInfo textureLoadInfo = mTextureInfoList[mCurrentTextureInfoIndex];
        loader.textureID = [NSString stringWithCString:textureLoadInfo.textureName.c_str() encoding:NSUTF8StringEncoding];
        loader.modelManager = modelManager;
        loader.bFullSize = bFullSize;
        loader.orientation = textureLoadInfo.photoFrameOrientation;
        loader.canCreateTexture = canCreateTexture;
        [loader setHandlerAfterImageLoad:self action:@selector(afterImageLoad)];
        
        ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
        [loader setAssetLibOwn:lib];
        [lib release];
        
        NSString* imageStr = [NSString stringWithCString:textureLoadInfo.pictureName.c_str() encoding:NSUTF8StringEncoding];
        NSURL* url = [NSURL URLWithString:imageStr];
        NSString* date = [NSString stringWithCString:textureLoadInfo.pictureDate.c_str() encoding:NSUTF8StringEncoding];
        if(bFullSize)
        {
            [loader loadCoreDataFullRepresentation:url date:date];
        }
        else
        {
            //[loader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(10, 10)];
            [loader loadImageThumbnailFromCoreData:url date:date];
        }

    }
    else
    {
        [self release];
    }
    
}
@end
///////////////////////
static BOOL canCreateTexture()
{
    SEViewNavigator* viewNav = [PhotoFrameAppDelegate getViewNavigator];
    UIView* rootView = viewNav.mRootView;
    for(UIView* child in rootView.subviews)
    {
        if([child isMemberOfClass:[SE3DPreview class]])
        {
            SE3DPreview* preview = (SE3DPreview*)child;
            if([preview isStartDraw])
                return YES;
            else
                return NO;
        }
    }
    return NO;
}
void SS_LoadImageTextureListAsync(std::list<SE_TextureLoadInfo>& textureLoadInfoList, SS_ModelManager* modelManager, void* viewNav, bool bFullSize)
{
    SETextureListLoader* listLoader = [[SETextureListLoader alloc] init];
    listLoader.modelManager = modelManager;
    listLoader.bFullSize = bFullSize == true;
    listLoader.canCreateTexture = canCreateTexture();
    [listLoader setTextureLoadInfoList:textureLoadInfoList];
    [listLoader load];
}
void SS_LoadThumbnailTextureForImageArray(void* viewNav, SS_ModelManager* modelManager, const std::vector<std::string>& imageNameArray,
                                          const std::vector<std::string>& imageDateArray, const std::vector<int> & orientation)
{
    assert(imageNameArray.size() == imageDateArray.size());
    std::list<SE_TextureLoadInfo> textureLoadInfoList;
    for(int i = 0 ; i < imageNameArray.size() ; i++)
    {
        std::string name = imageNameArray[i];
        std::string date = imageDateArray[i];
        int orient = orientation[i];
        SE_TextureLoadInfo info;
        info.pictureName = name;
        info.pictureDate = date;
        info.photoFrameOrientation = orient;
        info.textureName = name;
        textureLoadInfoList.push_back(info);
    }
    SS_LoadImageTextureListAsync(textureLoadInfoList, modelManager, viewNav, false);
    /*
    for(int i = 0 ; i < imageNameArray.size() ; i++)
    {
        std::string name = imageNameArray[i];
        std::string date = imageDateArray[i];
        int orient = orientation[i];
        SS_LoadImageTextureWithOrientationAsync(name.c_str(), date.c_str(), name.c_str(), modelManager, viewNav, false, orient);
    }
     */
}

void SS_LoadImageTextureAsync(const char* imageName, const char* imageDate, const char* textureID, SS_ModelManager* modelManager, void* viewNav, bool bFullSize, int photoFrameOrientation)
{
    SETextureLoader* loader = [[SETextureLoader alloc] init];
    loader.textureID = [NSString stringWithCString:textureID encoding:NSUTF8StringEncoding];
    loader.modelManager = modelManager;
    loader.bFullSize = bFullSize;
    loader.orientation = photoFrameOrientation;
    loader.canCreateTexture = canCreateTexture();

    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    [loader setAssetLibOwn:lib];
    [lib release];
    
    NSString* imageStr = [NSString stringWithCString:imageName encoding:NSUTF8StringEncoding];
    NSURL* url = [NSURL URLWithString:imageStr];
    NSString* date = [NSString stringWithCString:imageDate encoding:NSUTF8StringEncoding];
    if(bFullSize)
    {
        [loader loadCoreDataFullRepresentation:url date:date];
    }
    else
    {
        //[loader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(10, 10)];
        [loader loadImageThumbnailFromCoreData:url date:date];
    }
    
}
void SS_LoadImageTextureWithOrientationAsync(const char* imageName, const char* imageDate, const char* textureID, SS_ModelManager* modelManager, void* viewNav, bool bFullSize, int orientation)
{
    SETextureLoader* loader = [[SETextureLoader alloc] init];
    loader.textureID = [NSString stringWithCString:textureID encoding:NSUTF8StringEncoding];
    loader.modelManager = modelManager;
    loader.bFullSize = bFullSize;
    loader.orientation = orientation;
    loader.canCreateTexture = canCreateTexture();
    //SEImageAsyncLoader* asyncLoader = [[SEImageAsyncLoader alloc] init];
    //asyncLoader.mViewNav = (SEViewNavigator*)viewNav;
    ALAssetsLibrary* lib = [[ALAssetsLibrary alloc] init];
    [loader setAssetLibOwn:lib];
    [lib release];
    NSString* imageStr = [NSString stringWithCString:imageName encoding:NSUTF8StringEncoding];
    NSURL* url = [NSURL URLWithString:imageStr];
    NSString* date = [NSString stringWithCString:imageDate encoding:NSUTF8StringEncoding];
    if(bFullSize)
    {
        //[asyncLoader loadCoreDataFullRepresentation:url date: date withHandler:loader];
        [loader loadCoreDataFullRepresentation:url date:date];
    }
    else
    {
        //[asyncLoader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(10, 10) withHandler:loader];
        //[loader loadImageThumbnailFromPhotoLib:url size:CGSizeMake(10, 10)];
        [loader loadImageThumbnailFromCoreData:url date:date];
    }
    
}
