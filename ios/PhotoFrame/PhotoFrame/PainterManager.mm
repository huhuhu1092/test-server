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
#import <list>
#import <algorithm>
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
static PainterManager* sPainterManager;
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
@interface SEPainterManagerLoadPhoto : NSObject<SELoadedImageHandler>
{
    SEImageAsyncLoader* imageLoader;
    UIImage* currentImage;
    PainterManager* pm;
}
@property (nonatomic, assign) PainterManager* pm;
@property (nonatomic, retain) SEImageAsyncLoader* imageLoader;
@end
@implementation SEPainterManagerLoadPhoto
@synthesize pm;
@synthesize imageLoader;
-(void) setImage:(UIImage *)image
{
    [currentImage release];
    currentImage = [image retain];
}
- (void)dealloc
{
    NSUInteger count1 = [imageLoader retainCount];
    NSUInteger count2 = [currentImage retainCount];
    [currentImage release];
    [imageLoader release];

    [super dealloc];
}
- (void) preHandleImage
{}
- (void)handleImage
{
    //// for test
    /*
    CGImageRef im = [currentImage CGImage];
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
    ppm_t edgeD = edgeDetection(&infile);
    CGImageRef newImage = [PainterManager createCGImage:edgeD];
    ppm_kill(&infile);
    UIImage* uiImage = [UIImage imageWithCGImage:newImage];
    CGImageRelease(newImage);
    PHImageView* imageView = [[PainterManager painterManager] getImageView];
    imageView.image = uiImage;
    [imageView setNeedsDisplay];
     */
    ////
    
    const float pi = 3.1415926;
    CGImageRef imageRef = [currentImage CGImage];
    int width = CGImageGetWidth(imageRef);
    int height = CGImageGetHeight(imageRef);
    UIImageOrientation orientation = currentImage.imageOrientation;
    CGSize dsts = [pm currentPainterImageSize];
    CGContextRef context = MyCreateBitmapContext(dsts.width, dsts.height);
    pm.mCurrentImageOrientation = 0;
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
    CGImageRef theCGImage = CGBitmapContextCreateImage(context);
    CGContextRelease(context);
    UIImage* retImage = [UIImage imageWithCGImage:theCGImage];
    CGImageRelease(theCGImage);
    retImage = [retImage retain];
    [pm performSelectorInBackground:@selector(displayUIImageWithThread:) withObject:retImage];
     
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
        //modelManager->loadModel("MyFrame.cbf");
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
@synthesize  orient_first;
@synthesize  orient_last;
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
    {"比划密度", P_BRUSH_DENSITY},
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
        currentBrushSet = [[NSArray alloc] initWithObjects:@"paintbrush.pgm", @"defaultbrush.pgm", @"paintbrush02.pgm", nil];
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
- (void) saveCurrentPass;
- (PHImageView*)getImageView;
- (void) drawSignature;
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
    
    CFRelease(imageData);
    //CGImageRelease(im);
    
    NSLog(@"######################### get infile ####################\n");
    NSLog(@"## infile width = %d, height = %d ###\n", infile.width, infile.height);
    NSThread* mainThread = [NSThread mainThread];
    NSThread* currentThread = [NSThread currentThread];
    assert(currentThread != mainThread);
    startTime();
    RepaintData rd;
    PainterParam* myCurrentParam = [self currentParam];
    rd.calculateOnEdge = [myCurrentParam getSid:3] == 2;
    NSLog(@"### calculate edge = %d ###", rd.calculateOnEdge);
    repaint(&infile, NULL, rd);
    endTime();
    double t = getTime();
    NSLog(@"### consume time is %f ###\n", t);
    ppm_kill(&infile);
    g_rand_free(random_generator);

    BrushPiece bp;
    bp.last_piece = 1;
    SS_BrushList* bl = SS_BrushListCreate();
    SS_AddBrushPiece(bl, bp);
    SS_BrushListPool* blp = SS_GetBrushListPool();
    SS_AddBrushList(blp, bl);
    [self performSelectorOnMainThread:@selector(computeFinished:) withObject:nil waitUntilDone:NO];
    //SS_DrawInMainThread(NULL, 0);
}
- (void) computeFinished : (NSObject*) data
{
    if(mDrawFinishedArray[displayIndex] == NO)
    {
        SS_Pause((SS_PausePoint*)[self currentPausePoint]);
        
    }
    /*
    if(displayIndex == ([painterState paintTimes] - 1))
    {
        BrushPiece bp;
        bp.last_piece = 2;
        SS_BrushList* bl = SS_BrushListCreate();
        SS_AddBrushPiece(bl, bp);
        SS_BrushListPool* blp = SS_GetBrushListPool();
        SS_AddBrushList(blp, bl);
    }
     */
    [self nextDisplayStage];
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
    NSString* dataContent = [self readDataFile:fileName];
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
            if(strlen(data) == 10)
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
                buf[0] = data[9];
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
        case P_BRUSH_DENSITY:
        {
            float f = [token floatValue];
            p.brush_density = f;
        }
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
        default:
            break;
    }
}
- (void) saveCurrentPass
{
    PHImageView* imageView = [self getImageView];
    [self drawSignature];
    [self saveCurrentImage: imageView.image];
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
    PHImageView* imageView = [self getImageView];
    SignaturePointData spd = [mViewNav getCurrentSignaturePoints];
    if(spd.points.count == 0)
        return;
    [imageView setPoints: spd.points];
    [imageView setNeedsDisplay];
}

@end

@implementation PainterManager
@synthesize paramArray;
@synthesize painterState;
@synthesize bgWidth = _bgWidth;
@synthesize bgHeight = _bgHeight;
@synthesize painterProperty;
@synthesize imageArray;
@synthesize dateArray;
@synthesize isPause;
@synthesize currentImageIndex;
@synthesize mViewNav;
@synthesize mCurrentImageOrientation;
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
        BOOL ret = [self readParamDataFileFromDocuments:@"paramset_url.txt"];
        if(ret == NO)
            [self readParamFromDataFile:@"paramset.txt"];
        [self readPainterQuality:@"paintiddefine.txt"];
        painterProperty = [[PainterProperty alloc] init];
        painterProperty.percent = 7;
        painterProperty.times = 5;
        painterProperty.paper = [NSString stringWithString:@"bricks.ppgm"];
        threadShareData = [[SS_ThreadShareData alloc] init];
        drawOnePicture = YES;
        displayIndex = -1;
        isPause = YES;
        mChangedDrawingIndex = -1;
        /////
        data3D = [[SS_3DData alloc] init];
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

- (void)dealloc
{
    [painterState release];
    [paramArrayFromFile release];
    [paramArray release];
    [paramQualityArray release];
    [painterProperty release];
    [threadShareData release];
    [imageArray release];
    [dateArray release];
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
    pcvals.brush_density = p.brush_density;
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
    painterState.currentBrushSet = [[NSArray alloc] initWithObjects:p.brushName, p.brushName1, p.brushName2, nil];
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
    [self displayCGImage:imageRef];
    [image release];
    [pool release];
}
- (void) displayCGImageWithName:(NSString*) name
{
    NSLog(@"### create image ####");
    SEPainterManagerLoadPhoto* loadPhotoHandler = [[SEPainterManagerLoadPhoto alloc] init];
    ALAssetsLibrary* assetLib = [[ALAssetsLibrary alloc] init];
    loadPhotoHandler.pm = self;
    SEImageAsyncLoader* imageLoader =  [[SEImageAsyncLoader alloc] init];
    [imageLoader setAssetLibOwn:assetLib];
    [assetLib release];
    loadPhotoHandler.imageLoader = imageLoader;
    [imageLoader release];
    NSURL* url = [NSURL URLWithString:name];
    [imageLoader loadFullRepresentation:url withHandler:loadPhotoHandler];
    /*
    NSAutoreleasePool* pool = [[NSAutoreleasePool alloc] init];
    CGImageRef imageRef = [self getCGImageRef:name];
    [self displayCGImage:imageRef];
    [pool release];
     */

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
- (void) nextDisplayStage
{
    if(imageArray == nil || imageArray.count == 0)
        return;
    displayIndex++;
    //BOOL drawFinished = mWantToDrawIndex >= [painterState paintTimes];
    //mDrawFinished = NO;
    if(mViewNav.mNewConfig)
    {
        mViewNav.mNewConfig = NO;
        [paramArrayFromFile release];
        paramArrayFromFile = [NSMutableArray array];
        [paramArrayFromFile retain];
        BOOL ret = [self readParamFromDataFile:@"paramset_url.txt"];
        if(ret == NO)
        {
            [self readParamFromDataFile:@"paramset.txt"];
        }
    }

    if(displayIndex >= [painterState paintTimes])
    {
        NSLog(@"## all compute pass end ###");
        //mSaveImageIndex = currentImageIndex;
        mSaveImageURL = [imageArray objectAtIndex:currentImageIndex];
        mSaveImageDate = [dateArray objectAtIndex:currentImageIndex];
        for(int i = 0 ; i < PARAM_NUM ; i++)
        {
            mDrawFinishedArray[i] = NO;
        }
        mChangedDrawingIndex = displayIndex - 1;
        mSaveOrientation = mCurrentImageOrientation;
        //mNeedSaveImage = YES;
        /*
        if(drawFinished == YES)
        {
            [self saveCurrentPass];
            mNeedSaveImage = NO;
            mWantToDrawIndex = 0;
        }
        else 
        {
            mNeedSaveImage = YES;
            mWantToDrawIndex = -1;
        }
         */
        displayIndex = -1;
        currentImageIndex++;
        int imageCount = [imageArray count];
        if(currentImageIndex >= imageCount)
        {
            currentImageIndex = 0;
        }
        [mViewNav performSelectorOnMainThread:@selector(displayNextImage) withObject:nil waitUntilDone:NO];
        return;
    }
    painterState.currentSeq = displayIndex;
    NSArray* paramID = painterState.painterParamIDs;
    NSString* sid = [paramID objectAtIndex:displayIndex];
    PainterParam* p = [self getPainterParam:sid];
    painterState->wait_time = p.wait_time;
    [self initPcVals];
    [self setCurrentParamToGlobal];
    NSString* currentImageName = [imageArray objectAtIndex:currentImageIndex];
    //[self performSelectorInBackground:@selector(displayCGImageWithName:) withObject:currentImageName];
    [self displayCGImageWithName:currentImageName];
    [self startDrawing];
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
    std::list<int>::iterator min = std::min_element(timesList.begin(), timesList.end());
    *outMin = *min;
    std::list<int>::iterator max = std::max_element(timesList.begin(), timesList.end());
    *outMax = *max;
}
- (void*) modelManager
{
    return data3D->modelManager;
}
- (NSArray*) currentBrushSet
{
    return painterState.currentBrushSet;
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
    //NSString* url = [imageArray objectAtIndex:mSaveImageIndex];
    //NSString* urlDate = [dateArray objectAtIndex:mSaveImageIndex];
    NSString* url = mSaveImageURL;
    NSString* urlDate = mSaveImageDate;
    SelectedImage* si = [mViewNav getSelectedImageByUrl:url andDate:urlDate];
    if(si != nil)
    {
        NSLog(@"## save orientation = %d ##", mSaveOrientation);
        [mViewNav setSelectedImageProperty: url urlDate: urlDate orientation: mSaveOrientation image:image];
        [mViewNav saveImageAndThumbnailToCoreData:image urlName:url urlDate:urlDate index:0];    
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
- (void) drawFinished
{
    NSLog(@"## draw finished ##");
    /*
    if(mNeedSaveImage == YES)
    {
        [self saveCurrentPass];
        mNeedSaveImage = NO;
    }
     */
    if(mDrawingIndex == 0)
    {
        mDrawFinishedArrayNum = [painterState paintTimes];
    }
    if(mChangedDrawingIndex == -1)
    {
        mDrawFinishedArray[mDrawingIndex] = YES;
        mDrawingIndex++;
        if(mDrawingIndex == mDrawFinishedArrayNum)
        {
            [self saveCurrentPass];
        }
    }
    else 
    {
        assert(mDrawingIndex == mChangedDrawingIndex);
        mDrawingIndex = 0;
        mChangedDrawingIndex = -1; 
        mDrawFinishedArrayNum = 0;
        [self saveCurrentPass];
    }
    SS_NoPause((SS_PausePoint*)[self currentPausePoint]);
}
@end
