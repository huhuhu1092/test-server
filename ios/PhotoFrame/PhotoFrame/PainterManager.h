//
//  PainterManager.h
//  TestImageView
//
//  Created by 陈勇 on 11-10-13.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "ppmtool.h"
#import "SEProtocalDefine.h"
enum {
    PARAM_NUM = 10
};
typedef enum _PARAM_COLUMN_TYPE
{
    P_STATE ,
    P_ID,
    P_WIDTH,
    P_HEIGHT,
    P_ORIENT_TYPE,
    P_ORIENT_NUM,
    P_ORIENT_FIRST,
    P_ORIENT_LAST,
    P_SIZE_NUM,
    P_SIZE_FIRST,
    P_SIZE_LAST,
    P_SIZE_TYPE,
    P_BG_TYPE,
    P_PLACE_TYPE,
    P_BRUSH_DENSITY_MAX,
    P_BRUSH_DENSITY_MIN,
    P_WAITING_TIME,
    P_DRAWING_SPEED,
    P_COLOR_TYPE = 20,
    P_EDGEDETECT_LOWVALUE = 21
} SSParamColumnType;
@class SS_ThreadShareData;
@class SS_3DData;
@interface PainterParam : NSObject
{
    int state;
    NSString* paintid;
    int sid[4];
    int width;
    int height;
    int orient_type;
    int orient_num;
    float orient_first;
    float orient_last;
    int size_num;
    float size_first;
    float size_last;
    int size_type;
    int bg_type;
    int place_type;
    float brush_density;
    float paper_scale;
    float paper_relief;
    float brush_relief;
    int color_type;
    int drawing_speed;
    int wait_time;
    NSString* brushName;
    NSString* brushName1;
    NSString* brushName2;
    NSString* paperName;
    float edgeDetectLowValue;
}
@property (nonatomic, assign) float brush_density_max;
@property (nonatomic, assign) float brush_density_min;
@property (nonatomic, assign) float edgeDetectLowValue;
@property (nonatomic) int state;
@property (nonatomic, retain) NSString* paintid;
@property (nonatomic) int width;
@property (nonatomic) int height;
@property (nonatomic) int orient_type;
@property (nonatomic) int orient_num;
@property (nonatomic) float orient_first;
@property (nonatomic) float orient_last;
@property (nonatomic) int size_num;
@property (nonatomic) float size_first;
@property (nonatomic) float size_last;
@property (nonatomic) int size_type;
@property (nonatomic) int bg_type;
@property (nonatomic) int place_type;
@property (nonatomic) float brush_density;
@property (nonatomic) float paper_scale;
@property (nonatomic) float paper_relief;
@property (nonatomic) float brush_relief;
@property (nonatomic) int color_type;
@property (nonatomic) int drawing_speed;
@property (nonatomic) int wait_time;
@property (nonatomic, retain) NSString* brushName;
@property (nonatomic, retain) NSString* brushName1;
@property (nonatomic, retain) NSString* brushName2;
@property (nonatomic, retain) NSString* paperName;
- (int) getSid: (int) index;
- (void)setSid: (int*)d count:(int)count;
- (void)setParam:(int)ot :(int)on :(float)of :(float)ol :(int)sn :(float)sf :(float)sl :(int)st
                :(int)bt :(int)pt :(float)bd :(float)ps :(float)pr :(float)br :(int)ct :(int)ds :(int)wt;
@end
///////////
@interface BrushDefine : NSObject 
{
    NSMutableArray* brushNames;
    int brushGettingWay;
    int brushGettingLevel;
    int brushGettingName;
    int brushID;
    NSString* brushOutName;
}
@property (nonatomic, retain) NSString* brushOutName;
@property (nonatomic, readonly) NSMutableArray* brushNames;
@property (nonatomic, assign) int brushGettingWay;
@property (nonatomic, assign) int brushGettingLevel;
@property (nonatomic, assign) int brushGettingName;
@property (nonatomic, assign) int brushID;
- (void) addBrushName: (NSString*) brushName;
- (NSArray*) getBrushNames;
@end
/////////////
@interface BrushPackage : NSObject
{
    NSMutableArray* brushDefineArray;
    NSMutableArray* brushNameArray;
    NSMutableArray* nameArray;
    NSString* packageName;
}
@property (nonatomic, retain) NSString* packageName;
-(void) load: (NSString*) pn;
- (NSArray*) getBrushes: (int) currentBrushID;
- (NSArray*) getAllBrushID;
@end
///////////////
@interface PainterQuality : NSObject 
{
    int percent;
    int seq;
    NSMutableArray* paintIDArray;
}
@property (nonatomic) int percent;
@property (nonatomic) int seq;
@property (nonatomic, retain) NSMutableArray* paintIDArray;
@end
struct _BrushPiecesList;
@interface PainterState : NSObject {
@private
    int currentSeq;
    NSArray* painterParamIDs;
    NSArray* currentBrushSet;
    struct _BrushPiecesList* brushPieces;
    BOOL colorRevert;
    BOOL bSaveBrush;
    BOOL bShowSettingUI;
@public
    int wait_time;
}
@property (nonatomic) int currentSeq;
@property (nonatomic) BOOL colorRevert;
@property (nonatomic, retain) NSArray* painterParamIDs;
@property (retain) NSArray* currentBrushSet;
@property (nonatomic) BOOL bSaveBrush;
@property (nonatomic) BOOL bShowSettingUI;
- (int) paintTimes;
@end
@interface PainterProperty : NSObject
{
    int percent;
    int times;
    NSString* paper;
}
@property (nonatomic) int percent;
@property (nonatomic) int times;
@property (nonatomic, retain) NSString* paper;
@end
@class SEViewNavigator;
@interface PainterManager : NSObject
{
    NSArray* paramArray;
    int drawingState[PARAM_NUM];
    NSMutableArray* paramArrayFromFile;
    NSMutableArray* paramQualityArray;
    BrushPackage* brushPackage;
    int mCurrentBrush;
    PainterState* painterState;
    volatile int _bgWidth;
    volatile int _bgHeight;
    SS_ThreadShareData* threadShareData;
    SS_3DData* data3D;
    BOOL drawOnePicture;
    NSTimer* displayTimer;
    int displayIndex;
    NSArray* imageArray; // array of image URL in photo lib
    //int mCurrentPaintTimes; // set by nextDisplayImage in SEViewNavigator, it will not change before finished one image drawing
    //NSArray* mCurrentPaintParamIDs;//set by nextDisplayImage in SEViewNavigater, it will not change before finish one image drawing
    //NSArray* dateArray; //array of image URL date
    int currentImageIndex;
    BOOL isPause;
@private
    NSString* mCurrentComputeImageName;
    PainterProperty* painterProperty;
    SEViewNavigator* mViewNav;
    int mCurrentImageOrientation;
    BOOL mDrawFinishedArray[PARAM_NUM];
    int mDrawFinishedArrayNum;
    int mDrawingIndex;
    int mChangedDrawingIndex;
    BOOL mChangeToNewImage;
    //BOOL mDrawFinished;
    //BOOL mNeedSaveImage;
    //int mSaveImageIndex;
    NSString* mSaveImageURL;
    NSString* mSaveImageDate;
    int mSaveOrientation;
    int mWantToDrawIndex;
    //BOOL mStageDrawFinished;
    int mAdjustAngle;
    int mBrushTransparent;
    BOOL mComputeReady;
    BOOL mComputeThreadEnd;
    BOOL mFirstTimeDraw;
    BOOL mFromStartDrawImageState;
    BOOL mFromStopDrawImageState;
    int mCurrentBrushIndex;
    ////
    int mCurrentEdgeDetectValue;
    int mCurrentTransparentValue;
}
@property (nonatomic, retain) NSString* mSaveImageURL;
@property (nonatomic, retain) NSString* mSaveImageDate;
@property (nonatomic, readonly) int mBrushTransparent;
//@property (nonatomic, assign) BOOL mStageDrawFinished;
@property (nonatomic, assign) int mCurrentImageOrientation;
@property (nonatomic, assign) SEViewNavigator* mViewNav;
@property (nonatomic, assign) int currentImageIndex;
@property (nonatomic, readonly) BOOL isPause;
//@property (nonatomic, retain) NSArray* dateArray;
@property (nonatomic, retain) NSArray* imageArray;
@property (nonatomic, readonly) NSArray* paramArray;
@property (nonatomic, readonly) PainterState* painterState;
@property (nonatomic) volatile int bgWidth;
@property (nonatomic) volatile int bgHeight;
@property (nonatomic, retain) PainterProperty* painterProperty;
- (void)initPainterState: (int)quality withTimes:(int)times;
- (void)setDrawingState:(int)s index:(int)i ;
- (int)drawingState:(int)index;
- (void)setCurrentParamToGlobal;
- (PainterParam*)currentParam: (int)index;
- (PainterParam*)painterParam: (NSString*)sid;
- (void) clearPainterState;
- (NSArray*) painterParamsByQuality: (int) percent withTimes: (int) times;
- (void) setCurrentPainterParamID:(NSArray*)paramIDArray;
- (CGSize) currentPainterImageSize;
- (int)currentDrawingSpeed;
- (void)setParam: (PainterParam*)p withID: (NSString*)sid;
- (void) getMinMaxTimesValue: (int)percent outMin:(int*)outMin outMax:(int*)outMax;
- (NSArray*) currentBrushSet;
//- (void) setCurrentBrushSet: (NSArray*) brushArray;
+ (CGSize) computeFitSize: (CGSize)src toDst: (CGSize) dst;
+ (PainterManager*) painterManager;
+ (CGImageRef)createCGImage: (ppm_t) p;
+ (CGImageRef) createCGImageWithCopy:(ppm_t)p;
- (void*) currentBrushListPool;
- (void*) currentBrushCanvas;
- (void*) currentPausePoint;
- (void*) currentStatusPoint;
- (void*) modelManager;
- (void*) detectModelManager;
- (void) releaseModelManager;
- (void) nextDisplayStage;
- (void) setTimer;
- (void) updateImageView:(NSArray*)rectArray;
- (void) pauseDrawing;
- (void) startDrawing;
- (void) drawFinished;
- (void) promptNextImageDraw;
- (void) promptNextImageDrawAfterAnimation;
////private
- (void)timerUpdate:(NSTimer*)theTimer;
- (void) displayCGImageWithName:(NSString*) name;
- (void) saveCurrentImage: (UIImage*)image;
//- (void) changeDisplayIndex:(int) i;
- (void) displayNextImage;
- (NSArray*) getAllBrushID;
- (NSArray*) getBrushesById : (int) brushID;
- (BrushDefine*) getBrushDefine: (int) brushID;
- (BrushDefine*) getBrushDefineByOutName: (NSString*) outName;
//- (int) currentBrushID;
//- (void) setCurrentBrushID :(int) brushID;
- (BOOL) findNextProperImageIndex;
- (void) addLog: (NSString*) text;
- (void) setFirstNum: (NSNumber*) num;
- (void) setSecondNum: (NSNumber*)num;
- (void) showLoadingView: (id) param;
- (void) hideLoadingView: (id) param;
- (void) computeReady: (id)param;
- (void) paintSignatureToImage: (CGImageRef) imageRef frame: (CGRect)frame;
- (void) displayComputeLoadingView;
////////////////////////////////////////////////////////////////////////////
- (void) stopDrawImage;
- (void) startDrawImage;
- (void) pauseDrawImage;
- (void) releaseResourceForDraw;
- (BOOL) isComputeEnd;
- (void) doAction: (int)fromState toState: (int)toState;
- (void) setFirstTimeDraw: (BOOL) b;
- (void) drawFinishAction;
- (void)drawFinishWhenPause;
@end

