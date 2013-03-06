//
//  PGMDataReader.h
//  TestImageView
//
//  Created by 陈勇 on 11-10-7.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//
#ifndef __PGMDATAREADER_H
#define __PGMDATAREADER_H
#ifdef __cplusplus
class SS_BrushList;
class SS_BrushListPool;
class SS_PausePoint;
class SS_Canvas;
class SS_AtomicCounter;
class SS_ModelManager;
struct SE_Mesh;
class SS_OutputVertexData;
class SE_Texture;
#endif
//#ifdef __cplusplus
//extern "C" {
//#endif
#include "gimpressionist.h"
#include "ppmtool.h"
#include <string>
#include <vector>
#include <list>
struct ppm;

struct SE_BrushSet
{
    std::vector<std::string> brush;
};
struct SE_TextureLoadInfo // information for texture load
{
    std::string textureName;
    std::string pictureName;
    std::string pictureDate;
    int photoFrameOrientation;
};
typedef struct SS_MyRect_tag
{
    int x, y, w, h;
} SS_MyRect;
extern void SS_GetPgmData(const char* fn, const char** outData, int* outLen);
extern void SS_SetBackground(ppm_t background);
extern void SS_StartBrushPaint();
extern void SS_StartBrushPaintInMainQueue(BrushPiece inputbp);
extern void SS_DrawBackgroundImageSync();
extern int SS_GetDrawingSpeed();
extern void SS_GetCurrentPaintSize(int* outWidth, int* outHeight);
extern void SS_GetSettingBrush(SE_BrushSet* outBrushSet);
extern void SS_GetDestSize(int* dstWidth, int* dstHeight);
extern void SS_SaveByPNGFormat(const char* fileName, ppm_t ppm_image);
extern void SS_SaveBrush(const char* prefix, int brushIndex, ppm_t ppm_image);
extern void SS_SaveImage(const char* name);

//#ifdef __cplusplus
//}
//#endif
//#ifdef __cplusplus
extern SS_BrushList* SS_BrushListCreate(bool p = false);
extern void SS_BrushListRelease(SS_BrushList* brushList);    
extern void SS_AddBrushPiece(SS_BrushList* brushList, BrushPiece bp);
extern void SS_DrawBrushList(SS_BrushList* brushList, int index);
extern void SS_GetBrushListRects(SS_BrushList* brushList, SS_MyRect** outRects, int* outCount);

extern SS_BrushListPool* SS_BrushListPoolCreate();
extern void SS_BrushListPoolRelease(SS_BrushListPool* blp);
extern void SS_AddBrushList(SS_BrushListPool* blp, SS_BrushList* bl);
extern bool SS_AddBrush(SS_BrushListPool* blp, ppm_t brush, int pass);
extern void SS_ClearBrushListPool(SS_BrushListPool* blp);
extern void SS_ClearDrawingBrushes(SS_BrushListPool* blp, int pass);
extern SS_BrushList* SS_GetNextBrushList(SS_BrushListPool* blp);
extern SS_BrushListPool* SS_GetBrushListPool();

extern SS_PausePoint* SS_PausePointCreate();
extern void SS_PausePointRelease(SS_PausePoint* pp);
extern void SS_SetComputationPausePoint(SS_PausePoint* pp);
extern void SS_Pause(SS_PausePoint* pp);
extern void SS_NoPause(SS_PausePoint* pp);
extern SS_PausePoint* SS_GetCurrentPausePoint();

extern SS_Canvas* SS_CanvasCreate();
extern void SS_CanvasRelease(SS_Canvas* canvas);
extern void SS_CanvasDrawImage(SS_Canvas* canvas, void* pImageRef, SS_MyRect r);
extern int SS_DrawCanvas(SS_Canvas* canvas);
extern void SS_SetCanvasBackground(SS_Canvas* canvas, ppm_t background);
extern void SS_SetCanvasOriginMap(SS_Canvas* canvas, ppm_t* o);
extern SS_Canvas* SS_GetCurrentCanvas();

extern void SS_SetFirstNum(int n);
extern void SS_SetSecondNum(int n);
extern void SS_DrawInMainThread(SS_MyRect* rectList, int count);
extern void SS_UpdateImageView();

extern SS_AtomicCounter* SS_CreateAtomicConter();
extern void SS_SetAtomicCounterValue(SS_AtomicCounter* c, int v);
extern int SS_GetAtomicCounterValue(SS_AtomicCounter* c);
extern void SS_ReleaseAtomicCounter(SS_AtomicCounter* c);
extern SS_AtomicCounter* SS_GetCurrentStatusPoint();

extern SS_ModelManager* SS_GetModelManager();
extern void loadTexture(const char* texturename, SE_Texture* texture);
extern void loadGrayTexture(const char* texturename, SE_Texture* texture);
extern void createMeshVBO(SE_Mesh* mesh);
extern void removeVBOFromGL(SE_Mesh* mesh);
extern void SS_LoadImageTextureAsync(const char* imageName, const char* imageDate, const char* textureID, SS_ModelManager* modelManager, void* viewNav, bool bFullSize, int photoFrameOrientation);
extern void SS_LoadImageTextureListAsync(std::list<SE_TextureLoadInfo>& textureLoadInfoList, SS_ModelManager* modelManager, void* viewNav, bool bFullSize);
extern void SS_LoadImageTextureWithOrientationAsync(const char* imageName, const char* imageDate, const char* textureID, SS_ModelManager* modelManager, void* viewNav, bool bFullSize, int orientation);
extern void SS_LoadTextureForImage(void* viewNav, const char* imageName, const char* imageDate,SE_Texture* texture);
extern bool SS_IsDefaultSelectedImage(const std::string& name);
extern void SS_LoadThumbnailTextureForImageArray(void* viewNav, SS_ModelManager* modelManager, const std::vector<std::string>& imageNameArray,
                                                 const std::vector<std::string>& imageDateString, const std::vector<int> & orientation);
extern void SS_GetImageSize(void* viewNav, const char* imageName, const char* imageDate, int* width, int* height);
extern SS_OutputVertexData* SS_CreateOutputVerteData();
extern void SS_OutputVertexData_Output(SS_OutputVertexData* vp);
extern void SS_OutputVertexData_AddVertex(SS_OutputVertexData* vp, float x, float y , float z);
extern void SS_OutputVertexData_AddTexVertex(SS_OutputVertexData* vp, float x, float y);
extern void SS_OutputVertexData_AddFace(SS_OutputVertexData* vp, int v0 , int v1, int v2);
extern void SS_OutputVertexData_AddTexFace(SS_OutputVertexData* vp, int v0 , int v1, int v2);

extern double SS_StartTime();
extern double SS_EndTime();
extern void SS_SetLoadingStage(int stage);
extern int SS_GetLoadingStage();
extern void SS_AddLog(const char* fmt, ...);
extern void SS_ShowLoadingView();
extern void SS_HideLoadingView();
extern void SS_ComputeReady();
extern void SS_GetImageAcualSize(float width, float height, int orientation, float* outWidth, float* outHeight);
//extern void SS_DrawImageToGBackground(void* pImageRef, SS_MyRect r);
//#endif
#endif