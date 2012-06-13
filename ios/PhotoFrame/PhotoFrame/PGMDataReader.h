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
struct ppm;

typedef struct SS_BrushSet_tag
{
    char brush[3][64];
} SE_BrushSet;
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
extern SS_BrushList* SS_BrushListCreate();
extern void SS_BrushListRelease(SS_BrushList* brushList);    
extern void SS_AddBrushPiece(SS_BrushList* brushList, BrushPiece bp);
extern void SS_DrawBrushList(SS_BrushList* brushList, int index);
extern void SS_GetBrushListRects(SS_BrushList* brushList, SS_MyRect** outRects, int* outCount);

extern SS_BrushListPool* SS_BrushListPoolCreate();
extern void SS_BrushListPoolRelease(SS_BrushListPool* blp);
extern void SS_AddBrushList(SS_BrushListPool* blp, SS_BrushList* bl);
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
extern int SS_DrawCanvas(SS_Canvas* canvas);
extern void SS_SetCanvasBackground(SS_Canvas* canvas, ppm_t background);
extern SS_Canvas* SS_GetCurrentCanvas();


extern void SS_DrawInMainThread(SS_MyRect* rectList, int count);
extern void SS_UpdateImageView();

extern SS_ModelManager* SS_GetModelManager();
extern void loadTexture(const char* texturename, SE_Texture* texture);
extern void createMeshVBO(SE_Mesh* mesh);
extern void removeVBOFromGL(SE_Mesh* mesh);
extern void SS_LoadTextureForImage(void* viewNav, const char* imageName, const char* imageDate,SE_Texture* texture);
extern void SS_GetImageSize(void* viewNav, const char* imageName, const char* imageDate, int* width, int* height);
extern SS_OutputVertexData* SS_CreateOutputVerteData();
extern void SS_OutputVertexData_Output(SS_OutputVertexData* vp);
extern void SS_OutputVertexData_AddVertex(SS_OutputVertexData* vp, float x, float y , float z);
extern void SS_OutputVertexData_AddTexVertex(SS_OutputVertexData* vp, float x, float y);
extern void SS_OutputVertexData_AddFace(SS_OutputVertexData* vp, int v0 , int v1, int v2);
extern void SS_OutputVertexData_AddTexFace(SS_OutputVertexData* vp, int v0 , int v1, int v2);
extern void SS_LoadFullImageAsync(const char* imageName, const char* imageDate, const char* textureID, SS_ModelManager* modelManager, void* viewNav);
extern double SS_StartTime();
extern double SS_EndTime();
//#endif
#endif