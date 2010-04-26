#ifndef SE_RENDERER_H
#define SE_RENDERER_H

#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Common.h"
#include "SE_List.h"
#include "SE_GeometryData.h"
#ifdef __cplusplus
extern "C" {
#endif
struct SE_World_tag;
struct SE_Mesh_tag;
struct SE_Script_tag;
enum SE_TEX_FILTER {SE_NEAREST, SE_LINEAR};
enum SE_TEX_WRAP_VALUE {SE_REPEAT, SE_CLAMP, SE_CLAMP_TO_EDGE, SE_CLAMP_TO_BORDER};
enum SE_TEX_WRAP_TYPE {SE_S, SE_T, SE_R};
enum SE_TEX_TARGET {SE_1D, SE_2D, SE_3D};
enum SE_TEX_ENV {SE_REPLACE, SE_DECAL, SE_MODULATE};
typedef struct SE_RendererUnit_tag
{
    SE_Mesh_tag* mesh;
    SE_Script_tag* renderScript;
} SE_RendererUnit;
typedef struct SE_Renderer_tag
{
    SE_World_tag* currWorld;
    SE_List* rendererUnit;
} SE_Renderer ;   
extern SE_Result SE_Renderer_Init(SE_Renderer* renderer, struct SE_World_tag* currWorld);
extern SE_Result SE_Renderer_BeginDraw(SE_Renderer* renderer);
extern SE_Result SE_Renderer_EndDraw(SE_Renderer* renderer);
extern SE_Result SE_Renderer_Draw(SE_Renderer* renderer);
extern void SE_Renderer_Release(SE_Renderer* renderer);
extern void SE_Renderer_DrawGeometry(SE_Renderer* renderer, int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum);
extern void SE_Renderer_SetTexFilter(SE_Renderer* renderer, enum SE_TEX_TARGET target, enum SE_TEX_FILTER mag, enum SE_TEX_FILTER min);
extern void SE_Renderer_SetTexWrap(SE_Renderer* renderer, enum SE_TEX_TARGET target, enum SE_TEX_WRAP_TYPE type, enum SE_TEX_WRAP_VALUE v);
extern void SE_Renderer_SetTexEnv(SE_Renderer* renderer, enum SE_TEX_ENV env);
extern void SE_Renderer_BindTexture(SE_Renderer* renderer, enum SE_TEX_TARGET target, const char* texName);
#ifdef __cplusplus
}
#endif /** end __cplusplus*/

#endif /** end SE_UTILS_H*/
