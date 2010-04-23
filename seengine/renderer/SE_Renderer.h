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
extern void SE_Renderer_DrawGeometry(int type, SE_Vector3f* vertexArray, int vertexNum,
                                     SE_Face* faceArray, int faceNum, 
                                     SE_Vector3f* texVertexArray, int texVertexNum,
                                     SE_Face* texFaceArray, 
                                     SE_Vector3f* colorArray, int colorNum);
#ifdef __cplusplus
}
#endif /** end __cplusplus*/

#endif /** end SE_UTILS_H*/
