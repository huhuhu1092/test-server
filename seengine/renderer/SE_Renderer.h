#ifndef SE_RENDERER_H
#define SE_RENDERER_H

#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Common.h"
#include "SE_List.h"
#include "SE_GeometryData.h"
#include "SE_Spatial.h"
#include "SE_World.h"
#include "SE_ResourceManager.h"
#ifdef __cplusplus
extern "C" {
#endif
enum SE_GL_STATE {SE_TEX_2D, SE_LIGHT, SE_DEPTH};
enum SE_TEX_FILTER {SE_NEAREST, SE_LINEAR};
enum SE_TEX_WRAP_VALUE {SE_REPEAT, SE_CLAMP, SE_CLAMP_TO_EDGE, SE_CLAMP_TO_BORDER};
enum SE_TEX_WRAP_TYPE {SE_S, SE_T, SE_R};
enum SE_TEX_TARGET {SE_1D, SE_2D, SE_3D};
enum SE_TEX_ENV {SE_REPLACE, SE_DECAL, SE_MODULATE};
enum SE_SHADER {SE_VERTEX_SHADER, SE_FRAGMENT_SHADER};
typedef struct SE_RenderGeometry_tag
{
	SE_Vector3f* vertexArray;
	int vertexCount;
	SE_Vector2f* texVertexArray;
	int texVertexCount;
	SE_Matrix4f matrix;
} SE_RenderGeometry;
typedef struct SE_RenderUnit_tag
{
	SE_List renderGeometryList;
	int materialIndex;
	int subMaterialIndex;
	SE_Vector3f color;
} SE_RenderUnit;

typedef struct SE_Renderer_tag
{
    struct SE_World_tag* currWorld;
    SE_List rendererUnitList;
    SE_Matrix4f objToWorld;
    SE_Matrix4f worldToView;
    SE_Matrix4f viewToProjective;
    int wWidth;
    int wHeight;
    void* userData;
} SE_Renderer ;   
/**function about SE_RenderUnit*/
extern void SE_RenderGeometry_Release(void* rg);
extern void SE_RenderUnit_Release(void* ru);
/* function about SE_Renderer****/
extern SE_Result SE_Renderer_Init(SE_Renderer* renderer, struct SE_World_tag* currWorld, int w, int h);
extern SE_Result SE_Renderer_BeginDraw(SE_Renderer* renderer);
extern SE_Result SE_Renderer_EndDraw(SE_Renderer* renderer);
extern SE_Result SE_Renderer_Draw(SE_Renderer* renderer);
extern void SE_Renderer_Release(void* renderer);

extern void SE_Renderer_SetTexFilter(enum SE_TEX_TARGET target, enum SE_TEX_FILTER mag, enum SE_TEX_FILTER min);
extern void SE_Renderer_SetTexWrap(enum SE_TEX_TARGET target, enum SE_TEX_WRAP_TYPE type, enum SE_TEX_WRAP_VALUE v);
extern void SE_Renderer_SetTexEnv(enum SE_TEX_ENV env);
extern void SE_Renderer_BindTexture(SE_ResourceManager* resourceManager, enum SE_TEX_TARGET target, const char* texName);
extern void SE_Renderer_EnableState(enum SE_GL_STATE s);
extern void SE_Renderer_DisableState(enum SE_GL_STATE s);
extern void SE_Renderer_SetColor(float r, float g, float b, float a);
extern void SE_Renderer_SetAmbientMaterial(float rm, float gm, float bm, float am);
#ifdef __cplusplus
}
#endif /** end __cplusplus*/

#endif /** end SE_UTILS_H*/
