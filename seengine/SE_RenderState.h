#ifndef SE_RENDERSTATE_H
#define SE_RENDERSTATE_H
#include "SE_Common.h"
#include "SE_String.h"
#include "SE_ResourceManager.h"
#ifdef __cplusplus
extern "C" {
#endif
struct SE_Spatial_tag;
enum SE_RS_TYPE {SE_TEXTURE, SE_DEPATH, SE_WIREFRAME};
#define SE_RS_NUM 3
typedef struct SE_RenderStateUnit_tag
{
    int type; /*SE_RS_TYPE*/
    SE_String scriptName;
} SE_RenderStateUnit;
typedef struct SE_RenderState_tag
{
    SE_RenderStateUnit rsu[SE_RS_NUM]; 
    SE_ResourceManager* resourceManager;
} SE_RenderState;
extern SE_Result SE_RenderState_Init(SE_RenderState* rs, SE_ResourceManager* resourceManager);
extern void SE_RenderState_Release(void* rs);
extern void SE_RenderState_Update(SE_RenderState* dstRs, SE_RenderState* srcRs);
extern void SE_RenderState_Activate(SE_RenderState* rs, SE_Spatial_tag* spatial);

#ifdef __cplusplus
}
#endif

#endif
