#ifndef SE_RENDERSTATE_H
#define SE_RENDERSTATE_H
#include "SE_Common.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_RenderState_tag
{
} SE_RenderState;
extern SE_Result SE_RenderState_Init(SE_RenderState* rs);
extern void SE_RenderState_Release(void* rs);
#ifdef __cplusplus
}
#endif

#endif
