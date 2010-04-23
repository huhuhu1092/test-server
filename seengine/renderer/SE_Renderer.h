#ifndef SE_RENDERER_H
#define SE_RENDERER_H

#include "SE_Vector.h"
#include "SE_Matrix.h"
#include "SE_Quat.h"
#include "SE_Common.h"

#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_Renderer_tag
{

} SE_Renderer ;   
extern SE_Result SE_Renderer_Init(SE_Renderer* renderer);
extern SE_Result SE_Renderer_BeginDraw(SE_Renderer* renderer);
extern SE_Result SE_Renderer_EndDraw(SE_Renderer* renderer);
extern SE_Result SE_Renderer_Draw(SE_Renderer* renderer);
extern void SE_Renderer_Release(SE_Renderer* renderer);
#ifdef __cplusplus
}
#endif /** end __cplusplus*/

#endif /** end SE_UTILS_H*/
