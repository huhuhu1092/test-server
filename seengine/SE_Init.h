#ifndef SE_INIT_H
#define SE_INIT_H
#include "SE_Common.h"
#ifdef __cplusplus
extern "C" {
#endif
struct SE_World_tag;
extern struct SE_World_tag* SE_GetWorld();
extern SE_Result SE_InitWorld(int argc, char** argv);
extern SE_Result SE_ResizeWindow(int w, int h);
#ifdef __cplusplus
}
#endif

#endif
