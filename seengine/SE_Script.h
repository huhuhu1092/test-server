#ifndef SE_SCRIPT_H
#define SE_SCRIPT_H
#include "SE_Common.h"
#include "SE_String.h"

#ifdef __cplusplus
extern "C" {
#endif
struct ACCscript;
typedef void ACCvoid;
struct SE_Spatial_tag;
typedef struct SE_Script_GlobalEntry_tag
{
    const char* fName;
    ACCvoid* ff;
} SE_Script_GlobalEntry;
typedef struct SE_Script_RunEnv_tag
{
    void* data;
} SE_Script_RunEnv;
typedef struct SE_Script_Tag
{
    ACCscript* script;
    SE_Script_RunEnv runEnv;
} SE_Script;
extern SE_Result SE_Script_Init(SE_Script* script, const char* scriptSource, int scriptLen);
extern SE_Result SE_Script_Compile(SE_Script* script);
extern SE_Result SE_Script_Run(SE_Script* script);
/**
 * release function is invoke by script manager
 * */
extern void SE_Script_Release(void* script);
#ifdef __cplusplus
}
#endif

#endif
