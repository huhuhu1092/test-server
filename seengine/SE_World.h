#ifndef SE_WORLD_H
#define SE_WORLD_H
#include "SE_Camera.h"
#include "SE_List.h"
#include "SE_Spatial.h"
#include "SE_ResourceManager.h"
#include "SE_Script.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_World_tag
{
    SE_List cameraList;
    SE_Camera mainCamera;
    SE_Spatial sceneRoot;
    SE_Spatial playerController;
    SE_ResourceManager resourceManager;
    SE_String initScript;
} SE_World;
extern SE_Result SE_World_Init(SE_World* world, const char* initScriptName);
extern void SE_World_Release(void* world);
extern SE_Camera* SE_World_GetMainCamera(SE_World* world);
extern SE_Spatial* SE_World_GetSceneRoot(SE_World* world);
extern SE_Spatial* SE_World_GetPlayerController(SE_World* world);
extern SE_ResourceManager* SE_World_GetResourceManager(SE_World* world);
extern SE_Result SE_World_Update(SE_World* world, float time);
extern SE_Result SE_World_FixUpdate(SE_World* world, float fixTime);
#ifdef __cplusplus
}
#endif
#endif
