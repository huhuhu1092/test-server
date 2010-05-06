#include "SE_World.h"
SE_Result SE_World_Init(SE_World* world, const char* initScriptName)
{
    SE_Object_Clear(world, sizeof(SE_World));
    SE_List_Init(&world->cameraList);
    SE_String_Init(&world->initScript, initScriptName);
    SE_InputDevice_Init(&world->inputDevice);
    return SE_VALID;
}
void SE_World_Release(void* world)
{
    SE_World* w = (SE_World*)world;
    SE_List_Release(&w->cameraList);
    SE_Spatial_Release(&w->sceneRoot);
    SE_Spatial_Release(&w->playerController);
    SE_ResourceManager_Release(&w->resourceManager);
    SE_String_Release(&w->initScript);
}
SE_Camera* SE_World_GetMainCamera(SE_World* world)
{
    return &world->mainCamera;
}
SE_Spatial* SE_World_GetSceneRoot(SE_World* world)
{
    return &world->sceneRoot;
}
SE_Spatial* SE_World_GetPlayerController(SE_World* world)
{
    return &world->playerController;
}
SE_ResourceManager* SE_World_GetResourceManager(SE_World* world)
{
    return &world->resourceManager;
}
SE_InputDevice* SE_World_GetInputDevice(SE_World* world)
{
    return &world->inputDevice;
}
SE_Result SE_World_Update(SE_World* world, float time)
{
    return SE_VALID;
}
SE_Result SE_World_FixUpdate(SE_World* world, float fixTime)
{
    return SE_VALID;
}


