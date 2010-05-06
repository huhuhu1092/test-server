#include "SE_Init.h"
#include <math.h>
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_String.h"
#include "SE_Common.h"
#include "SE_World.h"
#include "SE_Camera.h"
#include "cscript/acc.h"
#include <ctype.h>
#include <dlfcn.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "./renderer/SE_Renderer.h"

static SE_World seWorld;
struct SE_World_tag* SE_GetWorld()
{
    return &seWorld;
}
SE_Result SE_InitWorld(int argc, char** argv)
{
    if(argc < 2)
        return 1;
    SE_World_Init(&seWorld, "world_init.rs");
    SE_ResourceManager* resourceManager = SE_World_GetResourceManager(&seWorld);
    SE_ResourceManager_InitFromFile(resourceManager, argv[0], argv[1]);
    SE_Spatial* root = SE_World_GetSceneRoot(&seWorld);
    SE_Spatial_Init(root, SE_NODE, "root", resourceManager,NULL);
    SE_Spatial_SetRenderState(root, SE_TEXTURE, "texture.rs");
    int meshCount = SE_ResourceManager_GetMeshCount(resourceManager);
    LOGI("### meshCount = %d ####\n", meshCount);
    int i;
    SE_List nameList;
    SE_List_Init(&nameList);
    int geometryNum = 0;
    for(i = 0 ; i < meshCount; i++)
    {
        SE_Mesh* mesh = SE_ResourceManager_GetMesh(resourceManager, i);
        SE_Spatial* spatial = SE_Spatial_Create();
        SE_String strName;
        SE_Object_Clear(&strName, sizeof(SE_String));
        SE_String_Concate(&strName, "%s_%d", SE_String_GetData(&mesh->name), i);
        if(mesh->subMeshNum == 0)
        {
            SE_Spatial_Init(spatial, SE_GEOMETRY, SE_String_GetData(&strName), resourceManager, mesh);
            geometryNum++;
        }
        else
        {
            SE_Spatial_Init(spatial, SE_NODE, SE_String_GetData(&strName), resourceManager, mesh);
            int j;
            for(j = 0 ; j < mesh->subMeshNum ; j++)
            {
                SE_Spatial* subs = SE_Spatial_Create();
                SE_String subName;
                SE_String_Concate(&subName, "%s_%d", SE_String_GetData(&strName), j);
                SE_Spatial_Init(subs, SE_GEOMETRY, SE_String_GetData(&subName), resourceManager, mesh);
                subs->subMeshIndex = j;
                SE_Spatial_AddChild(spatial, subs);
                SE_String_Release(&subName);
            }
        }
        SE_Element e;
        e.type = SE_STRING;
        SE_String_Init(&e.str, SE_String_GetData(&strName));
        SE_String_Release(&strName);
        SE_List_AddLast(&nameList, e);
        SE_Spatial_AddChild(root, spatial);
        SE_MaterialData* m = SE_ResourceManager_GetMaterialData(resourceManager, mesh->materialIndex);
        if(m)
        {
            SE_String str = m->texturename;
            if(!SE_String_IsEmpty(&str))
            {
                SE_Texture* tex = SE_ResourceManager_LoadTexture(resourceManager, SE_String_GetData(&m->texturename));
            }
        }
    }
    LOGI("... geometry with no sub mesh num = %d\n", geometryNum);
    SE_Spatial_UpdateRenderState(root);
    /*
    int nameCount = SE_List_Size(&nameList);
    SE_ASSERT(nameCount == meshCount);
    for(i = 0 ; i < nameCount ; i++)
    {
        SE_Element e = SE_List_GetAt(&nameList, i);
        SE_Spatial_RemoveChildByName(root, e.str);
    }
    */
    SE_List_Release(&nameList);
    return 0;
}

