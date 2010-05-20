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
#include "SE_SaveState.h"
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
                SE_Object_Clear(&subName, sizeof(SE_String));
                SE_String_Concate(&subName, "%s_%d", SE_String_GetData(&strName), j);
                SE_Spatial_Init(subs, SE_GEOMETRY, SE_String_GetData(&subName), resourceManager, mesh);
                subs->subMeshIndex = j;
                SE_Spatial_AddChild(spatial, subs);
                SE_String_Release(&subName);
            }
        }
        SE_Spatial_CreateLocalBV(spatial, SE_AABB_E);
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
    SE_Spatial_UpdateWorldBV(root);
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
SE_Result SE_ResizeWindow(int w, int h)
{
    if(w <= 0)
        w = 1;
    if(h <= 0)
        h = 1;
    SE_Camera* mainCamera = SE_World_GetMainCamera(&seWorld);
    SE_Vector3f loc, target, zDir,zAxis, yAxis;
    /*
    SE_Vec3f_Init(111.3221f,-338.9771f, 119.7675f, &loc);
    SE_Vec3f_Init(46.4345f, -123.8831f, 57.3685f, &target);
    */
    /*
    SE_Vec3f_Init(54.9162,	-240.5901,	95.9493, &loc);
    SE_Vec3f_Init(49.3477,	27.1996,	97.1154, &target);
    */
    SE_Vec3f_Init(45.4441,	-234.7624,	90.7012, &loc);
    SE_Vec3f_Init(0, -1, 0, &zAxis);
    SE_Vec3f_Init(0, 0, 1, &yAxis);
    SE_Camera_InitByDirectionUp(&loc, &zAxis, &yAxis, 90.0f, ((float)h) / w, 1.0f, 1000.0f, mainCamera); 
    /*
    SE_Camera_InitByLocationTarget(&loc, &target, 90.0f, ((float)h) / w, 1.0f, 1000.0f, mainCamera);
    SE_Camera_RotateLocalXYZAxis(mainCamera, 90.0f, 2);
    */
    SE_Camera_SetViewport(mainCamera, 0, 0, w, h);
    SE_World_SaveMainCamera(&seWorld);
    /*
    SE_Rectf nearrect;
    SE_Frustum_GetNearPlaneRect(&mainCamera->frustum, &nearrect);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glFrustum(nearrect.left, nearrect.right, nearrect.top, nearrect.bottom, 1.0f, 1000.0f);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity(); 
*/
    return SE_VALID;
}
