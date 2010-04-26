#include <stdio.h>
#include <math.h>
#include "./ase/aselib.h"
#include "SE_ResourceManager.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "SE_String.h"
#include "SE_Common.h"
int main(int argc, char** argv)
{
    float t = sqrtf(1.0);
    if(argc < 3)
        return 1;
    //ASE_Loader loader(argv[1], 0, 0);
    //loader.Load();
    //loader.Write(argv[2]);
    SE_ResourceManager resourceManager;
    SE_ResourceManager_InitFromFile(&resourceManager, "/home/luwei/program/streamserver_git/test-server/seengine", argv[2]);
    SE_Spatial root;
    SE_Spatial_Init(&root, SE_NODE, "root", NULL);
    int meshCount = SE_ResourceManager_GetMeshCount(&resourceManager);
    LOGI("### meshCount = %d ####\n", meshCount);
    int i;
    SE_List nameList;
    SE_List_Init(&nameList);
    for(i = 0 ; i < meshCount; i++)
    {
        SE_Mesh* mesh = SE_ResourceManager_GetMesh(&resourceManager, i);
        SE_Spatial* spatial = SE_Spatial_Create();
        SE_String strName;
        SE_Object_Clear(&strName, sizeof(SE_String));
        SE_String_Concate(&strName, "%s_%i", SE_String_GetData(&mesh->name), i);
        SE_Spatial_Init(spatial, SE_GEOMETRY, SE_String_GetData(&strName), mesh);
        SE_Element e;
        e.type = SE_STRING;
        SE_String_Init(&e.str, SE_String_GetData(&strName));
        SE_String_Release(&strName);
        SE_List_AddLast(&nameList, e);
        SE_Spatial_AddChild(&root, spatial);
    }
    int nameCount = SE_List_Size(&nameList);
    SE_ASSERT(nameCount == meshCount);
    for(i = 0 ; i < nameCount ; i++)
    {
        SE_Element e = SE_List_GetAt(&nameList, i);
        SE_Spatial_RemoveChildByName(&root, e.str);
    }
    SE_Spatial_Release(&root);
    SE_ResourceManager_Release(&resourceManager);
    SE_List_Release(&nameList);
    return 0;
}
