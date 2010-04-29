#include "SE_Script.h"
#include "SE_Utils.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "./cscript/acc.h"
#include "./renderer/SE_Renderer.h"
#include <dlfcn.h>
typedef int (*MainPtr)(void*);
/**
 * */
int run(MainPtr mainFunc, SE_Script_RunEnv* runEnv) {
    return mainFunc(runEnv);
}

static const char* SEC_GetTextureName(void* runEnv)
{
    return "DB.raw";
}
static void SEC_LOGF(float f)
{
    LOGI("%f\n", f);
}
static void SEC_LOGP(void* p)
{
    LOGI("%p\n", p);
}
static void SEC_BindTexture(void* runEnv, int target)
{
    SE_Script_RunEnv* currEnv = (SE_Script_RunEnv*)runEnv;
    LOGI("spatial = %p\n", currEnv->spatial); 
    SE_ASSERT(currEnv->spatial != NULL);
    SE_Spatial* spatial = currEnv->spatial;
    SE_ResourceManager* resourceManager = spatial->resourceManager;
    SE_ASSERT(resourceManager != NULL);
    SE_Mesh* mesh = currEnv->spatial->mesh;
    if(spatial->spatialType == SE_GEOMETRY)
    {
        SE_MaterialData* md = NULL;
        if(spatial->subMeshIndex != -1)
        {
            SE_SubMesh* subMesh = SE_Mesh_GetSubMesh(mesh, spatial->subMeshIndex);
            md = SE_ResourceManager_GetSubMaterialData(resourceManager, mesh->materialIndex, subMesh->subMaterialIndex);
        }
        else
        {
            md = SE_ResourceManager_GetMaterialData(resourceManager, mesh->materialIndex);
        } 
        if(md != NULL && !SE_String_IsEmpty(&md->texturename))
        {
            SE_Renderer_BindTexture(resourceManager, (enum SE_TEX_TARGET)target, SE_String_GetData(&md->texturename));
        } 
    } 
}
static SE_Script_GlobalEntry entryMap[] = {
    {"SEC_GetTextureName", (void *)&SEC_GetTextureName},
    {"SEC_LOGF", (void*)&SEC_LOGF},
    {"SEC_LOGP", (void*)&SEC_LOGP},
    {"SEC_BindTexture", (void*)&SEC_BindTexture}
};

static ACCvoid* symbolLookup(ACCvoid* pContext, const ACCchar* name) 
{
    int count = sizeof(entryMap) / sizeof(SE_Script_GlobalEntry);
    for(int i = 0 ; i < count ; i++)
    {
        SE_Script_GlobalEntry* e = &entryMap[i];
        if(strcmp(e->fName, name) == 0)
        {
            return e->ff;
        }
    }
    LOGI("can not file funcion implementation : %s\n", name);
    return NULL;
}
/**         function about script **/
SE_Result SE_Script_Init(SE_Script* script, const char* scriptSource)
{
    SE_ASSERT(script);
    SE_ASSERT(scriptSource);
    SE_Object_Clear(script, sizeof(SE_Script));
    script->script = accCreateScript();
    const ACCchar* text[] = {scriptSource};
    accScriptSource(script->script, 1, text, NULL);
    int result = accGetError(script->script);
    if(result != 0)
        return SE_INVALID;
    return SE_VALID;
 
}
SE_Result SE_Script_Compile(SE_Script* script)
{
    accRegisterSymbolCallback(script->script, symbolLookup, NULL);
    accCompileScript(script->script);
    int result = accGetError(script->script);

    if(result != 0)
        return SE_INVALID;
    return SE_VALID;
}
SE_Result SE_Script_Run(SE_Script* script)
{
    MainPtr mainPointer;
    accGetScriptLabel(script->script, "main", (ACCvoid**) &mainPointer);
    int result = accGetError(script->script);
    if (result != ACC_NO_ERROR) 
    {
        LOGI("Could not find main: %d\n", result);
    } 
    else 
    {
        LOGI("Executing compiled code:\n");
        result = run(mainPointer, &script->runEnv);
        LOGI("result: %d\n", result);
    }
}

void SE_Script_Release(void* script)
{
    SE_Script* s = (SE_Script*)script;
    if(s->script)
    {
        accDeleteScript(s->script);
    }
}

