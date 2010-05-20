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
static void SEC_SetTexWrap(void* runEnv, int target, int wrapType, int wrapValue)
{
    SE_Renderer_SetTexWrap((enum SE_TEX_TARGET)target, (enum SE_TEX_WRAP_TYPE)wrapType, (enum SE_TEX_WRAP_VALUE)wrapValue);
}
static void SEC_SetTexFilter(void* runEnv, int target, int mag, int min)
{
    SE_Renderer_SetTexFilter((enum SE_TEX_TARGET)target, (enum SE_TEX_FILTER)mag, (enum SE_TEX_FILTER)min);
}
static void SEC_SetTexEnv(void* runEnv, int env)
{
    SE_Renderer_SetTexEnv((enum SE_TEX_ENV)env);
}
static void SEC_BindTexture(void* runEnv, int target)
{
    SE_Script_RunEnv* currEnv = (SE_Script_RunEnv*)runEnv;
    /* LOGI("spatial = %p\n", currEnv->data); */
    SE_ASSERT(currEnv->data != NULL);
    SE_Spatial* spatial = (SE_Spatial*)currEnv->data;
    SE_ResourceManager* resourceManager = spatial->resourceManager;
    SE_ASSERT(resourceManager != NULL);
    SE_Mesh* mesh = spatial->mesh;
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
        if(md != NULL)
        {
            if(!SE_String_IsEmpty(&md->texturename))
            {
                SE_Renderer_EnableState(SE_TEX_2D);
                SE_Renderer_BindTexture(resourceManager, (enum SE_TEX_TARGET)target, SE_String_GetData(&md->texturename));
            }
            else
            {
                SE_Renderer_DisableState(SE_TEX_2D);
                SE_Renderer_EnableState(SE_LIGHT);
                SE_Renderer_SetAmbientMaterial(md->ambient.x, md->ambient.y, md->ambient.z, 1.0f);
                //SE_Renderer_SetDiffuseMaterial(md->diffuse);
            }
        } 
        else
        {
            SE_Renderer_DisableState(SE_TEX_2D);
            SE_Renderer_DisableState(SE_LIGHT);
            SE_Renderer_SetColor(mesh->wireframeColor.x, mesh->wireframeColor.y, mesh->wireframeColor.z, 1.0);
        }

    } 
}
static void SEC_EnableState(void* runEnv, int s)
{
    /*LOGI("### enable %d ###\n", s);*/
    SE_Renderer_EnableState((enum SE_GL_STATE)s);
}
static void SEC_DisableState(void* runEnv, int s)
{
    SE_Renderer_DisableState((enum SE_GL_STATE)s);
}
static SE_Script_GlobalEntry entryMap[] = {
    {"SEC_GetTextureName", (void *)&SEC_GetTextureName},
    {"SEC_LOGF", (void*)&SEC_LOGF},
    {"SEC_LOGP", (void*)&SEC_LOGP},
    {"SEC_BindTexture", (void*)&SEC_BindTexture},
    {"SEC_SetTexEnv", (void*)&SEC_SetTexEnv},
    {"SEC_SetTexFilter", (void*)&SEC_SetTexFilter},
    {"SEC_SetTexWrap", (void*)&SEC_SetTexWrap},
    {"SEC_EnableState", (void*)&SEC_EnableState},
    {"SEC_DisableState", (void*)&SEC_DisableState}
};

static ACCvoid* symbolLookup(ACCvoid* pContext, const ACCchar* name) 
{
    int count = sizeof(entryMap) / sizeof(SE_Script_GlobalEntry);
    int i;
    for(i = 0 ; i < count ; i++)
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
SE_Result SE_Script_Init(SE_Script* script, const char* scriptSource, int scriptLen)
{
    SE_ASSERT(script);
    SE_ASSERT(scriptSource);
    SE_Object_Clear(script, sizeof(SE_Script));
    script->script = accCreateScript();
    const ACCchar* text[] = {scriptSource};
    accScriptSource(script->script, 1, text, &scriptLen);
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
    accGetScriptLabel(script->script, "main", (ACCvoid**)&mainPointer);
    int result = accGetError(script->script);
    if (result != ACC_NO_ERROR) 
    {
        LOGI("Could not find main: %d\n", result);
    } 
    else 
    {
        result = run(mainPointer, &script->runEnv);
    }
    return SE_VALID;
}

void SE_Script_Release(void* script)
{
    SE_Script* s = (SE_Script*)script;
    if(s->script)
    {
        accDeleteScript(s->script);
    }
}

