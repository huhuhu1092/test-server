#include "SE_Script.h"
#include "SE_Utils.h"
#include "SE_Spatial.h"
#include "SE_Log.h"
#include "./cscript/acc.h"
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
static void SEC_LOGI(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    LOGVA(fmt, ap);
    va_end(ap);
}
static SE_Script_GlobalEntry entryMap[] = {
    {"SEC_GetTextureName", (void *)&SEC_GetTextureName},
    {"SEC_LOGI", (void*)&SEC_LOGI}
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

