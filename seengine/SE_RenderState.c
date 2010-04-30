#include "SE_RenderState.h"
#include "SE_ResourceManager.h"
SE_Result SE_RenderState_Init(SE_RenderState* rs, SE_ResourceManager* resourceManager)
{
    SE_Object_Clear(rs, sizeof(SE_RenderState));
    rs->resourceManager = resourceManager;
    return SE_VALID;
}
void SE_RenderState_Release(void* rs)
{
    SE_RenderState* renderState = (SE_RenderState*)rs;
    int i;
    for(i = 0 ; i < SE_RS_NUM ; i++)
    {
        SE_String_Release(&renderState->rsu[i].scriptName);
    }
}
void SE_RenderState_Activate(SE_RenderState* rs, SE_Spatial_tag* spatial)
{
    int i;
    for(i = 0  ; i < SE_RS_NUM ; i++)
    {
        SE_RenderStateUnit* ru = &rs->rsu[i];
        SE_Script* s = SE_ResourceManager_GetScript(rs->resourceManager, SE_String_GetData(&ru->scriptName));
        if(s != NULL)
        {
            s->runEnv.data = spatial;
            SE_ResourceManager_RunScript(rs->resourceManager, SE_String_GetData(&ru->scriptName));
        }
    }
}
void SE_RenderState_Update(SE_RenderState* dstRs, SE_RenderState* srcRs)
{
    int i;
    for(i = 0 ; i < SE_RS_NUM ; i++)
    {
        SE_RenderStateUnit* dstu = &dstRs->rsu[i];
        SE_RenderStateUnit* srcu = &srcRs->rsu[i];
        if(SE_String_Compare(dstu->scriptName, srcu->scriptName) != 0)
        {
            SE_String_Copy(&dstu->scriptName, &srcu->scriptName);
        }
    }
    dstRs->resourceManager = srcRs->resourceManager;
}
