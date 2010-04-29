#include "SE_Spatial.h"
#include "SE_Memory.h"
#include "SE_Utils.h"
#include "SE_ResourceManager.h"
#include "SE_Script.h"
SE_Spatial* SE_Spatial_Create()
{
    SE_Spatial* s = (SE_Spatial*)SE_Malloc(sizeof(SE_Spatial));
    SE_Object_Clear(s, sizeof(SE_Spatial));
    return s;
}
SE_Result SE_Spatial_Init(SE_Spatial* spatial, SE_SPATIAL_TYPE spatialType, const char* name, SE_ResourceManager* resourceManager,SE_Mesh_tag* mesh)
{
    SE_ASSERT(spatial);
    SE_Object_Clear(spatial, sizeof(SE_Spatial));
    SE_String* strName = &spatial->name;
    spatial->spatialType = spatialType;
    SE_String_Init(strName, name);
    SE_Quat_Identity(&spatial->localRotation);
    SE_Vec3f_Init(1, 1, 1, &spatial->localScale);
    spatial->subMeshIndex = -1;
    spatial->mesh = mesh;
    spatial->resourceManager = resourceManager;
    SE_RenderState_Init(&spatial->renderState, resourceManager);
    return SE_VALID;
}
void SE_Spatial_Release(void* s)
{
    SE_Spatial* spatial = (SE_Spatial*)s;
    SE_ASSERT(spatial);
    SE_BoundingVolume_Release(&spatial->worldBV);
    SE_BoundingVolume_Release(&spatial->localBV);
    SE_RenderState_Release(&spatial->renderState);
    SE_String_Release(&spatial->name);
    if(spatial->children)
    {
        SE_List_Release(spatial->children);
        SE_Free(spatial->children);
    }
}
SE_Result SE_Spatial_Copy(const SE_Spatial* spatialSrc, SE_Spatial* spatialDst)
{
    SE_ASSERT(spatialSrc);
    SE_ASSERT(spatialDst);
    return SE_VALID;
}
SE_Result SE_Spatial_UpdateWorldTransform(SE_Spatial* spatial)
{
    SE_ASSERT(spatial);
    SE_Spatial* parent = spatial->parent;
    if(parent)
    {
        SE_Matrix4f* parentWorldTransform = &parent->worldTransform;
        SE_Matrix4f localTransform;
        SE_CreateTransformByRST(&spatial->localRotation, &spatial->localScale, &spatial->localTranslation, &localTransform);
        SE_Mat4f_Mul(parentWorldTransform, &localTransform, &spatial->worldTransform);
    } 
    else
    {
        SE_CreateTransformByRST(&spatial->localRotation, &spatial->localScale, &spatial->localTranslation, &spatial->worldTransform);
    }
    if(spatial->children)
    {
        int size = SE_List_Size(spatial->children);
        int i;
        for(i = 0 ; i < size; i++)
        {
            SE_Element e = SE_List_GetAt(spatial->children, i);
            SE_Spatial* child = (SE_Spatial*)e.dp.data;
            SE_ASSERT(child->parent == spatial);
            SE_Spatial_UpdateWorldTransform(child);
        }
    }
    return SE_VALID;
}
SE_Result SE_Spatial_UpdateWorldBV(SE_Spatial* spatial)
{
    return SE_VALID;
}
SE_Result SE_Spatial_UpdateGeometricState(SE_Spatial* spatial)
{
    SE_Spatial_UpdateWorldTransform(spatial);
    SE_Spatial_UpdateWorldBV(spatial);
    return SE_VALID;
}
static void updateRenderStateFromRoot(SE_Spatial* spatial)
{
    if(spatial == NULL)
    {
        return ;
    }
    SE_Spatial* parent = spatial->parent;
    updateRenderStateFromRoot(parent);
    if(parent != NULL)
    {
        SE_RenderState* rs = &parent->renderState;
        SE_RenderState_Update(&spatial->renderState, rs);
    }
}
static void updateRenderStateToChild(SE_Spatial* child, SE_RenderState* parentState)
{
    SE_RenderState_Update(&child->renderState, parentState);
    if(child->children != NULL)
    {
        SE_ListIterator li;
        SE_ListIterator_Init(&li, child->children);
        SE_Element n;
        while(SE_ListIterator_Next(&li, &n))
        {
            SE_Spatial* c = (SE_Spatial*)n.dp.data;
            updateRenderStateToChild(c, &c->renderState);
        }
    }
}
SE_Result SE_Spatial_UpdateRenderState(SE_Spatial* spatial)
{
    updateRenderStateFromRoot(spatial);
    if(spatial->children != NULL)
    {
        SE_ListIterator li;
        SE_ListIterator_Init(&li, spatial->children);
        SE_Element n;
        while(SE_ListIterator_Next(&li, &n))
        {
            SE_Spatial* child = (SE_Spatial*)n.dp.data;
            updateRenderStateToChild(child, &spatial->renderState);    
        }
    } 
    return SE_VALID;
}
int SE_Spatial_HasChildren(SE_Spatial* spatial)
{
    return spatial->children != NULL;
}
int SE_Spatial_GetChildrenNum(SE_Spatial* spatial)
{
    if(SE_Spatial_HasChildren(spatial))
    {
        return SE_List_Size(spatial->children);
    }
    else
    {
        return 0;
    }
}
SE_Result SE_Spatial_AddChild(SE_Spatial* parent, SE_Spatial* child)
{
    if(parent == NULL)
        return SE_INVALID;
    if(parent->spatialType != SE_NODE)
    {
        return SE_INVALID;
    }
    if(parent->children == NULL)
    {
        parent->children = (SE_List*)SE_Malloc(sizeof(SE_List));
        SE_List_Init(parent->children);
    }
    if(parent->children == NULL)
        return SE_INVALID;
    SE_Element e;
    SE_Object_Clear(&e, sizeof(SE_Element));
    e.type = SE_DATA;
    e.dp.data = child;
    e.dp.fRelease = &SE_Spatial_Release;
    return SE_List_AddLast(parent->children, e); 
}
SE_Result SE_Spatial_RemoveChild(SE_Spatial* parent, SE_Spatial* child)
{
    if(parent == NULL)
        return SE_INVALID;
    if(parent->spatialType != SE_NODE)
    {
        return SE_INVALID;
    }
    if(parent->children == NULL)
    {
        return SE_INVALID;
    }
    SE_Element e;
    SE_Object_Clear(&e, sizeof(SE_Element));
    e.type = SE_DATA;
    e.dp.data = child;
    return SE_List_RemoveElement(parent->children, e);

}
static int compareSpatialByName(void* v1, void* v2)
{
    SE_Spatial* s1 = (SE_Spatial*)v1;
    SE_Spatial* s2 = (SE_Spatial*)v2;
    return SE_String_Compare(s1->name, s2->name);
}

SE_Result SE_Spatial_RemoveChildByName(SE_Spatial* parent, SE_String name)
{
    if(parent == NULL)
        return SE_INVALID;
    if(parent->spatialType != SE_NODE)
    {
        return SE_INVALID;
    }
    if(parent->children == NULL)
    {
        return SE_INVALID;
    }
    SE_Spatial tmpSpatial;
    tmpSpatial.name = name;
    SE_Element e;
    e.type = SE_DATA;
    e.dp.data = &tmpSpatial;
    e.dp.fCompare = &compareSpatialByName;
    return SE_List_RemoveElement(parent->children, e); 
}
SE_Result SE_Spatial_SetRenderState(SE_Spatial* spatial, enum SE_RS_TYPE rsType, const char* scriptname)
{
    if(scriptname == NULL)
        return SE_INVALID;
    SE_String* str = &spatial->renderState.rsu[rsType].scriptName;
    SE_String_CopyCharArray(str, scriptname);
    spatial->renderState.resourceManager = spatial->resourceManager;
    return SE_VALID;
}
