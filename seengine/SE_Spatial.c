#include "SE_Spatial.h"
SE_Result SE_Spatial_Init(SE_Spatial* spatial, SE_SPATIAL_TYPE spatialType, const char* name)
{
    SE_ASSERT(spatial);
    SE_Object_Clear(spatial, sizeof(SE_Spatial));
    SE_String* strName = SE_Spatial_GetName(spatial);
    spatial->spatialType = spatialType;
    SE_String_CopyFromCharArray(strName, name, 1);
    SE_Quat_Identity(&spatial->localRotaion);
    SE_Vec3f_Init(1, 1, 1, &spatial->localScale);
    return SE_VALID;
}
void SE_Spatial_Release(void* s)
{
    SE_Spatial* spatial = (SE_Spatial*)s;
    SE_ASSERT(spatial);
    SE_BoundingVolume_Release(&spatial->worldBV);
    SE_BoungingVolume_Release(&spatial->localBV);
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
    SE_Spatial* parent = SE_Spatial_GetParent(spatial);
    if(parent)
    {
        SE_Matrix4f* parentWorldTransform = SE_Spatial_GetWorldTransform(parent);
        SE_Matrix4f localTransform;
        SE_CreateTransformByRST(&spatial->localRotation, &spatial->localScale, &spatial->localTranslation, &localTransform);
        SE_Mat4f_Mul(parentWorldTransform, localTransform, &spatial->worldTransform);
    } 
    else
    {
        SE_CreateTransformByRST(&spatial->localRotation, &spatial->localScale, &spatail->localTranslation, &spatial->worldTransform);
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
    SE_UpdateWorldTransform(spatial);
    SE_UpdateWorldBV(spatial);
    return SE_VALID:
}
SE_Result SE_Spatial_UpdateRenderState(SE_Spatial* spatial)
{}
int SE_Spaital_HasChildren(SE_Spatial* spatial)
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

