#include "SE_Spatial.h"
#include "SE_Memory.h"
#include "SE_Utils.h"
#include "SE_ResourceManager.h"
#include "SE_Script.h"
#include "SE_GeometryIntersect.h"
#include "SE_Log.h"
SE_Spatial* SE_Spatial_Create()
{
    SE_Spatial* s = (SE_Spatial*)SE_Malloc(sizeof(SE_Spatial));
    SE_Object_Clear(s, sizeof(SE_Spatial));
    return s;
}
SE_Result SE_Spatial_Init(SE_Spatial* spatial, enum SE_SPATIAL_TYPE spatialType, const char* name, SE_ResourceManager* resourceManager, struct SE_Mesh_tag* mesh)
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
    spatial->renderType = SE_RENDERABLE;
    spatial->resourceManager = resourceManager;
    SE_RenderState_Init(&spatial->renderState, resourceManager);
    spatial->collisionType = SE_COLLISIONABLE;
    return SE_VALID;
}
void SE_Spatial_Release(void* s)
{
    SE_Spatial* spatial = (SE_Spatial*)s;
    SE_ASSERT(spatial);
    SE_BoundingVolume_Release(spatial->worldBV);
    if(spatial->worldBV)
        SE_Free(spatial->worldBV);
    SE_BoundingVolume_Release(spatial->localBV);
    if(spatial->localBV)
        SE_Free(spatial->localBV);
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
        SE_Element e;
        SE_ListIterator li;
        SE_ListIterator_Init(&li, spatial->children);
        while(SE_ListIterator_Next(&li, &e))
        {
            SE_Spatial* child = (SE_Spatial*)e.dp.data;
            SE_ASSERT(child->parent == spatial);
            SE_Spatial_UpdateWorldTransform(child);
        }
    }
    return SE_VALID;
}
static void createWorldBVFromLocalBV(SE_Spatial* spatial)
{
    if(spatial->worldBV)
    {
        if(spatial->worldBV->fRelease)
        {
            (*spatial->worldBV->fRelease)(spatial->worldBV);
        }
        SE_Free(spatial->worldBV);
        spatial->worldBV = NULL;
    }
    SE_ASSERT(spatial->worldBV == NULL);
    if(spatial->localBV)
    {
        spatial->worldBV =  SE_BoundingVolume_Clone(spatial->localBV);
    }
}
struct ContextDataForWorldBV
{
    int subMeshNum;
};
static void travelListToUpdateWorldBV(SE_Element* e, void* context)
{
    SE_Spatial* spatial = (SE_Spatial*)e->dp.data;
    struct ContextDataForWorldBV* d = (struct ContextDataForWorldBV*)context;
    if(spatial->subMeshIndex != -1)
    {
        d->subMeshNum++;
        return;
    }
    else
    {
        SE_Spatial_UpdateWorldBV(spatial);
    }
}
SE_Result SE_Spatial_UpdateWorldBV(SE_Spatial* spatial)
{
    /**
     * this implementation is a special case. we need to change 
     * it to a general version
     * */
    if(!spatial)
        return SE_VALID;
    SE_List* children = spatial->children;
    if(spatial->spatialType == SE_NODE)
    {
        struct ContextDataForWorldBV d;
        SE_Object_Clear(&d, sizeof(struct ContextDataForWorldBV));
        if(spatial->children)
        {
            SE_List_Apply(spatial->children, &travelListToUpdateWorldBV, &d);
            int childsize = SE_List_Size(spatial->children);
            if(d.subMeshNum > 0)
            {
                SE_ASSERT(d.subMeshNum == childsize);
                createWorldBVFromLocalBV(spatial);
            }
            
        }
        else
        {
            LOGI("node spatial has no children\n");
        }
    }
    else if(spatial->spatialType == SE_GEOMETRY)
    {
        createWorldBVFromLocalBV(spatial); 
    }
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
SE_Result SE_Spatial_CreateLocalBV(SE_Spatial* spatial, enum SE_BVType bvType)
{
    SE_Mesh* mesh = spatial->mesh;
    SE_ResourceManager* resourceManager = spatial->resourceManager;
    SE_GeometryData* geomData;
    SE_SphereBV* sphereBV = NULL;
    SE_AABBBV* aabbBV = NULL;
    if(!mesh)
        return SE_INVALID;
    geomData = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
    switch(bvType)
    {
    case SE_SPHERE_E:
        sphereBV = (SE_SphereBV*)SE_Malloc(sizeof(SE_SphereBV));
        if(!sphereBV)
            LOGE("out of memory when alloc sphere\n");
        SE_SphereBV_CreateFromPoints(sphereBV, geomData->vertexArray, geomData->vertexNum);
        spatial->localBV = (SE_BoundingVolume*)sphereBV;
        break;
    case SE_AABB_E:
        aabbBV = (SE_AABBBV*)SE_Malloc(sizeof(SE_AABBBV));
        SE_AABBBV_CreateFromPoints(aabbBV, geomData->vertexArray, geomData->vertexNum);
        spatial->localBV = (SE_BoundingVolume*)aabbBV;
        break;
    default:
        LOGE("can not implement \n");
        break;
    }
    return SE_VALID;
}
struct ContextData
{
    SE_Ray* ray;
    SE_List* spatialList;
};
static void travelSpatialChildren(SE_Element* e, void* context);
static int spatialIntersectRay(SE_Spatial* spatial, SE_Ray* ray, SE_List* spatialList)
{
    SE_BoundingVolume* bv = spatial->worldBV;
    if(bv)
    {
        SE_IntersectionResult result;
        SE_Object_Clear(&result, sizeof(SE_IntersectionResult));
        bv->fIntersectRayDetail(bv, ray, &result);
        if(result.intersected == 0)
            return 0;
        SE_List* children = spatial->children;
        if(children)
        {
            struct ContextData contextData;
            contextData.ray = ray;
            contextData.spatialList = spatialList;
            SE_List_Apply(children, &travelSpatialChildren, &contextData);
        }
        else
        {
            SE_Element e;
            e.type = SE_DATA;
            SE_IntersectionSpatialData* element = (SE_IntersectionSpatialData*)SE_Malloc(sizeof(SE_IntersectionSpatialData));
            if(element)
            {
                element->intersectionResult = result;
                element->spatial = spatial;
                e.dp.data = element;
                e.dp.fRelease = &SE_IntersectionSpatialData_Release;
                SE_List_AddLast(spatialList, e);
            }
        }
        return 0;
    }
    else
    {
        return 0;
    }
}
static void travelSpatialChildren(SE_Element* e, void* context)
{
    SE_Spatial* s = (SE_Spatial*)e->ptr;
    SE_ASSERT(e);
    SE_ASSERT(context);
    struct ContextData* contextData = (struct ContextData*)context;
    spatialIntersectRay(s, contextData->ray, contextData->spatialList);
}
SE_Result SE_Spatial_IntersectRay(SE_Spatial* spatial, SE_Ray* ray, SE_List* spatialList)
{
    if(!spatial)
        return SE_INVALID;
    if(!ray)
        return SE_INVALID;
    if(!spatialList)
        return SE_INVALID;
    spatialIntersectRay(spatial, ray, spatialList);
    return SE_VALID;
}
int SE_Spatial_MovingSphereIntersect(SE_Sphere* s, SE_Vector3f endPoint, SE_Spatial* spatial, SE_Vector3f* out)
{
    int ret = 0;
    SE_AABBBV* bv = NULL;
    if(!spatial)
        return 0;
    if(spatial->spatialType == SE_GEOMETRY && spatial->collisionType == SE_COLLISIONABLE)
    {
        bv = (SE_AABBBV*)spatial->worldBV;
        ret = SE_Intersect_MovingSphereStaticAABB(*s, &bv->aabb, endPoint, out);
        return ret;
    }
    else if(spatial->spatialType == SE_NODE)
    {
        SE_List* children = spatial->children;
        SE_ListIterator li;
        SE_Element e;
        SE_ListIterator_Init(&li, children);
        while(SE_ListIterator_Next(&li, &e))
        {
            SE_Spatial* childs = (SE_Spatial*)e.dp.data;
            bv = (SE_AABBBV*)childs->worldBV;
            if(childs->collisionType == SE_COLLISIONABLE)
                ret = SE_Intersect_MovingSphereStaticAABB(*s, &bv->aabb, endPoint, out);
            else 
                ret = 0;
            if(ret)
            {
                LOGI("### move intersect obj = %s ####\n", SE_String_GetData(&childs->name));
                return 1;
            }
        }
    }
    return 0;
}
/** */
void SE_IntersectionSpatialData_Release(void* isd)
{
    SE_IntersectionSpatialData* spatialData = (SE_IntersectionSpatialData*)isd;
    SE_IntersectionResult_Release(&spatialData->intersectionResult);
}
