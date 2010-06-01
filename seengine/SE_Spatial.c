#include "SE_Spatial.h"
#include "SE_Memory.h"
#include "SE_Utils.h"
#include "SE_ResourceManager.h"
#include "SE_Script.h"
#include "SE_GeometryIntersect.h"
#include "SE_Math.h"
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
    SE_Mat4f_Identity(&spatial->worldTransform);
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
        /*
        SE_Matrix3f rotateMatrix;
        SE_Vector3f translate;
        //SE_Mat4f_GetMatrix3fAndTranslate(&spatial->worldTransform, &rotateMatrix, &translate);
        SE_Quat_ToMatrix3f(&spatial->localRotation, &rotateMatrix);
        spatial->worldBV =  SE_BoundingVolume_Clone(spatial->localBV);
        spatial->worldBV->fTransform(spatial->localBV, &rotateMatrix, &spatial->localTranslation, &spatial->localScale, spatial->worldBV); 
        */
        SE_Mesh* mesh = spatial->mesh;
        SE_ResourceManager* resourceManager = spatial->resourceManager;
        SE_GeometryData* geomData;
        SE_SphereBV* sphereBV = NULL;
        SE_AABBBV* aabbBV = NULL;
        if(!mesh)
            return ;
        geomData = SE_ResourceManager_GetGeometryData(resourceManager, mesh->geomDataIndex);
        switch(spatial->localBV->type)
        {
        case SE_SPHERE_E:
            break;
        case SE_AABB_E:
            {
                int i;
                aabbBV = (SE_AABBBV*)SE_Malloc(sizeof(SE_AABBBV));
                SE_Vector3f* worldVertexData = (SE_Vector3f*)SE_Malloc(sizeof(SE_Vector3f) * geomData->vertexNum);
                for(i = 0 ; i < geomData->vertexNum ; i++)
                {
                    SE_Vector4f v1, v2;
                    v1.x = geomData->vertexArray[i].x;
                    v1.y = geomData->vertexArray[i].y;
                    v1.z = geomData->vertexArray[i].z;
                    v1.w = 1.0f;
                    SE_Mat4f_Map(&spatial->worldTransform, &v1, &v2);
                    worldVertexData[i].x = v2.x;
                    worldVertexData[i].y = v2.y;
                    worldVertexData[i].z = v2.z;
                }
                SE_AABBBV_CreateFromPoints(aabbBV, worldVertexData, geomData->vertexNum);
                SE_Free(worldVertexData);
                spatial->worldBV = (SE_BoundingVolume*)aabbBV;
#ifdef DEBUG
                SE_String ttt;
                SE_String_Init(&ttt, "Box04_2");
                if(SE_String_Compare(ttt, spatial->name) == 0)
                {
                    LOGI("## min.x = %f, y = %f, z = %f", aabbBV->aabb.min.x, aabbBV->aabb.min.y, aabbBV->aabb.min.z);
                    LOGI("## max.x = %f, y = %f, z = %f", aabbBV->aabb.max.x, aabbBV->aabb.max.y, aabbBV->aabb.max.z);

                }
#endif
            }
            break;
        default:
            LOGE("can not implement \n");
            break;
        }
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
    child->parent = parent;
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
int SE_Spatial_HasSubMesh(const SE_Spatial* spatial)
{
    if(!spatial)
        return 0;
    if(!spatial->mesh)
        return 0;
    return spatial->mesh->subMeshNum > 0;
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
        if(children && !SE_Spatial_HasSubMesh(spatial))
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
struct _IntersectPointData
{
    SE_Spatial* spatial;
    SE_Vector3f intersectPoint;
};
static void addIntersectPointToList(SE_Spatial* spatial, SE_Sphere* s, SE_AABB* aabb, SE_Vector3f endPoint, SE_List* intersectPointList)
{
    int ret = 0;
    SE_Vector3f intersectPoint;
    ret = SE_Intersect_MovingSphereStaticAABB(*s, aabb, endPoint, &intersectPoint);
    if(ret)
    {
        SE_Element e;
        struct _IntersectPointData* ipd = NULL;
        SE_Object_Clear(&e, sizeof(SE_Element));
        e.type = SE_DATA;
        ipd = (struct _IntersectPointData*)SE_Malloc(sizeof(struct _IntersectPointData));
        if(ipd == NULL)
        {
            LOGE("### out of memory when moveing sphere ###\n");
            return;
        }
        SE_Vec3f_Copy(&intersectPoint, &ipd->intersectPoint);
        ipd->spatial = spatial;
        e.dp.data = ipd;
        SE_List_AddLast(intersectPointList, e);
    }

}
int SE_Spatial_MovingSphereIntersect(SE_Sphere* s, SE_Vector3f endPoint, SE_Spatial* spatial, SE_Vector3f* out)
{
    int ret = 0;
    SE_AABBBV* aabbBV = NULL;
    SE_List intersectPointList;
    SE_ListIterator intersectPointListIt;
    SE_Vector3f intersectPoint;
    SE_Element intersectPointData;
    int size = 0;
    float minPoint = SE_FLT_MAX;
    struct _IntersectPointData* minIntersectPoint = NULL;
    if(!spatial)
        return 0;
    SE_List_Init(&intersectPointList);
    if(spatial->spatialType == SE_GEOMETRY && spatial->collisionType == SE_COLLISIONABLE)
    {
        switch(spatial->worldBV->type)
        {
        case SE_AABB_E:
            aabbBV = (SE_AABBBV*)spatial->worldBV;
            addIntersectPointToList(spatial, s, &aabbBV->aabb, endPoint, &intersectPointList);
            break;
        }
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
            if(childs->collisionType == SE_COLLISIONABLE)
            {
                switch(childs->worldBV->type)
                {
                case SE_AABB_E:
                    aabbBV = (SE_AABBBV*)childs->worldBV;
                    addIntersectPointToList(childs, s, &aabbBV->aabb, endPoint, &intersectPointList);
                    break;
                }
            }
        }
    }
    size = SE_List_Size(&intersectPointList);
    if(size == 0)
    {
        ret = 0;
    }
    else
    {
        ret = 1;
    }
    LOGI("### intersect obj size = %d ###\n", size);
    SE_ListIterator_Init(&intersectPointListIt, &intersectPointList);
    while(SE_ListIterator_Next(&intersectPointListIt, &intersectPointData))
    {
        struct _IntersectPointData* ipd = (struct _IntersectPointData*)intersectPointData.dp.data;
        SE_Vector3f distV;
        float len;
        SE_Vec3f_Subtract(&ipd->intersectPoint, &s->center, &distV);
        len = SE_Vec3f_LengthSquare(&distV) ;
        if(len < minPoint)
        {
            minPoint = len;
            minIntersectPoint = ipd;
        }
        
    }
    if(minIntersectPoint)
    {
        LOGI("### move intersect obj = %s ####\n", SE_String_GetData(&minIntersectPoint->spatial->name));
        SE_Vec3f_Copy(&minIntersectPoint->intersectPoint, out);
    }
    else
    {
        ret = 0;
    }
    SE_List_Release(&intersectPointList);
    return ret;
}
/** */
void SE_IntersectionSpatialData_Release(void* isd)
{
    SE_IntersectionSpatialData* spatialData = (SE_IntersectionSpatialData*)isd;
    SE_IntersectionResult_Release(&spatialData->intersectionResult);
}
SE_Result SE_Spatial_SetMoveType(SE_Spatial* spatial, enum SE_SPATIAL_MOVE_TYPE moveType)
{
    SE_ASSERT(spatial);
    spatial->moveType = moveType;
    return SE_VALID;
}
static void getMoveDir(SE_AXIS_TYPE axis, float dist, SE_Vector3f* out)
{
    SE_Vector3f xv, yv, zv;
    SE_Vec3f_Init(1,0,0, &xv);
    SE_Vec3f_Init(0,1,0, &yv);
    SE_Vec3f_Init(0,0,1, &zv);
    switch(axis)
    {
    case SE_AXIS_X:
        SE_Vec3f_Mul(&xv, dist, out);
        break;
    case SE_AXIS_Y:
        SE_Vec3f_Mul(&yv, dist, out);
        break;
    case SE_AXIS_Z:
        SE_Vec3f_Mul(&zv, dist, out);
        break;
    }
}
struct _IntersectMoveOBBData
{
    SE_Spatial* spatial;
    SE_OBB obb;
};
static void calculateIntersectSpatialByMovingOBB(SE_Spatial* root, SE_Spatial* moveSpatial, SE_OBB obb, SE_Vector3f endPoint, SE_AXIS_TYPE axis, SE_List* obbList)
{
    SE_AABBBV* aabbBV = NULL;
    int ret = 0;
    /*
    if(root->worldBV == NULL)
        return ;
        */
    if(root == moveSpatial)
        return ;
    if(root->collisionType == SE_NO_COLLISION)
        return;
    if(root->spatialType == SE_GEOMETRY || (root->spatialType == SE_NODE && SE_Spatial_HasSubMesh(root)))
    {
        switch(root->worldBV->type)
        {
        case SE_AABB_E:
            {
                SE_OBB obbIntersect;
                aabbBV = (SE_AABBBV*)root->worldBV;
                ret = SE_Intersect_MovingOBBStaticAABB(obb, &aabbBV->aabb, endPoint, axis, &obbIntersect);
                if(ret)
                {
                    SE_Element e;
                    struct _IntersectMoveOBBData* oe;
                    e.type = SE_DATA;
                    oe = (struct _IntersectMoveOBBData*)SE_Malloc(sizeof(struct _IntersectMoveOBBData));
                    if(oe)
                    {
                        oe->obb = obbIntersect;
                        oe->spatial = root;
                        e.dp.data = oe;
                        SE_List_AddLast(obbList, e);
                    }
                }
            }
            break;
        case SE_SPHERE_E:
            break;
        case SE_OBB_E:
            break;
        }
    }
    else if(root->spatialType == SE_NODE)
    {
        SE_List* children = root->children;
        if(SE_Spatial_HasSubMesh(root))
        {
            calculateIntersectSpatialByMovingOBB(root, moveSpatial, obb, endPoint, axis, obbList);
        }
        else
        {
            SE_Element e;
            SE_ListIterator it;
            SE_ListIterator_Init(&it, children);
            while(SE_ListIterator_Next(&it, &e))
            {
                SE_Spatial* schild = (SE_Spatial*)e.dp.data;
                calculateIntersectSpatialByMovingOBB(schild, moveSpatial, obb, endPoint, axis, obbList);
            }
        }
    }
}
SE_Result SE_Spatial_MoveByLocalAxis(SE_Spatial* root, SE_Spatial* spatial, SE_AXIS_TYPE axis, float dist)
{
    SE_AABBBV* aabbBV = NULL;
    SE_ASSERT(spatial);
    if(spatial->worldBV == NULL)
    {
        LOGI("spatial worldBV is null\n");
        return SE_VALID; 
    } 
    switch(spatial->worldBV->type)
    {
    case SE_AABB_E:
        {
            SE_Vector3f endPoint, localTranslate, moveDir;
            SE_OBB aabbOBB, endOBB;
            int size = 0;
            SE_List obbList;
            aabbBV = (SE_AABBBV*)spatial->worldBV;
            SE_OBB_CreateFromAABB(&aabbOBB, &aabbBV->aabb, SE_AXIS_NOAXIS, 0.0f);
            getMoveDir(axis, dist, &moveDir);
            SE_Vec3f_Add(&aabbOBB.center, &moveDir, &endPoint);
            SE_List_Init(&obbList);
            calculateIntersectSpatialByMovingOBB(root, spatial, aabbOBB, endPoint, axis, &obbList);
            size = SE_List_Size(&obbList);
#ifdef DEBUG
            LOGI("### move obb size = %d ###\n", size);
#endif
            if(size > 0)
            {
                SE_ListIterator it;
                SE_Element e;
                struct _IntersectMoveOBBData* minOBB = NULL;
                float minDist = SE_FLT_MAX;
                SE_ListIterator_Init(&it, &obbList);
                while(SE_ListIterator_Next(&it, &e))
                {
                    struct _IntersectMoveOBBData* s = (struct _IntersectMoveOBBData*)e.dp.data;
                    SE_Vector3f distV;
                    float dist;
                    SE_Vec3f_Subtract(&s->obb.center, &aabbOBB.center, &distV);
                    dist = SE_Vec3f_LengthSquare(&distV);
                    if(dist < minDist)
                    {
                        minDist = dist;
                        minOBB = s;
                    }
                }
                endOBB = minOBB->obb;
#ifdef DEBUG
                LOGI("### move obb intersect object = %s ###\n", SE_String_GetData(&minOBB->spatial->name));
#endif
                SE_Vec3f_Subtract(&endOBB.center, &aabbOBB.center, &moveDir);
            }
            SE_Spatial_GetLocalTranslate(spatial, &localTranslate);
            SE_Vec3f_Add(&localTranslate, &moveDir, &localTranslate);
            SE_Spatial_SetLocalTranslate(spatial, &localTranslate);
            SE_Spatial_UpdateGeometricState(spatial);
            SE_List_Release(&obbList);
        }
        break;
    case SE_SPHERE_E:
        break;
    case SE_OBB_E:
        break;
    }
    return SE_VALID;
}
SE_Result SE_Spatial_RotateByLocalAxis(SE_Spatial* spatial, SE_AXIS_TYPE axis, float angle)
{
    return SE_VALID;
}
SE_Spatial* SE_Spatial_Find(SE_Spatial* parent, SE_String name)
{
    SE_ASSERT(parent);
    SE_ListIterator li;
    SE_List* children = parent->children;
    SE_Element e;
    SE_Spatial* spatial = NULL;
    if(SE_String_Compare(parent->name, name) == 0)
    {
        return parent;
    }
    if(children == NULL)
        return NULL;
    SE_ListIterator_Init(&li, children);
    while(SE_ListIterator_Next(&li, &e))
    {
        SE_Spatial* schild = (SE_Spatial*)e.dp.data;
        spatial = SE_Spatial_Find(schild, name);
        if(spatial)
            break;
    }
    return spatial;  
}
SE_Result SE_Spatial_GetLocalRotateQuat(const SE_Spatial* spatial, SE_Quat* out)
{
    SE_ASSERT(spatial);
    SE_ASSERT(out);
    *out = spatial->localRotation;
    return SE_VALID;
}
SE_Result SE_Spatial_GetLocalTranslate(const SE_Spatial* spatial, SE_Vector3f* out)
{
    SE_ASSERT(spatial);
    SE_ASSERT(out);
    *out = spatial->localTranslation;
    return SE_VALID;
}
SE_Result SE_Spatial_GetLocalScale(const SE_Spatial* spatial, SE_Vector3f* out)
{
    SE_ASSERT(spatial);
    SE_ASSERT(out);
    *out = spatial->localScale;
    return SE_VALID;
}
SE_Result SE_Spatial_SetLocalRotateQuat(SE_Spatial* spatial, const SE_Quat* quat)
{
    SE_ASSERT(spatial);
    SE_ASSERT(quat);
    spatial->localRotation = *quat;
    return SE_VALID;
}
SE_Result SE_Spatial_SetLocalTranslate(SE_Spatial* spatial, const SE_Vector3f* translate)
{
    SE_ASSERT(spatial);
    SE_ASSERT(translate);
    spatial->localTranslation = *translate;
    return SE_VALID;
}
SE_Result SE_Spatial_SetLocalScale(SE_Spatial* spatial, const SE_Vector3f* scale)
{
    SE_ASSERT(spatial);
    SE_ASSERT(scale);
    spatial->localScale = *scale;
    return SE_VALID;
}

