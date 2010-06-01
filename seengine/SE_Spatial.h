#ifndef SE_SPATIAL_H
#define SE_SPATIAL_H

#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_BoundingVolume.h"
#include "SE_Matrix.h"
#include "SE_RenderState.h"
#include "SE_String.h"
#include "SE_List.h"
#include "SE_ResourceManager.h"

#ifdef __cplusplus
extern "C" {
#endif

struct SE_Mesh_tag;
enum SE_SPATIAL_TYPE {SE_SPATIAL, SE_NODE, SE_GEOMETRY};
enum SE_SPATIAL_RENDER_TYPE {SE_NO_RENDER, SE_RENDERABLE};
enum SE_SPATIAL_COLLISION_TYPE {SE_NO_COLLISION, SE_COLLISIONABLE};
enum SE_SPATIAL_MOVE_TYPE {SE_STATIC, SE_MOVABLE};
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDTRANSLATION)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDSCALE)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDROTATION)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDTRANSFORM)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDBV)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEGEOMETRICSTATE)(void* spatial);
typedef struct SE_Spatial_tag
{
    enum SE_SPATIAL_TYPE spatialType;
    enum SE_SPATIAL_RENDER_TYPE renderType;
    enum SE_SPATIAL_COLLISION_TYPE collisionType;
    enum SE_SPATIAL_MOVE_TYPE moveType;
    struct SE_Spatial_tag* parent;
    SE_Quat localRotation;
    SE_Vector3f localTranslation;
    SE_Vector3f localScale;
    SE_Matrix4f worldTransform;
    SE_BoundingVolume* worldBV;
    SE_BoundingVolume* localBV;
    SE_RenderState renderState;
    SE_String name;
    SE_List* children;
    struct SE_Mesh_tag* mesh;
    int subMeshIndex; //-1 : indicate this is not submesh
    SE_ResourceManager* resourceManager;
    SE_SPATIAL_UPDATEWORLDTRANSLATION fUpdateWorldTranslation;
    SE_SPATIAL_UPDATEWORLDSCALE fUpdateWorldScale;
    SE_SPATIAL_UPDATEWORLDROTATION fUpdateWorldRotation;
    SE_SPATIAL_UPDATEWORLDTRANSFORM fUpdateTransform;
    SE_SPATIAL_UPDATEWORLDBV fUpdateBV;
    SE_SPATIAL_UPDATEGEOMETRICSTATE fUpdateGeometricState;
} SE_Spatial;
/** intersection data with spatial*/
typedef struct SE_IntersectionSpatialData_tag
{
    SE_Spatial* spatial;
    SE_IntersectionResult intersectionResult;
} SE_IntersectionSpatialData;
extern void SE_IntersectionSpatialData_Release(void* isd);
/**
 * this function create a new SE_Spatial, you has the responsibility to release it, or add it to its parent spatial.
 * */
extern SE_Spatial* SE_Spatial_Create();
extern SE_Result SE_Spatial_Init(SE_Spatial* spatial, enum SE_SPATIAL_TYPE spatialType ,const char* name, SE_ResourceManager* resourceManager, struct SE_Mesh_tag* mesh);
extern void SE_Spatial_Release(void* spatial);
extern SE_Result SE_Spatial_Copy(const SE_Spatial* spatialSrc, SE_Spatial* spatialDst);
extern SE_Result SE_Spatial_GetLocalRotateQuat(const SE_Spatial* spatial, SE_Quat* out);
extern SE_Result SE_Spatial_GetLocalTranslate(const SE_Spatial* spatial, SE_Vector3f* out);
extern SE_Result SE_Spatial_GetLocalScale(const SE_Spatial* spatial, SE_Vector3f* out);
extern SE_Result SE_Spatial_SetLocalRotateQuat(SE_Spatial* spatail, const SE_Quat* quat);
extern SE_Result SE_Spatial_SetLocalTranslate(SE_Spatial* spatial, const SE_Vector3f* translate);
extern SE_Result SE_Spatial_SetLocalScale(SE_Spatial* spatial, const SE_Vector3f* scale);
extern SE_Result SE_Spatial_UpdateWorldTransform(SE_Spatial* spatial);
extern SE_Result SE_Spatial_UpdateWorldBV(SE_Spatial* spatial);
extern SE_Result SE_Spatial_UpdateGeometricState(SE_Spatial* spatial);
extern SE_Result SE_Spatial_UpdateRenderState(SE_Spatial* spatial);
extern int SE_Spatial_HasChildren(SE_Spatial* spatial);
extern int SE_Spatial_GetChildrenNum(SE_Spatial* spatial);
extern SE_Result SE_Spatial_AddChild(SE_Spatial* parent, SE_Spatial* child);
extern SE_Result SE_Spatial_RemoveChild(SE_Spatial* parent, SE_Spatial* child);
extern SE_Result SE_Spatial_RemoveChildByName(SE_Spatial* parent, SE_String name);
extern SE_Result SE_Spatial_SetRenderState(SE_Spatial* spatial, enum SE_RS_TYPE rsType, const char* scriptname);
extern SE_Result SE_Spatial_CreateLocalBV(SE_Spatial* spatial, enum SE_BVType bvType);
extern SE_Result SE_Spatial_IntersectRay(SE_Spatial* spatial, SE_Ray* ray, SE_List* spatialList);
extern int SE_Spatial_MovingSphereIntersect(SE_Sphere* s, SE_Vector3f endPoint,SE_Spatial* spatial, SE_Vector3f* out);
extern int SE_Spatial_HasSubMesh(const SE_Spatial* spatial);
extern SE_Result SE_Spatial_SetMoveType(SE_Spatial* spatial, enum SE_SPATIAL_MOVE_TYPE moveType);
/**
 *
 * */
extern SE_Result SE_Spatial_MoveByLocalAxis(SE_Spatial* root, SE_Spatial* spatial, enum SE_AXIS_TYPE axis, float dist);
extern SE_Result SE_Spatial_RotateByLocalAxis(SE_Spatial* spatial, enum SE_AXIS_TYPE axis, float angle); 
extern SE_Spatial* SE_Spatial_Find(SE_Spatial* parent, SE_String name);
#ifdef __cplusplus
}
#endif
#endif
