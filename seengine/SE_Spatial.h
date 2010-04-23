#ifndef SE_SPATIAL_H
#define SE_SPATIAL_H

#include "SE_Common.h"
#include "SE_Geometry3D.h"
#include "SE_Vector.h"
#include "SE_BoundingVolume.h"
#include "SE_Matrix.h"
#ifdef __cplusplus
extern "C" {
#endif
enum SE_SPATIAL_TYPE {SPATIAL, NODE, GEOMETRY};
enum SE_SPATIAL_RENDER_TYPE {NO_RENDER, RENDERABLE};
enum SE_SPATIAL_COLLISION_TYPE {NO_COLLISION, COLLISIONALBE};
enum SE_SPATIAL_MOVE_TYPE {STATIC, MOVABLE};
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDTRANSLATION)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDSCALE)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDROTATION)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDTRANSFORM)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEWORLDBV)(void* spatial);
typedef SE_Result (*SE_SPATIAL_UPDATEGEOMETRICSTATE)(void* spatial);
typedef SE_Spatial_tag
{
    SE_SPATIAL_TYPE spatialType;
    SE_SPATIAL_RENDER_TYPE renderType;
    SE_SPATIAL_COLLISION_TYPE collisionType;
    SE_SPATIAL_MOVE_TYPE moveType;
    struct SE_Spatial_tag* parent;
    SE_Quat localRotation;
    SE_Vector3f localTranslation;
    SE_Vector3f localScale;
    SE_Matrix4f worldTransform;
    SE_BoundingVolume worldBV;
    SE_BoundingVolume localBV;
    SE_RenderState renderState;
    SE_String name;
    SE_List* childrent;
    SE_GeometryData* geometryData;
    SE_FaceList* geometryFaces;
    SE_SPATIAL_UPDATEWORLDTRANSLATION fUpdateWorldTranslation;
    SE_SPATIAL_UPDATEWORLDSCALE fUpdateWorldScale;
    SE_SPATIAL_UPDATEWORLDROTATION fUpdateWorldRotation;
    SE_SPATIAL_UPDATEWORLDTRANSFORM fUpdateTransform;
    SE_SPATIAL_UPDATEWORLDBV fUpdateBV;
    SE_SPATIAL_UPDATEGEOMETRICSTATE fUpdateGeometricState;
} SE_Spatial;
#define SE_Spatial_GetParent(spatial) (spatial->parent)
#define SE_Spatial_GetLocalRotation(spatial) (&spatial->localRotation)
#define SE_Spatial_GetLocalTranslation(spatial) (&spatial->localTranslation)
#define SE_Spatial_GetLocalScale(spatial) (&spatial->localScale)
#define SE_Spatial_GetWorldTransform(spatial) (&spatial->worldTransform)
#define SE_Spatial_GetWorldBV(spatial) (&spatial->worldBV)
#define SE_Spatial_GetLocalBV(spatial) (&spatial->localBV)
#define SE_Spatial_GetRenderState(spatial) (&spatial->renderState)
#define SE_Spatial_GetName(spatial) (&spatial->name)
#define SE_Spatial_GetGeometryData(spatial) (&spatial->geometryData)
#define SE_Spatial_GetChildren(spatial) (&spatial->children)
extern SE_Result SE_Spatial_Init(SE_Spatial* spatial, SE_SPATIAL_TYPE spatialType ,const char* name);
extern SE_Result SE_Spatial_Release(void* spatial);
extern SE_Result SE_Spatial_Copy(const SE_Spatial* spatialSrc, SE_Spatial* spatialDst);
extern SE_Result SE_Spatial_UpdateWorldTransform(SE_Spatial* spatial);
extern SE_Result SE_Spatial_UpdateWorldBV(SE_Spatial* spatial);
extern SE_Result SE_Spatial_UpdateGeometricState(SE_Spatial* spatial);
extern SE_Result SE_Spatial_UpdateRenderState(SE_Spatial* spatial);
extern int SE_Spaital_HasChildren(SE_Spatial* spatial);
extern int SE_Spatial_GetChildrenNum(SE_Spatial* spatial);
extern SE_Result SE_Spatial_AddChild(SE_Spatial* parent, SE_Spatail* child);
extern SE_Result SE_Spatial_RemoveChild(SE_Spatial* parent, SE_Spatial* child);
extern SE_Result SE_Spatial_RemoveChildByName(SE_Spatial* parent, SE_String* name);
#ifdef __cplusplus
}
#endif
#endif
