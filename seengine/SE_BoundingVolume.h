#ifndef SE_BOUNDINGVOLUME_H
#define SE_BOUNDINGVOLUME_H
#include "SE_Common.h"
#include "SE_Geometry3D.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#ifdef __cplusplus
extern "C" {
#endif
enum SE_BVType {SE_Sphere, SE_AABB, SE_OBB, SE_Capsule};
struct SE_BoundingVolume_tag;
typedef SE_Result (*SE_BV_TRANSFORM)(struct SE_BoundingVolume_tag* bv, const SE_Quat* ratation, const SE_Vector3f* traslate, const SE_Vector3f* scale);
typedef enum SE_Plane_Side (*SE_BV_WHICH_SIDE)(struct SE_BoundingVolume_tag* bv, const SE_Plane* plane);
typedef SE_Result (*SE_BV_MERGE)(struct SE_BoundingVolume_tag* bvMerged, const struct SE_BoundingVolume_tag* bv);
typedef int (*SE_BV_CONTAINS)(const struct SE_BoundingVolume_tag* bv, const SE_Vector3f* point);
typedef int (*SE_BV_INTERSECT_RAY)(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray);
typedef SE_Result (*SE_BV_INTERSECT_RAY_DETAIL)(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray, SE_IntersectionResult* result);
typedef int (*SE_BV_INTERSECT_BV)(const struct SE_BoundingVolume_tag* bv1, const struct SE_BoundingVolume_tag* bv2);
typedef struct SE_BoundingVolume_tag
{
    enum SE_BVType type;
    SE_BV_TRANSFORM fTransform;
    SE_BV_WHICH_SIDE fWhichSide;
    SE_BV_MERGE fMerge;
    SE_BV_CONTAINS fContains;
    SE_BV_INTERSECT_RAY fIntersectRay;
    SE_BV_INTERSECT_RAY_DETAIL fIntersectRayDetail;
    SE_BV_INTERSECT_BV fIntersectBV;
} SE_BoundingVolume;
extern SE_Result SE_BoundingVolume_Init(SE_BoundingVolume* bv);
extern void SE_BoundingVolume_Release(void* bv);
#ifdef __cplusplus
}
#endif
#endif
