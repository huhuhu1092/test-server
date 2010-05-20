#ifndef SE_BOUNDINGVOLUME_H
#define SE_BOUNDINGVOLUME_H
#include "SE_Common.h"
#include "SE_Geometry3D.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_AABB.h"
#include "SE_Sphere.h"
#ifdef __cplusplus
extern "C" {
#endif
enum SE_BVType {SE_SPHERE_E, SE_AABB_E, SE_OBB_E, SE_CAPSULE_E};
struct SE_BoundingVolume_tag;
typedef SE_Result (*SE_BV_TRANSFORM)(struct SE_BoundingVolume_tag* bv, const SE_Matrix3f* ratation, const SE_Vector3f* traslate, const SE_Vector3f* scale);
typedef enum SE_Plane_Side (*SE_BV_WHICH_SIDE)(struct SE_BoundingVolume_tag* bv, const SE_Plane* plane);
typedef SE_Result (*SE_BV_MERGE)(struct SE_BoundingVolume_tag* bvMerged, const struct SE_BoundingVolume_tag* bv);
typedef int (*SE_BV_CONTAINS)(const struct SE_BoundingVolume_tag* bv, const SE_Vector3f* point);
typedef int (*SE_BV_INTERSECT_RAY)(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray);
typedef SE_Result (*SE_BV_INTERSECT_RAY_DETAIL)(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray, SE_IntersectionResult* result);
typedef int (*SE_BV_INTERSECT_BV)(const struct SE_BoundingVolume_tag* bv1, const struct SE_BoundingVolume_tag* bv2);
typedef struct SE_BoundingVolume_tag*  (*SE_BV_CLONE)(const struct SE_BoundingVolume_tag* bvsrc);
typedef void (*SE_BV_RELEASE)(void* bv);
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
    SE_BV_CLONE fClone;
    SE_BV_RELEASE fRelease;
} SE_BoundingVolume;
extern SE_Result SE_BoundingVolume_Init(SE_BoundingVolume* bv);
extern void SE_BoundingVolume_Release(void* bv);
extern SE_BoundingVolume* SE_BoundingVolume_Clone(const SE_BoundingVolume* src);
/**
 * sphere bounding volume
 * */
typedef struct SE_SphereBV_tag
{
    SE_BoundingVolume base;
    SE_Sphere sphere;
} SE_SphereBV;
extern SE_Result SE_SphereBV_CreateFromPoints(SE_SphereBV* sbv, SE_Vector3f* points, int pointNum);
extern SE_Result SE_SphereBV_CreateFromSphere(SE_SphereBV* sbv, SE_Sphere* s);
extern SE_Result SE_SphereBV_Transform(struct SE_BoundingVolume_tag* bv, const SE_Matrix3f* ratation, const SE_Vector3f* traslate, const SE_Vector3f* scale);
extern enum SE_Plane_Side SE_SphereBV_WhichSide(struct SE_BoundingVolume_tag* bv, const SE_Plane* plane);
extern SE_Result SE_SphereBV_Merge(struct SE_BoundingVolume_tag* bvMerged, const struct SE_BoundingVolume_tag* bv);
extern int SE_SphereBV_Contains(const struct SE_BoundingVolume_tag* bv, const SE_Vector3f* point);
extern int SE_SphereBV_IntersectRay(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray);
extern SE_Result SE_SphereBV_IntersectRayDetail(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray, SE_IntersectionResult* result);
extern int SE_SphereBV_BVIntersectBV(const struct SE_BoundingVolume_tag* bv1, const struct SE_BoundingVolume_tag* bv2);
extern  struct SE_BoundingVolume_tag* SE_SphereBV_Clone(const struct SE_BoundingVolume_tag* bvsrc);
extern void SE_SphereBV_Release(void* bv);
/*
 * AABB bounding volume
 * */
typedef struct SE_AABBBV_tag
{
    SE_BoundingVolume base;
    SE_AABB aabb;
} SE_AABBBV;
extern SE_Result SE_AABBBV_CreateFromPoints(SE_AABBBV* aabbBv, SE_Vector3f* points, int pointNum);
extern SE_Result SE_AABBBV_CreateFromAABB(SE_SphereBV* sbv, SE_AABB* aabb);
extern SE_Result SE_AABBBV_Transform(struct SE_BoundingVolume_tag* bv, const SE_Matrix3f* ratation, const SE_Vector3f* traslate, const SE_Vector3f* scale);
extern enum SE_Plane_Side SE_AABBBV_WhichSide(struct SE_BoundingVolume_tag* bv, const SE_Plane* plane);
extern SE_Result SE_AABBBV_Merge(struct SE_BoundingVolume_tag* bvMerged, const struct SE_BoundingVolume_tag* bv);
extern int SE_AABBBV_Contains(const struct SE_BoundingVolume_tag* bv, const SE_Vector3f* point);
extern int SE_AABBBV_IntersectRay(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray);
extern SE_Result SE_AABBBV_IntersectRayDetail(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray, SE_IntersectionResult* result);
extern int SE_AABBBV_BVIntersectBV(const struct SE_BoundingVolume_tag* bv1, const struct SE_BoundingVolume_tag* bv2);
extern struct SE_BoundingVolume_tag* SE_AABBBV_Clone(const struct SE_BoundingVolume_tag* bvsrc);
extern void SE_AABBBV_Release(void* bv);
#ifdef __cplusplus
}
#endif
#endif
