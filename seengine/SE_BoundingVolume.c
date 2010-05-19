#include "SE_BoundingVolume.h"
#include "SE_GeometryIntersect.h"
SE_Result SE_BoundingVolume_Init(SE_BoundingVolume* bv)
{
    return SE_VALID;
}
void SE_BoundingVolume_Release(void* bv)
{}
/***/
static void initBoundingVolumeForSphere(SE_BoundingVolume* bv)
{
    bv->type = SE_SPHERE_E;
    bv->fTransform = &SE_SphereBV_Transform;
    bv->fWhichSide = &SE_SphereBV_WhichSide;
    bv->fMerge = &SE_SphereBV_Merge;
    bv->fContains = &SE_SphereBV_Contains;
    bv->fIntersectRay = &SE_SphereBV_IntersectRay;
    bv->fIntersectRayDetail = &SE_SphereBV_IntersectRayDetail;
}
SE_Result SE_SphereBV_CreateFromPoints(SE_SphereBV* sbv, SE_Vector3f* points, int pointNum)
{
    SE_ASSERT(sbv);
    SE_ASSERT(points);
    SE_Object_Clear(sbv, sizeof(SE_SphereBV));
    initBoundingVolumeForSphere(&sbv->base);
    SE_Sphere_CreateFromPoints(&sbv->sphere, points, pointNum);
    return SE_VALID;
}
SE_Result SE_SphereBV_CreateFromSphere(SE_SphereBV* sbv, SE_Sphere* s)
{
    SE_ASSERT(sbv);
    SE_ASSERT(s);
    SE_Object_Clear(sbv, sizeof(SE_SphereBV));
    initBoundingVolumeForSphere(&sbv->base);
    sbv->sphere = *s;
    return SE_VALID;
}
SE_Result SE_SphereBV_Transform(struct SE_BoundingVolume_tag* bv, const SE_Matrix3f* ratation, const SE_Vector3f* translate, const SE_Vector3f* scale)
{
    SE_ASSERT(bv);
    SE_SphereBV* sbv = (SE_SphereBV*)bv;
    SE_Vector3f center;
    float scaleX;
    if(translate)
    {
        SE_Vec3f_Add(&sbv->sphere.center, translate, &center);
        SE_Vec3f_Copy(&center, &sbv->sphere.center);
    }
    if(scale)
    {
        scaleX = scale->x;
        sbv->sphere.radius *= scaleX;
    }
    return SE_VALID;
}
enum SE_Plane_Side SE_SphereBV_WhichSide(struct SE_BoundingVolume_tag* bv, const SE_Plane* plane)
{
    float dist;
    SE_ASSERT(bv);
    SE_ASSERT(plane);
    SE_SphereBV* sbv = (SE_SphereBV*)bv;
    dist = SE_Plane_PointDistance(plane, &sbv->sphere.center);
    if(dist > 0)
    {
        if(dist >= sbv->sphere.radius)
            return SE_POSITIVE;
        else
            return SE_ONSIDE;
    }
    else if(dist < 0)
    {
        if((-dist) >= sbv->sphere.radius)
            return SE_NEGATIVE;
        else
            return SE_ONSIDE;
    }
    return SE_ONSIDE;
}
SE_Result SE_SphereBV_Merge(struct SE_BoundingVolume_tag* bvMerged, const struct SE_BoundingVolume_tag* bv)
{
    return SE_VALID;
}
int SE_SphereBV_Contains(const struct SE_BoundingVolume_tag* bv, const SE_Vector3f* point)
{
    SE_ASSERT(bv);
    SE_SphereBV* sbv = (SE_SphereBV*)bv;
    return SE_Sphere_ContainPoint(&sbv->sphere, point);
}
int SE_SphereBV_IntersectRay(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray)
{
    SE_ASSERT(bv);
    SE_ASSERT(ray);
    SE_SphereBV* sbv = (SE_SphereBV*)bv;
    SE_IntersectionResult intersectResult;
    SE_Intersect_Ray_Sphere(ray, &sbv->sphere, &intersectResult);
    return intersectResult.intersected;
}
SE_Result SE_SphereBV_IntersectRayDetail(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray, SE_IntersectionResult* result)
{
    SE_ASSERT(bv);
    SE_ASSERT(ray);
    SE_SphereBV* sbv = (SE_SphereBV*)bv;
    SE_Intersect_Ray_Sphere(ray, &sbv->sphere, result);
    return SE_VALID;
}
int SE_SphereBV_BVIntersectBV(const struct SE_BoundingVolume_tag* bv1, const struct SE_BoundingVolume_tag* bv2)
{
    return 0;
}
/***/
static void initBoundingVolumeForAABB(SE_BoundingVolume* bv)
{
    bv->type = SE_AABB_E;
    bv->fTransform = &SE_AABBBV_Transform;
    bv->fWhichSide = &SE_AABBBV_WhichSide;
    bv->fMerge = &SE_AABBBV_Merge;
    bv->fContains = &SE_AABBBV_Contains;
    bv->fIntersectRay = &SE_AABBBV_IntersectRay;
    bv->fIntersectRayDetail = &SE_AABBBV_IntersectRayDetail;

}
SE_Result SE_AABBBV_CreateFromPoints(SE_AABBBV* aabbBv, SE_Vector3f* points, int pointNum)
{
    SE_ASSERT(aabbBv);
    SE_ASSERT(points);
    SE_Object_Clear(aabbBv, sizeof(SE_AABBBV));
    initBoundingVolumeForAABB(&aabbBv->base);
    SE_AABB_CreateFromPoints(&aabbBv->aabb, points, pointNum);
    return SE_VALID;
}
SE_Result SE_AABBBV_CreateFromAABB(SE_AABBBV* aabbBv, SE_AABB* aabb)
{
    SE_ASSERT(aabbBv);
    SE_ASSERT(aabb);
    SE_Object_Clear(aabbBv, sizeof(SE_AABBBV));
    initBoundingVolumeForAABB(&aabbBv->base);
    aabbBv->aabb = *aabb;
    return SE_VALID;
}
SE_Result SE_AABBBV_Transform(struct SE_BoundingVolume_tag* bv, const SE_Matrix3f* rotation, const SE_Vector3f* translate, const SE_Vector3f* scale)
{
    SE_AABB dst;
    SE_Matrix3f s, rs;
    SE_Vector3f t;
    SE_AABBBV* aabbBv;
    SE_ASSERT(bv);
    aabbBv = (SE_AABBBV*)bv;
    SE_Object_Clear(&t, sizeof(SE_Vector3f));
    SE_Mat3f_Identity(&rs);
    if(scale)
    {
        SE_Mat3f_Scale(scale->x, scale->y, scale->z, &rs); 
    }
    if(rotation)
    {
        SE_Mat3f_Mul(rotation, &rs, &s);
        SE_Mat3f_Copy(&s, &rs); 
    }
    if(translate)
    {
        SE_Vec3f_Copy(translate, &t);
    }
    SE_AABB_Transform(&aabbBv->aabb, &rs, &t, &dst);
    aabbBv->aabb = dst;
    return SE_VALID; 
}

enum SE_Plane_Side SE_AABBBV_WhichSide(struct SE_BoundingVolume_tag* bv, const SE_Plane* plane)
{
    SE_ASSERT(bv);
    SE_ASSERT(plane);
    SE_AABBBV* aabbBv = (SE_AABBBV*)bv;
    int bIntersect = 0;
    SE_Vector3f center;
    bIntersect = SE_Intersect_AABB_Plane(&aabbBv->aabb, plane);
    if(bIntersect)
    {
        return SE_ONSIDE;
    }
    SE_AABB_GetCenter(&aabbBv->aabb, &center);
    return SE_Plane_PointOnWhichSide(plane, &center);
}
SE_Result SE_AABBBV_Merge(struct SE_BoundingVolume_tag* bvMerged, const struct SE_BoundingVolume_tag* bv)
{
    return SE_VALID;
}
int SE_AABBBV_Contains(const struct SE_BoundingVolume_tag* bv, const SE_Vector3f* point)
{
    SE_ASSERT(bv);
    SE_ASSERT(point);
    SE_AABBBV* aabbBv = (SE_AABBBV*)bv;
    SE_AABB* aabb = &aabbBv->aabb;
    if(point->x >= aabb->min.x && point->y >= aabb->min.y && point->z >= aabb->min.z &&
       point->x <= aabb->max.x && point->y <= aabb->max.y && point->z <= aabb->max.z)
        return 1;
    return 0;
}
int SE_AABBBV_IntersectRay(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray)
{
    SE_ASSERT(bv);
    SE_ASSERT(ray);
    SE_AABBBV* aabbBv = (SE_AABBBV*)bv;
    SE_IntersectionResult result;
    SE_Intersect_Ray_AABB(ray, &aabbBv->aabb, &result);
    return result.intersected; 
}
SE_Result SE_AABBBV_IntersectRayDetail(const struct SE_BoundingVolume_tag* bv, const SE_Ray* ray, SE_IntersectionResult* result)
{
    SE_ASSERT(bv);
    SE_ASSERT(ray);
    SE_AABBBV* aabbBv = (SE_AABBBV*)bv;
    SE_Intersect_Ray_AABB(ray, &aabbBv->aabb, result);
    return SE_VALID;
}
int SE_AABBBV_BVIntersectBV(const struct SE_BoundingVolume_tag* bv1, const struct SE_BoundingVolume_tag* bv2)
{
    return 0;
}


