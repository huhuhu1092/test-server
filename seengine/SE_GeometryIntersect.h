#ifndef SE_GEOMETRYINTERSECT_H
#define SE_GEOMETRYINTERSECT_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Geometry3D.h"
#include "SE_Sphere.h"
#include "SE_AABB.h"
#include "SE_OBB.h"
#ifdef __cplusplus
extern "C" {
#endif
/*function about closest point 
 * these function find the closest point on geometry to the given point
*/
extern SE_Result SE_ClosestPoint_PointPlane(const SE_Vector3f* point, const SE_Plane* plane, SE_Vector3f* out);
extern SE_Result SE_ClosestPoint_PointSegment(const SE_Vector3f* point, const SE_Segment* seg, SE_Vector3f* outPoint, float* outt);
extern SE_Result SE_ClosestPoint_PointRay(const SE_Vector3f* point, const SE_Ray* ray, SE_Vector3f* outPoint, float* outt);
extern SE_Result SE_ClosestPoint_PointAABB(const SE_Vector3f* point, const SE_AABB* aabb, SE_Vector3f* outPoint);
extern SE_Result SE_ClosestPoint_PointOBB(const SE_Vector3f* point, const SE_OBB* obb, SE_Vector3f* outPoint);
extern SE_Result SE_ClosestPoint_PointRect3D(const SE_Vector3f* point, const SE_Rect3D* rect3D, SE_Vector3f* outPoint);

/****                   function about intersection      **/
extern SE_Result SE_Intersect_Segment_Plane(const SE_Segment* line, const SE_Plane* plane, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Segment_Segment(const SE_Segment* line1, const SE_Segment* line2, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Segment_Triangle(const SE_Segment* line, const SE_Triangle* tri, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Ray_Plane(const SE_Ray* ray, const SE_Plane* plane, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Ray_Ray(const SE_Ray* ray1, const SE_Ray* ray2, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Ray_Triangle(const SE_Ray* ray, const SE_Triangle* tri, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Ray_AABB(const SE_Ray* ray, const SE_AABB* aabb, SE_IntersectionResult* out);
extern SE_Result SE_Intersect_Ray_Sphere(const SE_Ray* ray, const SE_Sphere* sphere, SE_IntersectionResult* out);
/*
 * return value: 0: not intersect , 1 : intersected
 * */
extern int SE_Intersect_AABB_Plane(const SE_AABB* aabb, const SE_Plane* plane);
/* square distance of point to AABB
 * if point is in AABB  or at the bound surface of AABB, the distance is 0;
 * */
extern float SE_PointAABB_DistanceSquare(SE_Vector3f* point, SE_AABB* aabb);
/*
 * 0: not intersect
 * 1: intersect
 * */
extern int SE_Intersect_Sphere_AABB(SE_Sphere* sphere, SE_AABB* aabb);
/*
 * intersection of a moving sphere and a static AABB
 * ths out value t is in the interval [0, 1]
 * */
extern int SE_Intersect_MovingSphereStaticAABB(SE_Sphere sphere, SE_AABB* aabb, SE_Vector3f endPoint, SE_Vector3f* out);
/**
 * intersect moving sphere with plane, out is the intersection point of sphere center
 * dirOfSphere is the endpoint minus startpoint of sphere, 
 * */
extern int SE_Intersect_MovingSphereStaticPlane(const SE_Sphere* sphere, const SE_Plane* plane, const SE_Vector3f* dirOfSphere, SE_Vector3f* out);
extern int SE_Intersect_MovingOBBStaticAABB(const SE_OBB obb, const SE_AABB* aabb, enum SE_AXIS_TYPE axis, float dist, SE_OBB* out);
#ifdef __cplusplus
}
#endif

#endif
