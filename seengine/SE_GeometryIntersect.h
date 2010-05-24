#ifndef SE_GEOMETRYINTERSECT_H
#define SE_GEOMETRYINTERSECT_H
#include "SE_Vector.h"
#include "SE_Geometry3D.h"
#include "SE_Sphere.h"
#include "SE_AABB.h"
#ifdef __cplusplus
extern "C" {
#endif

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
#ifdef __cplusplus
}
#endif

#endif
