#ifndef SE_GEOMETRY3D_H
#define SE_GEOMETRY3D_H
#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"

#ifdef __cplusplus
extern "C" {
#endif
enum SE_Plane_Side {SE_POSITIVE, SE_ONSIDE, SE_NEGATIVE};
typedef struct SE_Rectf_tag
{
    float left, right, top, bottom;
} SE_Rectf;
typedef struct SE_Recti_tag
{
    int left, right, top, bottom;
} SE_Recti;
typedef struct SE_Segment_tag
{
   SE_Vector3f start;
   SE_Vector3f end; 
} SE_Segment;
/**
 * plane is represented by n * X - d = 0
 * */
typedef struct SE_Plane_tag
{
    SE_Vector3f n;
    float d;
} SE_Plane;
typedef struct SE_Ray_tag
{
    SE_Vector3f origin;
    SE_Vector3f dir;
} SE_Ray;
typedef struct SE_Triangle_tag
{
    SE_Vector3f p0;
    SE_Vector3f p1;
    SE_Vector3f p2;
} SE_Triangle;
typedef struct SE_Frustum_tag
{
    SE_Plane left;
    SE_Plane right;
    SE_Plane top;
    SE_Plane bottom;
    SE_Plane nearp;
    SE_Plane farp;
    float fovAngle; 
    float ratio;
    float n;
    float f;
} SE_Frustum;
typedef struct SE_IntersectionResult_tag
{
    SE_Vector3f* intersectPoint;
    int intersectPointNum;
    float* distance;
    int distanceNum;
    int intersected; // 0: no , 1: yes
} SE_IntersectionResult;
/**               function about segment     */
extern SE_Result SE_Segment_InitFromPoint(const SE_Vector3f* start, const SE_Vector3f* end, SE_Segment* out);
extern SE_Result SE_Segment_GetStart(const SE_Segment* seg, SE_Vector3f* out);
extern SE_Result SE_Segment_GetEnd(const SE_Segment* seg, SE_Vector3f* out);
/*
#define SE_Line_GetP0(line) (&line->p0)
#define SE_Line_GetP1(line) (&line->p1)
#define SE_Line_GetP0Copy(line, out) (SE_Vec3f_Copy(&line->p0, out))
#define SE_Line_GetP1Copy(line, out) (SE_Vec3f_Copy(&line->p1, out))
*/
/**
 * normal : 0: direction vector may not normalize
 *          1: direction vector need normalize
 * */
extern SE_Result SE_Segment_GetDirection(const SE_Segment* seg, SE_Vector3f* out, int normal);
/****            function about plane **/
extern SE_Result SE_Plane_InitFromPoint(const SE_Vector3f* p0, const SE_Vector3f* p1, const SE_Vector3f* p2, SE_Plane* out);
extern SE_Result SE_Plane_InitFromNormal(const SE_Vector3f* p0, const SE_Vector3f* n, int normal, SE_Plane* out);
extern SE_Result SE_Plane_GetNormal(const SE_Plane* plane, SE_Vector3f* out);

extern float SE_Plane_GetD(const SE_Plane* plane);
/*
#define SE_Plane_GetNormal(plane) (&plane->n)
#define SE_Plane_GetNormalCopy(plane, out) (SE_Vec3f_Copy(&plane->n, out))
*/
/*
 * 0: is on plane
 * 1: on the same dir with normal
 * -1 : on the against dir with normal
 * */
extern enum SE_Plane_Side SE_Plane_PointOnWhichSide(const SE_Plane* plane, const SE_Vector3f* point);
extern float SE_Plane_PointDistance(const SE_Plane* plane, const SE_Vector3f* point);

/***             function about ray **/
/*
#define SE_Ray_InitFromPoint(p0, p1, out) do { \
    SE_Vector3f v; \
    SE_Vec3f_Subtract(p1, p0, &v); \
    SE_Vec3f_Normalize(&v, &out->dir); \
    SE_Vec3f_Copy(p0, &out->origin); \
}while(0)
*/
/**
 * normal indicate whether dir is a normalized vector
 * */
/*
#define SE_Ray_InitFromDirection(origin, dir, normal, out) do{ \
    if(normal) \
    { \
        SE_Vec3f_Copy(dir, &out->dir); \
    } \
    else \
    { \
        SE_Vec3f_Normalize(dir, &out->dir); \
    } \
    SE_Vec3f_Copy(origin, &out->origin); \
}while(0)
*/
extern SE_Result SE_Ray_InitFromPoint(const SE_Vector3f* p0, const SE_Vector3f* p1, SE_Ray* out);
extern SE_Result SE_Ray_InitFromDirection(const SE_Vector3f* origin, const SE_Vector3f* dir, int normal, SE_Ray* out);
/*
#define SE_Ray_GetDirection(ray) (&ray->dir)
#define SE_Ray_GetOrigin(ray) (&ray->origin)
#define SE_Ray_GetDirectionCopy(ray, out) (SE_Vec3f_Copy(&ray->dir, out))
#define SE_Ray_GetOriginCopy(ray, out) (SE_Vec3f_Copy(&ray->origin, out)
*/

extern SE_Result SE_Ray_GetDirection(const SE_Ray* ray, SE_Vector3f* out);
extern SE_Result SE_Ray_GetOrigin(const SE_Ray* ray, SE_Vector3f* out);

/***             function about triangle  */
/**
 * the point is winding by the coordinate direction
 * left hand is clockwise , right hand is counter clockwise
 * **/
/*
#define SE_Triangle_InitFromPoint(p0, p1, p2, out) do{ \
    SE_Vec3f_Copy(p0, &out->p0); \
    SE_Vec3f_Copy(p1, &out->p1); \
    SE_Vec3f_Copy(p2, &out->p2); \
}while(0)
#define SE_Triangle_InitPlane(tri, plane) do{ \
    SE_Plane_InitFromPoint(&tri->p0, &tri->p1, &tri->p2, plane); \
}while(0)
#define SE_Triangle_GetP0(tri) (&tri->p0)
#define SE_Triangle_GetP1(tri) (&tri->p1)
#define SE_Triangle_GetP2(tri) (&tri->p2)
#define SE_Triangle_GetP0Copy(tri, out) (SE_Vec3f_Copy(&tri->p0, out))
#define SE_Triangle_GetP1Copy(tri, out) (SE_Vec3f_Copy(&tri->p1, out))
#define SE_Triangle_GetP2Copy(tri, out) (SE_Vec3f_Copy(&tri->p2, out))
*/
extern SE_Result SE_Triangle_InitFromPoint(const SE_Vector3f* p0, const SE_Vector3f* p1, const SE_Vector3f* p2, SE_Triangle* out);
extern SE_Result SE_Triangle_InitPlane(const SE_Triangle* tri, SE_Plane* out);
extern SE_Result SE_Triangle_GetPoint0(const SE_Triangle* tri, SE_Vector3f* out);
extern SE_Result SE_Triangle_GetPoint1(const SE_Triangle* tri, SE_Vector3f* out);
extern SE_Result SE_Triangle_GetPoint2(const SE_Triangle* tri, SE_Vector3f* out);
/**
 * fov is the horizonal angle
 * ratio is the ratio of height / width
 * */
extern SE_Result SE_Frustum_InitFromFOV(float fov, float ratio, float near, float far,SE_Frustum* out);
extern SE_Result SE_Frustum_GetNearPlaneRect(const SE_Frustum* ft, SE_Rectf* out);
extern SE_Result SE_Frustum_GetPerspectiveMatrix(const SE_Frustum* ft, SE_Matrix4f* out);
/*
#define SE_Frustum_GetLeftPlane(ft) (&ft->left)
#define SE_Frustum_GetRightPlane(ft) (&ft->right)
#define SE_Frustum_GetTopPlane(ft) (&ft->top)
#define SE_Frustum_GetBottomPlane(ft) (&ft->bottom)
#define SE_Frustum_GetNearPlane(ft) (&ft->near)
#define SE_Frustum_GetFarPlane(ft) (&ft->far)
*/
extern SE_Result SE_Frustum_GetLeft(const SE_Frustum* ft, SE_Plane* out);
extern SE_Result SE_Frustum_GetRight(const SE_Frustum* ft, SE_Plane* out);
extern SE_Result SE_Frustum_GetTop(const SE_Frustum* ft, SE_Plane* out);
extern SE_Result SE_Frustum_GetBottom(const SE_Frustum* ft, SE_Plane* out);
extern SE_Result SE_Frustum_GetNear(const SE_Frustum* ft, SE_Plane* out);
extern SE_Result SE_Frustum_GetFar(const SE_Frustum* ft, SE_Plane* out);
extern SE_Result SE_Frustum_Copy(const SE_Frustum* src, SE_Frustum* dst);
/***/
extern void SE_IntersectionResult_Release(void* intersectResult);

#ifdef __cplusplus
}
#endif

#endif
