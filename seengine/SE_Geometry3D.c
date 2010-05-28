#include "SE_Geometry3D.h"
#include "SE_Math.h"
#include "SE_Common.h"
#include "SE_Memory.h"

SE_Result SE_Segment_InitFromPoint(const SE_Vector3f* start, const SE_Vector3f* end, SE_Segment* out)
{
    SE_ASSERT(start);
    SE_ASSERT(end);
    SE_ASSERT(out);
    SE_Vec3f_Copy(start, &out->start);
    SE_Vec3f_Copy(end, &out->end);
    return SE_VALID;
}
SE_Result SE_Segment_GetStart(const SE_Segment* seg, SE_Vector3f* out)
{
    SE_ASSERT(seg);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&seg->start, out);
    return SE_VALID;
}
SE_Result SE_Segment_GetEnd(const SE_Segment* seg, SE_Vector3f* out)
{
    SE_ASSERT(seg);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&seg->end, out);
    return SE_VALID;
}
SE_Result SE_Segment_GetDirection(const SE_Segment* seg, SE_Vector3f* out, int normal)
{
    SE_ASSERT(seg);
    SE_ASSERT(out);
    SE_Vector3f r;
    if(normal)
    {
        SE_Vec3f_Subtract(&seg->end, &seg->start, &r);
        SE_Vec3f_Normalize(&r, out);
    }
    else
    {
        SE_Vec3f_Subtract(&seg->end, &seg->start, out);
    }
    return SE_VALID;
}
SE_Result SE_Plane_InitFromPoint(const SE_Vector3f* p0, const SE_Vector3f* p1, const SE_Vector3f* p2, SE_Plane* out)
{
    SE_ASSERT(p0);
    SE_ASSERT(p1);
    SE_ASSERT(p2);
    SE_ASSERT(out);

    SE_Vector3f v1, v2, n;
    SE_Vec3f_Subtract(p1, p0, &v1);
    SE_Vec3f_Subtract(p2, p0, &v2);
    SE_Vec3f_Cross(&v1, &v2, &n);
    SE_ASSERT(!SE_Vec3f_IsZero(&n));
    SE_Vec3f_Normalize(&n, &out->n);
    out->d = SE_Vec3f_Dot(&out->n, p0);
    return SE_VALID;
}
SE_Result SE_Plane_InitFromNormal(const SE_Vector3f* p0, const SE_Vector3f* n, int normal, SE_Plane* out)
{
    SE_ASSERT(p0);
    SE_ASSERT(n);
    SE_ASSERT(out);
    if(normal)
    {
        SE_Vec3f_Copy(n, &out->n);
        out->d = SE_Vec3f_Dot(n, p0); 
    }
    else
    {
        SE_Vec3f_Normalize(n, &out->n);
        out->d = SE_Vec3f_Dot(&out->n, p0);
    }
    return SE_VALID;
}
SE_Result SE_Plane_GetNormal(const SE_Plane* plane, SE_Vector3f* out)
{
    SE_ASSERT(plane);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&plane->n, out);
    return SE_VALID;
}
float SE_Plane_GetD(const SE_Plane* plane)
{
    return plane->d;
}
enum SE_Plane_Side SE_Plane_PointOnWhichSide(const SE_Plane* plane, const SE_Vector3f* point)
{
    SE_ASSERT(plane);
    SE_ASSERT(point);
    float ret = SE_Vec3f_Dot(&plane->n, point) - plane->d;
    if(ret == 0.0)
        return SE_ONSIDE;
    else if(ret > 0.0)
        return SE_POSITIVE;
    else
        return SE_NEGATIVE;
}
float SE_Plane_PointDistance(const SE_Plane* plane, const SE_Vector3f* point)
{
    return SE_Vec3f_Dot(&plane->n, point) - plane->d;
}
SE_Result SE_Ray_InitFromPoint(const SE_Vector3f* p0, const SE_Vector3f* p1, SE_Ray* out)
{
    SE_ASSERT(p0);
    SE_ASSERT(p1);
    SE_ASSERT(out);
    SE_Vector3f v;
    SE_Vec3f_Subtract(p1, p0, &v);
    SE_Vec3f_Normalize(&v, &out->dir);
    SE_Vec3f_Copy(p0, &out->origin);
    return SE_VALID;
}
SE_Result SE_Ray_InitFromDirection(const SE_Vector3f* origin, const SE_Vector3f* dir, int normal, SE_Ray* out)
{
    SE_ASSERT(origin);
    SE_ASSERT(dir);
    SE_ASSERT(out);
    if(normal)
    {
        SE_Vec3f_Copy(dir, &out->dir);
    }
    else
    {
        SE_Vec3f_Normalize(dir, &out->dir);
    }
    SE_Vec3f_Copy(origin, &out->origin);
    return SE_VALID;
}
SE_Result SE_Ray_GetDirection(const SE_Ray* ray, SE_Vector3f* out)
{
    SE_ASSERT(ray);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&ray->dir, out);
    return SE_VALID;
}
SE_Result SE_Ray_GetOrigin(const SE_Ray* ray, SE_Vector3f* out)
{
    SE_Vec3f_Copy(&ray->origin, out);
    return SE_VALID;
}
SE_Result SE_Triangle_InitFromPoint(const SE_Vector3f* p0, const SE_Vector3f* p1, const SE_Vector3f* p2, SE_Triangle* out)
{
    SE_ASSERT(p0);
    SE_ASSERT(p1);
    SE_ASSERT(p2);
    SE_ASSERT(out);
    SE_Vec3f_Copy(p0, &out->p0);
    SE_Vec3f_Copy(p1, &out->p1);
    SE_Vec3f_Copy(p2, &out->p2);
    return SE_VALID;
}
SE_Result SE_Triangle_InitPlane(const SE_Triangle* tri, SE_Plane* out)
{
    SE_ASSERT(tri);
    SE_ASSERT(out);
    SE_Plane_InitFromPoint(&tri->p0, &tri->p1, &tri->p2, out);
    return SE_VALID;
}
SE_Result SE_Triangle_GetPoint0(const SE_Triangle* tri, SE_Vector3f* out)
{
    SE_ASSERT(tri);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&tri->p0, out);
    return SE_VALID;
}
SE_Result SE_Triangle_GetPoint1(const SE_Triangle* tri, SE_Vector3f* out)
{
    SE_ASSERT(tri);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&tri->p1, out);
    return SE_VALID;
}
SE_Result SE_Triangle_GetPoint2(const SE_Triangle* tri, SE_Vector3f* out)
{
    SE_ASSERT(tri);
    SE_ASSERT(out);
    SE_Vec3f_Copy(&tri->p2, out);
    return SE_VALID;
}
SE_Result SE_Frustum_InitFromFOV(float fov, float ratio, float near, float far, SE_Frustum* out)
{
    SE_ASSERT(fov != 0.0);
    SE_ASSERT(ratio != 0.0);
    SE_ASSERT(fov >= 0.0 && fov <= 360.0);
    SE_ASSERT(near > 0 && far > 0);
    float fovRadian = SE_AngleToRadian(fov);
    float e = 1.0 / SE_Tanf(fovRadian / 2);
    float param1 = 1.0 / SE_Sqrtf(e * e + 1);
    float param2 = 1.0 / SE_Sqrtf(e * e + ratio * ratio);
    SE_Vec3f_Init(e / param1, 0, -1 / param1, &out->left.n);
    SE_Vec3f_Init(-e / param1, 0, -1 / param1, &out->right.n);
    SE_Vec3f_Init(0, -e / param2, -ratio / param2, &out->top.n);
    SE_Vec3f_Init(0, e / param2, -ratio / param2, &out->bottom.n);
    SE_Vec3f_Init(0,0, -1, &out->near.n );
    SE_Vec3f_Init(0, 0, 1, &out->far.n);
    out->left.d = 0;
    out->right.d = 0;
    out->top.d = 0;
    out->bottom.d = 0; 
    out->near.d = -near;
    out->far.d = far;
    out->fovAngle = fov;
    out->ratio = ratio;
    out->n = near;
    out->f = far;
    return SE_VALID; 
}
SE_Result SE_Frustum_GetNearPlaneRect(const SE_Frustum* ft, SE_Rectf* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    float fovRaidan = SE_AngleToRadian(ft->fovAngle);
    float e = 1.0 / SE_Tanf(fovRaidan / 2);
    out->left = - ft->n / e;
    out->right = ft->n / e;
    out->top = ft->n * ft->ratio / e;
    out->bottom = - ft->n * ft->ratio / e;
    return SE_VALID;
}

SE_Result SE_Frustum_GetLeft(const SE_Frustum* ft, SE_Plane* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    *out = ft->left;
    return SE_VALID;
}
SE_Result SE_Frustum_GetRight(const SE_Frustum* ft, SE_Plane* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    *out = ft->right;
    return SE_VALID;
}
SE_Result SE_Frustum_GetTop(const SE_Frustum* ft, SE_Plane* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    *out = ft->top;
    return SE_VALID;
}
SE_Result SE_Frustum_GetBottom(const SE_Frustum* ft, SE_Plane* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    *out = ft->bottom;
    return SE_VALID;
}
SE_Result SE_Frustum_GetNear(const SE_Frustum* ft, SE_Plane* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    *out = ft->near;
    return SE_VALID;
}
SE_Result SE_Frustum_GetFar(const SE_Frustum* ft, SE_Plane* out)
{
    SE_ASSERT(ft);
    SE_ASSERT(out);
    *out = ft->far;
    return SE_VALID;
}
void SE_IntersectionResult_Release(void* intersectResult)
{
    SE_IntersectionResult* r = (SE_IntersectionResult*)intersectResult;
    if(r->distance)
    {
        SE_Free(r->distance);
    }
    if(r->intersectPoint)
    {
        SE_Free(r->intersectPoint);
    }
    return;
}
