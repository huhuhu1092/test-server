#include "SE_GeometryIntersect.h"
#include "SE_Math.h"
#include "SE_Memory.h"
SE_Result SE_Intersect_Segment_Plane(const SE_Segment* seg, const SE_Plane* plane, SE_IntersectionResult* out)
{
    return SE_VALID;
}

SE_Result SE_Intersect_Segment_Segment(const SE_Segment* seg1, const SE_Segment* seg2, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Segment_Triangle(const SE_Segment* line, const SE_Triangle* tri, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Plane(const SE_Ray* ray, const SE_Plane* plane, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Ray(const SE_Ray* ray1, const SE_Ray* ray2, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Triangle(const SE_Ray* ray, const SE_Triangle* tri, SE_IntersectionResult* out)
{
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_AABB(const SE_Ray* ray, const SE_AABB* aabb, SE_IntersectionResult* out)
{
    float tmin = -SE_FLT_MAX;
    float tmax = SE_FLT_MAX;
    SE_Vector3f dir, origin, tmp;
    int i;
    float ood, t1, t2;
    SE_Ray_GetDirection(ray, &dir);
    SE_Ray_GetOrigin(ray, &origin);
    SE_Object_Clear(out, sizeof(SE_IntersectionResult));
    for(i = 0 ; i < 3 ; i++)
    {
        if(SE_Fabs(dir.d[i]) < SE_FLOAT_EQUAL_EPSILON)
        {
            if(origin.d[i] < aabb->min.d[i] || origin.d[i] > aabb->max.d[i])
            {
                out->intersected = 0;
                return SE_VALID;
            }
        }
        else
        {
            ood = 1.0f / dir.d[i];
            t1 = (aabb->min.d[i] - origin.d[i]) * ood;
            t2 = (aabb->max.d[i] - origin.d[i]) * ood;
            if(t1 > t2)
            {
                float tmp;
                tmp = t1;
                t1 = t2;
                t2 = tmp;
            }
            if(t1 > tmin)
                tmin = t1;
            if(t2 < tmax)
                tmax = t2;
            if(tmin > tmax)
            {
                out->intersected = 0;
                return SE_VALID;
            }
            if(tmax < 0)
            {
                out->intersected = 0;
                return SE_VALID;
            }
        }
    }
    out->intersected = 1;
    out->distanceNum = 1;
    out->distance = (float*)SE_Malloc(sizeof(float));
    if(out->distance)
        *out->distance = tmin;
    out->intersectPointNum = 1;
    out->intersectPoint = (SE_Vector3f*)SE_Malloc(sizeof(SE_Vector3f));
    if(out->intersectPoint)
    {
        SE_Vec3f_Mul(&dir, tmin, &tmp);
        SE_Vec3f_Add(&origin, &tmp, &out->intersectPoint[0]);
    }
    /*
    SE_Vector3f dir, origin, tmp;
    int i;
    int intersect = 0;
    SE_Ray_GetDirection(ray, &dir);
    SE_Ray_GetOrigin(ray, &origin);
    SE_Object_Clear(out, sizeof(SE_IntersectionResult));
    int index[3][2];
    index[0][0] = 1;
    index[0][1] = 2;
    index[1][0] = 0;
    index[1][1] = 2;
    index[2][0] = 0;
    index[2][1] = 1;
    for(i = 0 ; i < 3 ; i++)
    {
        float v = dir.d[i];
        float t = 0, ood;
        float p[2];
        int j;
        ood = 1.0f / v;
        if(v > 0)
        {
            t = (aabb->min.d[i] - origin.d[i]) * ood;
        }
        else
        {
            t = (aabb->max.d[i] - origin.d[i]) * ood;
        }
        for(j = 0 ; j < 2 ; j++)
        {
            p[j] = origin.d[index[i][j]] + t * dir.d[index[i][j]];
        }
        for(j = 0 ;j < 2 ; j++)
        {
            if(p[j] > aabb->max.d[index[i][j]] || p[j] < aabb->min.d[index[i][j]])
               break; 
        }
        if(j == 2)
        {
            intersect = 1;
            break;
        }
    }
    if(intersect)
    {
        out->intersected = 1;
    }
    */
    return SE_VALID;
}
SE_Result SE_Intersect_Ray_Sphere(const SE_Ray* ray, const SE_Sphere* sphere, SE_IntersectionResult* out)
{
    SE_Vector3f origin, dir, m, resultP;
    float b, c; /*quadratic equation b and c : t2 + 2bt + c = 0*/
    float discr, t;
    SE_Object_Clear(out, sizeof(SE_IntersectionResult));
    SE_Ray_GetOrigin(ray, &origin);
    SE_Ray_GetDirection(ray, &dir);
    SE_Vec3f_Subtract(&origin, &sphere->center, &m);
    b = SE_Vec3f_Dot(&m, &dir);
    c = SE_Vec3f_Dot(&m, &m) - sphere->radius * sphere->radius;
    if(c > 0.0f && b > 0.0f)
    {
        out->intersected = 0;
        return SE_VALID;
    }
    discr = b * b - c;
    if(discr < 0.0f)
    {
        out->intersected = 0;
        return SE_VALID;
    }
    t = -b - SE_Sqrtf(discr);
    if(t < 0.0f)
        t = 0.0f;
    SE_Vec3f_Mul(&dir, t, &resultP);
    out->intersected = 1;
    out->intersectPointNum = 1;
    out->intersectPoint = (SE_Vector3f*)SE_Malloc(sizeof(SE_Vector3f));
    if(out->intersectPoint)
    {
        SE_Vec3f_Add(&origin, &resultP, &out->intersectPoint[0]);
    }
    return SE_VALID;
}
int SE_Intersect_AABB_Plane(const SE_AABB* aabb, const SE_Plane* plane)
{
    SE_Vector3f centerAABB, positiveExtent, normal;
    float r, s;
    SE_AABB_GetCenter(aabb, &centerAABB);
    SE_Vec3f_Subtract(&aabb->max, &centerAABB, &positiveExtent);
    SE_Plane_GetNormal(plane, &normal);
    r = positiveExtent.x * SE_Fabs(normal.x) + positiveExtent.y * SE_Fabs(normal.y) + positiveExtent.z * SE_Fabs(normal.z);
    s = SE_Vec3f_Dot(&normal, &centerAABB) - plane->d;
    return SE_Fabs(s) <= r;
}
float SE_PointAABB_DistanceSquare(SE_Vector3f* point, SE_AABB* aabb)
{
    float sqDist = 0.0f;
    int i;
    for(i = 0 ; i < 3 ; i++)
    {
        float v = point->d[i];
        if(v < aabb->min.d[i]) sqDist += (aabb->min.d[i] - v) * (aabb->min.d[i] - v);
        if(v > aabb->max.d[i]) sqDist += (v - aabb->max.d[i]) * (v - aabb->max.d[i]);
    }
    return sqDist;
}
int SE_Intersect_Sphere_AABB(SE_Sphere* sphere, SE_AABB* aabb)
{
    float sqDist = SE_PointAABB_DistanceSquare(&sphere->center, aabb);
    return sqDist <= sphere->radius * sphere->radius;
}
int SE_Intersect_MovingSphereStaticAABB(SE_Sphere sphere, SE_AABB* aabb, SE_Vector3f endPoint, SE_Vector3f* out)
{
    SE_Sphere s;
    SE_Vector3f tmp1, tmp2;
    float r, interval;
    SE_Vec3f_Subtract(&endPoint, &sphere.center, &tmp1);
    SE_Vec3f_Mul(&tmp1, 0.5, &tmp2);
    SE_Vec3f_Add(&sphere.center, &tmp2, &s.center);
    r = SE_Vec3f_Length(&tmp2) + sphere.radius;
    s.radius = r;
    if(!SE_Intersect_Sphere_AABB(&s, aabb))
        return 0;
    interval = SE_Vec3f_Length(&tmp1);
    if(interval < 2.0f)
    {
        *out = sphere.center;
        return 1;
    }
    if(SE_Intersect_MovingSphereStaticAABB(sphere, aabb, s.center, out))
        return 1;
    return SE_Intersect_MovingSphereStaticAABB(s, aabb, endPoint, out);
}
int SE_Intersect_MovingSphereStaticPlane(const SE_Sphere* sphere, const SE_Plane* plane, const SE_Vector3f* dirOfSphere, SE_Vector3f* out)
{
    SE_Vector3f planeNormal;
    SE_Plane_GetNormal(plane, &planeNormal);
    float planeD = SE_Plane_GetD(plane);
    float dist = SE_Vec3f_Dot(&planeNormal, &sphere->center) - planeD;
    if(SE_Fabs(dist) <= sphere->radius)
    {
        SE_Vec3f_Copy(&sphere->center, out);
        return 1;
    }
    else
    {
        float denom = SE_Vec3f_Dot(&planeNormal, dirOfSphere);
        if(denom * dist >= 0.0f)
        {
            return 0;
        }
        else
        {
            float r = dist >0.0f ? sphere->radius : -sphere->radius;
            float t = (r - dist) / denom;
            if(t >= 0.0f && t <= 1.0f)
            {
                SE_Vector3f tmp1, tmp2, tmp3;
                SE_Vec3f_Mul(dirOfSphere, t, &tmp1);
                SE_Vec3f_Add(&sphere->center, &tmp1, &tmp2);
                SE_Vec3f_Mul(&planeNormal, r, &tmp3);
                SE_Vec3f_Subtract(&tmp2, &tmp3, out);
                return 1;
            }
            else
            {
                return 0;
            }
        }
    }
    
}
int SE_Intersect_MovingOBBStaticAABB(const SE_OBB obb, const SE_AABB* aabb, const SE_Vector3f endCenter, int axis, SE_OBB* out)
{
    SE_OBB aabbObb;
    SE_OBB obbWithEnd;
    SE_Vector3f aabbCenter;
    SE_Vector3f aabbExtent;
    SE_Vector3f distV, mid;
    float moveAxisExtent; /*this is the extent of the moving direction */
    float interval;
    SE_AABB_GetCenter(aabb, &aabbCenter);
    SE_AABB_GetExtent(aabb, &aabbExtent);
    SE_Vec3f_Copy(&aabbCenter, &aabbObb.center);
    SE_Vec3f_Init(1, 0, 0, &aabbObb.axis[0]);
    SE_Vec3f_Init(0, 1, 0, &aabbObb.axis[1]);
    SE_Vec3f_Init(0, 0, 1, &aabbObb.axis[2]);
    aabbObb.e[0] = aabbExtent.x / 2;
    aabbObb.e[1] = aabbExtent.y / 2;
    aabbObb.e[2] = aabbExtent.z / 2;
    SE_Vec3f_Subtract(&endCenter, &obb.center, &distV);
    SE_Vec3f_Mul(&distV, 0.5, &mid);
    SE_Vec3f_Add(&obb.center, &mid, &obbWithEnd.center);
    SE_Vec3f_Copy(&obb.axis[0], &obbWithEnd.axis[0]);
    SE_Vec3f_Copy(&obb.axis[1], &obbWithEnd.axis[1]);
    SE_Vec3f_Copy(&obb.axis[2], &obbWithEnd.axis[2]);
    obbWithEnd.e[0] = obb.e[0];
    obbWithEnd.e[1] = obb.e[1];
    obbWithEnd.e[2] = obb.e[2];
    moveAxisExtent = SE_Vec3f_Length(&distV) + 2 * obb.e[axis];
    obbWithEnd.e[axis] = moveAxisExtent / 2;
    if(!SE_OBB_IntersectOBB(&aabbObb, &obbWithEnd))
       return 0;
    interval = SE_Vec3f_Length(&distV);
    if(interval < 2.0f)
    {
        *out = obb;
        return 1;
    }
    if(SE_Intersect_MovingOBBStaticAABB(obb, aabb, obbWithEnd.center, axis, out))
        return 1;
    SE_Vec3f_Copy(&obb.axis[0], &obbWithEnd.axis[0]);
    SE_Vec3f_Copy(&obb.axis[1], &obbWithEnd.axis[1]);
    SE_Vec3f_Copy(&obb.axis[2], &obbWithEnd.axis[2]);
    obbWithEnd.e[0] = obb.e[0];
    obbWithEnd.e[1] = obb.e[1];
    obbWithEnd.e[2] = obb.e[2];
    return SE_Intersect_MovingOBBStaticAABB(obbWithEnd, aabb, endCenter, axis, out);
}
