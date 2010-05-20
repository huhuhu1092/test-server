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
    /*
    float tmin = 0.0;
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
            if(t2 > tmax)
                tmax = t2;
            if(tmin > tmax)
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
    */
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
