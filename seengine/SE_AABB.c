#include "SE_AABB.h"
SE_Result SE_AABB_CreateFromPoints(SE_AABB* aabb, SE_Vector3f* points, int pointNum)
{
    SE_ASSERT(points);
    SE_ASSERT(aabb);
    SE_ASSERT(pointNum > 0);
    int i;
    SE_Vector3f min, max;
    SE_Vector3f* v = &points[0];
    SE_Vec3f_Copy(v, &min);
    SE_Vec3f_Copy(v, &max);
    for(i = 1 ; i < pointNum ; i++)
    {
        SE_Vector3f* v = &points[i];
        if(v->x < min.x)
            min.x = v->x;
        if(v->y < min.y)
            min.y = v->y;
        if(v->z < min.z)
            min.z = v->z;
        if(v->x > max.x)
            max.x = v->x;
        if(v->y > max.y)
            max.y = v->y;
        if(v->z > max.z)
            max.z = v->z;
    }
    SE_Vec3f_Copy(&min, &aabb->min);
    SE_Vec3f_Copy(&max, &aabb->max);
    return SE_VALID;
}
int SE_AABB_IntersectAABB(const SE_AABB* aabb1, const SE_AABB* aabb2)
{
    if(aabb1->max.x < aabb2->min.x || aabb1->min.x > aabb2->max.x)
    return 0;
    if(aabb1->max.y < aabb2->min.y || aabb1->min.y > aabb2->max.y)
    return 0;
    if(aabb1->max.z < aabb2->min.z || aabb1->min.z > aabb2->max.z)
    return 0;
    return 1;
}
SE_Result SE_AABB_GetExtent(const SE_AABB* aabb, SE_Vector3f* out)
{
    SE_Vec3f_Subtract(&aabb->max, &aabb->min, out);
    return SE_VALID;
}
SE_Result SE_AABB_GetCenter(const SE_AABB* aabb, SE_Vector3f* out)
{
    SE_Vector3f ext, exthalf;
    SE_AABB_GetExtent(aabb, &ext);
    SE_Vec3f_Mul(&ext, 0.5, &exthalf);
    SE_Vec3f_Add(&aabb->min, &exthalf, out);
    return SE_VALID;
}
SE_Result SE_AABB_Transform(const SE_AABB* aabbsrc, const SE_Matrix3f* rs, const SE_Vector3f* translate, SE_AABB* aabbdst)
{
    SE_ASSERT(aabbsrc);
    SE_ASSERT(rs);
    SE_ASSERT(translate);
    SE_ASSERT(aabbdst);
    int i, j;
    for(i = 0 ; i < 3 ; i++)
    {
        aabbdst->min.d[i] = aabbdst->max.d[i] = translate->d[i];
        for(j = 0 ; j < 3 ; j++)
        {
            float e = rs->d[i * 3 + j] * aabbsrc->min.d[j];
            float f = rs->d[i * 3 + j] * aabbsrc->max.d[j];
            if(e < f)
            {
                aabbdst->min.d[i] += e;
                aabbdst->max.d[i] += f;
            }
            else
            {
                aabbdst->min.d[i] += f;
                aabbdst->max.d[i] += e;
            }
        }
    } 
    return SE_VALID;
}

