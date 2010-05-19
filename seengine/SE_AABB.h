#ifndef SE_AABB_H
#define SE_AABB_H

#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_AABB_tag
{
    SE_Vector3f min;
    SE_Vector3f max;
} SE_AABB;

extern SE_Result SE_AABB_CreateFromPoints(SE_AABB* aabb, SE_Vector3f* points, int pointNum);
/**
 * 0: not intersect
 * 1: intersect
 * */
extern int SE_AABB_IntersectAABB(const SE_AABB* aabb1, const SE_AABB* aabb2);
extern SE_Result SE_AABB_GetExtent(const SE_AABB* aabb, SE_Vector3f* out);
extern SE_Result SE_AABB_GetCenter(const SE_AABB* aabb, SE_Vector3f* out);
/*
 * rs is the rotate and scale matrix, matrix is column order 
 * translate is the translate vector
 * */
extern SE_Result SE_AABB_Transform(const SE_AABB* aabbsrc, const SE_Matrix3f* rs, const SE_Vector3f* translate, SE_AABB* aabbdst);
#ifdef __cplusplus
}
#endif
#endif
