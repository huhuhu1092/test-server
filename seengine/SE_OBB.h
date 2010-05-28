#ifndef SE_OBB_H
#define SE_OBB_H
#include "SE_Vector.h"
#include "SE_Matrix.h"
#ifdef __cplusplus
extern "C" {
#endif
struct SE_AABB_tag;
/**
 * Region R = {x | x = center + r * axis[0] + s * axis[1] + t * axis[2], |r| <= e[0], |s| <= e[1], |t| <= e[2]}
 * */
typedef struct SE_OBB_tag
{
    SE_Vector3f center;
    SE_Vector3f axis[3]; //0: x , 1: y, 2: z;
    float e[3]; // positive halfwidth extent
} SE_OBB;
extern SE_Result SE_OBB_CreateFromPoints(SE_OBB* obb, SE_Vector3f* points, int numPoint);
/**
 * axis : -1 : no axis and no rotation, 0: x axis, 1 : y axis, 2: z axis
 * angle: the rotate angle around axis
 * */
extern SE_Result SE_OBB_CreateFromAABB(SE_OBB* obb, const struct SE_AABB_tag* aabb, int axis, float angle, const SE_Vector3f* translate);
/**
 * a is the main OBB, we will translate b to the coordinate of a.
 * 0: not intersect
 * 1: intersect
 * */
extern int SE_OBB_IntersectOBB(const SE_OBB* a, const SE_OBB* b);
#ifdef __cplusplus
}
#endif

#endif
