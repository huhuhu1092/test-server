#ifndef SE_OBB_H
#define SE_OBB_H
#include "SE_Common.h"
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
extern SE_Result SE_OBB_CreateFromAABB(SE_OBB* obb, const struct SE_AABB_tag* aabb, enum SE_AXIS_TYPE axis, float angle);
/**
 * a is the main OBB, we will translate b to the coordinate of a.
 * 0: not intersect
 * 1: intersect
 * */
extern int SE_OBB_IntersectOBB(const SE_OBB* a, const SE_OBB* b);
/**
 * the vertex is ranged from bottom face to top face
 * bottom face: 0, 1, 2, 3 four vertex counterclockwise
 * top face : 4 ,5 ,6 ,7 four vertex counterclockwise
 *           bottom face:
 *         
 *         |-----------> x
 *         |  0----3
 *         |  |    |
 *         |  |    |
 *         |  1----2
 *       z\|/
 *         
 *
 *
 *           top face:
 *           4----7
 *           |    |
 *           |    |
 *           5----6

 *
 * */
extern SE_Result SE_OBB_GetVertex(const SE_OBB* obb, SE_Vector3f points[]);
#ifdef __cplusplus
}
#endif

#endif
