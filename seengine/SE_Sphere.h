#ifndef SE_SPHERE_H
#define SE_SPHERE_H

#include "SE_Common.h"
#include "SE_Vector.h"
#include "SE_Matrix.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_Sphere_tag
{
    SE_Vector3f center;
    float radius;
} SE_Sphere;
extern int SE_Sphere_IntersectSphere(const SE_Sphere* a, const SE_Sphere* b);
extern SE_Result SE_Sphere_CreateFromPoints(SE_Sphere* s, SE_Vector3f* points, int pointNum);
extern float SE_Sphere_GetRadius(const SE_Sphere* s);
extern SE_Result SE_Sphere_GetCenter(const SE_Sphere* s, SE_Vector3f* out);
/**
 * 0: not contain
 * 1: contain
 */
extern int SE_Sphere_ContainPoint(const SE_Sphere* sphere, const SE_Vector3f* point);
#ifdef __cplusplus
}
#endif

#endif
