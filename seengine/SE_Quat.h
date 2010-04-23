#ifndef SE_QUAT_H
#define SE_QUAT_H

#include "SE_Vector.h"
#include "SE_Matrix.h"
#ifdef __cplusplus
extern "C" {
#endif

typedef struct SE_Quat_tag
{
    union
    {
        float d[4];
        struct
        {
            SE_Vector3f v;
            float w;
        };
    };
} SE_Quat;

extern SE_Result SE_Quat_Init(float x, float y, float z, float w, SE_Quat* out);
extern SE_Result SE_Quat_InitFromVector(const SE_Vector3f* v, float w, SE_Quat* out);
extern SE_Result SE_Quat_InitFromArray(float data[4], SE_Quat* out);
extern SE_Result SE_Quat_InitFromAngleAxis(float angle, const SE_Vector3f* axis, SE_Quat* out);
extern SE_Result SE_Quat_Identity(SE_Quat* q);
extern SE_Result SE_Quat_Clear(SE_Quat* q);
extern SE_Result SE_Quat_IsZero(const SE_Quat* q);
extern SE_Result SE_Quat_Neg(const SE_Quat* q, SE_Quat* out);
extern SE_Result SE_Quat_Add(const SE_Quat* q1, const SE_Quat* q2, SE_Quat* out);
extern SE_Result SE_Quat_Sub(const SE_Quat* q1, const SE_Quat* q2, SE_Quat* out);
extern SE_Result SE_Quat_Mul(const SE_Quat* q1, const SE_Quat* q2, SE_Quat* out);
extern SE_Result SE_Quat_Conjugate(const SE_Quat* q1, SE_Quat* out);
extern SE_Result SE_Quat_Inverse(const SE_Quat* q1, SE_Quat* out);
extern float SE_Quat_Length(const SE_Quat* q);
extern float SE_Quat_LengthSquare(const SE_Quat* q);
extern SE_Result SE_Quat_MapVec3f(const SE_Quat* q, const SE_Vector3f* v, SE_Vector3f* out);
extern SE_Result SE_Quat_ToMatrix3f(const SE_Quat* q, SE_Matrix3f* out);

#ifdef __cplusplus
}
#endif
#endif
