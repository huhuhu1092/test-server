#include "SE_Quat.h"
#include "SE_Math.h"
#include "SE_Common.h"
#include <string.h>
SE_Result SE_Quat_Init(float x, float y, float z, float w, SE_Quat* out)
{
    SE_ASSERT(out);
    out->d[0] = x;
    out->d[1] = y;
    out->d[2] = z;
    out->d[3] = w;
    return 1;
}
SE_Result SE_Quat_InitFromVector(const SE_Vector3f* v, float w, SE_Quat* out)
{
    SE_ASSERT(v);
    SE_ASSERT(out);
    SE_Vec3f_Copy(v, &out->v);
    out->w = w;
    return 1;
}
SE_Result SE_Quat_InitFromArray(float data[4], SE_Quat* out)
{
    memcpy(out, data, sizeof(SE_Quat));
    return 1;
}
SE_Result SE_Quat_Identity(SE_Quat* q)
{
    SE_ASSERT(q);
    SE_Vec3f_Clear(&q->v);
    q->w = 1;
    return 1;
}
SE_Result SE_Quat_Neg(const SE_Quat* q, SE_Quat* out)
{
    SE_ASSERT(q);
    SE_ASSERT(out);
    out->w = -q->w;
    out->v.x = -q->v.x;
    out->v.y = -q->v.y;
    out->v.z = -q->v.z;
    return 1;
}
SE_Result SE_Quat_Add(const SE_Quat* q1, const SE_Quat* q2, SE_Quat* out)
{
    SE_ASSERT(q1);
    SE_ASSERT(q2);
    SE_ASSERT(out);
    out->v.x = q1->v.x + q2->v.x;
    out->v.y = q1->v.y + q2->v.y;
    out->v.z = q1->v.z + q2->v.z;
    out->w = q1->w + q2->w;
    return 1;
}
SE_Result SE_Quat_Sub(const SE_Quat* q1, const SE_Quat* q2, SE_Quat* out)
{
    SE_ASSERT(q1);
    SE_ASSERT(q2);
    SE_ASSERT(out);
    out->v.x = q1->v.x - q2->v.x;
    out->v.y = q1->v.y - q2->v.y;
    out->v.z = q1->v.z - q2->v.z;
    out->w = q1->w - q2->w;
    return 1;
}
SE_Result SE_Quat_Mul(const SE_Quat* q1, const SE_Quat* q2, SE_Quat* out)
{
    SE_ASSERT(q1);
    SE_ASSERT(q2);
    SE_ASSERT(out);
    out->w = q1->w * q2->w - q1->v.x * q2->v.x - q1->v.y * q2->v.y - q1->v.z * q2->v.z;
    out->v.x = q1->w * q2->v.x + q1->v.x * q2->w + q1->v.y * q2->v.z - q1->v.z * q2->v.y;
    out->v.y = q1->w * q2->v.y - q1->v.x * q2->v.z + q1->v.y * q2->w + q1->v.z * q2->v.x;
    out->v.z = q1->w * q2->v.z + q1->v.x * q2->v.y - q1->v.y * q2->v.x + q1->v.z * q2->w;
    return 1;
}
SE_Result SE_Quat_Conjugate(const SE_Quat* q, SE_Quat* out)
{
    SE_ASSERT(q);
    SE_ASSERT(out);
    out->w = q->w;
    out->v.x = -q->v.x;
    out->v.y = -q->v.y;
    out->v.z = -q->v.z;
    return 1;
}
SE_Result SE_Quat_Clear(SE_Quat* q)
{
    SE_ASSERT(q);
    memset(q, 0, sizeof(SE_Quat));
    q->w = 1.0f;
    return SE_VALID;
}
SE_Result SE_Quat_Inverse(const SE_Quat* q, SE_Quat* out)
{
    SE_ASSERT(q);
    SE_ASSERT(out);
    SE_Quat conj;
    SE_Quat_Conjugate(q, &conj);
    float len = SE_Quat_Length(q);
    if(len == 0)
    {
        SE_Quat_Clear(out);
        return 0;
    }    
    out->w = conj.w / len;
    out->v.x = conj.v.x / len;
    out->v.y = conj.v.y / len;
    out->v.z = conj.v.z / len;
    return 1;
}
float SE_Quat_Length(const SE_Quat* q)
{
    SE_ASSERT(q);
    float len = SE_Sqrtf(q->w * q->w + q->v.x * q->v.x + q->v.y * q->v.y + q->v.z * q->v.z);
    return len;
}
float SE_Quat_LengthSquare(const SE_Quat* q)
{
    return q->w * q->w + q->v.x * q->v.x + q->v.y * q->v.y + q->v.z * q->v.z;
}
SE_Result SE_Quat_InitFromAngleAxis(float angle, const SE_Vector3f* axis, SE_Quat* out)
{
    SE_ASSERT(axis);
    SE_ASSERT(out);
    if(SE_Vec3f_IsZero(axis))
    {
        SE_Quat_Clear(out);
        return 0;
    }
    SE_Vector3f axNorm;
    SE_Vec3f_Normalize(axis, &axNorm);
    float radian = SE_AngleToRadian(angle) / 2;
    float sinAngle = SE_Sinf(radian);
    out->w = SE_Cosf(radian);
    out->v.x = axNorm.x * sinAngle;
    out->v.y = axNorm.y * sinAngle;
    out->v.z = axNorm.z * sinAngle;
    return 1;
}
SE_Result SE_Quat_IsZero(const SE_Quat* q)
{
    if(q->w == 0 && q->v.x == 0 && q->v.y == 0 && q->v.z == 0)
        return 1;
    else 
        return 0;
}
SE_Result SE_Quat_MapVec3f(const SE_Quat* q, const SE_Vector3f* v, SE_Vector3f* out)
{
    SE_ASSERT(q);
    SE_ASSERT(v);
    SE_ASSERT(out);
    SE_Quat vq;
    SE_Quat_InitFromVector(v, 0.0f, &vq);
    SE_Quat qinverse;
    SE_Result ret = SE_Quat_Inverse(q, &qinverse);
    if(ret == 0)
    {
        SE_Vec3f_Clear(out);
        return 0;
    }
    SE_Quat result1;
    SE_Quat_Mul(q, &vq, &result1);
    SE_Quat result;
    SE_Quat_Mul(&result1, &qinverse, &result);
    out->x = result.v.x;
    out->y = result.v.y;
    out->z = result.v.z;
    return 1;
}
SE_Result SE_Quat_ToMatrix3f(const SE_Quat* q, SE_Matrix3f* out)
{
    SE_ASSERT(q);
    SE_ASSERT(out);
    float m00 = 1 - 2 * q->v.y * q->v.y - 2 * q->v.z * q->v.z;
    float m01 = 2 * q->v.x * q->v.y - 2 * q->w * q->v.z;
    float m02 = 2 * q->v.x * q->v.z + 2 * q->w * q->v.y;
    float m10 = 2 * q->v.x * q->v.y + 2 * q->w * q->v.z;
    float m11 = 1 - 2 * q->v.x * q->v.x - 2 * q->v.z * q->v.z;
    float m12 = 2 * q->v.y * q->v.z - 2 * q->w * q->v.x;
    float m20 = 2 * q->v.x * q->v.z - 2 * q->w * q->v.y;
    float m21 = 2 * q->v.y * q->v.z + 2 * q->w * q->v.x;
    float m22 = 1 - 2 * q->v.x * q->v.x - 2 * q->v.y * q->v.y;
    SE_Mat3f_Init(m00, m01, m02,
                  m10, m11, m12,
                  m20, m21, m22, out);
    return SE_VALID;
}

