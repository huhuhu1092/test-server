#ifndef SE_VECTOR_H
#define SE_VECTOR_H
#include "SE_Common.h"
#ifdef __cplusplus
extern "C" {
#endif
typedef struct SE_Vector2f_tag
{
    union 
    {
        float d[2];
        struct 
        {
            float x, y;
        };
    };
} SE_Vector2f;
typedef struct SE_Vector3f_tag
{
    union
    {
        float d[3];
        struct
        {
            float x, y, z;
        };
    };
} SE_Vector3f;
typedef struct SE_Vector4f_tag
{
    union
    {
        float d[4];
        struct
        {
            float x, y, z, w;
        };
    };
} SE_Vector4f;
extern SE_Result SE_Vec2f_Init(float x, float y, SE_Vector2f* out);
extern int SE_Vec2f_IsZero(const SE_Vector2f* v);
extern SE_Result SE_Vec2f_Compare(const SE_Vector2f* v1, const SE_Vector2f* v2);
extern SE_Result SE_Vec2f_Clear(SE_Vector2f* v);
extern SE_Result SE_Vec2f_Add(const SE_Vector2f* leftV, const SE_Vector2f* rightV, SE_Vector2f* out);
extern SE_Result SE_Vec2f_Subtract(const SE_Vector2f* leftV, const SE_Vector2f* rightV, SE_Vector2f* out);
extern float SE_Vec2f_Dot(const SE_Vector2f* leftV, const SE_Vector2f* rightV);
extern SE_Result SE_Vec2f_Mul(const SE_Vector2f* v, float scalar, SE_Vector2f* out);
extern SE_Result SE_Vec2f_Normalize(const SE_Vector2f* v, SE_Vector2f* out);
extern float SE_Vec2f_Length(const SE_Vector2f* v);
extern float SE_Vec2f_LengthSquare(const SE_Vector2f* v);

extern SE_Result SE_Vec3f_Init(float x, float y, float z, SE_Vector3f* out);
extern int SE_Vec3f_IsZero(const SE_Vector3f* v);
/**
 * euqal return 1 else return 0
 * */
extern int SE_Vec3f_Compare(const SE_Vector3f* v1, const SE_Vector3f* v2);
extern SE_Result SE_Vec3f_Clear(SE_Vector3f* v);
extern SE_Result SE_Vec3f_Copy(const SE_Vector3f* v, SE_Vector3f* out);
extern SE_Result SE_Vec3f_Add(const SE_Vector3f* leftV, const SE_Vector3f* rightV, SE_Vector3f* out);
extern SE_Result  SE_Vec3f_Subtract(const SE_Vector3f* leftV, const SE_Vector3f* rightV, SE_Vector3f* out);
extern float SE_Vec3f_Dot(const SE_Vector3f* leftV, const SE_Vector3f* rightV);
extern SE_Result SE_Vec3f_Neg(const SE_Vector3f* v, SE_Vector3f* out);
extern SE_Result  SE_Vec3f_Mul(const SE_Vector3f* v, float scalar, SE_Vector3f* out);
extern SE_Result SE_Vec3f_Normalize(const SE_Vector3f* v, SE_Vector3f* out);
extern float SE_Vec3f_Length(const SE_Vector3f* v);
extern float SE_Vec3f_LengthSquare(const SE_Vector3f* v);
extern SE_Result  SE_Vec3f_Cross(const SE_Vector3f* firstV, const SE_Vector3f* secondV, SE_Vector3f* out);
extern float SE_Vec3f_Distance(const SE_Vector3f* v1, const SE_Vector3f* v2);
extern SE_Result SE_Vec3f_Decompose(const SE_Vector3f* vDecomposed, const SE_Vector3f* vAxis, SE_Vector3f* vPerpendicular, SE_Vector3f* vParallel);
extern SE_Result SE_Vec3f_PointMove(const SE_Vector3f* point, const SE_Vector3f* dir, float dist, SE_Vector3f* out);
/**
 * this function implement w.(u X v)
 * if return value is 0 u v w is at the same plane
 * else it is the volume formed by u v w
 * */
extern float SE_Vec3f_ScalarTripleProduct(const SE_Vector3f* u, const SE_Vector3f* v, const SE_Vector3f* w);

extern SE_Result SE_Vec4f_Init(float x, float y, float z, float w, SE_Vector4f* out);
extern SE_Result SE_Vec4f_Copy(const SE_Vector4f* src, SE_Vector4f* out);
extern int SE_Vec4f_IsZero(const SE_Vector4f* v);
extern int SE_Vec4f_Compare(const SE_Vector4f* v1, const SE_Vector3f* v2);
extern SE_Result SE_Vec4f_Add(const SE_Vector4f* v1, const SE_Vector4f* v2, SE_Vector4f* out);
extern SE_Result SE_Vec4f_Subtract(const SE_Vector4f* v1, const SE_Vector4f* v2 ,SE_Vector4f* out);
extern float SE_Vec4f_Length(const SE_Vector4f* v);
extern float SE_Vec4f_LengthSquare(const SE_Vector4f* v);
extern SE_Result SE_Vec4f_Neg(const SE_Vector4f* v, SE_Vector4f* out);
extern float SE_Vec4f_Dot(const SE_Vector4f* v1, const SE_Vector4f* v2);

#ifdef __cplusplus
}
#endif
#endif
