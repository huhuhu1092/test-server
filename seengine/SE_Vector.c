#include "SE_Vector.h"
#include "SE_Common.h"
#include "SE_Math.h"
SE_Result SE_Vec2f_Init(float x, float y, SE_Vector2f* out)
{
    out->x = x;
    out->y = y;
    return SE_VALID;
}
int SE_Vec2f_IsZero(const SE_Vector2f* v)
{
    SE_ASSERT(v != NULL);
    if(v->x == 0 && v->y == 0)
        return 1;
    else
        return 0;
}
SE_Result SE_Vec2f_Compare(const SE_Vector2f* v1, const SE_Vector2f* v2)
{
    int i;
    for(i = 0 ; i < 2 ; i++)
    {
        if(SE_Fabs(v1->d[i] - v2->d[i]) > SE_VEC_EQUAL_EPSILON)
        {
            return 0;
        }
    }
    return 1;
}

SE_Result SE_Vec2f_Add(const SE_Vector2f* leftV, const SE_Vector2f* rightV, SE_Vector2f* out)
{
    out->x = leftV->x + rightV->x;
    out->y = leftV->y + rightV->y;
    return 1;
}
SE_Result SE_Vec2f_Subtract(const SE_Vector2f* leftV, const SE_Vector2f* rightV, SE_Vector2f* out)
{
    out->x = leftV->x - rightV->x;
    out->y = leftV->y - rightV->y;
    return 1;
}
float SE_Vec2f_Dot(const SE_Vector2f* leftV, const SE_Vector2f* rightV)
{
    return leftV->x * rightV->x + leftV->y * rightV->y;
}
SE_Result SE_Vec2f_Mul(const SE_Vector2f* v, float scalar, SE_Vector2f* out)
{
    out->x = v->x * scalar;
    out->y = v->y * scalar;
    return 1;
}
SE_Result SE_Vec2f_Normalize(const SE_Vector2f* v, SE_Vector2f* out)
{
    float len = SE_Vec2f_Length(v);
    if(len == 0)
    {
        out->x = 0;
        out->y = 0;
        return 0;
    }
    out->x = v->x / len;
    out->y = v->y / len;
    return 1;
}
float SE_Vec2f_Length(const SE_Vector2f* v)
{
    SE_ASSERT(v);
    return SE_Sqrtf(v->x * v->x + v->y * v->y );
}
float SE_Vec2f_LengthSquare(const SE_Vector2f* v)
{
    SE_ASSERT(v);
    return v->x * v->x + v->y * v->y;
}
SE_Result SE_Vec2f_Clear(SE_Vector2f* v)
{
    SE_ASSERT(v);
    v->x = v->y = 0;
    return 1;
}
/*
SE_Result SE_Vec2f_Cross(const SE_Vector2f* firstV, const SE_Vector2f* secondV, SE_Vector2f* out)
{
    
}
*/

SE_Result SE_Vec3f_Init(float x, float y, float z, SE_Vector3f* out)
{
    out->x = x;
    out->y = y;
    out->z = z;
    return SE_VALID;
}
int SE_Vec3f_IsZero(const SE_Vector3f* v)
{
    if(v->x == 0 && v->y ==0 && v->z == 0)
        return 1;
    else
        return 0;
}
int SE_Vec3f_Compare(const SE_Vector3f* v1, const SE_Vector3f* v2)
{
    int i;
    for(i = 0 ; i < 3 ; i++)
    {
        if(SE_Fabs(v1->d[i] - v2->d[i]) > SE_VEC_EQUAL_EPSILON)
        {
            return 0;
        }
    }
    return 1;
}
SE_Result SE_Vec3f_Copy(const SE_Vector3f* v, SE_Vector3f* out)
{
    SE_ASSERT(v);
    SE_ASSERT(out);
    out->x = v->x;
    out->y = v->y;
    out->z = v->z;
    return 1;
}
SE_Result SE_Vec3f_Add(const SE_Vector3f* leftV, const SE_Vector3f* rightV, SE_Vector3f* out)
{
    SE_ASSERT(leftV != NULL);
    SE_ASSERT(rightV != NULL);
    SE_ASSERT(out != NULL);
    out->x = leftV->x + rightV->x;
    out->y = leftV->y + rightV->y;
    out->z = leftV->z + rightV->z;
    return 1;
}
SE_Result SE_Vec3f_Subtract(const SE_Vector3f* leftV, const SE_Vector3f* rightV, SE_Vector3f* out)
{
    SE_ASSERT(leftV != NULL);
    SE_ASSERT(rightV != NULL);
    SE_ASSERT(out != NULL);
    out->x = leftV->x - rightV->x;
    out->y = leftV->y - rightV->y;
    out->z = leftV->z - rightV->z;
    return 1;    
}
float SE_Vec3f_Dot(const SE_Vector3f* leftV, const SE_Vector3f* rightV)
{
    SE_ASSERT(leftV != NULL);
    SE_ASSERT(rightV != NULL);
    return leftV->x * rightV->x + leftV->y * rightV->y + leftV->z * rightV->z;
}
SE_Result SE_Vec3f_Neg(const SE_Vector3f* v, SE_Vector3f* out)
{
    out->x = -v->x;
    out->y = -v->y;
    out->z = -v->z;
    return SE_VALID;
}
SE_Result SE_Vec3f_Mul(const SE_Vector3f* v, float scalar, SE_Vector3f* out)
{
    SE_ASSERT(v != NULL);
    SE_ASSERT(out != NULL);
    out->x = v->x * scalar;
    out->y = v->y * scalar;
    out->z = v->z * scalar;
    return 1;
}
SE_Result SE_Vec3f_Clear(SE_Vector3f* v)
{
    SE_ASSERT(v != NULL);
    v->x = v->y = v->z = 0;
    return SE_VALID;
}
SE_Result SE_Vec3f_Normalize(const SE_Vector3f* v, SE_Vector3f* out)
{
    float len, iLen;
    len = SE_Sqrtf(v->x * v->x + v->y * v->y + v->z * v->z);
    if(len == 0)
    {
        SE_Vec3f_Clear(out);
        return 0;
    }
    iLen = 1.0 / len;
    out->x = v->x * iLen;
    out->y = v->y * iLen;
    out->z = v->z * iLen;
    return SE_VALID;
}
float SE_Vec3f_Length(const SE_Vector3f* v)
{
    return SE_Sqrtf(v->x * v->x + v->y * v->y + v->z * v->z);
}
float SE_Vec3f_LengthSquare(const SE_Vector3f* v)
{
    return v->x * v->x + v->y * v->y + v->z * v->z;
}
SE_Result SE_Vec3f_Cross(const SE_Vector3f* firstV, const SE_Vector3f* secondV, SE_Vector3f* out)
{
    SE_ASSERT(firstV);
    SE_ASSERT(secondV);
    SE_ASSERT(out);
    out->x = firstV->y * secondV->z - firstV->z * secondV->y;
    out->y = firstV->z * secondV->x - firstV->x * secondV->z;
    out->z = firstV->x * secondV->y - firstV->y * secondV->x;
    return SE_VALID;

}
float SE_Vec3f_Distance(const SE_Vector3f* v1, const SE_Vector3f* v2)
{
    SE_Vector3f v;
    SE_Vec3f_Subtract(v2, v1, &v);
    return SE_Vec3f_Length(&v);
}
SE_Result SE_Vec3f_Decompose(const SE_Vector3f* vDecomposed, const SE_Vector3f* vAxis, SE_Vector3f* vPerpendicular, SE_Vector3f* vParallel)
{
    SE_ASSERT(!SE_Vec3f_IsZero(vDecomposed)) ;
    SE_ASSERT(!SE_Vec3f_IsZero(vAxis));
    float dotProduct = SE_Vec3f_Dot(vDecomposed, vAxis);
    float lenSquare = SE_Vec3f_LengthSquare(vAxis);
    SE_Vec3f_Mul(vAxis, dotProduct / lenSquare, vParallel);
    SE_Vec3f_Subtract(vDecomposed, vParallel, vPerpendicular);
    return SE_VALID;
}
SE_Result SE_Vec4f_Init(float x, float y, float z, float w, SE_Vector4f* out)
{
    out->x = x;
    out->y = y;
    out->z = z;
    out->w = w;
    return SE_VALID;
}
SE_Result SE_Vec4f_Copy(const SE_Vector4f* src, SE_Vector4f* out)
{
    if(src == out)
        return SE_VALID;
    out->x = src->x;
    out->y = src->y;
    out->z = src->z;
    out->w = src->w;
    return SE_VALID;
}
int SE_Vec4f_IsZero(const SE_Vector4f* v)
{
    if(v->x == 0.0f && v->y == 0.0f && v->z == 0.0f && v->w == 0.0f)
    {
        return 1;
    }
    else
        return 0;
}
int SE_Vec4f_Compare(const SE_Vector4f* v1, const SE_Vector3f* v2)
{
    int i;
    for(i = 0 ; i < 4 ; i++)
    {
        if(SE_Fabs(v1->d[i] - v2->d[i]) > SE_VEC_EQUAL_EPSILON)
        {
            return 0;
        }
    }
    return 1;

}
SE_Result SE_Vec4f_Add(const SE_Vector4f* v1, const SE_Vector4f* v2, SE_Vector4f* out)
{
    out->x = v1->x + v2->x;
    out->y = v1->y + v2->y;
    out->z = v1->z + v2->z;
    out->w = v1->w + v2->w;
    return SE_VALID;
}
SE_Result SE_Vec4f_Subtract(const SE_Vector4f* v1, const SE_Vector4f* v2 ,SE_Vector4f* out)
{
    out->x = v1->x - v2->x;
    out->y = v1->y - v2->y;
    out->z = v1->z - v2->z;
    out->w = v1->w - v2->w;
    return SE_VALID;

}
float SE_Vec4f_Length(const SE_Vector4f* v)
{
    SE_ASSERT(v);
    if(SE_Vec4f_IsZero(v))
        return 0.0f;
    return SE_Sqrtf(v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w);
}
float SE_Vec4f_LengthSquare(const SE_Vector4f* v)
{
    SE_ASSERT(v);
    if(SE_Vec4f_IsZero(v))
        return 0.0f;
    return (v->x * v->x + v->y * v->y + v->z * v->z + v->w * v->w);
}

SE_Result SE_Vec4f_Neg(const SE_Vector4f* v, SE_Vector4f* out)
{
    SE_ASSERT(v);
    SE_ASSERT(out);
    out->x = -v->x;
    out->y = -v->y;
    out->z = -v->z;
    out->w = -v->w;
    return SE_VALID;
}
float SE_Vec4f_Dot(const SE_Vector4f* v1, const SE_Vector4f* v2)
{
    SE_ASSERT(v1);
    SE_ASSERT(v2);
    return v1->x * v2->x + v1->y * v2->y + v1->z * v2->z + v1->w * v2->w;
}

