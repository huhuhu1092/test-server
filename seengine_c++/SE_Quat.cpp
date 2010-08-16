#include "SE_Quat.h"
const SE_Quat SE_Quat::IDENTITY = SE_Quat(0, 0, 0, 1);
SE_Quat::SE_Quat(float angle, const SE_Vector3f& axis)
{
    set(angle, axis);
}
void SE_Quat::set(float angle, const SE_Vector3f& axis)
{
    if(axis.isZero())
    {
	    identity();
	    return;
    }
    SE_Vector3f axisNorm = axis.normalize();
    float radian = SE_AngleToRadian(angle) / 2;
    float sinRadian = SE_Sinf(radian);
    w = SE_Cosf(radian);
    x = axisNorm.x * sinRadian;
    y = axisNorm.y * sinRadian;
    z = axisNorm.z * sinRadian;
}
SE_Quat SE_Quat::conjugate() const
{
    return SE_Quat(-x, -y, -z, w);    
}
SE_Quat SE_Quat::mul(const SE_Quat& rq) const
{
    SE_Quat ret;
    ret.w = w * rq.w - x * rq.x - y * rq.y - z * rq.z;
    ret.x = w * rq.x + x * rq.w + y * rq.z - z * rq.y;
    ret.y = w * rq.y - x * rq.z + y * rq.w + z * rq.x;
    ret.z = w * rq.z + x * rq.y - y * rq.x + z * rq.w;
    return ret; 
}

SE_Quat SE_Quat::inverse() const
{
    SE_Quat conj = conjugate();
    float len = length();
    if(len == 0)
    {
	    return IDENTITY;
    }
    return SE_Quat(conj.x / len, conj.y / len, conj.z / len, conj.w / len);
}
float SE_Quat::length() const
{
    return SE_Sqrtf(w * w + x * x + y * y + z * z);
}
float SE_Quat::lengthSquare() const
{
    return w * w + x * x + y * y + z * z;
}
SE_Vector3f SE_Quat::map(const SE_Vector3f& v) const
{
    SE_Quat vq(v, 0);
    if(!vq.hasInverse())
    {
        return SE_Vector3f(0, 0, 0);
    }
    SE_Quat ret = mul(vq).mul(inverse());
    return SE_Vector3f(ret.x, ret.y , ret.z);
}
SE_Matrix3f SE_Quat::toMatrix3f() const
{
    float m00 = 1 - 2 * y * y - 2 * z * z;
    float m01 = 2 * x * y - 2 * w * z;
    float m02 = 2 * x * z + 2 * w * y;
    float m10 = 2 * x * y + 2 * w * z;
    float m11 = 1 - 2 * x * x - 2 * z * z;
    float m12 = 2 * y * z - 2 * w * x;
    float m20 = 2 * x * z - 2 * w * y;
    float m21 = 2 * y * z + 2 * w * x;
    float m22 = 1 - 2 * x * x - 2 * y * y;
    return SE_Matrix3f(m00, m01, m02, m10, m11, m12, m20, m21, m22);

}

