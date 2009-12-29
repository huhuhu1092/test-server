#include "OssMath.h"
#include <stdlib.h>
#include <string.h>
namespace Oss{
    const Vector3 Vector3::UNIT_X(1.0, 0.0, 0.0);
    const Vector3 Vector3::UNIT_Y(0.0, 1.0, 0.0);
    const Vector3 Vector3::UNIT_Z(0.0, 0.0, 1.0);
/////////////////////////////////////////////////////
const Matrix3 Matrix3::IDENTITY(1.0, 0.0, 0.0,
                                0.0, 1.0, 0.0,
                                0.0, 0.0, 1.0);
Matrix3::Matrix3()
{
    memset(m, 0, sizeof(Real) * 9);
}

Matrix3::Matrix3(Real a0, Real a1, Real a2, 
            Real a3, Real a4, Real a5,
            Real a6, Real a7, Real a8)
{
    m[0] = a0;
    m[1] = a1;
    m[2] = a2;
    m[3] = a3;
    m[4] = a4;
    m[5] = a5;
    m[6] = a6;
    m[7] = a7;
    m[8] = a8;
}
Matrix3::Matrix3(const Real* a)
{
    /*
    for(int i = 0 ; i < 9 ; i++)
    {
        m[i] = a[i];
    }
    */
    memcpy(this->m, a, sizeof(Real) * 9);

}

Matrix3::Matrix3(const Matrix3& m)
{
    memcpy(this->m, m.m, sizeof(Real) * 9);

}
Matrix3& Matrix3::operator=(const Matrix3& m)
{
    memcpy(this->m, m.m, sizeof(Real) * 9);
    return *this;
}
void Matrix3:: set(int index, Real value)
{
    m[index] = value;
}
void Matrix3::set(const Real* a)
{
    memcpy(m, a, sizeof(Real) * 9);
}
Real Matrix3::get(int index) const
{
    return m[index];
}
const Real* Matrix3::get() const
{
    return m;
}
Real Matrix3::det() const
{
    Real c00 = m[0] * (m[8] * m[4] - m[5] * m[7]);
    Real c01 = -m[4] * (m[8] * m[1] - m[2] * m[7]);
    Real c02 = m[8] * (m[5] * m[1] - m[2] * m[4]);
    return c00 + c01 + c02;
}
Matrix3 Matrix3::transpose() const
{
    Matrix3 ret;
    ret.m[0] = m[0];
    ret.m[1] = m[3];
    ret.m[2] = m[6];
    ret.m[3] = m[1];
    ret.m[4] = m[4];
    ret.m[5] = m[7];
    ret.m[6] = m[2];
    ret.m[7] = m[5];
    ret.m[8] = m[8];
    return ret;
}
bool Matrix3::inverse(Matrix3*  mOut) const
{
    if(mOut == NULL)
        return false;
    Real detM = det();
    if(Math::RealEqual(detM, 0.0, EPSLION3))
        return false;
    Matrix3 tmp;
    tmp.m[0] = (m[8] * m[4] - m[5] * m[7]) / detM;
    tmp.m[3] = -(m[8] * m[1] - m[2] * m[7]) / detM;
    tmp.m[6] = (m[5] * m[1] - m[2] * m[4]) / detM;
    tmp.m[1] = -(m[8] * m[3] - m[5] * m[6]) / detM;
    tmp.m[4] = (m[8] * m[0] - m[2] * m[6]) / detM;
    tmp.m[7] = -(m[5] * m[0] - m[2] * m[3]) / detM;
    tmp.m[2] = (m[7] * m[3] - m[4] * m[6]) / detM;
    tmp.m[5] = -(m[7] * m[0] - m[1] * m[6]) / detM;
    tmp.m[8] = (m[4] * m[0] - m[1] * m[3]) / detM;
    *mOut = tmp.transpose();
    return true;
}
Matrix3 Matrix3::mul(const Matrix3& m1, const Matrix3& m2)
{
    Matrix3 ret;
    ret.m[0] = m1.m[0] * m2.m[0] + m1.m[3] * m2.m[1] + m1.m[6] * m2.m[2];
    ret.m[3] = m1.m[0] * m2.m[3] + m1.m[3] * m2.m[4] + m1.m[6] * m2.m[5];
    ret.m[6] = m1.m[0] * m2.m[6] + m1.m[3] * m2.m[7] + m1.m[6] * m2.m[8];

    ret.m[1] = m1.m[1] * m2.m[0] + m1.m[4] * m2.m[1] + m1.m[7] * m2.m[2];
    ret.m[4] = m1.m[1] * m2.m[3] + m1.m[4] * m2.m[4] + m1.m[7] * m2.m[5];
    ret.m[7] = m1.m[1] * m2.m[6] + m1.m[4] * m2.m[7] + m1.m[7] * m2.m[8];

    ret.m[2] = m1.m[2] * m2.m[0] + m1.m[5] * m2.m[1] + m1.m[8] * m2.m[2];
    ret.m[5] = m1.m[2] * m2.m[3] + m1.m[5] * m2.m[4] + m1.m[8] * m2.m[5];
    ret.m[8] = m1.m[2] * m2.m[6] + m1.m[5] * m2.m[7] + m1.m[8] * m2.m[8];

    return ret;
    
}
Matrix4 Matrix3::transfer(const Matrix3& m)
{
    Matrix4 ret = Matrix4::IDENTITY;
    ret.set(0, m.get(0));
    ret.set(1, m.get(1));
    ret.set(2, m.get(2));
    ret.set(4, m.get(3));
    ret.set(5, m.get(4));
    ret.set(6, m.get(5));
    ret.set(8, m.get(6));
    ret.set(9, m.get(7));
    ret.set(10, m.get(8));
    return ret;
}
Vector3 Matrix3::map(const Vector3& v)
{
    Vector3 ret;
    ret.mX = m[0] * v.mX + m[3] * v.mY + m[6] * v.mZ;
    ret.mY = m[1] * v.mX + m[4] * v.mY + m[7] * v.mZ;
    ret.mZ = m[2] * v.mX + m[5] * v.mY + m[8] * v.mZ;
    return ret;
}

/////////////////////////////////////////////////////
const Matrix4 Matrix4::IDENTITY(1.0, 0.0, 0.0, 0.0,
                                0.0, 1.0, 0.0, 0.0,
                                0.0, 0.0, 1.0, 0.0,
                                0.0, 0.0, 0.0, 1.0);

Matrix4::Matrix4()
{
    memset(m, 0, sizeof(Real) * 16);
}
Matrix4::Matrix4(const Real* a)
{
    //memcpy(m, a, sizeof(Real) * 16);
    for(int i = 0 ; i < 16 ; i++)
    {
        this->m[i] = a[i];
    }

}
Matrix3 Matrix4::transfer(const Matrix4& m)
{
    Matrix3 ret = Matrix3::IDENTITY;
    ret.set(0, m.get(0));
    ret.set(1, m.get(1));
    ret.set(2, m.get(2));
    ret.set(3, m.get(4));
    ret.set(4, m.get(5));
    ret.set(5, m.get(6));
    ret.set(6, m.get(8));
    ret.set(7, m.get(9));
    ret.set(8, m.get(10));
    return ret;

}

Matrix4::Matrix4(Real a0, Real a1, Real a2, Real a3,
            Real a4, Real a5, Real a6, Real a7,
            Real a8, Real a9, Real  a10, Real a11, 
            Real a12, Real a13, Real a14, Real a15  )
{
    m[0] = a0;
    m[1] = a1;
    m[2] = a2;
    m[3] = a3;
    m[4] = a4;
    m[5] = a5;
    m[6] = a6;
    m[7] = a7;
    m[8] = a8;
    m[9] = a9;
    m[10] = a10;
    m[11] = a11;
    m[12] = a12;
    m[13] = a13;
    m[14] = a14;
    m[15] = a15;
}
Matrix4::Matrix4(const Matrix4& m)
{
    //memcpy(this->m, m.m, sizeof(Real) * 16);
    for(int i = 0 ; i < 16 ; i++)
    {
        this->m[i] = m.m[i];
    }

}
Matrix4& Matrix4::operator=(const Matrix4& m)
{
    //memcpy(this->m, m.m, sizeof(Real) * 16);
    for(int i = 0 ; i < 16 ; i++)
    {
        this->m[i] = m.m[i];
    }

    return *this;
}
void Matrix4::set(int index, Real value)
{
    m[index] = value;
}
void Matrix4::set(const Real* a)
{
    //memcpy(m, a, sizeof(Real) * 16);
    for(int i = 0 ; i < 16 ; i++)
    {
        this->m[i] = a[i];
    }

}
Real Matrix4::get(int index) const
{
    return m[index];
}
const Real* Matrix4::get() const
{
    return m;
}
Matrix4 Matrix4::transpose()
{
    Matrix4 tmp;
    tmp.m[0] = m[0];
    tmp.m[1] = m[4];
    tmp.m[2] = m[8];
    tmp.m[3] = m[12];
    tmp.m[4] = m[1];
    tmp.m[5] = m[5];
    tmp.m[6] = m[9];
    tmp.m[7] = m[13];
    tmp.m[8] = m[2];
    tmp.m[9] = m[6];
    tmp.m[10] = m[10];
    tmp.m[11] = m[14];
    tmp.m[12] = m[3];
    tmp.m[13] = m[7];
    tmp.m[14] = m[11];
    tmp.m[15] = m[15];
    return tmp;
        
}
Real Matrix4::det()
{
    
    return 0.0;
}
bool Matrix4::inverse(Matrix4* outM) const
{
    if(outM == NULL)
        return false;
    Matrix3 m3 = transfer(*this);
    Real detM3 = m3.det();
    if(Math::RealEqual(detM3, 0.0, EPSLION3))
        return false;
    Matrix3 inverseM3;
    m3.inverse(&inverseM3);
    Matrix4 tmp = Matrix3::transfer(inverseM3);
    Vector3 t;
    t.mX =  m[12];
    t.mY = m[13];
    t.mZ = m[14];
    t = inverseM3.map(t);
    tmp.m[12] = -t.mX;
    tmp.m[13] = -t.mY;
    tmp.m[14] = -t.mZ;
    *outM = tmp;
    return true;
}
void Matrix4::add(const Matrix4& m)
{
    for(int i = 0 ; i < 16 ; i++)
    {
        this->m[i] += m.m[i];
    }
}
void Matrix4::sub(const Matrix4& m)
{
    for(int i = 0 ; i < 16 ; i++)
    {
        this->m[i] -= m.m[i];
    }

}
/*
 *  0  4  8   12  |  0  4  8   12
//  1  5  9   13  |  1  5  9   13  
//  2  6  10  14  |  2  6  10  14
//  3  7  11  15  |  3  7  11  15
 * 
 */
Matrix4 Matrix4::mul(const Matrix4& first, const Matrix4& second)
{
    Matrix4 ret;
    ret.m[0] = first.m[0] * second.m[0] + first.m[4] * second.m[1] + first.m[8] * second.m[2] + first.m[12] * second.m[3];
    ret.m[4] = first.m[0] * second.m[4] + first.m[4] * second.m[5] + first.m[8] * second.m[6] + first.m[12] * second.m[7];
    ret.m[8] = first.m[0] * second.m[8] + first.m[4] * second.m[9] + first.m[8] * second.m[10] + first.m[12] * second.m[11];
    ret.m[12] = first.m[0] * second.m[12] + first.m[4] * second.m[13] + first.m[8] * second.m[14] + first.m[12] * second.m[15];

    ret.m[1] = first.m[1] * second.m[0] + first.m[5] * second.m[1] + first.m[9] * second.m[2] + first.m[13] * second.m[3];
    ret.m[5] = first.m[1] * second.m[4] + first.m[5] * second.m[5] + first.m[9] * second.m[6] + first.m[13] * second.m[7];
    ret.m[9] = first.m[1] * second.m[8] + first.m[5] * second.m[9] + first.m[9] * second.m[10] + first.m[13] * second.m[11];
    ret.m[13] = first.m[1] * second.m[12] + first.m[5] * second.m[13] + first.m[9] * second.m[14] + first.m[13] * second.m[15];

    ret.m[2] = first.m[2] * second.m[0] + first.m[6] * second.m[1] + first.m[10] * second.m[2] + first.m[14] * second.m[3];
    ret.m[6] = first.m[2] * second.m[4] + first.m[6] * second.m[5] + first.m[10] * second.m[6] + first.m[14] * second.m[7];
    ret.m[10] = first.m[2] * second.m[8] + first.m[6] * second.m[9] + first.m[10] * second.m[10] + first.m[14] * second.m[11];
    ret.m[14] = first.m[2] * second.m[12] + first.m[6] * second.m[13] + first.m[10] * second.m[14] + first.m[14] * second.m[15];

    ret.m[3] = first.m[3] * second.m[0] + first.m[7] * second.m[1] + first.m[11] * second.m[2] + first.m[15] * second.m[3];
    ret.m[7] = first.m[3] * second.m[4] + first.m[7] * second.m[5] + first.m[11] * second.m[6] + first.m[15] * second.m[7];
    ret.m[11] = first.m[3] * second.m[8] + first.m[7] * second.m[9] + first.m[11] * second.m[10] + first.m[15] * second.m[11];
    ret.m[15] = first.m[3] * second.m[12] + first.m[7] * second.m[13] + first.m[11] * second.m[14] + first.m[15] * second.m[15];
     return ret; 

}
void Matrix4::mulPre(const Matrix4& m)
{
    *this = mul(*this, m);    
}
void Matrix4::mulPost(const Matrix4& m)
{
    *this = mul(m , *this);
}
void Matrix4::mulScalar(Real factor)
{
    for(int i = 0 ; i < 16 ; i++)
    {
        m[i] *= factor;
    }
}
Vector4 Matrix4::map(const Vector4& v)
{
    Vector4 ret;
    ret.mX = m[0] * v.mX + m[4] * v.mY + m[8] * v.mZ + m[12] * v.mW;
    ret.mY = m[1] * v.mX + m[5] * v.mY + m[9] * v.mZ + m[13] * v.mW;
    ret.mZ = m[2] * v.mX + m[6] * v.mY + m[10] * v.mZ + m[14] * v.mW;
    ret.mW = m[3] * v.mX + m[7] * v.mY + m[11] * v.mZ + m[15] * v.mW;
    return ret;
}
void Matrix4::setRotate(const Vector3& ax, Real angle)
{
    //Matrix4 ret(IDENTITY);
    *this = IDENTITY;
    Real c = Math::CosAngle(angle);
    Real s = Math::SinAngle(angle);

    if(ax == Vector3::UNIT_X)
    {
        m[5] = c;
        m[9] = -s;
        m[6] = s;
        m[10] = c;
    }
    else if(ax == Vector3::UNIT_Y)
    {
        m[0] = c;
        m[2] = -s;
        m[8] = s;
        m[10] = c;        
    }
    else if(ax == Vector3::UNIT_Z)
    {
        m[0] = c;
        m[1] = s;
        m[4] = -s;
        m[5] = c;
    }
    else
    {
        Vector3 r(ax);
        if(r.isZero())
           return;
        r.normalize();
        m[0] = c + (1 - c) * r.mX * r.mX;
        m[1] = (1 - c) * r.mX * r.mY + s * r.mZ;
        m[2] = ( 1 - c) * r.mX * r.mZ - s * r.mY;

        m[4] = (1 - c) * r.mX * r.mY - s * r.mZ;
        m[5] = c + (1 - c) * r.mY * r.mY;
        m[6] = (1 - c) * r.mY * r.mZ + s * r.mX;
       
        m[8] = (1 - c) * r.mX * r.mZ + s * r.mY;
        m[9] = (1 - c) * r.mY * r.mZ - s * r.mX;
        m[10] = c + (1 - c) * r.mZ * r.mZ;   
    }
}
void Matrix4::setTranslate(Real x, Real y, Real z)
{
    *this = IDENTITY;
    m[12] = x;
    m[13] = y;
    m[14] = z;
}
void Matrix4::setScale(Real sx, Real sy, Real sz)
{
    *this = IDENTITY;
    m[0] = sx;
    m[5] = sy;
    m[10] = sz;
}

};
