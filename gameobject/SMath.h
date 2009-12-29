#ifndef SMATH_H
#define SMATH_H
//One Small Step Project
#include <math.h>
#define EPSILON1  1E-03
#define EPSLION2  1E-06
#define EPSLION3  1E-08
typedef float Real;
#define PI 3.1415926
class SMath
{
public:
    static Real Sqrt(Real f)
    {
        return sqrt(f);
    }
    static bool RealEqual(Real f1, Real f2, Real tolerance = EPSLION3)
    {
        Real dif = fabs(f1 - f2);
        if(dif <= tolerance)
            return true;
        else 
            return false;
    }
    static Real RealAbs(Real f)
    {
        return fabs(f);
    }
    
    static Real CosAngle(Real angle)
    {
        return cos( angle * PI / 180.0);
    }
    static Real SinAngle(Real angle)
    {
        return sin(angle * PI / 180.0);
    }
    static Real TanAngle(Real angle)
    {
        return tan(angle * PI / 180.0);
    }
    //return angle
    //input: should be [-1, 1]a
    //return: [0, 180]
    static Real ACosAngle(Real value)
    {
        return acos(value) * 180.0 / PI;
    }
    //return angle
    //input: should be [-1, 1]a
    //return: [0, 180]
    
    static Real ASinAngle(Real value)
    {
        return asin(value) * 180.0 / PI;
    }
    //return angle
    //input: should be [-1, 1]a
    //return: [-90, 90]
    static Real ATanAngle(Real value)
    {
        return atan(value) * 180.0 / PI;
    }
};
class SVector3
{
public:
    SVector3()
    {
        mX = mY = mZ = 0.0;
    }
    SVector3(Real x, Real y, Real z)
    {
        mX = x;
        mY = y;
        mZ = z;
    }
    SVector3(const SVector3& v)
    {
        mX = v.mX;
        mY = v.mY;
        mZ = v.mZ;
    }    
    SVector3& operator=(const SVector3& v)
    {
        mX = v.mX;
        mY = v.mY;
        mZ = v.mZ;
        return *this;
    }
    bool operator ==(const SVector3& v) const
    {
        return mX == v.mX && mY == v.mY && mZ == v.mZ;
    }
    void set(Real x, Real y, Real z)
    {
        mX = x;
        mY = y;
        mZ = z;
    }
    void add(const SVector3& v)
    {
        mX += v.mX;
        mY += v.mY;
        mZ += v.mZ;
    }
    void subst(const SVector3& v)
    {
        mX -= v.mX;
        mY -= v.mY;
        mZ -= v.mZ;
    }
    void mul(Real factor)
    {
        mX *= factor;
        mY *= factor;
        mZ *= factor;
    }
    Real lengthSquare() const
    {
        return mX * mX + mY * mY + mZ * mZ;
    }
    Real length() const
    {
        return Math::Sqrt(lengthSquare());
    }
    bool isZero() const
    {
        return mX == 0.0 && mY == 0.0 && mZ == 0.0;
    }
    void zero()
    {
        mX = mY = mZ = 0.0;
    }
    Real normalize()
    {
        Real l = length();
        if(!Math::RealEqual(l, 0.0f, EPSLION3))
        {
            mX /= l;
            mY /= l;
            mZ /= l;
        }
        return l;
    }
    Real dotProduct(const SVector3& v) const
    {
        return mX * v.mX + mY * v.mY + mZ * v.mZ;
    }
    Real AbsDosProduct(const SVector3 v) const
    {
        return Math::RealAbs(dotProduct(v));
    }
    SVector3 crossProduct(const SVector3& v) const
    {
        SVector3 ret;
        ret.mX = mY * v.mZ - mZ * v.mY;
        ret.mY = mZ * v.mX - mX * v.mZ;
        ret.mZ = mX * v.mY - mY * v.mX;
        return ret;
 
    } 
    SVector3 perpendicular() const
    {
        SVector3 tmp(mX, mY, mZ);
        SVector3 cp = tmp.crossProduct(SVector3::UNIT_X);
        Real mag = cp.length();
        Real selfMag = length();
        if(Math::RealEqual(selfMag, 0.0, EPSLION2))
            return *this;
        Real sinalpha = mag / (Real)(length() * UNIT_X.length());
        if(Math::RealEqual(sinalpha, 0.0, EPSLION2))
        {
            cp = tmp.crossProduct(SVector3::UNIT_Y);
        }
        return cp;
    }   
public:
    static const SVector3 UNIT_X;
    static const SVector3 UNIT_Y;
    static const SVector3 UNIT_Z;
public:
    Real mX;
    Real mY;
    Real mZ;
};
class SVector4
{
public:
    Real mX, mY, mZ, mW;
    SVector4()
    {
        mX = mY = mZ = mW = 0.0;
    }
    SVector4(Real x, Real y, Real z, Real w) : mX(x), mY(y), mZ(z), mW(w)
    {}
    SVector4(const SVector4& v)
    {
        mX = v.mX;
        mY = v.mY;
        mZ = v.mZ;
        mW = v.mW;
    }
    SVector4& operator=(const SVector4& v)
    {
        mX = v.mX;
        mY = v.mY;
        mZ = v.mZ;
        mW = v.mW;
        return *this;
    }
    void set(Real x, Real y, Real z, Real w)
    {
        mX = x;
        mY = y;
        mZ = z;
        mW = w;
    }
    bool operator==(const SVector4& v) const
    {
        return mX == v.mX && mY == v.mY && mZ == v.mZ && mW == v.mW;
    }
    void add(const SVector4& v)
    {
        mX += v.mX;
        mY += v.mY;
        mZ += v.mZ;
        mW += v.mW;
    }
    void sub(const SVector4& v)
    {
        mX -= v.mX;
        mY -= v.mY;
        mZ -= v.mZ;
        mW -= v.mW;
    }
    void mul(Real factor)
    {
        mX *= factor;
        mY *= factor;
        mZ *= factor;
        mW *= factor;
    }
};
// 0 3 6 |  0 3 6
// 1 4 7 |  1 4 7
// 2 5 8 |  2 5 8
class SSMatrix4;
class SMatrix3
{
public:
    Matrix3();
    Matrix3(Real a0, Real a1, Real a2, 
            Real a3, Real a4, Real a5,
            Real a6, Real a7, Real a8);
    Matrix3(const Real* a);
    Matrix3(const Matrix3& m);
    Matrix3& operator=(const Matrix3& m);
    void set(int index, Real value);
    void set(const Real* a);
    Real get(int index) const;
    const Real* get() const;
    Real det() const;
    Matrix3 transpose() const;
    bool inverse(Matrix3* mOut) const;
    SVector3 map(const SVector3& v);
    Matrix3 mul(const Matrix3& m1, const Matrix3& m2);
    static SMatrix4 transfer(const Matrix3& m);
public:
    static const Matrix3 IDENTITY;
private:
    Real m[9];
};
// SVector3 use column vector
// -    -
// | x  |
// | y  |
// | z  |
// -    -
//Matrix use column sequence
//  0  4  8   12    Ux  Vx  Zx Tx
//  1  5  9   13    Uy  Vy  Zy Ty
//  2  6  10  14    Uz  Vz  Zz Tz
//  3  7  11  15    0   0   0  1
//
class SMatrix4
{
public:
    SMatrix4();
    SMatrix4(Real a0, Real a1, Real a2, Real a3,
            Real a4, Real a5, Real a6, Real a7,
            Real a8, Real a9, Real  a10, Real a11, 
            Real a12, Real a13, Real a14, Real a15  );
    SMatrix4(const Real* a);
    SMatrix4(const SMatrix4& m);
    SMatrix4& operator=(const SMatrix4& m);
    void set(int index, Real value);
    void set(const Real* a);
    Real get(int index) const;
    const Real* get() const;
    SMatrix4 transpose();
    Real det();
    bool inverse(SMatrix4* outM) const;
    void add(const SMatrix4& m);
    void sub(const SMatrix4& m);
    // this * m
    void mulPre(const SMatrix4& m);
    // m * this
    void mulPost(const SMatrix4& m);
    void mulScalar(Real factor);
    SVector4 map(const SVector4& v);
    //set rotate angle
    void setRotate(const SVector3& ax, Real angle);
    void setTranslate(Real x, Real y, Real z);
    void setScale(Real sx, Real sy, Real sz);
    static Matrix3 transfer(const SMatrix4& m);
public:
    static const SMatrix4 IDENTITY;
private:
    SMatrix4 mul(const SMatrix4& first, const SMatrix4& second);
private:
    Real m[16];
};
#endif
