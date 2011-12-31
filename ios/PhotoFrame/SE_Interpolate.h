//
//  SE_Interpolate.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-12-16.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#ifndef SE_Interpolate_h
#define SE_Interpolate_h
#include "SE_Vector.h"
#include "SE_Matrix.h"
//this use Hermite Curve to do interpolate
class SE_HermiteInterpolate
{
public:
    //return : x is cofactor of t3, y is cofactor of t2, z is cofactor of t, w is cofactor of 1
    SE_Vector4f interpolate(float n1, float n2, float s1, float s2);
};
class SE_CardinalSplineInterpolate
{
public:
    class Interpolator
    {
    public:
        float calc(float u)
        {
            return (-s * u * u * u + 2 * s * u * u - s * u) * p1 + ((2 - s ) * u * u * u + (s - 3) * u * u + 1) * p2 + ((s - 2) * u * u * u + (3 - 2 * s) * u * u + s * u) * p3 + (s * u * u * u - s * u * u) * p4;
        }
        float p1, p2, p3, p4, s;
    };
    Interpolator getInterpolator(float p1, float p2, float p3, float p4, float s)
    {
        Interpolator p;
        p.p1 = p1;
        p.p2 = p2;
        p.p3 = p3;
        p.p4 = p4;
        p.s = s;
        return p;
    }
};
class SE_VectorInterpolate
{
public:
    // reqiure v1 is not parallel to v2
    SE_VectorInterpolate(const SE_Vector3f& v1, const SE_Vector3f& v2)
    {
        mV1 = v1;
        mV2 = v2;
        mTheta = radianBetweenVector(v1, v2);
    }
    SE_VectorInterpolate()
    {
        mTheta = 0;
    }
    SE_Vector3f interpolate(float t);
private:
    SE_Vector3f mV1;
    SE_Vector3f mV2;
    float mTheta; //radian of v1 an v2
};
#endif
