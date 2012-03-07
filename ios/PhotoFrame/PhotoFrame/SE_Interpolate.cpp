//
//  SE_Interpolate.cpp
//  PhotoFrame
//
//  Created by 陈勇 on 11-12-16.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include "SE_Interpolate.h"

SE_Vector4f SE_HermiteInterpolate::interpolate(float n1, float n2, float s1, float s2)
{
    SE_Matrix4f m;
    m.setRow(0, SE_Vector4f(2, -2, 1, 1));
    m.setRow(1, SE_Vector4f(-3, 3, -2, -1));
    m.setRow(2, SE_Vector4f(0, 0, 1, 0));
    m.setRow(3, SE_Vector4f(1, 0, 0, 0));
    return m.map(SE_Vector4f(n1, n2, s1, s2));
}
SE_Vector3f SE_VectorInterpolate::interpolate(float t)
{
    float a1 = SE_Sinf((1 - t) * mTheta) / SE_Sinf(mTheta);
    float a2 = SE_Sinf(t * mTheta) / SE_Sinf(mTheta);
    return mV1.mul(a1) + mV2.mul(a2);
}