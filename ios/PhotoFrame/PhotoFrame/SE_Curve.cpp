//
//  SE_Curve.cpp
//  PhotoFrame
//
//  Created by 陈勇 on 11-12-20.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#include "SE_Curve.h"
#include "SE_Interpolate.h"
#include "SE_Log.h"
#include <algorithm>
///
static bool isSamplePoint(int index, std::vector<int>& samplePointIndex)
{
    std::vector<int>::iterator it = std::find(samplePointIndex.begin(),samplePointIndex.end(), index);
    return it != samplePointIndex.end();
}
///
SE_Curve::SE_Curve(SamplePoint* samplePoints, int pointNum, int splineCurvePointNum)
{
    mData = NULL;
    mVertexNum = 0;
    mFloatSize = 0;
    mTension = 0.5;
    mSamplePoints.resize(pointNum);
    mCurvePointNum = splineCurvePointNum;
    for(int i = 0 ; i < pointNum ; i++)
    {
        mSamplePoints[i] = samplePoints[i];
    }
    mSamplePointIndex.resize(pointNum);
    mForwardingStep = 0;
    mCurrentPoint = 0;
}
SE_Curve::~SE_Curve()
{
    delete[] mData;
}
void SE_Curve::createCurve()
{
    if(mSamplePoints.size() < 3)
    {
        LOGI("curve key point must be greater than 3\n");
        return;
    }
    int curvePointNum = mCurvePointNum;
    float s = (1 - mTension) / 2;
    int curveNum = mSamplePoints.size() - 1;
    float tstep = 1.0 / (float)curvePointNum;
    size_t pointNum = mSamplePoints.size();
    std::vector<int> samplePointIndex;
    samplePointIndex.resize(mSamplePoints.size());
    mVertexNum = curvePointNum * curveNum + 1;
    mFloatSize = mVertexNum * 3;
    mData = new float[mFloatSize];
    // calculate p0 --> p1
    SE_Vector3f v = mSamplePoints[2].point - mSamplePoints[0].point;
    v.mul(s);
    SE_HermiteInterpolate hip;
    SE_Vector4f hipx = hip.interpolate(mSamplePoints[0].point.x, mSamplePoints[1].point.x, 0, v.x);
    SE_Vector4f hipy = hip.interpolate(mSamplePoints[0].point.y, mSamplePoints[1].point.y, 0, v.y);
    SE_Vector4f hipz = hip.interpolate(mSamplePoints[0].point.z, mSamplePoints[1].point.z, 0, v.z);
    float t = 0;
    int j = 0;
    samplePointIndex[0] = 0;
    for(int i = 0 ; i < curvePointNum ; i++)
    {
        mData[j++] = hipx.x * t * t * t + hipx.y * t * t + hipx.z * t + hipx.w;
        mData[j++] = hipy.x * t * t * t + hipy.y * t * t + hipy.z * t + hipy.w;
        mData[j++] = hipz.x * t * t * t + hipz.y * t * t + hipz.z * t + hipz.w;
        t += tstep;
    }
    // calculate p1 --> p(n - 2)
    SE_CardinalSplineInterpolate csInterpolate;
    for(int k = 1 ; k < (pointNum - 2) ; k++)
    {
        SE_CardinalSplineInterpolate::Interpolator ipx = csInterpolate.getInterpolator(mSamplePoints[k - 1].point.x, mSamplePoints[k].point.x, mSamplePoints[k + 1].point.x, mSamplePoints[k + 2].point.x, s);
        SE_CardinalSplineInterpolate::Interpolator ipy = csInterpolate.getInterpolator(mSamplePoints[k - 1].point.y, mSamplePoints[k].point.y, mSamplePoints[k + 1].point.y, mSamplePoints[k + 2].point.y, s);
        SE_CardinalSplineInterpolate::Interpolator ipz =  csInterpolate.getInterpolator(mSamplePoints[k - 1].point.z, mSamplePoints[k].point.z, mSamplePoints[k + 1].point.z, mSamplePoints[k + 2].point.z, s);
        t =  0;
        samplePointIndex[k] = j;
        for(int i = 0 ; i < curvePointNum ; i++)
        {
            mData[j++] = ipx.calc(t);
            mData[j++] = ipy.calc(t);
            mData[j++] = ipz.calc(t);
            t += tstep;
        }
    }
    //calculate p(n - 1) --> pn
    v = mSamplePoints[pointNum - 1].point - mSamplePoints[pointNum - 3].point;
    v.mul(s);
    hipx = hip.interpolate(mSamplePoints[pointNum - 2].point.x, mSamplePoints[pointNum - 1].point.x, v.x, 0);
    hipy = hip.interpolate(mSamplePoints[pointNum - 2].point.y, mSamplePoints[pointNum - 1].point.y, v.y, 0);
    hipz = hip.interpolate(mSamplePoints[pointNum - 2].point.z, mSamplePoints[pointNum - 1].point.z, v.z, 0);
    t = 0;
    samplePointIndex[pointNum - 2] = j;
    for(int i = 0 ; i < curvePointNum ; i++)
    {
        mData[j++] = hipx.x * t * t * t + hipx.y * t * t + hipx.z * t + hipx.w;
        mData[j++] = hipy.x * t * t * t + hipy.y * t * t + hipy.z * t + hipy.w;
        mData[j++] = hipz.x * t * t * t + hipz.y * t * t + hipz.z * t + hipz.w;
        t += tstep;
    }
    //last point pn
    samplePointIndex[pointNum - 1] = j;
    mData[j++] = mSamplePoints[pointNum - 1].point.x;
    mData[j++] = mSamplePoints[pointNum - 1].point.y;
    mData[j++] = mSamplePoints[pointNum - 1].point.z;
    SE_ASSERT(j == mFloatSize);
    mCurvePoints.resize(mVertexNum);
    int i = 0;
    int sp = 0;
    for(i = 0 ; i < (mVertexNum - 1); i++)
    {
        float x1 = mData[3 * i];
        float y1 = mData[3 * i + 1];
        float z1 = mData[3 * i + 2];
        float x2 = mData[3 * (i + 1)];
        float y2 = mData[3 * (i + 1) + 1];
        float z2 = mData[3 * (i + 1) + 2];
        bool b = isSamplePoint(3 * i, samplePointIndex);
        CurvePoint p;
        p.bSamplePoint = b;
        p.point.x = x1;
        p.point.y = y1;
        p.point.z = z1;
        SE_Vector3f v = SE_Vector3f(x1, y1, z1) - SE_Vector3f(x2, y2, z2);
        p.distanceToNext = v.length();
        mCurvePoints[i] = p;
        if(b)
        {
            mSamplePointIndex[sp++] = i;
        }
    }
    SE_ASSERT(sp == mSamplePoints.size() - 1);
    mSamplePointIndex[sp] = mVertexNum - 3;
    mCurvePoints[i].point.x = mData[3 * mVertexNum - 3];
    mCurvePoints[i].point.y = mData[3 * mVertexNum - 2];
    mCurvePoints[i].point.z = mData[3 * mVertexNum - 1];
    mCurvePoints[i].distanceToNext = 0;
}

float SE_Curve::getTotalCurveLength()
{
    float dist = 0;
    for(size_t i = 0 ; i < mCurvePoints.size() ; i++)
    {
        dist += mCurvePoints[i].distanceToNext;
    }
    return dist;
}
size_t SE_Curve::getSplineCurveNum()
{
    return mSamplePoints.size() - 1;
}
void SE_Curve::getSplineCurveStartEndPoint(int splineCurveIndex, int& start, int& end)
{
    int startIndex = splineCurveIndex;
    int endIndex = splineCurveIndex + 1;
    start = mSamplePointIndex[startIndex];
    end = mSamplePointIndex[endIndex];
}
float SE_Curve::getSplineCurveLength(int index)
{
    int start, end;
    getSplineCurveStartEndPoint(index, start, end);
    float dist = 0;
    for(int i = start ; i < end ; i++)
    {
        dist += mCurvePoints[i].distanceToNext;
    }
    return dist;
}
void SE_Curve::startForwarding(float step)
{
    mForwardingStep = step;
    mCurrentPoint = 0;
}
std::vector<SE_Vector3f> SE_Curve::getCurvePoint(int startSamplePointIndex, int endSamplePointIndex, const std::vector<float>& stepvector)
{
    int start, end;
    start = mSamplePointIndex[startSamplePointIndex];
    end = mSamplePointIndex[endSamplePointIndex];
    return getComposeCurvePoint(start, end, stepvector);
}
std::vector<SE_Vector3f> SE_Curve::getComposeCurvePoint(int startIndex, int endIndex, const std::vector<float>& stepvector)
{
    std::list<SE_Vector3f> points;
    size_t start = startIndex;
    std::vector<float>::const_iterator it;
    points.push_back(mCurvePoints[startIndex].point);
    for(it = stepvector.begin() ; it != stepvector.end() ; it++)
    {
        float step = *it;
        size_t i;
        float dist = 0;
        for(i = start ; i < endIndex ; i++)
        {
            if(dist < step)
            {
                dist += mCurvePoints[i].distanceToNext;
            }
            else
            {
                float dif = dist - step;
                LOGI("## dif = %f ##\n", dif);
                break;
            }
        }
        if(i < endIndex)
        {
            start = i;
            points.push_back(mCurvePoints[i].point);
        }
        else
        {
            points.push_back(mCurvePoints[i - 1].point);
            break;
        }
    }
    std::vector<SE_Vector3f> ret(points.size());
    std::copy(points.begin(), points.end(), ret.begin());
    return ret;
}
std::vector<SE_Vector3f> SE_Curve::getCurvePoint(int splineIndex, const std::vector<float>& stepvector)
{
    std::list<SE_Vector3f> points;
    int startIndex, endIndex;
    if(splineIndex == -1)
    {
        startIndex = 0;
        endIndex = mCurvePoints.size();
    }
    else
    {
        getSplineCurveStartEndPoint(splineIndex, startIndex, endIndex);
    }
    size_t start = startIndex;
    std::vector<float>::const_iterator it;
    points.push_back(mCurvePoints[startIndex].point);
    for(it = stepvector.begin() ; it != stepvector.end() ; it++)
    {
        float step = *it;
        size_t i;
        float dist = 0;
        for(i = start ; i < endIndex ; i++)
        {
            if(dist < step)
            {
                dist += mCurvePoints[i].distanceToNext;
            }
            else
            {
                float dif = dist - step;
                LOGI("## dif = %f ##\n", dif);
                break;
            }
        }
        if(i < endIndex)
        {
            start = i;
            points.push_back(mCurvePoints[i].point);
        }
        else
        {
            points.push_back(mCurvePoints[i - 1].point);
            break;
        }
    }
    std::vector<SE_Vector3f> ret(points.size());
    std::copy(points.begin(), points.end(), ret.begin());
    return ret;
}
bool SE_Curve::getNextCurvePoint(SE_Vector3f& outV)
{
    float dist = 0;
    size_t i;
    if(mCurrentPoint == (mCurvePoints.size() - 1))
        return false;
    for(i = mCurrentPoint ; i < mCurvePoints.size() ; i++)
    {
        if(dist < mForwardingStep)
            dist += mCurvePoints[i].distanceToNext;
        else
        {
            float dif = dist - mForwardingStep;
            LOGI("## dif = %f ##\n", dif);
            break;
        }
    }
    if(i < mCurvePoints.size())
    {
        mCurrentPoint = i;
        outV = mCurvePoints[i].point;
        return true;
    }
    else
    {
        mCurrentPoint = mCurvePoints.size() - 1;
        outV = mCurvePoints[mCurvePoints.size() - 1].point;
        return true;
    }
}
