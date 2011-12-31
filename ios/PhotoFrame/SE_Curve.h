//
//  SE_Curve.h
//  PhotoFrame
//
//  Created by 陈勇 on 11-12-20.
//  Copyright 2011年 __MyCompanyName__. All rights reserved.
//

#ifndef SE_Curve_h
#define SE_Curve_h
#include "SE_Vector.h"
#include <vector>
#include <list>
class SE_Curve
{
public:
    struct SamplePoint
    {
        SE_Vector3f point;
        int time;
    };
    struct CurvePoint
    {
        SE_Vector3f point;
        float distanceToNext;
        bool bSamplePoint;
    };
    SE_Curve(SamplePoint* samplePoints, int pointNum, int splineCurvePointNum = 10);
    ~SE_Curve();
    int getVertexNum() const
    {
        return mVertexNum;
    }
    float* getData() const
    {
        return mData;
    }
    void setTension(float t)
    {
        mTension = t;
    }
    float getTension() const
    {
        return mTension;
    }
    int getFloatSize() const
    {
        return mFloatSize;
    }
    void createCurve();
    float getTotalCurveLength();
    size_t getSplineCurveNum();
    float getSplineCurveLength(int index);
    
    void startForwarding(float step);
    bool getNextCurvePoint(SE_Vector3f& outV);
    //if splineIndex == -1 it will get curve point at total curve
    std::vector<SE_Vector3f> getCurvePoint(int splineIndex, const std::vector<float>& stepvector);
    std::vector<SE_Vector3f> getCurvePoint(int startSamplePointIndex, int endSamplePointIndex, const std::vector<float>& stepvector);
private:
    void getSplineCurveStartEndPoint(int splineCurveIndex, int& start, int& end);
    std::vector<SE_Vector3f> getComposeCurvePoint(int startIndex, int endIndex, const std::vector<float>& stepvector);
private:
    float* mData;
    int mVertexNum;
    int mFloatSize;
    float mTension;
    int mCurvePointNum;
    std::vector<SamplePoint> mSamplePoints;
    std::vector<CurvePoint> mCurvePoints;
    std::vector<int> mSamplePointIndex;
    float mForwardingStep;
    size_t mCurrentPoint;
};

#endif
