#include "SAABBBox.h"
SAABBBox::SAABBBox(const SVector3& min, const SVector3& max )
{
    mMin = min;
    mMax = max;
}
SAABBBox::SAABBBox()
{
}
void SAABBBox::set(const SVector3& min, const SVector3& max)
{
    mMin = min;
    mMax = max;
}
SVector3 SAABBBox::getMin()
{
    return mMin;
}
SVector3 SAABBBox::getMax()
{
    return mMax;
}
bool SAABBBox::contain(const SVector3& point)
{
    return false;
}

