#include "SE_Layer.h"
#include <stdio.h>
SE_Layer::SE_Layer()
{
}
SE_Layer::SE_Layer(int layer) 
{
    char buf[10];
    memset(buf, 0, 10);
    snprintf(buf, 9, "%d", layer);
    mLayer = buf;
}
SE_Layer::~SE_Layer()
{}
SE_Layer::SE_Layer(const SE_Layer& layer)
{
    mLayer = layer.mLayer;
}
SE_Layer& SE_Layer::operator=(const SE_Layer& layer)
{
    if(this == &layer)
        return *this;
    mLayer = layer.mLayer;
    return *this;
}
bool SE_Layer::operator==(const SE_Layer& left, const SE_Layer& right)
{
    return left.mLayer == right.mLayer;
}
bool SE_Layer::operator<(const SE_Layer& left, const SE_Layer& right)
{
    return left.mLayer < right.mLayer;
}
bool SE_Layer::operator>(const SE_Layer& left, const SE_Layer& right)
{
    return left.mLayer > right.mLayer;
}
SE_Layer SE_Layer::operator+(const SE_Layer& left, const SE_Layer& right)
{
    return SE_Layer(left.mLayer + right.mLayer);
}
/*
SE_Layer operator-(const SE_Layer& left, const SE_Layer& right)
{
    return SE_Layer(left.mLayer - right.mLayer);
}
*/
