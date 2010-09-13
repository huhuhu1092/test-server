#include "SE_Layer.h"
SE_Layer::SE_Layer()
{
    mLayer = LAYER1;
}
SE_Layer::SE_Layer(int layer) : mLayer(layer)
{
    
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
bool operator==(const SE_Layer& left, const SE_Layer& right)
{
    return left.mLayer == right.mLayer;
}
bool operator<(const SE_Layer& left, const SE_Layer& right)
{
    return left.mLayer < right.mLayer;
}
bool operator>(const SE_Layer& left, const SE_Layer& right)
{
    return left.mLayer > right.mLayer;
}
SE_Layer operator+(const SE_Layer& left, const SE_Layer& right)
{
    return SE_Layer(left.mLayer + right.mLayer);
}
SE_Layer operator-(const SE_Layer& left, const SE_Layer& right)
{
    return SE_Layer(left.mLayer - right.mLayer);
}
