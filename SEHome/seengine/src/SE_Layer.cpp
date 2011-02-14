#include "SE_Layer.h"
#include <stdio.h>
SE_Layer::SE_Layer()
{
}
SE_Layer::SE_Layer(int layer) 
{
    char buf[10];
    memset(buf, 0, 10);
#if defined(WIN32)
    _snprintf(buf, 9, "%d", layer);
#else
    snprintf(buf, 9, "%d", layer);
#endif
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
