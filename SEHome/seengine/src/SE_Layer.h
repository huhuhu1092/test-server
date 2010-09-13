#ifndef SE_LAYER_H
#define SE_LAYER_H
#include "SE_Common.h"
class SE_Layer
{
public:
    enum {LAYER1, LAYER2, LAYER3, LAYER4, LAYER5, LAYER6, LAYER7, LAYER8, LAYER_NUN};
    SE_Layer();
    SE_Layer(int layer);
    ~SE_Layer();
    SE_Layer(const SE_Layer& layer);
    SE_Layer& operator=(const SE_Layer& layer);
    friend bool operator==(const SE_Layer& left, const SE_Layer& right);
    friend bool operator<(const SE_Layer& left, const SE_Layer& right);
    friend bool operator>(const SE_Layer& left, const SE_Layer& right);
    friend SE_Layer operator+(const SE_Layer& left, const SE_Layer& right);
    friend SE_Layer operator-(const SE_Layer& left, const SE_Layer& right);
 /*
    int getLayer() const
    {
        return mLayer;
    }
    void setLayer(int layer)
    {
        mLayer = layer;
    }
    */
private:
    int mLayer;
};
#endif
