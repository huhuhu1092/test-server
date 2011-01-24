#ifndef SE_LAYER_H
#define SE_LAYER_H
#include "SE_Common.h"
#include <string>
class SE_Layer
{
public:
    SE_Layer();
    SE_Layer(int layer);
    ~SE_Layer();
    SE_Layer(const SE_Layer& layer);
    SE_Layer& operator=(const SE_Layer& layer);
    static bool operator==(const SE_Layer& left, const SE_Layer& right);
    static bool operator<(const SE_Layer& left, const SE_Layer& right);
    static bool operator>(const SE_Layer& left, const SE_Layer& right);
    static SE_Layer operator+(const SE_Layer& left, const SE_Layer& right);
    /*
    static SE_Layer operator-(const SE_Layer& left, const SE_Layer& right);
    */
private:
    std::string mLayer;
};
#endif
