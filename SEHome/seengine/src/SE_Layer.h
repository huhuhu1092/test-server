#ifndef SE_LAYER_H
#define SE_LAYER_H
#include "SE_Common.h"
#include <string>
class SE_BufferInput;
class SE_BufferOutput;
class SE_Layer
{
public:
    SE_Layer();
    SE_Layer(int layer);
    ~SE_Layer();
    SE_Layer(const SE_Layer& layer);
    SE_Layer& operator=(const SE_Layer& layer);
	friend bool operator==(const SE_Layer& left, const SE_Layer& right)
	{
		return left.mLayer == right.mLayer;
	}
	friend bool operator<(const SE_Layer& left, const SE_Layer& right)
	{
		return left.mLayer < right.mLayer;
	}
	friend bool operator>(const SE_Layer& left, const SE_Layer& right)
	{
		return left.mLayer > right.mLayer;
	}
	friend SE_Layer operator+(const SE_Layer& left, const SE_Layer& right)
	{
		SE_Layer l;
		l.mLayer = (left.mLayer + right.mLayer);
		return l;
	}
    void read(SE_BufferInput& input);
	void write(SE_BufferOutput& output);
    /*
    static SE_Layer operator-(const SE_Layer& left, const SE_Layer& right);
    */
private:
    std::string mLayer;
};
#endif
