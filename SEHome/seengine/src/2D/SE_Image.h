#ifndef SE_IMAGE_H
#define SE_IMAGE_H
#include "SE_ID.h"
#include "SE_ImageMap.h"
#include <string>
class SE_RectPrimitive;
class SE_Surface;
class SE_Image
{
public:
	SE_Image(const char* url);
	int getWidth();
	int getHeight();
	int getPivotX()
	{
		return mPivotx;
	}
	int getPivotY()
	{
		return mPivoty;
	}
	SE_ImageUnit getBaseColor()
	{
		return mBaseColor;
	}
	SE_ImageUnit getRChannel()
	{
		return mRChannel;
	}
	SE_ImageUnit getGChannel()
	{
		return mGChannel;
	}
	SE_ImageUnit getBChannel()
	{
		return mBChannel;
	}
	SE_ImageUnit getAChannel()
	{
		return mAChannel;
	}
	void setImageData(SE_RectPrimitive* primitive);
	void setSurface(SE_Surface* surface);
private:
	void parse();
	SE_ImageUnit createImageDataFullPath(const char* inputstr);
	void calculateDimension();
private:
	struct _ImageUnitData
	{
		int valid;
		SE_ImageUnit* imageUnit;
		_ImageUnitData()
		{
			valid = 0;
			imageUnit = NULL;
		}
	};
	int mWidth;
	int mHeight;
	int mPivotx;
	int mPivoty;
	std::string mUrl;
    SE_ImageUnit mRChannel;
	SE_ImageUnit mGChannel;
	SE_ImageUnit mBChannel;
	SE_ImageUnit mAChannel;
	SE_ImageUnit mBaseColor;
	_ImageUnitData mImageUnits[5];

};
#endif