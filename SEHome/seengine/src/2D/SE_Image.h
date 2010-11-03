#ifndef SE_IMAGE_H
#define SE_IMAGE_H
#include "SE_ID.h"
#include <string>
class SE_Surface;
class SE_Image
{
public:
	SE_Image(const char* url, const char* imagemapref);
	int getWidth();
	int getHeight();
	SE_StringID getBaseColor()
	{
		return mBaseColor;
	}
	SE_StringID getRChannel()
	{
		return mRChannel;
	}
	SE_StringID getGChannel()
	{
		return mGChannel;
	}
	SE_StringID getBChannel()
	{
		return mBChannel;
	}
	SE_StringID getAChannel()
	{
		return mAChannel;
	}
	void setSurface(SE_Surface* surface);
private:
	void parse();
	SE_StringID createImageDataFullPath(const char* inputstr);
	void calculateDimension();
private:
	std::string mUrl;
	std::string mDefaultImageMapRef;
    SE_StringID mRChannel;
	SE_StringID mGChannel;
	SE_StringID mBChannel;
	SE_StringID mAChannel;
	SE_StringID mBaseColor;

};
#endif