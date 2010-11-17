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
	enum {IMG_SIZE = 5};
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
	int getValidImageNum();
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
	_ImageUnitData mImageUnits[IMG_SIZE];

};
class SE_ComposeImage
{
public:
    enum {TEX_SIZE = 4}
    SE_ComposeImage();
    ~SE_ComposeImage();
    void setBackground(const SE_StringID& background);
    void setChannel(const SE_StringID& channel);
    void setTexture(int index, const SE_StringID& texture);
    void setColor(int index, const SE_Vector3i& color)
	{
		mColor[index] = color;
	}
    void setFunction(int index, int fn)
	{
		mFunction[index] = fn;
	}
	void setAlpha(int index, int alpha)
	{
		mAlpha[index] = alpha;
	}
    void setImageData(SE_RectPrimitive* primivite);
    void setSurface(SE_Surface* surface);
private:
	int getValidImageNum();
private:
    SE_Image* mBackground;
    SE_Image* mChannel;
    SE_Image* mTexture[TEX_SIZE];
    int mFunction[TEX_SIZE];
    int mAlpha[TEX_SIZE];
    SE_Vector3i mColor[TEX_SIZE];
};
#endif
