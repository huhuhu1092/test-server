#ifndef SE_IMAGE_H
#define SE_IMAGE_H
#include "SE_ID.h"
#include "SE_ImageMap.h"
#include "SE_Vector.h"
#include <string>
class SE_RectPrimitive;
class SE_Surface;
class SE_Element;
class SE_ImageData;
class SE_Image
{
public:
	enum {IMG_SIZE = 5};
	SE_Image(const char* url);
	SE_Image();
	virtual ~SE_Image() {}
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
	std::string getUrl()
	{
		return mUrl;
	}
	static SE_ImageData* getImageData(const SE_ImageUnit& imageUnit);
	virtual void setImageData(SE_RectPrimitive* primitive);
	virtual void setSurface(SE_Surface* surface);
	SE_Image* clone();
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
class SE_ColorEffectImage
{
public:
    enum {TEX_SIZE = 4};
    SE_ColorEffectImage();
    ~SE_ColorEffectImage();
    void setBackground(const SE_StringID& background);
    void setChannel(const SE_StringID& channel);
    void setTexture(int index, const SE_StringID& texture);
	void setAlpha(int alpha)
	{
		mBackgroundAlpha = alpha;
	}
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
	void createTextureElement(SE_Element* parent);
private:
	SE_ImageData* createTextureElement(SE_Element* parent, SE_Image* img);
	void setImageData(SE_RectPrimitive* primitive, SE_TEXUNIT_TYPE texType, SE_ImageData* imageData, SE_Image* image);
	int getValidImageNum();
private:
    SE_Image* mBackground;
	SE_ImageData* mBackgroundImageData;
    SE_Image* mChannel;
	SE_ImageData* mChannelImageData;
    SE_Image* mTexture[TEX_SIZE];
	SE_ImageData* mTextureImageData[TEX_SIZE];
    int mFunction[TEX_SIZE];
    int mAlpha[TEX_SIZE];
    SE_Vector3i mColor[TEX_SIZE];
	int mBackgroundAlpha;
};
#endif
