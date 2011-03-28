#ifndef SE_COLOREFFECTCONTROLLERELEMENT_H
#define SE_COLOREFFECTCONTROLLERELEMENT_H
#include "SE_Element.h"
#include "SE_ID.h"
#include "SE_Common.h"
class SE_TimeKey;
class SE_Value;
class SE_Spatial;
class SE_ColorEffectController;
class SE_Primitive;
class SE_Surface;
class SE_ImageData;
class SE_ImageElement;
class SE_ParamValueList;
class SE_ColorEffectControllerElement : public SE_2DNodeElement
{
public:
	SE_ColorEffectControllerElement(const SE_StringID& uri);
	~SE_ColorEffectControllerElement();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	SE_Spatial* createSpatial();
	void spawn();
	void layout();
	SE_Element* clone();
private:
	SE_ColorEffectController* mColorEffectController;
	SE_2DNodeElement* mCurrentElement;
};
class SE_ColorEffectElement : public SE_2DNodeElement
{
public:
	enum {MARK_A, MARK_R, MARK_G, MARK_B, MARK_NUM};
    enum {FN_ADD, FN_MULTIPLY, FN_NUM};
	struct _TextureMark
	{
        bool valid;
		SE_StringID mTextureAddress;
		SE_StringID mTextureValue;
		int mTextureArity;
		SE_StringID mColorAlphaAddress;
        int mColorAlphaValue;
		SE_StringID mFnAddress;
		int mFnValue;
		SE_StringID mTextureFnAddress;
		int mTextureFnValue;
		SE_StringID mColorAddress;
        SE_SignColor mColorValue;
		SE_StringID mColor2Address;
		SE_SignColor mColor2Value;
		_TextureMark()
		{
			mColorAlphaValue = 255;
			mFnValue = FN_ADD;
			mTextureFnValue = FN_ADD;
			mTextureArity = 0;
			valid = false;
		}
	};
	void setBackgroundAddress(const SE_StringID& address)
	{
		mBackgroundAddress = address;
	}
	void setBackgroundValue(const SE_StringID& v)
	{
		mBackgroundAddress = v;
	}
	void setChannelAddress(const SE_StringID& address)
	{
		mChannelAddress = address;
	}
	void setChannelValue(const SE_StringID& v)
	{
		mChannelValue = v;
	}
	void setBackgroundAlphaAddress(const SE_StringID& address)
	{
		mBackgroundAlphaAddress = address;
	}
	void setBackgroundAlphaValue(int a)
	{
		mBackgroundAlphaValue = a;
	}
	void setTextureMark(int index, const _TextureMark& tm)
	{
		if(index >= MARK_A && index < MARK_NUM)
		    mTextureMark[index] = tm;
	}
	SE_ColorEffectElement();
	~SE_ColorEffectElement();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);

	SE_Spatial* createSpatial();
	void spawn();
	void layout();
	SE_Element* clone();
private:
	void setImageData(SE_Primitive* primitive);
	void setImageData(SE_Primitive* primitive, SE_ImageData* imageData, SE_TEXUNIT_TYPE texType);
	void setImageData(SE_Primitive* primitive, const SE_StringID& imageID, SE_TEXUNIT_TYPE texType);
	void setSurface(SE_Surface* surface);
	void calculateValue();
	SE_XMLTABLE_TYPE getBackgroundType();
	void getExtractImageProperty(SE_XMLTABLE_TYPE& t, int& width, int& height);
	SE_ImageElement* createImageElement(const SE_StringID& textureURL, SE_ImageData*& imageData);
	bool isTextureEnd(std::vector<SE_Element*>::iterator textureIt[4], SE_Element* texture[4]);
	SE_Element* mergeElement(SE_Element* background, SE_Element* channel, SE_Element* texture[4]);
	void mergeElement();
	int uriToInt(const SE_StringID& uri);
private:
	SE_XMLTABLE_TYPE mBackgroundType;
	SE_StringID mBackgroundAddress;
	SE_StringID mBackgroundValue;
	SE_ImageData* mBackgroundImageData;
	SE_ImageDataID mBackgroundImageDataID;
	SE_ImageElement* mBackgroundImageElement;
	int mBackgroundArity;
	SE_StringID mChannelAddress;
	SE_StringID mChannelValue;
	int mChannelArity;
	SE_ImageData* mChannelImageData;
	SE_ImageDataID mChannelImageDataID;
	SE_ImageElement* mChannelImageElement;
	SE_StringID mBackgroundAlphaAddress;
    int mBackgroundAlphaValue;
	_TextureMark mTextureMark[MARK_NUM];
	SE_2DNodeElement* mBackgroundElement;
	SE_2DNodeElement* mChannelElement;
	SE_2DNodeElement* mTextureElement[MARK_NUM];
	SE_ImageData* mTextureImageData[MARK_NUM];
	SE_ImageDataID mTextureImageDataID[MARK_NUM];
	SE_ImageElement* mTextureImageElement[MARK_NUM];
	SE_2DNodeElement* mMergedElement;
};
#endif
