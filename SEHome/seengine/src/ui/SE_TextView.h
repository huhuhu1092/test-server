#ifndef SE_TEXTVIEW_H
#define SE_TEXTVIEW_H
#include "SE_Widget.h"
#include "SE_CharStyle.h"
#include "SE_CharCode.h"
#include <vector>
#include <map>
class SE_Spatial;
class SE_Primitive;
class SE_Surface;
class SE_CharView : public SE_Widget
{
public:
    SE_CharView();
    ~SE_CharView();
    void setCharCode(const SE_CharCode& c)
    {
        mCharCode = c;
    }
    void setFontColor(const SE_Vector3i& c)
    {
        mFontColor = c;
    }
    void setFontSize(int fontSize)
    {
        mFontSize = fontSize;
    }
    void setCharStyle(const SE_CharStyle& s)
    {
        mCharStyle = s;
    }
    void setImageData(SE_ImageData* i)
    {
        mCharImage = i;
    }
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
protected:
	virtual void setImageData(SE_Primitive* primitive);
	virtual void setSurface(SE_Surface* surface);
private:
	SE_CharCode mCharCode;
	SE_Vector3i mFontColor;
    int mFontSize;
    SE_CharStyle mCharStyle;
	SE_ImageData* mCharImage;
};
class SE_TextView : public SE_Widget
{
public:
    enum ALIGN {LEFT, RIGHT, TOP, BOTTOM, MID};
    enum ORIENTATION {VERTICAL, HORIZONTAL};
    SE_TextView();
    void setText(const SE_StringID& t)
    {
        mText = t;
    }
    void setFontColor(int state, const SE_Vector3i& c)
    {
        mTextProperty[state].mFontColor = c;
    }
    void setFontSize(int state, int fontSize)
    {
        mTextProperty[state].mFontSize = fontSize;
    }
    void setCharStyle(int state, const SE_CharStyle& s)
    {
        mTextProperty[state].mCharStyle = s;
    }
	void setAlign(int state, ALIGN a)
	{
		mTextProperty[state].mAlign = a;
	}
	void setOrientation(int state, ORIENTATION o)
	{
		mTextProperty[state].mOrientation = o;
	}
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
private:
	struct _TextProperty
	{
        SE_Vector3i mFontColor;
        int mFontSize;
        SE_CharStyle mCharStyle;
        ALIGN mAlign;
        ORIENTATION mOrientation;
	};
private:
	void calculateTextBound(float& outWidth, float& outHeight);
private:
    SE_StringID mText;
	std::map<int, _TextProperty> mTextProperty;
    std::vector<SE_ImageData*> mCharImageData;
};
#endif
