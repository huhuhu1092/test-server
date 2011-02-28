#ifndef SE_TEXTVIEW_H
#define SE_TEXTVIEW_H
#include "SE_Widget.h"
#include "SE_CharStyle.h"
#include "SE_CharCode.h"
#include <vector>
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
    void setFontColor(const SE_Vector3f& c)
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
	SE_CharCode mText;
	SE_Vector3f mFontColor;
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
    void setFontColor(const SE_Vector3f& c)
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
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
private:
    SE_StringID mText;
    SE_Vector3f mFontColor;
    int mFontSize;
    SE_CharStyle mCharStyle;
    ALIGH mAligh;
    ORIENTATION mOrientation;
    std::vector<SE_ImageData*> mCharImageData;
};
#endif
