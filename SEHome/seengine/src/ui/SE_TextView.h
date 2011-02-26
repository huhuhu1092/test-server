#ifndef SE_TEXTVIEW_H
#define SE_TEXTVIEW_H
#include "SE_Widget.h"
#include <vector>
class SE_Spatial;
class SE_Primitive;
class SE_Surface;
class SE_TextView : public SE_Widget
{
public:
    SE_TextView();
    ~SE_TextView();
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
protected:
	virtual void setImageData(SE_Primitive* primitive);
	virtual void setSurface(SE_Surface* surface);
private:
	SE_StringID mText;
	SE_Vector3f mColor;
	std::vector<SE_ImageDataID> mCharImageIDArray;
	std::vector<SE_ImageData*> mCharImageArray;
};
#endif
