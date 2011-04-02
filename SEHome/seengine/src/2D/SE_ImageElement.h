#ifndef SE_IMAGEELEMENT_H
#define SE_IMAGEELEMENT_H
#include "SE_Element.h"
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_ImageMap.h"
class SE_Spatial;
class SE_Surface;
class SE_TimeKey;
class SE_Value;
class SE_ParamValueList;
class SE_Primitive;
class SE_ImageElement : public SE_2DNodeElement
{
public:
	SE_ImageElement(const SE_StringID& uri);
	~SE_ImageElement();
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	SE_Element* clone();
protected:
	bool isValid();
	virtual void setImageData(SE_Primitive* primitive);
	virtual void setSurface(SE_Surface* surface);
	void initImage();
private:
	enum {IMG_SIZE = 5};
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
	SE_ImageUnit mRChannel;
	SE_ImageUnit mGChannel;
	SE_ImageUnit mBChannel;
	SE_ImageUnit mAChannel;
	SE_ImageUnit mBaseColor;
	_ImageUnitData mImageUnits[IMG_SIZE];
    float mImageWidth;
    float mImageHeight;
};
#endif
