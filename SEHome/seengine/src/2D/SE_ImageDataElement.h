#ifndef SE_IMAGEDATA_ELEMENT_H
#define SE_IMAGEDATA_ELEMENT_H
#include "SE_Element.h"
#include "SE_Common.h"
#include "SE_ImageData.h"
class SE_Primitive;
class SE_Surface;
class SE_ImageDataElement : public SE_2DNodeElement
{
public:
	SE_ImageDataElement(const SE_ImageDataID& imageDataID);
	void setImageDataID(const SE_ImageDataID& id)
	{
		mImageDataID = id;
	}
	SE_ImageDataID getImageDataID() const
	{
		return mImageDataID;
	}
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
	SE_Element* clone();
protected:
    virtual void setImageData(SE_Primitive* primitive);
	virtual void setSurface(SE_Surface* surface);
private:
    SE_ImageData* getImageData();	
private:
    SE_ImageDataID mImageDataID;	
};
#endif
