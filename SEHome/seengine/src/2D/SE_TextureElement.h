#ifndef SE_TEXTUREELEMENT_H
#define SE_TEXTUREELEMENT_H
#include "SE_Element.h"
#include "SE_ID.h"
class SE_RawImage;
class SE_ImageData;
class SE_TimeKey;
class SE_Value;
class SE_ParamValueList;
class SE_Primitive;
class SE_Surface;

class SE_TextureElement : public SE_2DNodeElement
{
public:
	SE_TextureElement(const SE_StringID& uri);
	~SE_TextureElement();
	void setImage(const SE_ImageDataID& id, SE_ImageData* imageData);
	void spawn();
	void layout();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
    void setContentChild(SE_2DNodeElement* c)
	{
		mContentChild = c;
	}
	SE_Spatial* createSpatial();
	SE_Element* clone();
protected:
	void setImageData(SE_Primitive* primitive);
	void setSurface(SE_Surface* surface);
	void init();
private:
	SE_ImageDataID mImageDataID;
	SE_ImageData* mImageData;
	SE_RenderTargetID mRenderTargetID;
	SE_2DNodeElement* mContentChild;
};
#endif
