#ifndef SE_TEXTUREELEMENT_H
#define SE_TEXTUREELEMENT_H
#include "SE_Element.h"
#include "SE_ID.h"
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
	//void setImage(const SE_ImageDataID& id, SE_ImageData* imageData);
	void spawn();
	void layout();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	SE_Spatial* createSpatial();
	SE_Element* clone();
    void setRenderTargetID(const SE_RenderTargetID& renderTarget);
    void setRenderTargetSeq(const SE_RenderTargetSeq& renderTargetSeq);
protected:
	void setImageData(SE_Primitive* primitive);
	void setSurface(SE_Surface* surface);
	void init();
	SE_2DNodeElement* createChildContent();
private:
	SE_ImageDataID mImageDataID;
	SE_ImageData* mImageData;
	SE_RenderTargetID mContentRenderTargetID;
	SE_2DNodeElement* mContentChild;
    bool mShareContent;
};
#endif
