#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Layer.h"
class SE_Element
{
public:
    SE_Element();
    SE_Element(int left, int right, int top, int bottom);
    virtual ~SE_Element();
    float getWidth()
    {
        return mWidth;
    }
    float getHeight()
    {
        return mHeight;
    }
    void setWidth(float w)
    {
        mWidth = w;
    }
    void setHeight(float h)
    {
        mHeight = h;
    }
    void setImage(const SE_ImageDataID& imageDataID);
    SE_ImageDataID getImageDataID();
    //it is rotated around the center
    void setLocalRotate(const SE_Quat& q);
    SE_Quat getLocalRotate();
    //it is scaled from its center
    void setLocalScale(const SE_Vector3f& scale);
    SE_Vector3f getLocalScale();
    //the translate is the top and left of this element relatevie to its parent
    void setLocalTranslate(const SE_Vector3f& translate);
    SE_Vector3f getLocalTranslate();
    void setLocalLayer(const SE_Layer& layer);
    SE_Layer getLocalLayer();
public:
    virtual SE_Spatial* createSpatial();
private:
    float mWidth;
    float mHeight; 
    SE_Element* mParent;
    SE_ImageDataID mImageDataID;
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;
    SE_Layer mLocalLayer;
};
#endif
