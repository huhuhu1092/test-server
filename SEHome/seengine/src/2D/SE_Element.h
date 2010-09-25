#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Layer.h"
class SE_Element
{
public:
    SE_Element();
    SE_Element(float left, float right, float top, float bottom);
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
    void setLeft(float left)
    {
        mLeft = left;
    }
    void setTop(float top)
    {
        mTop = top
    }
    void setImage(const SE_ImageDataID& imageDataID)
    {
        mImageDataID = imageDataID;
    }
    SE_ImageDataID getImageDataID()
    {
        return mImageDataID;
    }
    //it is rotated around the center
    void setLocalRotate(const SE_Quat& q)
    {
        mLocalRotate = q;
    }
    SE_Quat getLocalRotate()
    {
        return mLocalRotate;
    }
    //it is scaled from its center
    void setLocalScale(const SE_Vector3f& scale)
    {
        mLocalScale = scale;
    }
    SE_Vector3f getLocalScale()
    {
        return mLocalScale;
    }
    void setLocalLayer(const SE_Layer& layer)
    {
        mLocalLayer = layer;
    }
    SE_Layer getLocalLayer()
    {
        return mLocalLayer;
    }
public:
    virtual SE_Spatial* createSpatial();
    virtual void updateWorldTransform();
private:
    float mTop;
    float mLeft;
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
