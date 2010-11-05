#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Layer.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_ID.h"
#include "SE_Animation.h"
#include "SE_MountPoint.h"
#include <string>
#include <list>
#include <map>
class SE_Spatial;
class SE_Element;
class SE_ElementTravel
{
public:
    virtual ~SE_ElementTravel()
    {}
    virtual void visit(SE_Element* e) = 0;
};
class SE_Element
{
public:
    SE_Element();
    SE_Element(float left, float top, float width, float height);
    virtual ~SE_Element();
    float getLeft()
    {
	return mLeft;
    }
    float getTop()
    {
        return mTop;
    }
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
        mTop = top;
    }
    void setElementRef(const SE_StringID& elementref)
    {
        mElementRef = elementref;
    }
    SE_StringID getElementRef()
    {
        return mElementRef;
    }
    void setImage(const SE_StringID& image)
    {
		mImageID = image;
    }
    SE_StringID getImage()
    {
		return mImageID;
    }
	SE_StringID getActionID()
	{
		return mActionID;
	}
	void setActionID(const SE_StringID& actionID)
	{
		mActionID = actionID;
	}
	void setStateTableID(const SE_StringID& stID)
	{
		mStateTableID = stID;
	}
	SE_StringID getStateTableID()
	{
		return mStateTableID;
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
    void setID(const SE_ElementID& id)
    {
        mID = id;
    }
    SE_ElementID getID()
    {
        return mID;
    }
    void setParent(SE_Element* parent)
    {
        mParent = parent;
    }
    SE_Element* getParent()
    {
        return mParent;
    }
	/*
    void setImageDataX(int x)
    {
        mImageX = x;
    }
    void setImageDataY(int y)
    {
        mImageY = y;
    }
    void setImageDataWidth(int w)
    {
        mImageWidth = w;
    }
    void setImageDataHeight(int h)
    {
        mImageHeight = h;
    }
	*/
    SE_SimObjectID getSimObjectID()
    {
        return mSimObjectID;
    }
    SE_SpatialID getSpatialID()
    {
        return mSpatialID;
    }
    SE_PrimitiveID getPrimitiveID()
    {
        return mPrimitiveID;
    }
    float getPivotX()
    {
        return mPivotX;
    }
    float getPivotY()
    {
        return mPivotY;
    }
    void setPivotX(float x)
    {
        mPivotX = x;
    }
    void setPivotY(float y)
    {
        mPivotY = y;
    }
	/*
	void setImageMapRef(const SE_StringID& imageMapRef)
	{
		mImageMapRef = imageMapRef;
	}
	SE_StringID getImageMapRef()
	{
		return mImageMapRef;
	}
	*/
	void setMountPointRef(const SE_MountPointID& mp)
	{
		mMountPointID = mp;
	}
	SE_MountPointID getMountPointRef()
	{
		return mMountPointID;
	}
	SE_Animation* getAnimation()
	{
		return mAnimation;
	}
	void setAnimation(SE_Animation* animation)
	{
		mAnimation = animation;
	}
	/*
	void addElementRef(const SE_StringID& elementRefID)
	{
		mElementRefList.push_back(elementRefID);
	}
	*/
	//SE_StringID getWorldImageMapRef();
    void addMountPoint(const SE_MountPoint& mountPoint);
    void removeMountPoint(const SE_MountPointID& mountPointID);
    void clearMountPoint();
    SE_MountPoint findMountPoint(const SE_MountPointID& mountPointID);
public:
    virtual SE_Spatial* createSpatial(SE_Spatial* parent);
    virtual void updateWorldTransform();
    virtual void addChild(SE_Element* e);
    virtual void removeChild(SE_Element* e);
	virtual void removeChild(const SE_ElementID& id);
    virtual void travel(SE_ElementTravel* travel);
private:
    SE_Element(const SE_Element&);
    SE_Element& operator=(const SE_Element&);
	SE_Spatial* createSpatialFromImageData(SE_Spatial* parent);
	SE_Spatial* createSpatialFromActionData(SE_Spatial* parent);
	SE_Spatial* createSpatialFromStateTableData(SE_Spatial* parent);
	SE_Spatial* createSpatialFromElementRef(SE_Spatial* parent);
	void calculateRect(int imageWidth, int imageHeight);
	static const float INVALID_GEOMINFO;
private:
    float mTop;
    float mLeft;
    float mWidth;
    float mHeight; 

    float mPivotX;
    float mPivotY;
    //int mImageX;
    //int mImageY;
    //int mImageWidth;
    //int mImageHeight;
    SE_Element* mParent;
    SE_StringID mImageID;
    SE_StringID mActionID;
    SE_StringID mStateTableID;
	//SE_StringID mImageMapRef;//indicate imagemap file name
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;
    SE_Layer mLocalLayer;
    SE_ElementID mID;
    SE_SimObjectID mSimObjectID;
    SE_SpatialID mSpatialID;
    SE_PrimitiveID mPrimitiveID;
	SE_MountPointID mMountPointID;
    SE_StringID mElementRef;
	SE_Animation* mAnimation;
    typedef std::map<SE_MountPointID, SE_MountPoint> _MountPointMap;
    _MountPointMap mMountPointMap;
    typedef std::list<SE_Element*> _ElementList;
    _ElementList mChildren;
	//typedef std::list<SE_StringID> _ElementRefList;
	//_ElementRefList mElementRefList;
};
#endif
