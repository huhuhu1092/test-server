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
struct SE_ElementData
{
	SE_StringID dataID;
	SE_MountPointID mountPointID;
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
    void setImageDataID(const SE_ImageDataID& imageDataID)
    {
        mImageDataID = imageDataID;
    }
    SE_ImageDataID getImageDataID()
    {
        return mImageDataID;
    }
	SE_ActionID getActionID()
	{
		return mActionID;
	}
	void setActionID(const SE_ActionID& actionID)
	{
		mActionID = actionID;
	}
	void setStateTableID(const SE_StateTableID& stID)
	{
		mStateTableID = stID;
	}
	SE_StateTableID getStateTableID()
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
    void setID(const char* id)
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
	void setImageMapRef(const SE_StringID& imageMapRef)
	{
		mImageMapRef = imageMapRef;
	}
	SE_StringID getImageMapRef()
	{
		return mImageMapRef;
	}
	SE_StringID getWorldImageMapRef();
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
	SE_Spatial* createSpatialFromImageData();
	SE_Spatial* createSpatialFromActionData();
	SE_Spatial* createSpatialFromStateTableData();
private:
    float mTop;
    float mLeft;
    float mWidth;
    float mHeight; 
    float mPivotX;
    float mPivotY;
    int mImageX;
    int mImageY;
    int mImageWidth;
    int mImageHeight;
    SE_Element* mParent;
    SE_ElementData mImage;
    SE_ElementData mAction;
    SE_ElementData mStateTable;
	SE_StringID mImageMapRef;//indicate imagemap file name
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;
    SE_Layer mLocalLayer;
    SE_ElementID mID;
    SE_SimObjectID mSimObjectID;
    SE_SpatialID mSpatialID;
    SE_PrimitiveID mPrimitiveID;
    typedef std::map<SE_MountPointID, SE_MountPoint> _MountPointMap;
    _MountPointList mMountPointMap;
    typedef std::list<SE_Element*> _ElementList;
    _ElementList mChildren;
};
#endif
