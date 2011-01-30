#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_TreeStruct.h"
#include "SE_ParamObserver.h"
#include "SE_URI.h"
class SE_Element : public SE_ParamObserver, public SE_TreeStruct<SE_Element>
{
    friend class SE_ElementManager;
    friend class SE_ElementSchema;
public:
    enum STATE {NORMAL, SELECTED, HIGHLIGHTED, INVISIBLE, INACTIVE, 
                ANIMATE_BEGIN, ANIMATE_RUNNING, ANIMATE_SUSPEND, ANIMATE_END};
    SE_Element();
    virtual ~SE_Element();
    int getState() const
    {
        return mState;
    }
    int getType() const
    {
        return mType;
    }
    void setType(int type)
    {
        mType = type;
    }
    //it is rotated around the center
    void setLocalRotate(const SE_Quat& q)
    {
        mLocalRotate = q;
    }
    SE_Quat getLocalRotate() const
    {
        return mLocalRotate;
    }
    //it is scaled from its center
    void setLocalScale(const SE_Vector3f& scale)
    {
        mLocalScale = scale;
    }
    SE_Vector3f getLocalScale() const
    {
        return mLocalScale;
    }
    void setLocalTranslate(const SE_Vector3f& v)
    {
        mLocalTranslate = v;
    }
    SE_Vector3f getLocalTranslate() const
    {
        return mLocalTranslate;
    }
    void setLocalLayer(const SE_Layer& layer)
    {
        mLocalLayer = layer;
    }
    SE_Layer getLocalLayer() const
    {
        return mLocalLayer;
    }
    void setName(const SE_StringID& name)
    {
        mName = name
    }
    SE_StringID getName() const
    {
        return mName;
    }
    void setFullPathName(const SE_StringID& name)
	{
		mFullPathName = name;
	}
	SE_StringID getFullPathName() const
	{
		return mFullPathName;
	}
	void setURI(const SE_StringID& uri);
	SE_StringID getURI() const
	{
		return mURI.getURI();
	}
    SE_StringID getURL() const
	{
		return mURI.getURL();
	}
	void setOwnRenderTargetCamera(bool own)
	{
		mOwnRenderTargetCamera = own;
	}
    int getSeqNum() const
	{
		return mSeqNum;
	}
	void setSeqNum(int i) 
	{
		mSeqNum = i;
	}
    void setKeyFrameController(SE_KeyFrameController* kfc);
    void setPivotX(float px)
    {
        mPivotX = px;
    }
    void setPivotY(float py)
    {
        mPivotY = py;
    }
    void setMountPointX(float x)
    {
        mMountPointX = x;
    }
    void setMountPointY(float y)
    {
        mMountPointY = y;
    }
    void setSpatialType(int spatialType)
    {
        mSpatialType = spatialType;
    }
public:
    virtual void spawn();
    virtual void update(const SE_TimeKey& timeKey);
    virtual void layout();
    virtual SE_Spatial* createSpatial(int spatialType);
    virtual void clone(const SE_Element* srcElement);
protected:
    int mState;
    int mType;
    float mLeft, mTop, mWidth, mHeight;
    float mPivotX, mPivotY, mMountPointX, mMountPointY;
    float mDeltaLeft, mDeltaTop;
    int mKeyFrameNum;
    int mSeqNum;
    int mSpatialType; // spatial type: GEOMETRY, COMMONNODE, BSPNODE
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;
    SE_Layer mLocalLayer;
    SE_StringID mName;
    SE_StringID mFullPathName;
    SE_MountPointID mMountPointID;
    SE_TimeKey mTimeKey;
    SE_TimeKey mStartKey;
    SE_TimeKey mEndKey;
    SE_URI mURI;
    SE_KeyFrameController* mKeyFrameController;
    SE_MountPointSet mMountPointSet;
    /////////////
    SE_ElementID mPrevElement;
    SE_ElementID mNextElement;
    SE_SpatialID mSpatialID;
    SE_SimObjectID mSimObjectID;
    SE_PrimitiveID mPrimitiveID;
    SE_AnimationID mAnimationID;
    SE_RenderTargetID mRenderTarget;
    bool mOwnRenderTargetCamera;
    bool mNeedUpdateTransform;

};
class SE_2DNodeElement : public SE_Element
{
public:
    SE_2DNodeElement();
    ~SE_2DNodeElement();
    virtual void spawn();
    virtual void update(const SE_TimeKey& timeKey);
    virtual void layout();
    virtual SE_Spatial* createSpatial();    
};
#endif
