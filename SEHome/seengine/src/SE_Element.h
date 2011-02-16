#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_TreeStruct.h"
#include "SE_ParamObserver.h"
#include "SE_URI.h"
#include "SE_Object.h"
#include "SE_TimeKey.h"
#include "SE_MountPoint.h"
#include "SE_Layer.h"
#include "SE_Primitive.h"
class SE_KeyFrameController;
class SE_Spatial;
class SE_Element;
class SE_Value;
class SE_ParamValueList;
class SE_ImageData;
class SE_Animation;
class SE_Spatial;
class SE_RectPrimitive;
class SE_Surface;
class SE_ElementTravel
{
public:
    virtual ~SE_ElementTravel()
    {}
    virtual void visit(SE_Element* e) = 0;
};

class SE_BufferOutput;
class SE_BufferInput;
class SE_Element : public SE_ParamObserver, public SE_TreeStruct<SE_Element>
{
    friend class SE_ElementManager;
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
	int getKeyFrameNum()
	{
		return mKeyFrameNum;
	}
    void setName(const SE_StringID& name)
    {
        mName = name;
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

	SE_TimeKey getTimeKey() const
	{
		return mTimeKey;
	}
	void setTimeKey(const SE_TimeKey& key)
	{
		mTimeKey = key;
	}
	SE_TimeKey getStartKey() const
	{
		return mTimeKey;
	}
	void setStartKey(const SE_TimeKey& key)
	{
		mStartKey = key;
	}
	SE_TimeKey getEndKey() const
	{
		return mEndKey;
	}
	void setEndKey(const SE_TimeKey& key)
	{
		mEndKey = key;
	}

    void setSpatialType(int spatialType)
    {
        mSpatialType = spatialType;
    }
	int getSpatialType() const
	{
		return mSpatialType;
	}
	void setSpatialID(const SE_SpatialID& spatialID)
	{
		mSpatialID = spatialID;
	}
    SE_SpatialID getSpatialID() const
    {
        return mSpatialID;
    }
	void setPrimitiveID(const SE_PrimitiveID& id)
	{
		mPrimitiveID = id;
	}
    SE_PrimitiveID getPrimitiveID() const
    {
        return mPrimitiveID;
    }
	void setSimObjectID(const SE_SimObjectID& simObj)
	{
		mSimObjectID = simObj;
	}
    SE_SimObjectID getSimObjectID()
    {
        return mSimObjectID;
    }
	void setAnimationID(const SE_AnimationID& animID)
	{
		mAnimationID = animID;
	}
	SE_AnimationID getAnimationID() const
	{
		return mAnimationID;
	}
    void setRenderQueueSeq(int q)
    {
        mRenderQueueSeq = q;
    }
	SE_Element* getPrev() const
	{
		return mPrevElement;
	}
    void setPrev(SE_Element* prev)
	{
		mPrevElement = prev;
	}
	SE_Element* getNext() const
	{
		return mNextElement;
	}
	void setNext(SE_Element* next)
	{
		mNextElement = next;
	}
    void setSceneRenderSeq(const SE_SceneRenderSeq& seq);
    void setRenderTargetID(const SE_RenderTargetID& renderTarget);
    void travel(SE_ElementTravel* travel);
    void show();
    void hide();
    //dismiss will make spatial node remove from spatial manager
    void dismiss();
	void setAnimation(SE_Animation* anim)
	{
		mAnimation = anim;
	}
	SE_Animation* getAnimation() const
	{
		return mAnimation;
	}
	SE_Element* getParent();
    void setPrevMatrix(const SE_Matrix4f& m)
    {
        mPrevMatrix = m;
    }
    void setPostMatrix(const SE_Matrix4f& m)
    {
        mPostMatrix = m;
    }
    SE_Matrix4f getPrevMatrix() const
    {
        return mPrevMatrix;
    }
    SE_Matrix4f getPostMatrix() const
    {
        return mPostMatrix;
    }
    bool isRoot();
    void clearChildren();
public:
    virtual void spawn();
    virtual void update(const SE_TimeKey& timeKey);
	virtual void update(SE_ParamValueList& paramValueList);
	virtual void update(const SE_AddressID& address, const SE_Value& value);
	virtual void updateSpatial();
    virtual void layout();
    virtual SE_Spatial* createSpatial();
    virtual void read(SE_BufferInput& inputBuffer);
    virtual void write(SE_BufferOutput& outputBuffer);
	virtual SE_Element* clone();
	virtual void setImageData(SE_RectPrimitive* primitive);
	virtual void setSurface(SE_Surface* surface);
protected:
    virtual void clone(SE_Element* src, SE_Element* dst);
protected:
    int mState;
    int mType;
    int mSpatialType; // spatial type: GEOMETRY, COMMONNODE, BSPNODE, etc.
    int mKeyFrameNum;
    int mSeqNum;
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;
    SE_Layer mLocalLayer;
    SE_StringID mName;
    SE_StringID mFullPathName;
    SE_TimeKey mTimeKey;
    SE_TimeKey mStartKey;
    SE_TimeKey mEndKey;
    SE_URI mURI;
    SE_KeyFrameController* mKeyFrameController;
    /////////////
    SE_Element* mPrevElement;
    SE_Element* mNextElement;
    SE_SpatialID mSpatialID;
    SE_SimObjectID mSimObjectID;
    SE_PrimitiveID mPrimitiveID;
    SE_AnimationID mAnimationID;
    SE_RenderTargetID mRenderTarget;
    bool mOwnRenderTargetCamera;
    bool mNeedUpdateTransform;
    SE_SceneRenderSeq mSceneRenderSeq;
    int mRenderQueueSeq;
	SE_Animation* mAnimation;
    SE_Matrix4f mPrevMatrix;
    SE_Matrix4f mPostMatrix;
};
class SE_2DNodeElement : public SE_Element, public SE_Object
{
public:
    SE_2DNodeElement();
    ~SE_2DNodeElement();
	void setRect(float left, float top, float width, float height)
	{
		mLeft = left;
		mTop = top;
		mWidth = width;
		mHeight = height;
	}
    void setPivotX(float px)
    {
        mPivotX = px;
    }
    void setPivotY(float py)
    {
        mPivotY = py;
    }
	float getPivotX() const
	{
		return mPivotX;
	}
	float getPivotY() const
	{
		return mPivotY;
	}
    void setMountPointRef(const SE_MountPointID& ref)
    {
        mMountPointID = ref;
    }
    void setMountPointX(float x)
    {
        mMountPointX = x;
    }
    void setMountPointY(float y)
    {
        mMountPointY = y;
    }
	void setMountPoint(float x, float y)
	{
		mMountPointX = x;
		mMountPointY = y;
	}
	void setMountPoint(const SE_MountPointSet& mountPointSet)
	{
		mMountPointSet = mountPointSet;
	}
    void clearMountPoint()
    {
        mMountPointSet.clearMountPoint();
    }
	float getLeft() const
	{
		return mLeft;
	}
	float getTop() const
	{
		return mTop;
	}
	void setLeft(float left)
	{
		mLeft = left;
	}
	void setTop(float top)
	{
		mTop = top;
	}
	float getWidth() const
	{
		return mWidth;
	}
	float getHeight() const
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
	float getDeltaLeft() const
	{
		return mDeltaLeft;
	}
    float getDeltaTop() const
	{
		return mDeltaTop;
	}
	void setDeltaLeft(float l)
	{
		mDeltaLeft = l;
	}
	void setDeltaTop(float t)
	{
		mDeltaTop = t;
	}
	void addMountPoint(const SE_MountPoint& mountPoint)
    {
        mMountPointSet.addMountPoint(mountPoint);
    }
    void removeMountPoint(const SE_MountPointID& mountPointID)
    {
        mMountPointSet.removeMountPoint(mountPointID);
    }
    SE_MountPoint getMountPoint(const SE_MountPointID& mountPointID) const
	{
		return mMountPointSet.getMountPoint(mountPointID);
	}
public:
    virtual void spawn();
    virtual void update(const SE_TimeKey& timeKey);
	virtual void update(SE_ParamValueList& paramValueList);
	virtual void update(const SE_AddressID& address, const SE_Value& value);
    virtual void layout();
    virtual SE_Spatial* createSpatial();  
    virtual void read(SE_BufferInput& inputBuffer);
    virtual void write(SE_BufferOutput& outputBuffer); 
    virtual SE_Element* clone();
protected:
    virtual void clone(SE_Element* src, SE_Element* dst);
protected:
	void updateMountPoint();
	//if pivotx == INVALID_GEOMINFO mPivotX will not be changed
	// if pivoty == INVALID_GEOMINFO mPivotY will not be changed
	//if width == INVALID_GEOMINFO || height == INVALID_GEOMINFO , error will be raised
	void calculateRect(float pivotx, float pivoty, float width, float height);
	SE_Spatial* createNode();
	SE_Spatial* createSpatialByImage();
	void createPrimitive(SE_PrimitiveID& outID, SE_RectPrimitive*& outPrimitive);
	SE_ImageData* createImageData(const SE_ImageDataID& imageDataID);
	SE_CameraID createRenderTargetCamera(float left, float top, float width, float height);
protected:
    float mLeft, mTop, mWidth, mHeight;
    float mPivotX, mPivotY, mMountPointX, mMountPointY;
    float mDeltaLeft, mDeltaTop;
    SE_MountPointSet mMountPointSet;
    SE_MountPointID mMountPointID;
};
#endif
