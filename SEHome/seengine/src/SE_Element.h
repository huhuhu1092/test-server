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
#include "SE_ObjectManager.h"
#include <vector>
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
class SE_MotionEvent;
class SE_KeyEvent;
class SE_ElementClickHandler;
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
    enum STATE {INVALID, NORMAL, SELECTED, HIGHLIGHTED, INVISIBLE, INACTIVE, 
                ANIMATE_BEGIN, ANIMATE_RUNNING, ANIMATE_SUSPEND, ANIMATE_END};
    SE_Element();
    virtual ~SE_Element();
    //mState is a type of STATE
    int getState() const
    {
        return mState;
    }
	// update indicate whether element will update is spatial to make 
	// according state's appearance work
	void setState(int state, bool update = false);
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
	SE_StringID getURI() const;
    SE_StringID getURL() const;
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
	SE_SceneRenderSeq getSceneRenderSeq()
	{
		return mSceneRenderSeq;
	}
    void travel(SE_ElementTravel* travel);
	void setClickHandler(SE_ElementClickHandler* clickHandler)
	{
		mClickHandler = clickHandler;
	}
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
	void setStateURI(int state, const SE_StringID& uri);
	SE_URI getURI(int state) const;
    SE_StringID getURL(int state) const;
	bool canPointed() const
	{
		return mCanPointed;
	}
	void setCanPointed(bool b);
	SE_Element* getCanPointedElement();
	static SE_StringID getStateName(int state);
	static int getStateFromName(const char* name);
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
	virtual bool dispatchMotionEvent(const SE_MotionEvent& motionEvent);
	virtual bool dispatchKeyEvent(const SE_KeyEvent& keyEvent);
    virtual void show();
    virtual void hide();
    //dismiss will make spatial node remove from spatial manager
    virtual void dismiss();
	virtual bool click();
    virtual void onStateChange(int newState, int oldState);
private:
	class _DeleteURI : public SE_ObjectManagerVisitor<int, SE_URI>
	{
	public:
		void visit(const int& state, const SE_URI& uri)
		{
			element->removeObserverFromParamManager(&uri);
		}
		SE_Element* element;
	};
private:
    void removeObserverFromParamManager(const SE_URI* uri);
	void addObserverToParamManager(const SE_URI* uri);
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
    //SE_URI mURI;
    SE_KeyFrameController* mKeyFrameController;
	SE_ObjectManager<int , SE_URI> mStateURIManager;
    /////////////
    SE_Element* mPrevElement;
    SE_Element* mNextElement;
    SE_SpatialID mSpatialID;
    std::vector<SE_SimObjectID> mSimObjectIDArray;
    std::vector<SE_PrimitiveID> mPrimitiveIDArray;
    SE_AnimationID mAnimationID;
    SE_RenderTargetID mRenderTarget;
    bool mOwnRenderTargetCamera;
    bool mNeedUpdateTransform;
    SE_SceneRenderSeq mSceneRenderSeq;
    int mRenderQueueSeq;
	SE_Animation* mAnimation;
    SE_Matrix4f mPrevMatrix;
    SE_Matrix4f mPostMatrix;
	SE_ElementClickHandler* mClickHandler;
	bool mCanPointed;
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
	void setRectPatchType(int rectPatchType)
	{
		mRectPatchType = rectPatchType;
	}
	int getRectPatchType() const
	{
		return mRectPatchType;
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
    virtual void setImageData(SE_Primitive* primitive);
	virtual void setSurface(SE_Surface* surface);
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
	SE_Spatial* createRectPatchSpatial();
	void createPrimitive(SE_PrimitiveID& outID, SE_Primitive*& outPrimitive);
	SE_ImageData* createImageData(const SE_ImageDataID& imageDataID);
	SE_CameraID createRenderTargetCamera(float left, float top, float width, float height);
protected:
    float mLeft, mTop, mWidth, mHeight;
    float mPivotX, mPivotY, mMountPointX, mMountPointY;
    float mDeltaLeft, mDeltaTop;
    SE_MountPointSet mMountPointSet;
    SE_MountPointID mMountPointID;
	int mRectPatchType;
};
#endif
