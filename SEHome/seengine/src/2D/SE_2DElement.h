#ifndef SE_2DELEMENT_H
#define SE_2DELEMENT_H
#include "SE_Element.h"
#include "SE_Layer.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_ID.h"
#include "SE_MountPoint.h"
#include "SE_Action.h"
#include "SE_TableManager.h"
#include "SE_Geometry3D.h"
#include "SE_Utils.h"
#include "SE_Common.h"
#include "SE_ParamObserver.h"
#include "SE_ImageMap.h"
#include "SE_URI.h"
#include <string>
#include <list>
#include <vector>
#include <map>
class SE_Spatial;
class SE_Element;
class SE_KeyFrameController;
class SE_Animation;
class SE_ImageBase;
class SE_Image;
class SE_RawImage;
class SE_ImageData;
class SE_Sequence;
class SE_ColorEffectImage;
class SE_ColorEffectController;
class SE_ElementContent;
class SE_RectPrimitive;
class SE_Surface;
class SE_RectPrimitive;
class SE_StateMachine;
class SE_ParamValueList;
/*
    SE_Element is a tree structure. SE_Element is the node , it can has child.
	An element which has content can not has child, it responsibility is just to create
	a real spatial for display.
*/
/*
class SE_Element : public SE_ParamObserver
{
public:
    SE_Element(float left = 0, float top = 0, float width = 0, float height = 0);
    virtual ~SE_Element();
    float getLeft() const
    {
	    return mLeft;
    }
    float getTop() const
    {
        return mTop;
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
    void setLeft(float left)
    {
        mLeft = left;
    }
    void setTop(float top)
    {
        mTop = top;
    }
	void setRect(float left, float top, float width, float height)
	{
		mLeft = left;
		mTop = top;
		mWidth = width;
		mHeight = height;
	}
    void setCamera(const SE_CameraID& camera)
    {
        mCamera = camera;
    }
    SE_CameraID getCamera() const
    {
        return mCamera;
    }
    void setElementRef(const SE_StringID& elementref)
    {
        mElementRef = elementref;
    }
    SE_StringID getElementRef() const
    {
        return mElementRef;
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
        mName = name;
    }
    const SE_StringID& getName()
    {
        return mName;
    }
    
    void setID(const SE_ElementID& id)
    {
        mID = id;
    }
    SE_ElementID getID() const
    {
        return mID;
    }
	void setFullPathName(const SE_StringID& name)
	{
		mFullPathName = name;
	}
	SE_StringID getFullPathName() const
	{
		return mFullPathName;
	}
    void setParent(SE_Element* parent)
    {
        mParent = parent;
    }
    SE_Element* getParent() const
    {
        return mParent;
    }
	void setSimObjectID(const SE_SimObjectID& simObj)
	{
		mSimObjectID = simObj;
	}
    SE_SimObjectID getSimObjectID()
    {
        return mSimObjectID;
    }
	void addContent(SE_ElementContent* ec);
	int getContentNum() const;
	SE_ElementContent* getContent(int index) const;
	void clearContent();
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
	void setAnimationID(const SE_AnimationID& animID)
	{
		mAnimationID = animID;
	}
	SE_AnimationID getAnimationID() const
	{
		return mAnimationID;
	}
	void setRenderTarget(const SE_RenderTargetID& id);
	SE_RenderTargetID getRenderTarget() const
	{
		return mRenderTarget;
	}
    float getPivotX() const
    { 
        return mPivotX;
    }
    float getPivotY() const
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
	float getDeltaLeft() const
	{
		return mDeltaLeft;
	}
	float getDeltaTop() const
	{
		return mDeltaTop;
	}
	void setDeltaLeft(float f)
	{
		mDeltaLeft = f;
	}
	void setDeltaTop(float f)
	{
		mDeltaTop = f;
	}
	void setMountPointRef(const SE_MountPointID& mp)
	{
		mMountPointID = mp;
	}
	SE_MountPointID getMountPointRef() const
	{
		return mMountPointID;
	}
	SE_KeyFrameController* getKeyFrameController() const
	{
		return mKeyFrameController;
	}
	void setKeyFrameController(SE_KeyFrameController* kfc)
	{
		mKeyFrameController = kfc;
	}
	SE_Animation* getAnimation() const
	{
		return mAnimation;
	}
	void setAnimation(SE_Animation* anim);
    void addMountPoint(const SE_MountPoint& mountPoint);
    void removeMountPoint(const SE_MountPointID& mountPointID);
    void clearMountPoint();
    SE_MountPoint getMountPoint(const SE_MountPointID& mountPointID) const;
	void setTimeKey(unsigned int key)
	{
		mTimeKey = key;
	}
	unsigned int getTimeKey() const
	{
		return mTimeKey;
	}
	void startAnimation();
	void setStartKey(unsigned int key)
	{
		mStartKey = key;
	}
	void setEndKey(unsigned int key)
	{
		mEndKey = key;
	}
	void setMountPoint(float x, float y)
	{
		mMountPointX = x;
		mMountPointY = y;
	}
	float getMountPointX() const
	{
		return mMountPointX;
	}
	float getMountPointY() const
	{
		return mMountPointY;
	}
	unsigned int getStartKey() const
	{
		return mStartKey;
	}
	unsigned int getEndKey() const
	{
		return mEndKey;
	}
    void addChild(SE_Element* e);
    void removeChild(SE_Element* e = NULL);
	SE_Element* removeChild(const SE_ElementID& id);
	//if pivotx == INVALID_GEOMINFO mPivotX will not be changed
	// if pivoty == INVALID_GEOMINFO mPivotY will not be changed
	//if width == INVALID_GEOMINFO || height == INVALID_GEOMINFO , error will be raised
	void calculateRect(float pivotx, float pivoty, float width, float height);
	void setPrev(SE_Element* prev)
	{
		mPrevElement = prev;
	}
	void setNext(SE_Element* next)
	{
		mNextElement = next;
	}
	SE_Element* getPrev() const
	{
		return mPrevElement;
	}
	SE_Element* getNext() const
	{
		return mNextElement;
	}
	int getSeqNum() const
	{
		return mSeqNum;
	}
	void setSeqNum(int i) 
	{
		mSeqNum = i;
	}
	void setNeedUpdateTransform(bool b)
	{
		mNeedUpdateTransform = b;
	}
	void setOwnRenderTargetCamera(bool own)
	{
		mOwnRenderTargetCamera = own;
	}
	//this function is used by inherited class
	//user can not use it
	const std::list<SE_Element*>& getChildren() const
	{
		return mChildren;
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
	static SE_Element* getElement(const SE_StringID& uri);
public:
    virtual SE_Spatial* createSpatial();
    virtual void update(unsigned int key);
    virtual void update(SE_ParamValueList& paramValueList);
	virtual void update(const SE_AddressID& address, const SE_Value& value);
	// spawn has different function for element node and element leaf
	// for element node it will calculate the origin point in its parent coordinate system
	// for element leaf it must calculate the origin point and its boudary in its parent coordinate system
    // for element node it will also make the content URI become element node or leaf, and then 
	// continue to spawn the element created by content URI
	virtual void spawn();
	// measure will invoke after spawn
	// it will use element leaf's boundary to calculate element node boundary
	virtual void measure();
    virtual void travel(SE_ElementTravel* travel) const;
	virtual SE_Element* clone();
	virtual int getKeyFrameNum() const;
	virtual void getImageData(SE_ImageDataID& imageDataID, const SE_ImageData*& imageData) const
	{}
protected:
	virtual void setImageData(SE_RectPrimitive* primitive);
	virtual void setSurface(SE_Surface* surface);
protected:
	SE_Spatial* createNode();
	SE_Spatial* createSpatialByImage();
	void merge(SE_Rect<float>& mergedRect ,const SE_Rect<float>& srcRect);
	void clone(SE_Element *src, SE_Element* dst);
	void updateMountPoint();
	void createPrimitive(SE_PrimitiveID& outID, SE_RectPrimitive*& outPrimitive);
	SE_ImageData* createImageData(const SE_ImageDataID& imageDataID);
	SE_CameraID createRenderTargetCamera(float left, float top, float width, float height);
    void clearChildren();
private:
    SE_Element(const SE_Element&);
    SE_Element& operator=(const SE_Element&);
protected:
    float mTop;
    float mLeft;
    float mWidth;
    float mHeight; 
    float mPivotX;
    float mPivotY;
	float mMountPointX;
	float mMountPointY;
	float mDeltaLeft;
	float mDeltaTop;
    SE_Vector3f mLocalTranslate;
    SE_Vector3f mLocalScale;
    SE_Quat mLocalRotate;
    SE_Layer mLocalLayer;
    SE_ElementID mID;
	SE_StringID mFullPathName;
    SE_StringID mName;
	SE_MountPointID mMountPointID;
    SE_StringID mElementRef;
	SE_KeyFrameController* mKeyFrameController;
    SE_MountPointSet mMountPointSet;
    typedef std::list<SE_Element*> _ElementList;
    _ElementList mChildren;
	SE_Animation* mAnimation;
    unsigned int mTimeKey;
	unsigned int mStartKey;
	unsigned int mEndKey;
	SE_Element* mPrevElement;
	SE_Element* mNextElement;
	int mKeyFrameNum;
	typedef std::list<SE_ElementContent*> _ElementContentList;
	_ElementContentList mElementContentList;
	int mSeqNum;// the sequence number in its parent element
    // dynamic data which will not copy
    SE_Element* mParent;
    SE_SimObjectID mSimObjectID;
    SE_SpatialID mSpatialID;
    SE_PrimitiveID mPrimitiveID;
	SE_AnimationID mAnimationID;
	SE_RenderTargetID mRenderTarget;
	bool mNeedUpdateTransform;
	bool mOwnRenderTargetCamera;
	SE_URI mURI;
};
*/
///////////////////////
class SE_ImageElement : public SE_Element
{
public:
	SE_ImageElement(const SE_StringID& uri);
	~SE_ImageElement();
	void spawn();
	void layout();
	SE_Spatial* createSpatial();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
protected:
	bool isValid();
	virtual void setImageData(SE_RectPrimitive* primitive);
	virtual void setSurface(SE_Surface* surface);
	void initImage();
private:
	enum {IMG_SIZE = 5};
	struct _ImageUnitData
	{
		int valid;
		SE_ImageUnit* imageUnit;
		_ImageUnitData()
		{
			valid = 0;
			imageUnit = NULL;
		}
	};
	SE_ImageUnit mRChannel;
	SE_ImageUnit mGChannel;
	SE_ImageUnit mBChannel;
	SE_ImageUnit mAChannel;
	SE_ImageUnit mBaseColor;
	_ImageUnitData mImageUnits[IMG_SIZE];
};

class SE_TextureElement : public SE_Element
{
public:
	SE_TextureElement(const SE_StringID& uri);
	~SE_TextureElement();
	void setElementImage(SE_RawImage* image);
	void setImage(const SE_ImageDataID& id, SE_ImageData* imageData);
	void spawn();
	void layout();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
    void setContentChild(SE_Element* c)
	{
		mContentChild = c;
	}
	SE_Spatial* createSpatial();
protected:
	void setImageData(SE_RectPrimitive* primitive);
	void setSurface(SE_Surface* surface);
	void init();
private:
	SE_ImageDataID mImageDataID;
	SE_ImageData* mImageData;
	SE_RenderTargetID mRenderTargetID;
	SE_Element* mContentChild;
};
class SE_ActionElement : public SE_Element
{
public:
	SE_ActionElement(const SE_StringID& uri);
	void spawn();
	SE_Spatial* createSpatial();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	void addHeadElement(SE_Element* e)
	{
		mHeadElementList.push_back(e);
	}
private:
	SE_Action* mAction;
	typedef std::list<SE_Element*> _HeadElementList;
	_HeadElementList mHeadElementList;
};
class SE_StateTableElement : public SE_Element
{
public:
	SE_StateTableElement(const SE_StringID& uri);
	void update(unsigned int key);
	void spawn();
	void layout();
    void update(const SE_AddressID& address, const SE_Value& value);

	SE_Spatial* createSpatial();
private:
	SE_StateMachine* mStateTable;
};
class SE_NullElement : public SE_Element
{
public:
	SE_NullElement()
	{}
	SE_Spatial* createSpatial();
};
class SE_SequenceElement : public SE_Element
{
public:
	SE_SequenceElement(const SE_StringID& uri);
    void spawn();
	SE_Spatial* createSpatial();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);
    void update(SE_ParamValueList& paramValueList);
	int getKeyFrameNum();
private:
	SE_Sequence* mSequence;
	SE_Element* mCurrentElement;
};
class SE_ColorEffectControllerElement : public SE_Element
{
public:
	SE_ColorEffectControllerElement(const SE_StringID& uri);
	~SE_ColorEffectControllerElement();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);

	SE_Spatial* createSpatial();
	void spawn();
	void layout();
private:
	SE_ColorEffectController* mColorEffectController;
	SE_Element* mCurrentElement;
};
class SE_ColorEffectElement : public SE_Element
{
public:
	enum {MARK_A, MARK_R, MARK_G, MARK_B, MARK_NUM};
    enum {FN_ADD, FN_MULTIPLY, FN_NUM};
	struct _TextureMark
	{
		bool valid;
		SE_StringID mTextureAddress;
		SE_StringID mTextureValue;
		int mTextureArity;
		SE_StringID mColorAlphaAddress;
        int mColorAlphaValue;
		SE_StringID mFnAddress;
		int mFnValue;
		SE_StringID mTextureFnAddress;
		int mTextureFnValue;
		SE_StringID mColorAddress;
        SE_SignColor mColorValue;
		SE_StringID mColor2Address;
		SE_SignColor mColor2Value;
		_TextureMark()
		{
			mColorAlphaValue = 255;
			mFnValue = FN_ADD;
			mTextureFnValue = FN_ADD;
			mTextureArity = 0;
			valid = false;
		}
	};
	void setBackgroundAddress(const SE_StringID& address)
	{
		mBackgroundAddress = address;
	}
	void setBackgroundValue(const SE_StringID& v)
	{
		mBackgroundAddress = v;
	}
	void setChannelAddress(const SE_StringID& address)
	{
		mChannelAddress = address;
	}
	void setChannelValue(const SE_StringID& v)
	{
		mChannelValue = v;
	}
	void setBackgroundAlphaAddress(const SE_StringID& address)
	{
		mBackgroundAlphaAddress = address;
	}
	void setBackgroundAlphaValue(int a)
	{
		mBackgroundAlphaValue = a;
	}
	void setTextureMark(int index, const _TextureMark& tm)
	{
		if(index >= MARK_A && index < MARK_NUM)
		    mTextureMark[index] = tm;
	}
	SE_ColorEffectElement();
	~SE_ColorEffectElement();
	void update(const SE_TimeKey& key);
    void update(const SE_AddressID& address, const SE_Value& value);

	SE_Spatial* createSpatial();
	void spawn();
	void layout();
private:
	void setImageData(SE_RectPrimitive* primitive);
	void setImageData(SE_RectPrimitive* primitive, SE_ImageData* imageData, SE_TEXUNIT_TYPE texType);
	void setImageData(SE_RectPrimitive* primitive, const SE_StringID& imageID, SE_TEXUNIT_TYPE texType);
	void setSurface(SE_Surface* surface);
	void calculateValue();
	SE_XMLTABLE_TYPE getBackgroundType();
	void getExtractImageProperty(SE_XMLTABLE_TYPE& t, int& width, int& height);
	SE_ImageElement* createImageElement(const SE_StringID& textureURL, SE_ImageData*& imageData);
	bool isTextureEnd(std::vector<SE_Element*>::iterator textureIt[4], SE_Element* texture[4]);
	SE_Element* mergeElement(SE_Element* background, SE_Element* channel, SE_Element* texture[4]);
	void mergeElement();
	int uriToInt(const SE_StringID& uri);
private:
	SE_XMLTABLE_TYPE mBackgroundType;
	SE_StringID mBackgroundAddress;
	SE_StringID mBackgroundValue;
	SE_ImageData* mBackgroundImageData;
	SE_ImageDataID mBackgroundImageDataID;
	SE_ImageElement* mBackgroundImageElement;
	int mBackgroundArity;
	SE_StringID mChannelAddress;
	SE_StringID mChannelValue;
	int mChannelArity;
	SE_ImageData* mChannelImageData;
	SE_ImageDataID mChannelImageDataID;
	SE_ImageElement* mChannelImageElement;
	SE_StringID mBackgroundAlphaAddress;
    int mBackgroundAlphaValue;
	_TextureMark mTextureMark[MARK_NUM];
	SE_Element* mBackgroundElement;
	SE_Element* mChannelElement;
	SE_Element* mTextureElement[MARK_NUM];
	SE_ImageData* mTextureImageData[MARK_NUM];
	SE_ImageDataID mTextureImageDataID[MARK_NUM];
	SE_ImageElement* mTextureImageElement[MARK_NUM];
	SE_Element* mMergedElement;
};
typedef SE_Table<SE_StringID, SE_Element*> SE_ElementMap;
typedef SE_Table<SE_StringID, SE_ElementMap*> SE_ElementTable;

#endif
