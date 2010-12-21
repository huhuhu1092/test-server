#ifndef SE_ELEMENT_H
#define SE_ELEMENT_H
#include "SE_Layer.h"
#include "SE_Vector.h"
#include "SE_Quat.h"
#include "SE_ID.h"
#include "SE_MountPoint.h"
#include "SE_Action.h"
#include "SE_TableManager.h"
#include "SE_Geometry3D.h"
#include <string>
#include <list>
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
	void setSimObjectID(const SE_SimObjectID& simObj)
	{
		mSimObjectID = simObj;
	}
    SE_SimObjectID getSimObjectID()
    {
        return mSimObjectID;
    }
	void addContent(SE_ElementContent* ec);
	SE_ElementContent* getContent(int index);
	void clearContent();
	void setSpatialID(const SE_SpatialID& spatialID)
	{
		mSpatialID = spatialID;
	}
    SE_SpatialID getSpatialID()
    {
        return mSpatialID;
    }
	void setPrimitiveID(const SE_PrimitiveID& id)
	{
		mPrimitiveID = id;
	}
    SE_PrimitiveID getPrimitiveID()
    {
        return mPrimitiveID;
    }
	void setAnimationID(const SE_AnimationID& animID)
	{
		mAnimationID = animID;
	}
	SE_AnimationID getAnimationID()
	{
		return mAnimationID;
	}
	void setRenderTarget(const SE_RenderTargetID& id);
	SE_RenderTargetID getRenderTarget()
	{
		return mRenderTarget;
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
	float getDeltaLeft()
	{
		return mDeltaLeft;
	}
	float getDeltaTop()
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
	SE_MountPointID getMountPointRef()
	{
		return mMountPointID;
	}
	SE_KeyFrameController* getKeyFrameController()
	{
		return mKeyFrameController;
	}
	void setKeyFrameController(SE_KeyFrameController* kfc)
	{
		mKeyFrameController = kfc;
	}
	SE_Animation* getAnimation()
	{
		return mAnimation;
	}
	void setAnimation(SE_Animation* anim);
    void addMountPoint(const SE_MountPoint& mountPoint);
    void removeMountPoint(const SE_MountPointID& mountPointID);
    void clearMountPoint();
    SE_MountPoint getMountPoint(const SE_MountPointID& mountPointID);
	void setTimeKey(unsigned int key)
	{
		mTimeKey = key;
	}
	unsigned int getTimeKey()
	{
		return mTimeKey;
	}
	void startAnimation();
	void setActionLayer(SE_Action::_ActionLayer* actionLayer)
	{
		mActionLayer = actionLayer;
	}
	SE_Action::_ActionLayer* getActionLayer()
	{
		return mActionLayer;
	}
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
	float getMountPointX()
	{
		return mMountPointX;
	}
	float getMountPointY()
	{
		return mMountPointY;
	}
	unsigned int getStartKey()
	{
		return mStartKey;
	}
	unsigned int getEndKey()
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
	SE_Element* getPrev()
	{
		return mPrevElement;
	}
	SE_Element* getNext()
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
public:
    virtual SE_Spatial* createSpatial();
    virtual void update(unsigned int key);
	virtual void spawn();
	virtual void measure();
    virtual void travel(SE_ElementTravel* travel);
	virtual SE_Element* clone();
	virtual int getKeyFrameNum();
protected:
	SE_Spatial* createSpatialByImage(SE_ImageBase* image);
	void merge(SE_Rect<float>& mergedRect ,const SE_Rect<float>& srcRect);
	void clone(SE_Element *src, SE_Element* dst);
	void updateMountPoint();
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
    SE_Element* mParent;
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
	SE_KeyFrameController* mKeyFrameController;
    SE_MountPointSet mMountPointSet;
    typedef std::list<SE_Element*> _ElementList;
    _ElementList mChildren;
	SE_Animation* mAnimation;
    unsigned int mTimeKey;
	unsigned int mStartKey;
	unsigned int mEndKey;
	SE_Action::_ActionLayer* mActionLayer;
	SE_Element* mPrevElement;
	SE_Element* mNextElement;
	int mKeyFrameNum;
	SE_RenderTargetID mRenderTarget;
	SE_AnimationID mAnimationID;
	typedef std::list<SE_ElementContent*> _ElementContentList;
	_ElementContentList mElementContentList;
	int mSeqNum;// the sequence number in its parent element
	bool mNeedUpdateTransform;
	bool mOwnRenderTargetCamera;
};
class SE_ElementContent
{
public:
	virtual SE_Element* createElement(float mpx, float mpy) = 0;
	virtual ~SE_ElementContent() {}
	virtual SE_ElementContent* clone();
public:
	void setID(const SE_StringID& id)
	{
		mID = id;
	}
	SE_StringID getID()
	{
		return mID;
	}
private:
	SE_StringID mID;
};
class SE_ImageContent : public SE_ElementContent
{
public:
	SE_ImageContent(const SE_StringID& imageURI);
	SE_Element* createElement(float mpx, float mpy);
	SE_ElementContent* clone();
private:
	SE_StringID mImageURI;
};

class SE_ActionContent : public SE_ElementContent
{
public:
	SE_ActionContent(const SE_StringID& actionURI);
	SE_Element* createElement(float mpx, float mpy);
	SE_ElementContent* clone();
private:
	SE_StringID mActionURI;
};
class SE_StateTableContent : public SE_ElementContent
{
public:
	SE_StateTableContent(const SE_StringID& stateTableURI);
	SE_Element* createElement(float mpx, float mpy);
	SE_ElementContent* clone();
private:
	SE_StringID mStateTableURI;
};
///////////////////////
class SE_ImageElement : public SE_Element
{
public:
	SE_ImageElement();
	SE_ImageElement(SE_Image* img) : mImage(img)
	{}
	~SE_ImageElement();
    void setImage(const SE_StringID& image)
    {
		mImageID = image;
    }
	void spawn();
	void measure();
	SE_Spatial* createSpatial();
private:
    SE_StringID mImageID;
	SE_Image* mImage;
};
class SE_TextureElement : public SE_Element
{
public:
	SE_TextureElement(SE_RawImage* image = NULL);
	~SE_TextureElement();
	void setElementImage(SE_RawImage* image);
	void setImage(const SE_ImageDataID& id, SE_ImageData* imageData);
	void spawn();
	void measure();
	void saveRenderTargetID(const SE_RenderTargetID& id)
	{
		mRenderTargetID = id;
	}
	SE_Spatial* createSpatial();
private:
	SE_RawImage* mElementImage;
	SE_RenderTargetID mRenderTargetID;
};
class SE_ActionElement : public SE_Element
{
public:
	SE_ActionElement();
	SE_StringID getActionID()
	{
		return mActionID;
	}
	void setActionID(const SE_StringID& actionID)
	{
		mActionID = actionID;
	}
	void spawn();
	SE_Spatial* createSpatial();
	void update(unsigned int key);
	void addHeadElement(SE_Element* e)
	{
		mHeadElementList.push_back(e);
	}
private:
	SE_StringID mActionID;
	SE_Action* mAction;
	typedef std::list<SE_Element*> _HeadElementList;
	_HeadElementList mHeadElementList;
};
class SE_StateTableElement : public SE_Element
{
public:
	void setStateTableID(const SE_StringID& stID)
	{
		mStateTableID = stID;
	}
	SE_StringID getStateTableID()
	{
		return mStateTableID;
	}
	void update(unsigned int key);
private:
    SE_StringID mStateTableID;
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
	SE_SequenceElement(SE_Sequence* sequence);
    void spawn();
	SE_Spatial* createSpatial();
	void update(unsigned int key);
	int getKeyFrameNum();
private:
	SE_Sequence* mSequence;
	SE_Element* mCurrentElement;
};
/*
class SE_ColorEffectImageElement : public SE_Element
{
public:
	SE_ColorEffectImageElement(SE_ColorEffectImage* image);
	// it will delete mColorEffectImage
	~SE_ColorEffectImageElement();
    void spawn();
	SE_Spatial* createSpatial();
	void update(unsigned int key);
private:
	SE_ColorEffectImage* mColorEffectImage;
};
*/
class SE_ColorEffectControllerElement : public SE_Element
{
public:
	SE_ColorEffectControllerElement(SE_ColorEffectController* colorEffectController) : mColorEffectController(colorEffectController)
	{}
	~SE_ColorEffectControllerElement();
	void update(unsigned int key);
	SE_Spatial* createSpatial();
	void spawn();
private:
	SE_ColorEffectController* mColorEffectController;
};
class SE_ColorEffectElement : public SE_Element
{
public:
	enum {MARK_A, MARK_R, MARK_G, MARK_B, MARK_NUM};
    enum {FN_ADD, FN_MULTIPLY, FN_NUM};
	enum {SIGN_NO, SIGN_PLUS, SIGN_MINUS, SIGN_NUM};
	struct _ColorWithSign
	{
        int sign;
		int value;
		_ColorWithSign()
		{
			sign = SIGN_NO;
			value = 255;
		}
	};
	struct _TextureMark
	{
		SE_StringID mTextureAddress;
		SE_StringID mTextureValue;
		SE_StringID mColorAlphaAddress;
        int mColorAlphaValue;
		SE_StringID mFnAddress;
		int mFnValue;
		SE_StringID mTextureFnAddress;
		int mTextureFnValue;
		SE_StringID mColorAddress;
        _ColorWithSign mColorValue[3];
		_TextureMark()
		{
			mColorAlphaValue = 255;
			mFnValue = FN_ADD;
			mTextureFnValue = FN_ADD;
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
	void update(unsigned int key);
	SE_Spatial* createSpatial();
	void spawn();
private:
	SE_StringID mBackgroundAddress;
	SE_StringID mBackgroundValue;
	SE_StringID mChannelAddress;
	SE_StringID mChannelValue;
	SE_StringID mBackgroundAlphaAddress;
    int mBackgroundAlphaValue;
	_TextureMark mTextureMark[MARK_NUM];
};
typedef SE_Table<SE_StringID, SE_Element*> SE_ElementMap;
typedef SE_Table<SE_StringID, SE_ElementMap*> SE_ElementTable;

#endif
