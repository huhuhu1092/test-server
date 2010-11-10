#ifndef SE_ACTION_H
#define SE_ACTION_H
#include "SE_ID.h"
#include "SE_Layer.h"
#include "SE_KeyFrame.h"
#include "SE_TableManager.h"
class SE_ActionUnit
{
public:
    virtual ~SE_ActionUnit() {}
    virtual void action() {}
    SE_StringID getID() const
    {
        return mID;
    }
    void setID(const SE_StringID& id)
    {
        mID = id;
    }
    void setLayer(const SE_Layer& layer)
	{
		mLayer = layer;
	}
	SE_Layer getLayer() const
	{
		return mLayer;
	}
private:
    SE_StringID mID;
	SE_Layer mLayer;
};
class SE_AnimationObject : public SE_ActionUnit
{
public:
    virtual void action();
    void setControllerRef(const SE_StringID& controllerref)
    {
        mControllerRef = controllerref;
    }
    SE_StringID getControllerRef()
    {
        mControllerRef;
    }
    int getRepeatMode()
    {
        return mRepeatMode;
    }
    void setRepeatMode(int repeatMode)
    {
        mRepeatMode = repeatMode;
    }
    void setTimeMode(int timeMode)
    {
        mTimeMode = timeMode;
    }
    int getTimeMode()
    {
        return mTimeMode;
    }
private:
    SE_StringID mControllerRef;
    int mTimeMode;
    int mRepeatMode;
};
class SE_ImageAnimationObject : public SE_AnimationObject
{
public:
	void action();
	void setImageRef(const SE_StringID& imageRef)
	{
		mImageRef = imageRef;
	}
	SE_StringID getImageRef()
	{
		return mImageRef;
	}
private:
	SE_StringID mImageRef;
};
class SE_TextureAnimationObject : public SE_AnimationObject
{
public:
	void action();
    void setTextureRef(const SE_StringID& textureRef)
    {
        mTextureRef = textureRef;
    }
    SE_StringID getTextureRef()
    {
        return mTextureRef;
    }
private:
	SE_StringID mTextureRef;
};
class SE_SequenceAnimationObject : public SE_AnimationObject
{
public:
    void setSequenceFrameRef(const SE_StringID& sequenceFrameRef)
    {
        mSequenceFrameRef = sequenceFrameRef;
    }
    SE_StringID getSequenceFrameRef()
    {
        return mSequenceFrameRef;
    }
private:
	SE_StringID mSequenceFrameRef;
};
class SE_DeleteAction : public SE_ActionUnit
{
public:
    void setRef(const SE_StringID& ref)
    {
        mRef = ref;
    }
    SE_StringID getRef()
    {
        return mRef;
    }
    void action();
private:
    SE_StringID mRef;
};

class SE_MusicObjectAction : public SE_ActionUnit
{
};

class SE_Action
{
public:
    enum {RENDER_TO_BUFFER, RENDER_TO_TEXTURE};
    SE_Action();
    ~SE_Action();
    void addActionUnit(unsigned int key, SE_ActionUnit* au);
    void removeActionUnit(const SE_StringID& auID);
	void sort();
    void addEndKey(unsigned int key, const SE_Layer& layer);
    void removeEndKey(unsigned int key, const SE_Layer& layer);
    SE_ActionUnit* getActionUnit(const SE_StringID& auID);
    
    void setRenderMode(int renderMode)
    {
        mRenderMode = renderMode;
    }
    int getRenderMode()
    {
        return mRenderMode;
    }
	void setImageMapRef(const SE_StringID& imageMapRef)
	{
		mImageMapRef = imageMapRef;
	}
	SE_StringID getImageMapRef()
	{
		return mImageMapRef;
	}
	void addMountPoint(const SE_MountPoint& mp)
    {
        mMountPointSet.addMountPoint(mp);
    }
    void setPivotX(int pivotx)
    {
        mPivotX = pivotx;
    }
    void setPivotY(int pivoty)
    {
        mPivotY = pivoty;
    }
    int getPivotX()
    {
        return mPivotX;
    }
    int getPivotY()
    {
        return mPivotY;
    }

private:
    struct _ActionLayer
	{
		SE_Layer layer;
        SE_KeyFrameSequence<SE_ActionUnit*> sequences;
		unsigned int startkey;
		unsigned int endkey;
		_ActionLayer()
		{
			startkey = 0;
			endkey = 0;
		}
	};
    struct _EndKey
    {
        unsigned int key;
        SE_Layer layer;
        _EndKey()
        {
            key = 0;
        }
    };
	struct _EndKeyEqual
	{
		bool operator()(const _EndKey& key)
		{
			if(justCompareLayer)
			{
				if(key.layer == ek.layer)
					return true;
				else
					return false;
			}
			else
			{
				if(key.key == ek.key && key.layer == ek.layer)
					return true;
				else
					return false;
			}
		}
		_EndKey ek;
		bool justCompareLayer;
	};
	void addKeyFrame(SE_KeyFrame<SE_ActionUnit*>* keyframe);
	static bool compareActionLayer(_ActionLayer* first, _ActionLayer* second);
    static bool compareEndKey(const _EndKey& first, const _EndKey& second);
    _EndKey getAllLayerEndKey();
private:
	typedef std::list<_ActionLayer*> _ActionLayerList;
    typedef std::list<_EndKey> _EndKeyList;
    _ActionLayerList mActionLayerList;
    _EndKeyList mEndKeyList;
    SE_StringID mImageMapRef;
    int mRenderMode;
    SE_MountPointSet mMountPointSet;
    int mPivotX;
    int mPivotY;
};
typedef SE_TableManager<SE_StringID, SE_Action*> SE_ActionMap;
typedef SE_TableManager<SE_StringID, SE_ActionMap*> SE_ActionMapSet;
typedef SE_TableManager<SE_StringID, SE_ActionMapSet*> SE_ActionTable;
#endif
