#ifndef SE_ACTION_H
#define SE_ACTION_H
#include "SE_ID.h"
#include "SE_Common.h"
#include "SE_Layer.h"
#include "SE_KeyFrame.h"
#include "SE_TableManager.h"
#include "SE_MountPoint.h"
#include "SE_ColorEffectController.h"
#include "SE_TimeKey.h"
#include <list>
class SE_Element;
class SE_ActionUnit
{
public:
	SE_ActionUnit()
	{
		mPivotX = mPivotY = 0;
	}
    virtual ~SE_ActionUnit() {}
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
	SE_Layer getLayer() const
	{
		return mLayer;
	}
    void setMountPointRef(const SE_MountPointID& mp)
	{
		mMountPointRef = mp;
	}
	SE_MountPointID getMountPointRef()
	{
		return mMountPointRef;
	}
	virtual SE_Element* createElement()
	{
		return NULL;
	}
    virtual std::vector<SE_TimeKey> getKeys() const
    {
        return std::vector<SE_TimeKey>();
    }
	virtual SE_StringID getURI() const
	{
		return "";
	}
private:
    SE_StringID mID;
	SE_Layer mLayer;
	SE_MountPointID mMountPointRef;
	int mPivotX;
	int mPivotY;
};
class SE_AnimationObject : public SE_ActionUnit
{
public:
    void setControllerRef(const SE_StringID& controllerref)
    {
        mControllerRef = controllerref;
    }
    SE_StringID getControllerRef()
    {
        return mControllerRef;
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
	void setImageRef(const SE_StringID& imageRef)
	{
		mImageRef = imageRef;
	}
	SE_StringID getImageRef()
	{
		return mImageRef;
	}
	SE_StringID getURI() const
	{
		return mImageRef;
	}
	SE_Element* createElement();
private:
	SE_StringID mImageRef;
};
class SE_TextureAnimationObject : public SE_AnimationObject
{
public:
    void setTextureRef(const SE_StringID& textureRef)
    {
        mTextureRef = textureRef;
    }
    SE_StringID getTextureRef() const
    {
        return mTextureRef;
    }
	SE_Element* createElement(); 
	SE_StringID getURI() const
	{
		return mTextureRef;
	}
private:
	SE_StringID mTextureRef;
};
class SE_SequenceAnimationObject : public SE_AnimationObject
{
public:
    void setSequenceRef(const SE_StringID& sequenceFrameRef)
    {
        mSequenceFrameRef = sequenceFrameRef;
    }
    SE_StringID getSequenceRef() const
    {
        return mSequenceFrameRef;
    }
	SE_Element* createElement();
    std::vector<SE_TimeKey> getKeys() const;
	SE_StringID getURI() const
	{
		return mSequenceFrameRef;
	}
private:
	SE_StringID mSequenceFrameRef;
};
class SE_ColorEffectAnimationObject : public SE_AnimationObject
{
public:
    void setColorEffectRef(const SE_StringID& id)
    {
        mColorEffectRef = id;
    }
    SE_StringID getColorEffectRef() const
    {
        return mColorEffectRef;
    }
	SE_Element* createElement();
	SE_StringID getURI() const
	{
		return mColorEffectRef;
	}
private:
    SE_StringID mColorEffectRef;
    //SE_ColorEffectInput mColorEffectInput;
};
class SE_DeleteAction : public SE_ActionUnit
{
public:
    void setIDRef(const SE_StringID& ref)
    {
        mRef = ref;
    }
    SE_StringID getIDRef()
    {
        return mRef;
    }
	SE_Element* createElement();
private:
    SE_StringID mRef;
};

class SE_MusicObjectAction : public SE_AnimationObject
{
public:
    void setMusicRef(const SE_StringID& musicID)
    {
        mMusicRef = musicID;
    }
    SE_StringID getMusicRef()
    {
        return mMusicRef;
    }
	SE_Element* createElement();

private:
    SE_StringID mMusicRef;
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
	void addMountPoint(const SE_MountPoint& mp)
    {
        mMountPointSet.addMountPoint(mp);
    }
    SE_MountPoint getMountPoint(const SE_MountPointID& id)
    {
        return mMountPointSet.getMountPoint(id);
    }
    SE_MountPointSet getMountPoint() const
    {
        return mMountPointSet;
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
    std::list<SE_Element*> createElement(const SE_TimeKey& timeKey);
    SE_TimeKey getStartKey();
    SE_TimeKey getEndKey();
public:
	class _ActionLayer
	{
	public:
		SE_Layer layer;
        SE_KeyFrameSequence<SE_ActionUnit*> sequences;
		SE_TimeKey startkey;
		SE_TimeKey endkey;
		_ActionLayer()
		{
			startkey = 0;
			endkey = 0;
		}
	};
	const std::list<_ActionLayer*>& getActionLayerList() const
	{
		return mActionLayerList;
	}
private:
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
    int mRenderMode;
    SE_MountPointSet mMountPointSet;
    int mPivotX;
    int mPivotY;

};
typedef SE_Table<SE_StringID, SE_Action*> SE_ActionMapSet;
typedef SE_Table<SE_StringID, SE_ActionMapSet*> SE_ActionTable;
#endif
