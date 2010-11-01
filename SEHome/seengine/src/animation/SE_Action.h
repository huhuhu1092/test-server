#ifndef SE_ACTION_H
#define SE_ACTION_H
#include "SE_ID.h"
#include "SE_Layer.h"
#include "SE_KeyFrame.h"
class SE_ActionUnit
{
public:
    virtual ~SE_ActionUnit() {}
    virtual void action() {}
    SE_StringID getID()
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
	SE_Layer getLayer()
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
	void play();
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
	void addKeyFrame(SE_KeyFrame<SE_ActionUnit*>* keyframe);
	static bool compareActionLayer(_ActionLayer* first, _ActionLayer* second);
private:
	typedef std::list<_ActionLayer> _ActionLayerList;
    _ActionLayerList mActionLayerList;
    SE_StringID mImageMapRef;
    int mRenderMode;
};
#endif
