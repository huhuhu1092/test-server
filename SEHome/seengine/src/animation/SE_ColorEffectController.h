#ifndef SE_COLOREFFECTCONTROLLER_H
#define SE_COLOREFFECTCONTROLLER_H
#include "SE_KeyFrame.h"
#include "SE_ID.h"
#include "SE_Vector.h"
#include "SE_MountPoint.h"
#include "SE_TableManager.h"
#include "SE_Vector.h"
class SE_Element;
class SE_ChannelInput
{
public:
	enum {FN_INVALID = -1, ALPHA_INVALID = -1};
	static SE_Vector3i COLOR_INVALID;
	SE_StringID texture;
	int fn;
	int alpha;
	SE_Vector3i color;
	bool valid;
	SE_ChannelInput()
	{
		fn = FN_INVALID;
		alpha = ALPHA_INVALID;
		color = COLOR_INVALID;
		valid = false;
	}
};
class SE_ColorEffectInput
{
public:
	SE_StringID background;
	SE_StringID channel;
	int alpha;
	bool valid;
    SE_ChannelInput channelInput[4];
	SE_ColorEffectInput()
	{
		alpha = SE_ChannelInput::ALPHA_INVALID;
		valid = false;
	}
};
class SE_ColorEffectFrame
{
public:
    virtual ~SE_ColorEffectFrame()
    {}
    virtual SE_Element* createElement() {return NULL;}
	void setPivotX(float pivotx)
	{
		mPivotX = pivotx;
	}
	float getPivotX()
	{
		return mPivotX;
	}
	void setPivotY(float pivoty)
	{
		mPivotY = pivoty;
	}
	float getPivotY()
	{
		return mPivotY;
	}
	void setMountPointRef(const SE_MountPointID& id)
	{
		mMountPointRef = id;
	}
	SE_MountPointID getMountPointRef()
	{
		return mMountPointRef;
	}
	static SE_StringID getURL(const SE_StringID& uri);
	virtual SE_StringID getBackground() const
	{
		return "";
	}
    virtual SE_StringID getChannel() const
	{
		return "";
	}
	virtual SE_StringID getTexture(int i) const
	{
		return "";
	}
private:
	SE_MountPointID mMountPointRef;
	float mPivotX;
	float mPivotY;
};
class SE_ColorEffect : public SE_ColorEffectFrame
{
public:
    enum {MARK_A, MARK_R, MARK_G, MARK_B, MARK_NUM};
    enum {FN_ADD, FN_MULTIPLY, FN_NUM};
	enum {SIGN_NO, SIGN_PLUS, SIGN_MINUS, SIGN_NUM};
    struct _TextureColor
    {
        SE_StringID mTextureID;
        SE_StringID mColor;
		SE_StringID mColor2;
		//SE_StringID mColorSign[SIGN_NUM];
        SE_StringID fn;
		SE_StringID colorAlpha;
		SE_StringID texturefn;
		_TextureColor()
		{
		}
    };
    SE_ColorEffect();
    ~SE_ColorEffect();
    void setBackground(const SE_StringID& background)
    {
        mBackgroundID = background;
    }
    SE_StringID getBackground() const
    {
        return mBackgroundID;
    }
    void setChannel(const SE_StringID& channel)
    {
        mChannelID = channel;
    }
    SE_StringID getChannel() const
    {
        return mChannelID;
    }
    void setBackgroundAlpha(const SE_StringID& alpha)
    {
        mBackgroundAlpha = alpha;
    }
    SE_StringID getBackgroundAlpha()
    {
        return mBackgroundAlpha;
    }
    void setTextureColor(int index, _TextureColor* tc)
    {
        if(index < MARK_A || index >= MARK_NUM)
            return;
        _TextureColor* oldtc = mTextureColorData[index];
        if(oldtc)
            delete oldtc;
        mTextureColorData[index] = tc;
    }
    _TextureColor* getTextureColor(int index)
    {
        if(index < MARK_A || index >= MARK_NUM)
            return NULL;
        return mTextureColorData[index];
    }
    SE_StringID getTexture(int i) const
	{
		return mTextureColorData[i]->mTextureID;
	}
    SE_Element* createElement();
private:
    SE_StringID mBackgroundID;
    SE_StringID mChannelID;
    SE_StringID mBackgroundAlpha;
    std::vector<_TextureColor*> mTextureColorData;
};
class SE_ColorEffectReload : public SE_ColorEffectFrame
{
public:
    SE_Element* createElement(const SE_ColorEffectInput& input);
    void setMark(int mark)
    {
        mMark = mark;
    }
    int getMark()
    {
        return mMark;
    }
    void setTexture(const SE_StringID& texture)
    {
        mTexture = texture;
    }
    SE_StringID getTexture()
    {
        return mTexture;
    }
private:
    int mMark;
    SE_StringID mTexture;
};
class SE_ColorEffectController
{
public:
	void setID(const char* id)
	{
		mID = id;
	}
    SE_StringID getID()
	{
		return mID;
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
	void addMountPoint(const SE_MountPoint& mp)
	{
		mMountPointSet.addMountPoint(mp);
	}
	SE_MountPoint getMountPoint(const SE_MountPointID& id)
	{
		return mMountPointSet.getMountPoint(id);
	}
    void addKeyFrame(unsigned int key, SE_ColorEffectFrame* frame);
    SE_ColorEffectFrame* getKeyFrame(unsigned int key) const;
    std::vector<unsigned int> getKeys() const;
	int getKeyFrameNum();
private:
    SE_KeyFrameSequence<SE_ColorEffectFrame*> mKeySequence;
	int mPivotX;
	int mPivotY;
	SE_StringID mID;
	SE_MountPointSet mMountPointSet;
};
typedef SE_Table<SE_StringID, SE_ColorEffectController*> SE_ColorEffectControllerSet;
typedef SE_Table<SE_StringID, SE_ColorEffectControllerSet*> SE_ColorEffectControllerTable;
#endif
