#ifndef SE_ANIMATION_H
#define SE_ANIMATION_H
#include "SE_Time.h"
#include "SE_ID.h"
#include "SE_TreeStruct.h"
#include "SE_TimeKey.h"
#include <vector>
class SE_Interpolate;
class SE_Animation;
class SE_Animation : public SE_ListStruct<SE_Animation>
{
	friend class SE_Element;
public:
    enum RUN_MODE {NOT_REPEAT, REPEAT, ONE_FRAME, REVERSE_NOT_REPEAT, REVERSE_REPEAT, REVERSE_ONE_FRAME};
    enum TIME_MODE {REAL, SIMULATE};
    enum ANIM_STATE{RUNNING, END, PAUSE};
    SE_Animation();
    virtual ~SE_Animation();
    void run();
    void pause();
    void restore();
    void end();
    void nextFrame(SE_TimeMS realDelta, SE_TimeMS simulateDeltaTime);
    //if duration == 0, it will player animation for ever, until you 
    //end the animation by invoke end() function
    void setDuration(SE_TimeMS duration)
    {
        mDuration = duration;
    }
    ANIM_STATE getAnimState()
    {
        return mAnimState;
    }
    SE_TimeMS getDuration()
    {
        return mDuration;
    }
    void setInterpolate(SE_Interpolate* interpolate)
    {
        mInterpolate = interpolate;
    }
    SE_Interpolate* getInterpolate()
    {
        return mInterpolate;
    }
    void setRunMode(RUN_MODE m)
    {
        mRunMode = m;
    }
    RUN_MODE getRunMode()
    {
        return mRunMode;
    }
    void setTimeMode(TIME_MODE tmode)
    {
        mTimeMode = tmode;
    }
    TIME_MODE getTimeMode()
    {
        return mTimeMode;
    }
    SE_TimeMS getPassedTime()
    {
        return mPassedTime;
    }
    bool isEnd()
    {
        return mAnimState == END;
    }
    bool isRunning()
    {
        return mAnimState == RUNNING;
    }
    bool isPause()
    {
        return mAnimState == PAUSE;
    }
    void setSpatialID(const SE_SpatialID& spatialID)
    {
        mSpatialID = spatialID;
    }
    void setPrimitiveID(const SE_PrimitiveID& primitiveID)
    {
        mPrimitiveID = primitiveID;
    }
    void setSimObjectID(const SE_SimObjectID& simObjectID)
    {
        mSimObjectID = simObjectID;
    }
    SE_SpatialID getSpatialID()
    {
        return mSpatialID;
    }
    SE_PrimitiveID getPrimitiveID()
    {
        return mPrimitiveID;
    }
    SE_SimObjectID getSimObjectID()
    {
        return mSimObjectID;
    }
	void setFrameNum(int frameNum)
	{
		mFrameNum = frameNum;
	}
	int getFrameNum()
	{
		return mFrameNum;
	}
	SE_TimeMS getTimePerFrame()
	{
		return mTimePerFrame;
	}
	void setTimePerFrame(SE_TimeMS t)
	{
		mTimePerFrame = t;
	}
	int getCurrentFrame()
	{
		return mCurrFrame;
	}
	void reset()
	{
		mPassedTime = 0;
		mCurrFrame = -1;
	}
	void setCurrentFrame(int f)
    {
        mCurrFrame = f;
    }
	void setKeys(const std::vector<SE_TimeKey>& keys)
	{
		mKeys = keys;
	}
	std::vector<SE_TimeKey> getKeys() const
	{
		return mKeys;
	}
public:
    virtual void update(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    virtual void onRun();
    virtual void onPause();
    virtual void onEnd();
    virtual void onRestore();
    virtual void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex);
	virtual SE_Animation* clone();
protected:
	void clone(SE_Animation* dst);
    void setAnimState(ANIM_STATE as)
    {
        mAnimState = as;
    }
    void setPassedTime(SE_TimeMS passt)
    {
        mPassedTime = passt;
    }

private:
    void oneFrame(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
    ANIM_STATE mAnimState;
    RUN_MODE mRunMode;
    TIME_MODE mTimeMode;
    SE_TimeMS mDuration;
    SE_TimeMS mPassedTime;
    SE_Interpolate* mInterpolate;
    SE_SpatialID mSpatialID;
    SE_PrimitiveID mPrimitiveID;
    SE_SimObjectID mSimObjectID;
	int mFrameNum;
	int mCurrFrame;
	SE_TimeMS mTimePerFrame;
	std::vector<SE_TimeKey> mKeys;
};
#endif
