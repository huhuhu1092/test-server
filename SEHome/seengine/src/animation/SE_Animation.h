#ifndef SE_ANIMATION_H
#define SE_ANIMATION_H
#include "SE_Time.h"
class SE_Interpolate;
class SE_Animation
{
public:
    enum RUN_MODE {NOT_REPEAT, REPEAT, ONE_FRAME, REVERSE_NOT_REPEAT, REVERSE_REPEAT, REVERSE_ONE_FRAME};
    enum TIME_MODE {REAL, SIMULATE};
    SE_Animation();
    virtual ~SE_Animation();
    void run();
    void pause();
    void restore();
    void end();
    void nextFrame(SE_TimeMS realDelta, SE_TimeMS simulateDeltaTime);
    void setDuration(SE_TimeMS duration)
    {
        mDuration = duration;
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
public:
    virtual void update(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
    virtual void onRun();
    virtual void onPause();
    virtual void onEnd();
    virtual void onRestore();
    virtual void onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent);
private:
    void oneFrame(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
    enum ANIM_STATE{RUNNING, END, PAUSE};
    ANIM_STATE mAnimState;
    RUN_MODE mRunMode;
    TIME_MODE mTimeMode;
    SE_TimeMS mDuration;
    SE_TimeMS mPassedTime;
    SE_Interpolate* mInterpolate;
};
#endif
