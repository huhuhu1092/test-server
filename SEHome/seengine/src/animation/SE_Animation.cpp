#include "SE_Animation.h"
#include "SE_Interpolate.h"
SE_Animation::SE_Animation()
{
    mAnimState = END;
    mRunMode = NOT_REPEAT;
    mTimeMode = SIMULATE;
    mDuration = 0;
    mPassedTime = 0;
    mInterpolate = NULL;
}
SE_Animation::~SE_Animation()
{
    if(mInterpolate)
        delete mInterpolate;
}
void SE_Animation::run()
{
    if(mAnimState == END)
    {
        mAnimState = RUNNING;
        onRun();
    }
}
void SE_Animation::restore()
{
    if(mAnimState == PAUSE)
    {
        mAnimState = RUNNING;
        onRestore();
    }
}
void SE_Animation::pause()
{
    if(mAnimState == RUNNING)
    {
        mAnimState = PAUSE;
        onPause();
    }
}
void SE_Animation::end()
{
    if(mAnimState != END)
    {
        mAnimState = END;
        onEnd();
        mPassedTime = 0;
    }
}
void SE_Animation::nextFrame(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    if(mRunMode == ONE_FRAME || mRunMode == REVERSE_ONE_FRAME)
    {
        oneFrame(realDelta, simulateDelta);
    }
}
void SE_Animation::oneFrame(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    if(mPassedTime > mDuration)
    {
        end();
        return;
    }
    float percent = SE_Time::div(mPassedTime, mDuration);
    if(mInterpolate)
        percent = mInterpolate->calc(percent);
    onUpdate(realDelta, simulateDelta, percent);
    if(mTimeMode == REAL)
        mPassedTime += realDelta;
    else
        mPassedTime += simulateDelta;
}
void SE_Animation::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    if(mRunMode == ONE_FRAME || mRunMode == REVERSE_ONE_FRAME)
        return;
    if(mAnimState != RUNNING)
        return;
    oneFrame(realDelta, simulateDelta);
}
void SE_Animation::onRun()
{}
void SE_Animation::onPause()
{}
void SE_Animation::onEnd()
{}
void SE_Animation::onRestore()
{}
void SE_Animation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent)
{}
