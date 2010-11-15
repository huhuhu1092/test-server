#include "SE_Animation.h"
#include "SE_Interpolate.h"
#include "SE_Log.h"
SE_Animation::SE_Animation()
{
    mAnimState = END;
    mRunMode = NOT_REPEAT;
    mTimeMode = SIMULATE;
    mDuration = 0;
    mPassedTime = 0;
    mInterpolate = NULL;
	mCurrFrame = -1;
	mFrameNum = 0;
	mTimePerFrame = 0;
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
		if(mRunMode == NOT_REPEAT || mRunMode == REVERSE_NOT_REPEAT)
		{
            end();
            return;
		}
		else
		{
			mPassedTime -= mDuration;
		}
    }

    float percent = SE_Time::div(mPassedTime, mDuration);
    if(mInterpolate)
        percent = mInterpolate->calc(percent);
	SE_TimeMS passedTime = getPassedTime();
	LOGI("## passedTime = %d ##\n", passedTime);
    int frame = mCurrFrame;
	if(mRunMode != ONE_FRAME && mRunMode != REVERSE_ONE_FRAME)
	{
		if(mRunMode == REPEAT || mRunMode == NOT_REPEAT)
		{
	        frame = passedTime / mTimePerFrame;
		}
		else if(mRunMode == REVERSE_REPEAT || mRunMode == REVERSE_NOT_REPEAT)
		{
            frame = (mDuration - passedTime) /  mTimePerFrame;
		}
	}
	else
	{
		if(mRunMode == ONE_FRAME)
		{
			frame++;
			if(frame >= mFrameNum)
				frame = 0;
		}
		else
		{
			frame--;
			if(frame < 0)
				frame = mFrameNum - 1;
		}
	}
    onUpdate(realDelta, simulateDelta, percent, frame);
	mCurrFrame = frame;
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
void SE_Animation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex)
{}
SE_Animation* SE_Animation::clone()
{
	return NULL;
}
void SE_Animation::clone(SE_Animation* dst)
{
	if(!dst)
		return;
    dst->setAnimState(getAnimState());
    dst->setRunMode(getRunMode());
    dst->setTimeMode(getTimeMode());
    dst->setDuration(getDuration());
    dst->setPassedTime(getPassedTime());
    SE_Interpolate* interpolate = getInterpolate();
    if(interpolate)
        dst->setInterpolate(interpolate->clone());
    dst->setSpatialID(getSpatialID());
    dst->setPrimitiveID(getPrimitiveID());
    dst->setSimObjectID(getSimObjectID());
    dst->setFrameNum(getFrameNum());
    dst->setCurrentFrame(getCurrentFrame());
    dst->setTimePerFrame(getTimePerFrame());
}