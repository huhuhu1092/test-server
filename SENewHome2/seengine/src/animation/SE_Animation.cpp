#include "SE_Animation.h"
#include "SE_Interpolate.h"
#include "SE_Log.h"
#include "SE_Application.h"
#include "SE_Message.h"
#include "SE_MessageDefine.h"

#ifdef ANDROID
static void flushScreen()
{
    SE_Message* msg = new SE_Message;
    msg->type = SE_MSG_UPATEWORLD;
    SE_Struct* sestruct = new SE_Struct(1);
    SE_StructItem* sitem = new SE_StructItem(1);
    SE_StdString* stdString = new SE_StdString;
    stdString->data = "update world.";
    sitem->setDataItem(stdString);
    sestruct->setStructItem(0, sitem);
    msg->data = sestruct;
    SE_Application::getInstance()->sendMessage(msg);
    
}
#endif

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

    mPlayMode = CPU_NO_SHADER;
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
    onUpdate(realDelta, simulateDelta, percent, frame,mPlayMode);
	mCurrFrame = frame;
    if(mTimeMode == REAL)
        mPassedTime += realDelta;
    else
        mPassedTime += simulateDelta;
}
void SE_Animation::update(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    //for android
#ifdef ANDROID
    flushScreen();
#endif

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
void SE_Animation::onUpdate(SE_TimeMS realDelta, SE_TimeMS simulateDelta, float percent, int frameIndex,PLAY_MODE playmode)
{}
SE_Animation* SE_Animation::clone()
{
	return NULL;
}
