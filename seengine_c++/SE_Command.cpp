#include "SE_Command.h"
SE_Command::~SE_Command()
{}
bool SE_Command::expire(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_TimeMS compareTime;
    if(mbRealTime)
    {
        compareTime = realDelta;
    }
    else
        compareTime = simulateDelta;
    if(mWaitTime > compareTime)
    {
        mWaitTime -= compareTime;
        return false;
    }
    else
    {
        mWaitTime = 0;
        return true;
    }
}

