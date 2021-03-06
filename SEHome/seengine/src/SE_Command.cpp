#include "SE_Command.h"
SE_Command::~SE_Command()
{}
bool SE_Command::expire(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    SE_TimeMS compareTime;
    if(mTimeType == REAL)
    {
        compareTime = realDelta;
    }
    else
        compareTime = simulateDelta;
    if(mWaitTime > 0)
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

