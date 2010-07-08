#include "SE_Time.h"
#if defined(WIN32)
#else
#include <sys/time.h>
#include <unistd.h>
#endif
SE_TimeMS SE_Time::getCurrentTimeMS()
{
#if defined(WIN32)
#else
    timeval tp;
    gettimeofday(&tp, 0);
    uint32_t timeMS = tp.tv_sec * 1000 + tp.tv_usec / 1000;    return timeMS; 
#endif    
}
/*
SE_Timer::SE_Timer(int frameRate)
{
    mFrameRate = frameRate;
    mStartTime = 0;
    mSimulateStartTime = 0;
}
void SE_Timer::start()
{
    mStartTime = getCurrentTimeMS();
    mSimulateStartTime = 0;
}
SE_TimeMS SE_Timer::getSpanMS()
{
    if(mStartTime == 0)
        return 0;
    SE_TimeMS currTime = getCurrentTimeMS();
    return (currTime - mStartTime);
}
SE_Timer::update()
{
    mSimulateStartTime += mFrameRate * 
}
*/
