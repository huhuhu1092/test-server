#include "STime.h"
#if defined(WIN32)
#else
#include <sys/time.h>
#include <unistd.h>
#endif
STimeMS STime::getCurrentTimeMS()
{
#if defined(WIN32)
#else
    timeval tp;
    gettimeofday(&tp, 0);
    uint32_t timeMS = tp.tv_sec * 1000 + tp.tv_usec / 1000;    return timeMS; 
#endif    
}
STimeUS STime::getCurrentTimeUS()
{
#if defined(WIN32)
#else
    timeval tp;
    gettimeofday(&tp, 0);
    return tp.tv_sec * 1000000 + tp.tv_usec; 
#endif
}
void STime::start()
{
    mStartTime = getCurrentTimeUS();
}
STimeMS STime::getSpanMS()
{
    if(mStartTime == 0)
        return 0;
    STimeUS currTime = getCurrentTimeUS();
    return (currTime - mStartTime) / 1000;
}
STime::STime()
{
    mStartTime = 0;
}
