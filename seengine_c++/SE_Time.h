#ifndef SE_TIME_H
#define SE_TIME_H
#if defined(WIN32)

#else
typedef unsigned int SE_TimeMS;
class SE_Time
{
public:
    static SE_TimeMS getCurrentTimeMS();
};
#endif
#endif