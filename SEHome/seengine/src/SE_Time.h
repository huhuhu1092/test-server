#ifndef SE_TIME_H
#define SE_TIME_H
#if defined(WIN32)
#include <windows.h>
typedef unsigned long SE_TimeMS;
typedef unsigned long SE_TimeUS;
class SE_Time
{
public:
	static SE_TimeMS getCurrentTimeMS();
    static SE_TimeUS getCurrentTimeUS();
};
#else
typedef unsigned int SE_TimeMS;
typedef unsigned long SE_TimeUS;

class SE_Time
{
public:
    static SE_TimeMS getCurrentTimeMS();
    static SE_TimeUS getCurrentTimeUS();
};
#endif
#endif
