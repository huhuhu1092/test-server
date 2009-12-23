#ifndef STIME_H
#define STIME_H
#include "SType.h"
#include <stdexcept>
#include <string>
using namespace std;
class STimeExcept : public runtime_error
{
public:
    STimeExcept(const string& msg) : runtime_error(msg)
    {}
};
#if defined(WIN32)

#else
typedef uint32_t STimeMS;
typedef uint64_t STimeUS;
class STime
{
public:
    STime();
    static STimeMS getCurrentTimeMS();
    static STimeUS getCurrentTimeUS();
    void start();
    STimeMS getSpanMS();
private:
    STimeUS mStartTime;
};
#endif
#endif
