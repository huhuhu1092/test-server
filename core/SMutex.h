#ifndef SMUTEX_H
#define SMUTEX_H
#include "SUtil.h"
#include <memory>
using namespace std;
class SMutex
{
public:
    SMutex();
    ~SMutex();
    bool lock();
    bool unlock();
private:
    S_DECLARE_NONECOPY(SMutex);
private:
    class SMutexImpl;
    SMutexImpl* mImpl;
    //auto_ptr<SMutexImpl> mImpl;
};
class SAutoMutex
{
public:
    SAutoMutex(SMutex* source) : mSource(source)
    {
        mSource->lock();
    }
    ~SAutoMutex()
    {
        mSource->unlock();
    }
private:
    SMutex* mSource;
};

#endif
