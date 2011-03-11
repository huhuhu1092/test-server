#ifndef SE_MUTEX_H
#define SE_MUTEX_H
#include "SE_Utils.h"
#include "SE_Common.h"
#include <memory>
using namespace std;
class SE_Mutex
{
public:
    SE_Mutex();
    ~SE_Mutex();
    bool lock();
    bool unlock();
private:
    SE_DECLARE_NONECOPY(SE_Mutex);
private:
    class SE_MutexImpl;
    SE_MutexImpl* mImpl;
    //auto_ptr<SMutexImpl> mImpl;
};
class SE_AutoMutex
{
public:
    SE_AutoMutex(SE_Mutex* source) : mSource(source)
    {
        mSource->lock();
    }
    ~SE_AutoMutex()
    {
        mSource->unlock();
    }
private:
    SE_Mutex* mSource;
};

#endif
