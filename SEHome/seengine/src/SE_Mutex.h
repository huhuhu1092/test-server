#ifndef SE_MUTEX_H
#define SE_MUTEX_H
#include "SE_Common.h"
#if defined(SE_HAS_MUTEX)
#include <pthread.h>
#endif
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
#if defined(SE_HAS_MUTEX)
    pthread_mutex_t mMutex;
#endif
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
