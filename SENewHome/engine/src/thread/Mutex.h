#ifndef OMS_MUTEX_H
#define OMS_MUTEX_H
#if defined(OMS_HAS_MUTEX)
#include <pthread.h>
#endif
#include "base/Type.h"
namespace oms
{
class Mutex
{
public:
	Mutex();
    ~Mutex();
    bool lock();
    bool unlock();
#if defined(OMS_HAS_MUTEX)
	pthread_mutex_t* getMutex()
	{
		return &mMutex;
    }
#else
	Mutex* getMutex()
	{
	    return NULL;
	}
#endif

private:
	OMS_DECLARE_NO_COPY(Mutex);
private:
#if defined(OMS_HAS_MUTEX)
    pthread_mutex_t mMutex;
#endif
};
class AutoMutex
{
public:
    AutoMutex(Mutex* source) : mSource(source)
	{
		mSource->lock();
	}
	~AutoMutex()
	{
		mSource->unlock();
	}
private:
    Mutex* mSource;
};
class MutexCondition
{
public:
	MutexCondition();
	void signal();
	void wait();
	void setMutex(Mutex* m);
private:
	Mutex* mMutex;
#if defined(OMS_HAS_MUTEX)
	pthread_cond_t mCond;
#endif
};
}
#endif
