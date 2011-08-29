#ifndef SE_MUTEX_H
#define SE_MUTEX_H
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
#if defined(SE_HAS_MUTEX)
	pthread_mutex_t* getMutex()
	{
		return &mMutex;
	}
#else
	SE_Mutex* getMutex()
	{
		return NULL;
	}
#endif
private:
    SE_Mutex(const SE_Mutex&);
    SE_Mutex& operator=(const SE_Mutex&);
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
    SE_AutoMutex(const SE_AutoMutex&);
    SE_AutoMutex& operator=(const SE_AutoMutex&);
private:
    SE_Mutex* mSource;
};
class SE_MutexCondition
{
public:
    SE_MutexCondition();
    void signal();
    void wait();
    void setMutex(SE_Mutex* m);
private:
    SE_MutexCondition(const SE_MutexCondition&);
    SE_MutexCondition& operator=(const SE_MutexCondition&);
private:
    SE_Mutex* mMutex;
#if defined(SE_HAS_MUTEX)
    pthread_cond_t mCond;
#endif
};
#endif
