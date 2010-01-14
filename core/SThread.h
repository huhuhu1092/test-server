#ifndef STHREAD_H
#define STHREAD_H
#if defined(WIN32)
#else
#include <pthread.h>
#endif
#include "SUtil.h"
class SThreadId
{
public:
#if defined(WIN32)
    HANDLE mId;
#else
    pthread_t mId;
#endif
};
#if defined(WIN32)
#else
typedef void* (*THREADFUN)(void*);
#endif
class SThread
{
public:
    enum PRIORITY {HIGHT, NORMAL, LOW};
    SThread();
    virtual ~SThread();
    virtual void run(PRIORITY priority = NORMAL, int stack = 0);
    virtual void readyToRun();
    virtual void threadLoop();
    SThreadId id()
    {
        return mThreadId;
    }
    static SThreadId currentThread();
    static bool isEqual(const SThreadId& left, const SThreadId& right );
private:
    static void createThread(THREADFUN f, void* args);
private:
    S_DECLARE_NONECOPY(SThread);
private:
    SThreadId mThreadId;
#if defined(WIN32)
#else
    static void* threadFun(void *);
#endif
};
#endif
