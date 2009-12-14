#ifndef STHREAD_H
#define STHREAD_H
class SThreadId
{
public:
#if defined(WIN32)
    HANDLE mId;
#else
#endif
};
typedef void* (*THREADFUN)(void*)
class SThread
{
public:
     SThreadId createThread(THREADFUN threadFun, void* arg);	
};
#endif
