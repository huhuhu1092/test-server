#ifndef SE_THREAD_H
#define SE_THREAD_H
#include <pthread.h>
#include "SE_Common.h"
typedef void* (*THREADFUNC)(void*);
class SE_Thread
{
public:
    enum PRIORITY {LOW, NORMAL, HIGHT};
    SE_Thread();
    virtual ~SE_Thread();
    pthread_t id() const
    {
        return mID;
    }
    void start();
    static pthread_t currentThread();
    static bool isEqual(pthread_t left, pthread_t right);
protected:
    virtual void run() = 0;
    virtual void threadLoop();
private:
    static void createThread(THREADFUNC f, void* args);
    static void* threadFun(void *);
private:
    SE_DECLARE_NONECOPY(SE_Thread);
private:
    pthread_t mID;
};
#endif
