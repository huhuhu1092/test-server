#include "SThread.h"
#include "SType.h"
#if defined(WIN32)
#else
#include <pthread.h>
#endif

void SThread::createThread(THREADFUN f, void* args)
{
#if defined(WIN32)
#else
    SThread* const thread = static_cast<SThread*>(args);
    int tid = pthread_create(&thread->mThreadId.mId, NULL, f, args);
#endif
}
SThread::SThread()
{}
SThread::~SThread()
{}
void SThread::run(PRIORITY priority, int stack)
{
    createThread(threadFun, this);
}
void SThread::readyToRun()
{}
void SThread::threadLoop()
{}
void* SThread::threadFun(void *data)
{
    SThread* const thread = static_cast<SThread*>(data);
    thread->readyToRun();
    thread->threadLoop();
}
SThreadId SThread::currentThread()
{
#if defined(WIN32)
#else
    SThreadId currId;
    currId.mId = pthread_self();
    return currId;
#endif
}
bool SThread::isEqual(const SThreadId& left, const SThreadId& right )
{
#if defined(WIN32)
#else
    return pthread_equal(left.mId, right.mId);
#endif

}

