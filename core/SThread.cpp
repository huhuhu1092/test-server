#include "SThread.h"
#include "SType.h"
#if defined(WIN32)
#else
#include <pthread.h>
#endif

static void createThread(THREADFUN f, void* args)
{
#if defined(WIN32)
#else
    pthread_t thread;
    int tid = pthread_create(&thread, NULL, f, args);
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
