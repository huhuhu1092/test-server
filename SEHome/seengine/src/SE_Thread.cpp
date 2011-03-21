#include "SE_Thread.h"
SE_Thread::SE_Thread()
{}
SE_Thread::~SE_Thread()
{}
void SE_Thread::threadLoop()
{}
pthread_t SE_Thread::currentThread()
{
    return pthread_self();
}
bool SE_Thread::isEqual(pthread_t left, pthread_t right)
{
    return pthread_equal(left, right);
}
void SE_Thread::createThread(THREADFUNC f, void* args)
{
    SE_Thread* thread = static_cast<SE_Thread*>(args);
    pthread_create(&thread->mID, NULL, f, args); 
}
void* SE_Thread::threadFun(void * args)
{
    SE_Thread* thread = static_cast<SE_Thread*>(args);
    thread->run();
    thread->threadLoop();
	return NULL;
}
void SE_Thread::start()
{
    createThread(&SE_Thread::threadFun, this);
}
