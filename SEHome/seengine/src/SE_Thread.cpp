#include "SE_Thread.h"
#include "SE_Application.h"
#include "SE_ThreadManager.h"
#include "SE_Log.h"
class _DeleteThreadCommand : public SE_Command
{
public:
    _DeleteThreadCommand(SE_Application* app) : SE_Command(app)
    {}
    SE_ThreadID threadid;
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        SE_ThreadManager* threadManager = mApp->getThreadManager();
        SE_Thread* thread = threadManager->remove(threadid);
        threadManager->release(thread, SE_RELEASE_NO_DELAY);
    }
};
SE_Thread::SE_Thread(bool deleteAfterEnd) : mIsDeleteAfterEnd(deleteAfterEnd)
{
    mIsThreadRunEnd = false;
}
SE_Thread::~SE_Thread()
{
    LOGI("##### thread deleted ######\n");
}
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
    thread->mEndStateMutex.lock();
    thread->mIsThreadRunEnd = true;
    thread->mEndStateMutex.unlock();
    if(thread->mIsDeleteAfterEnd)
    {
        _DeleteThreadCommand* t = new _DeleteThreadCommand(SE_Application::getInstance());
        t->threadid = thread->getID();
        SE_Application::getInstance()->postCommand(t);
    }
	return NULL;
}
class _ThreadCommand : public SE_Command
{
public:
    SE_Thread* thread;
    bool deleteWhenEnd;
    _ThreadCommand(SE_Application* app) : SE_Command(app)
    {}
    ~_ThreadCommand()
    {
        if(deleteWhenEnd)
        {
            SE_ThreadManager* threadManager = mApp->getThreadManager();
            LOGI("@@@@ thread = %p @@@@@\n", thread);
            SE_Thread* tmpThread = threadManager->remove(thread->getID());
            LOGI("### release tmp thread =%p ###\n", tmpThread);
            threadManager->release(tmpThread, SE_RELEASE_NO_DELAY);
            LOGI("### release thread end ###\n");
        }
    }
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
    {
        LOGI("### thread command run %p #####\n", thread);
        thread->run();
        LOGI("### thread command run end #####\n");
    }
};
void SE_Thread::start()
{
#if defined(SE_HAS_THREAD)
    createThread(&SE_Thread::threadFun, this);
#else
    _ThreadCommand* threadCommand = new _ThreadCommand(SE_Application::getInstance());
    LOGI("### thread this = %p ####\n", this);
    threadCommand->thread = this;
    threadCommand->deleteWhenEnd = mIsDeleteAfterEnd;
    SE_Application::getInstance()->postCommand(threadCommand);
#endif
}
bool SE_Thread::isEnd()
{
    bool end;
    mEndStateMutex.lock();
    end = mIsThreadRunEnd;
    mEndStateMutex.unlock();
    return end;
}
void SE_Thread::reset()
{
    mEndStateMutex.lock();
    mIsThreadRunEnd = false;
    mEndStateMutex.unlock();

}
