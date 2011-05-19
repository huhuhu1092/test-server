#ifndef SE_THREADMANAGER_H
#define SE_THREADMANAGER_H
#include "SE_Common.h"
#include "SE_TreeStruct.h"
#include "SE_TreeStructManager.h"
#include "SE_Thread.h"
class SE_Thread;
class SE_ThreadManager
{
public:
    SE_ThreadID add(SE_Thread* thread);
    SE_Thread* remove(SE_ThreadID id);
    void release(SE_Thread* thread, int delay = SE_RELEASE_DELAY);
private:
    SE_TreeStructManager<SE_Thread> mThreadManager;
};
#endif
