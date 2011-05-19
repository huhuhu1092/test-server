#include "SE_ThreadManager.h"
SE_ThreadID SE_ThreadManager::add(SE_Thread* thread)
{
    return mThreadManager.add(SE_ThreadID::NULLID, thread);
}
SE_Thread* SE_ThreadManager::remove(SE_ThreadID id)
{
    return mThreadManager.remove(id);
}
void SE_ThreadManager::release(SE_Thread* thread, int delay)
{
    mThreadManager.release(thread, delay);
}

