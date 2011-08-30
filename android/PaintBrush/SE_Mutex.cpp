#include "SE_Mutex.h"
#include "type.h"
/////////////////////////////////

SE_Mutex::SE_Mutex()
{
#if defined(SE_HAS_MUTEX)
    pthread_mutex_init(&mMutex, NULL);
#endif
}
SE_Mutex::~SE_Mutex()
{
    
}
bool SE_Mutex::lock()
{
#if defined(SE_HAS_MUTEX)
    //LOGI("mutex locked\n");
    pthread_mutex_lock(&mMutex);
#endif
     return true;
}
bool SE_Mutex::unlock()
{
#if defined(SE_HAS_MUTEX)
    //LOGI("mutex unlock\n");
    pthread_mutex_unlock(&mMutex);
#endif
    return true;
}
SE_MutexCondition::SE_MutexCondition() : mMutex(NULL)
{
    pthread_cond_init(&mCond, NULL);
}
void SE_MutexCondition::setMutex(SE_Mutex* m)
{
	mMutex = m;
}
void SE_MutexCondition::signal()
{
	if(!mMutex)
	{
		LOGE("#### condition mutex is NULL ####\n");
		return;
	}
    pthread_cond_signal(&mCond);
}
void SE_MutexCondition::wait()
{
	if(!mMutex)
	{
		LOGE("### condition mutex is NULL ####\n");
		return;
	}
    pthread_cond_wait(&mCond, mMutex->getMutex());
}
