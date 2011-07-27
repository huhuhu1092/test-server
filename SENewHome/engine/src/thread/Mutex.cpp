#include "thread/Mutex.h"
#include "base/Log.h"
namespace oms
{
Mutex::Mutex()
{
#if defined(OMS_HAS_MUTEX)
    pthread_mutex_init(&mMutex, NULL);
#endif
}
Mutex::~Mutex()
{
    
}
bool Mutex::lock()
{
#if defined(OMS_HAS_MUTEX)
     pthread_mutex_lock(&mMutex);
#endif
     return true;
}
bool Mutex::unlock()
{
#if defined(OMS_HAS_MUTEX)
    pthread_mutex_unlock(&mMutex);
#endif
    return true;
}
MutexCondition::MutexCondition() : mMutex(NULL)
{
#if defined(OMS_HAS_MUTEX)
    pthread_cond_init(&mCond, NULL);
#endif
}
void MutexCondition::setMutex(Mutex* m)
{
	mMutex = m;
}
void MutexCondition::signal()
{
	if(!mMutex)
	{
		LOGE("#### condition mutex is NULL ####\n");
		return;
	}
#if defined(OMS_HAS_MUTEX)
    pthread_cond_signal(&mCond);
#endif
}
void MutexCondition::wait()
{
	if(!mMutex)
	{
		LOGE("### condition mutex is NULL ####\n");
		return;
	}
#if defined(OMS_HAS_MUTEX)
    pthread_cond_wait(&mCond, mMutex->getMutex());
#endif
}
}
