#include "SE_Mutex.h"
#if defined(WIN32)
#else
#include <pthread.h>
#endif
class SE_Mutex::SE_MutexImpl
{
public:
    SE_MutexImpl() 
    {
#if defined(WIN32)
#else
    pthread_mutex_init(&mutex, NULL);
#endif

    }
    ~SE_MutexImpl() {}
#if defined(WIN32)
#else
    pthread_mutex_t mutex;
#endif
};

/////////////////////////////////

SE_Mutex::SE_Mutex(): mImpl(new SE_MutexImpl)
{}
SE_Mutex::~SE_Mutex()
{
    delete mImpl;
}
bool SE_Mutex::lock()
{
#if defined(WIN32)
#else
    pthread_mutex_lock(&mImpl->mutex);
#endif 
     return true;
}
bool SE_Mutex::unlock()
{
#if defined(WIN32)
#else
    pthread_mutex_unlock(&mImpl->mutex);
#endif 

    return true;
}

