#include "SMutex.h"
#if defined(WIN32)
#else
#include <pthread.h>
#endif
class SMutex::SMutexImpl
{
public:
    SMutexImpl() 
    {
#if defined(WIN32)
#else
    pthread_mutex_init(&mutex, NULL);
#endif

    }
    ~SMutexImpl() {}
#if defined(WIN32)
#else
    pthread_mutex_t mutex;
#endif
};

/////////////////////////////////

SMutex::SMutex(): mImpl(new SMutexImpl)
{}
SMutex::~SMutex()
{
    delete mImpl;
}
bool SMutex::lock()
{
#if defined(WIN32)
#else
    pthread_mutex_lock(&mImpl->mutex);
#endif 
     return true;
}
bool SMutex::unlock()
{
#if defined(WIN32)
#else
    pthread_mutex_unlock(&mImpl->mutex);
#endif 

    return true;
}

