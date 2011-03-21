#include "SE_Mutex.h"
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
     pthread_mutex_lock(&mMutex);
#endif
     return true;
}
bool SE_Mutex::unlock()
{
#if defined(SE_HAS_MUTEX)
    pthread_mutex_unlock(&mMutex);
#endif
    return true;
}

