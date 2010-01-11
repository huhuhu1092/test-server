#include "SSem.h"
SSem::SSem(int initVlaue)
{
#if defined(WIN32)
#else
    mInitValue = initValue
    sem_init(&mSem, 0, mInitValue);
#endif
    
}
SSem::~SSem()
{
#if defined(WIN32)
#else
    sem_destroy(&mSem);
#endif
    
}
int SSem::p()
{
#if defined(WIN32)
#else
    while(sem_post(&mSem) != 0)
    {}
    return 0;
#endif

}
int SSem::v()
{
#if defined(WIN32)
#else
    while(sem_wait(&mSem) != 0)
    {}
    return 0;
#endif
    
}

