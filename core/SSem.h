#ifndef SSEM_H
#define SSEM_H
#if defined(WIN32)
#else
#include <pthread.h>
#include <semaphore.h>
#endif
class SSem
{
public:
    SSem(int initValue = 0);
    ~SSem();
    int p();
    int v();
private:
#if defined(WIN32)
#else
    sem_t mSem;
    unsigned int mInitValue;
#endif
};
#endif
