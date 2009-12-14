#ifndef SMUTEX_H
#define SMUTEX_H
#include <memory>
using namespace std;
class SMutex
{
public:
    SMutex();
    ~SMutex() {}
    bool lock();
    bool unlock();
private:
    //class SMutexImpl;
    //auto_ptr<SMutexImpl> mImpl;
};
class SAutoMutex
{
public:
    SAutoMutex(SMutex* source) : mSource(source)
    {
        mSource->lock();
    }
    ~SAutoMutex()
    {
        mSource->unlock();
    }
private:
    SMutex* mSource;
};

#endif
