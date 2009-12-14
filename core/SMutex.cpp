#include "SMutex.h"
/*
class SMutex::SMutexImpl
{
public:
    SMutexImpl() {}
    ~SMutexImpl() {}
};
*/
/////////////////////////////////
SMutex::SMutex()//: mImpl(new SMutexImpl)
{}
bool SMutex::lock()
{
     return true;
}
bool SMutex::unlock()
{
    return true;
}

