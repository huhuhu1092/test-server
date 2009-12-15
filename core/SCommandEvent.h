#ifndef SCOMMANDEVENT_H
#define SCOMMANDEVENT_H
#include <string>
#include "SEvent.h"
using namespace std;
class SCommandEvent : public SEvent
{
public:
    SCommandEvent() : SEvent(SEvent::Command)
    {}
    virtual ~SCommandEvent() {}
    void setData(void* dataRef)
    {
        mDataRef = dataRef;
    }
    void* getData()
    {
        return mDataRef;
    }
    virtual void pack(char*& out, int& len) = 0;
    virtual void unpack(const char* input) = 0;
    virtual bool handle() = 0;
private:
    void* mDataRef;
};
#endif
