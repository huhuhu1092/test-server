#ifndef SCOMMANDEVENT_H
#define SCOMMANDEVENT_H
#include <string>
#include "SEvent.h"
#include "SNetAddress.h"
using namespace std;
class SCommandEvent : public SEvent
{
public:
    SCommandEvent() : SEvent(SEvent::Command)
    {
        //mCanDelete = false;
    }
    virtual ~SCommandEvent() {}
    /*
    void setData(void* dataRef)
    {
        mDataRef = dataRef;
    }
    void* getData()
    {
        return mDataRef;
    }
    bool canDelete()
    {
        return mCanDelete;
    }
    bool setCanDelete(bool canDelete)
    {
        mCanDelete = canDelete;
    }
    */
    virtual void pack(char*& out, int& len) = 0;
    virtual void unpack(const char* input) = 0;
    virtual bool handle() = 0;
    void setClientID(const SNetAddress& na)
    {
        mClientId = na;
    }
    SNetAddress getClientID()
    {
        return mClientId;
    }
private:
    SNetAddress mClientId;
   // bool mCanDelete;
    //void* mDataRef;
};
#endif
