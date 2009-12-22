#ifndef SEVENT_H
#define SEVENT_H
#include "SType.h"
#include "SNetAddress.h"
#include "SSocket.h"
class SObject;
class SEvent;
class SClient;
class SPostEvent
{
public:
    enum {HIGH_PRIORITY, NORMAL_PRIORITY, LOW_PRIORITY};
    SPostEvent(SObject* r, SEvent* e, int priority)
    {
        receiver = r;
        event = e;
        this->priority = priority;
    }
    SPostEvent() : receiver(0), event(0), priority(0)
    {}
    SObject* receiver;
    SEvent* event;
    int priority;
};
class SEvent
{
public:
    enum Type {
        CREATE_CLIENT,
        EXIT_THREAD_LOOP,
        DESTROY_CLIENT,
        Command,
        User = 1000,
        MaxUser = 65535
    };
    SEvent(Type t): mType(t)
    {}
    virtual ~SEvent() {}
    Type type()
    {
        return mType;
    }    
private:
    Type mType;

};
template <class T>
class SEventWithData : public SEvent
{
public:
    SEventWithData(Type t, T* data, bool own) : SEvent(t)
    {
        this->data = data;
        this->own = own;
    }
    ~SEventWithData()
    {
        if(own)
            delete data;
    }
    T* data;
    bool own;
};
class SCreateClientEvent : public SEvent
{
public:
    SCreateClientEvent(const SNetAddress& address, const SSocket& s) : SEvent(CREATE_CLIENT)
    {
        mAddress = address;
        mSocket = s;
    }
    SNetAddress getAddress()
    {
        return mAddress;
    }
    SSocket getSocket()
    {
        return mSocket;
    }
private:
    SNetAddress mAddress;
    SSocket mSocket;
};
class SDestroyClientEvent : public SEvent
{
public:
    SDestroyClientEvent(SClient* client) : SEvent(DESTROY_CLIENT)
    {
        mClient = client;
    }
    SClient* mClient;
};
#endif
