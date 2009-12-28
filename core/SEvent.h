#ifndef SEVENT_H
#define SEVENT_H
#include "SType.h"
#include "SNetAddress.h"
#include "SSocket.h"
#include "STime.h"
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
        CREATE_CLIENT, // when a new client connection arrive
        EXIT_THREAD_LOOP,
        DESTROY_CLIENT, //when client disconnect
	    REMOVE_CLIENT, // remove client from communication's remove client list
	    NEW_INCOMING_DATA, // read data from socket and the data lenght is geater than zero. that means client send data to server
        Command,
        User = 1000,
        MaxUser = 65535
    };
    SEvent(Type t): mType(t)
    {
        mTriggerTime = STime::getCurrentTimeMS();
    }
    virtual ~SEvent() {}
    Type type()
    {
        return mType;
    }   
    STimeMS getTriggerTime()
    {
        return mTriggerTime;
    } 
    void setTriggerTime(STimeMS tt)
    {
        mTriggerTime = tt;
    }
private:
    Type mType;
    STimeMS mTriggerTime;
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
class SRemoveClientEvent : public SEvent
{
public:
    SRemoveClientEvent() : SEvent(REMOVE_CLIENT)
    {}
    SClient* client;
    SNetAddress address;
    STimeMS createTime;
};
#endif
