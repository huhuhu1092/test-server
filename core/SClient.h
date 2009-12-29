#ifndef SCLIENT_H
#define SCLIENT_H
#include "SNetAddress.h"
#include "SSocket.h"
#include "SMessageStream.h"
#include "SMutex.h"
#include "STime.h"
class SEvent;
class SClientConnectionState;
class SClient
{
public:
    enum STATE {CONNECTING, CONNECTED, EXITING, EXITED};
    SClient(const SNetAddress& address, const SSocket& s, const STimeMS& createTime);
    virtual ~SClient();
    bool canRemove() const;
    void setCanRemove(bool r);
    bool canHandleEvent(SEvent*) const;
    void processMessageFromClient();
    SSocket* getSocket()
    {
        return &mSocket;
    }
    SNetAddress getNetAddress()
    {
        return mAddress;
    }
    SMessageStream* getInputStream()
    {
        return &mInputStream;
    }
    SMessageStream& getOutputStream()
    {
        return mOutputStream;
    }
    //readData and writeData are used by communication thread
    //other thread can not use this two function.
    void readData();
    void writeData();
    STATE getState()
    {
        STATE s;
        mStateMutex.lock();
        s = mState;
        mStateMutex.unlock();
        return s;
    }
    void setState(STATE s)
    {
        mStateMutex.lock();
        mState = s;
        mStateMutex.unlock();
    }
    STimeMS getCreateTime()
    {
        return mCreateTime;
    }
protected:
    bool connectionStateTransition(SClientConnectionState* conState);
private:
    SNetAddress mAddress;
    SSocket mSocket;
    SMessageStream mInputStream;
    SMessageStream mOutputStream;
    mutable bool mCanRemove;
    mutable SMutex mCanRemoveMutex;
    mutable STATE mState;
    mutable SMutex mStateMutex;
    SClientConnectionState* mCurrentConnectionState;
    const STimeMS mCreateTime;
private:
    friend class SWorkingThreadManager;
    friend class SResourceThreadManager;
    friend class SCommunicationThreadManager;
    friend class SClientConnectionState;
};
#endif
