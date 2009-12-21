#ifndef SCLIENT_H
#define SCLIENT_H
#include "SNetAddress.h"
#include "SSocket.h"
#include "SMessageStream.h"
#include "SMutex.h"
class SEvent;
class SClientConnectionState;
class SClient
{
public:
    enum STATE {CONNECTING, CONNECTED, EXITING, EXITED};
    SClient(const SNetAddress& address, const SSocket& s);
    ~SClient();
    bool canRemove();
    void setCanRemove(bool r);
    bool canHandleEvent(SEvent*);
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
    void readData();
    void writeData();
    STATE getState()
    {
        STATE s;
        mStateMutex->lock();
        s = mState;
        mStateMutex->unlock();
        return s;
    }
    void setState(STATE s)
    {
        mStateMutex->lock();
        mState = s;
        mStateMutex->unlock();
    }
protected:
    bool connectionStateTransition(SClientConnectionState* conState);
private:
    SNetAddress mAddress;
    SSocket mSocket;
    SMessageStream mInputStream;
    SMessageStream mOutputStream;
    bool mCanRemove;
    SMutex mCanRemoveMutex;
    STATE mState;
    SMutex mStateMutex;
    SClientConnectionState* mCurrentConnectionState;
private:
    friend class SWorkingThreadManager;
    friend class SResourceThreadManager;
    friend class SCommunicationThreadManager;
    friend class SClientConnectionState;
};
#endif
