#ifndef SCLIENTCONNECTIONSTATE_H
#define SCLIENTCONNECTIONSTATE_H
class SClient;
class SEvent;
class SClientConnectionState
{
public:
    SClientConnectionState(SClient* client) : mState(0), mClient(client) {}
    virtual ~SClientConnectionState() {}
    virtual bool transition() {}
    virtual bool canHandleEvent(SEvent* se)
    {
        return true;
    }
    int getState() {return mState;}
    void setState(int state) {mState = state;}
protected:
    virtual void changeState() {};
protected:
    int mState;
    SClient* mClient;
    friend class SClient;
};

class SClientConnectedState : public SClientConnectionState
{
public:
    SClietnConnectedState(SClient* client);
    virtual ~SClientConnectedState();
    virtual bool transition();
};

class SClientExitingState : public SClientConnectionState
{
public:
    SClientExitingState(SClient* client);
};
class SClientExitedState : public SClientConnectionState
{
public:
    SClientExitedState(SClient* client);
};
#endif
