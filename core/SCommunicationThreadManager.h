#ifndef SCOMMUNICATIONTHREADMANAGER_H
#define SCOMMUNICATIONTHREADMANAGER_H
#include "SActivityThread.h"
#include "SUtil.h"
/*
#include <list>
using namespace std;
class SClient;
*/
class SCommunicationThreadManager : public SActivityThread
{
public:
    static SCommunicationThreadManager* getInstance();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
    void addRemovedClientData(SClient* client, STimeMS createTime, SNetAddress address);
protected:
    virtual void processEvents();
private:
    void clearBuffer();
    bool isClientInRemovingList(SClient* client);
    SCommunicationThreadManager(int bufferSize = 256 * 1024);
    ~SCommunicationThreadManager();
    S_DECLARE_NONECOPY(SCommunicationThreadManager);
private:
    struct ClientData
    {
        SClient* client;
        SNetAddress clientAddress;
        STimeMS clientCreateTime;
        bool operator==(const ClientData& right)
        {
            return client == right.client && clientAddress == right.clientAddress && clientCreateTime == right.clientCreateTime;
        }
    };
    static SCommunicationThreadManager* instance;
    SClientList mClientList;
    int mBufferSize;
    unsigned char* mBuffer;
    typedef list<ClientData> SClientDataList;
    SClientDataList mRemovingClientDataList;
};
#endif
