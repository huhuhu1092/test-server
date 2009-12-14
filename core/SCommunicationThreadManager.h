#ifndef SCOMMUNICATIONTHREADMANAGER_H
#define SCOMMUNICATIONTHREADMANAGER_H
#include "SActivityThread.h"
#include "SUtil.h"
#include <list>
using namespace std;
class SClient;
class SCommunicationThreadManager : public SActivityThread
{
public:
    static SCommunicationThreadManager* getInstance();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
protected:
    virtual void processEvents();
private:
    void clearBuffer();
    SCommunicationThreadManager(int bufferSize = 256 * 1024);
    ~SCommunicationThreadManager();
    S_DECLARE_NONECOPY(SCommunicationThreadManager);
private:
    static SCommunicationThreadManager* instance;
    typedef list<SClient*> SClientList;
    SClientList mClientList;
    int mBufferSize;
    unsigned char* mBuffer;
};
#endif
