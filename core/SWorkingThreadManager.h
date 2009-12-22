#ifndef SWORKINGTHREADMANAGER_H
#define SWORKINGTHREADMANAGER_H
#include "SActivityThread.h"
#include "SUtil.h"
class SCommandEvent;
class SClient;
class SNetAddress;
class SCommandEventFactory;
class SWorkingThreadManager : public SActivityThread
{
public:
    static SWorkingThreadManager* getInstance();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
    bool getClientList(SClientList& clientList);
    SClient* findClient(const SNetAddress& sa);
    void setCommandEventFactory(SCommandEventFactory* factory);
    ~SWorkingThreadManager();
    SCommandEvent* create(int commandId, unsigned char* data);
private:
    SWorkingThreadManager();
    S_DECLARE_NONECOPY(SWorkingThreadManager);
protected:
    bool processCommandEvent(SCommandEvent*);
    void processEvents();
private:
    static SWorkingThreadManager* instance;
    class SImplData;
    SImplData* mImplData;
};
#endif
