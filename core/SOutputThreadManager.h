#ifndef SOUTPUTTHREADMANAGER_H
#define SOUTPUTTHREADMANAGER_H
#include "SActivityThread.h"
#include "SUtil.h"
#include "SSem.h"
#include "SEvent.h"
class SOutputThreadManager : public SActivityThread
{
public:
    static SOutputThreadManager* getInstance();
    //void postData(char* data, int len, bool own = true);
    void sendOutput(SOutputDataEvent*);
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
protected:
    virtual void processEvents();
private:
    SOutputThreadManager();

    S_DECLARE_NONECOPY(SOutputThreadManager);
private:
    static SOutputThreadManager* instance;
    SSem mSem;
    SClientList mClientsHasData;
};
#endif
