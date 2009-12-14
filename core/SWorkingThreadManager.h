#ifndef SWORKINGTHREADMANAGER_H
#define SWORKINGTHREADMANAGER_H
#include "SActivityThread.h"
#include "SUtil.h"
class SCommandEvent;
class SWorkingThreadManager : public SActivityThread
{
public:
    static SWorkingThreadManager* getInstance();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
    ~SWorkingThreadManager();
private:
    SWorkingThreadManager();
    S_DECLARE_NONECOPY(SWorkingThreadManager);
protected:
    bool processCommandEvent(SCommandEvent*);
private:
    static SWorkingThreadManager* instance;
    class SImplData;
    SImplData* mImplData;
};
#endif
