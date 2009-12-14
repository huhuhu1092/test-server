#ifndef SWORKINGTHREADMANAGER_H
#define SWORKINGTHREADMANAGER_H
#include "SActivityThread.h"
class SCommandEvent;
class SWorkingThreadManager : public SActivityThread
{
public:
    static SWorkdingThreadManager* getInstance();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
private:
    SWorkingThreadManager();
    S_DECLARE_NONE_COPY(SWorkingThreadManager);
protected:
    bool processCommandEvent(SCommandEvent*);
private:
    static SWorkingThreadManager* instance;
};
#endif
