#ifndef SRESOURCETHREADMANAGER_H
#define SRESOURCETHREADMANAGER_H
#include "SActivityThread.h"
class SResourceThreadManager : public SActivityThrad
{
public:
    static SResourceThreadManager* getInstance();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
private:
    SResourceThreadManager();
    S_DECLARE_NONECOPY(SResourceThreadManager);
private:
    static SResourceThreadManager* instance;
};
#endif
