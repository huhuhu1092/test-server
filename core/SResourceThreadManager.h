#ifndef SRESOURCETHREADMANAGER_H
#define SRESOURCETHREADMANAGER_H
#include "SActivityThread.h"
#include "SUtil.h"
#include <memory>
using namespace std;
class SCommandEvent;
class SCommandEventFactory;
class SCreateClientEvent;
class SResourceThreadManager : public SActivityThread
{
public:
    static SResourceThreadManager* getInstance();
    void startup(SCommandEventFactory* factory);
    SCommandEvent* create(int commandId, unsigned char* data);
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject*, SEvent*);
private:
    void createNewClient(SCreateClientEvent*);
private:
    SResourceThreadManager();
    S_DECLARE_NONECOPY(SResourceThreadManager);
private:
    static SResourceThreadManager* instance;
    auto_ptr<SCommandEventFactory> mCommandEventFactory;
    class SImplData;
    auto_ptr<SImplData> mImplData; 
};
#endif
