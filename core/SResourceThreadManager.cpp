#include "SResourceThreadManager.h"
#include "SCommandEventFactory.h"
#include "SUtil.h"
#include "SClient.h"
#include "SCommunicationThreadManager.h"
#include "SWorkingThreadManager.h"
#include <map>
class SResourceThreadManager::SImplData
{
public:
    typedef map<SNetAddress, SClient*> SClientSet;
    SClientSet mClientSet;
};
/////////////////////////////////////////
SResourceThreadManager* SResourceThreadManager::instance = NULL;
SResourceThreadManager* SResourceThreadManager::getInstance()
{
    if(instance == NULL)
        instance = new SResourceThreadManager;
    return instance;
}
bool SResourceThreadManager::event(SEvent* event)
{
    switch(event->type())
    {
    case SEvent::CREATE_CLIENT:
        createNewClient((SCreateClientEvent*)event);
        return true;
    }
    return false;
}
SResourceThreadManager::SResourceThreadManager() : mImplData(new SImplData)
{
}
bool SResourceThreadManager::eventFilter(SObject*, SEvent*)
{
    return false;
}
SCommandEvent* SResourceThreadManager::create(int commandId, unsigned char* data)
{
    return mCommandEventFactory->create(commandId, data);    
}
void SResourceThreadManager::startup(SCommandEventFactory* factory)
{
    mCommandEventFactory.reset(factory);
}
void SResourceThreadManager::createNewClient(SCreateClientEvent* event)
{
    SASSERT(event);
    SResourceThreadManager::SImplData::SClientSet::iterator it = mImplData->mClientSet.find(event->getAddress());
    if(it != mImplData->mClientSet.end())
        return;
    mImplData->mClientSet[event->getAddress()] = new SClient(event->getAddress(), event->getSocket());    
    SClient* sc = mImplData->mClientSet[event->getAddress()];
    SEventWithData<SClient>* cData = new SEventWithData<SClient>(SEvent::CREATE_CLIENT, sc, false);
    SCommunicationThreadManager::getInstance()->postEvent(NULL, cData, SPostEvent::HIGH_PRIORITY);
    SEventWithData<SClient>* wData = new SEventWithData<SClient>(SEvent::CREATE_CLIENT, sc, false);
    SWorkingThreadManager::getInstance()->postEvent(NULL, wData, SPostEvent::HIGH_PRIORITY);
    delete event;
}
