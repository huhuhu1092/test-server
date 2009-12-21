#include "SResourceThreadManager.h"
#include "SCommandEventFactory.h"
#include "SUtil.h"
#include "SClient.h"
#include "SCommunicationThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SLog.h"
#include "SMutex.h"
#include <map>
class SResourceThreadManager::SImplData
{
public:
    typedef map<SNetAddress, SClient*> SClientSet;
    typedef list<SClient*> SRemovedClientList;
    SClientSet mClientSet;
    SMutex mClientSetMutex;
    SRemovedClientList mRemovedClientList;
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
    case SEvent::DESTROY_CLIENT:
        {
            SEventWithData<SClient>* swd = (SEventWithData<SClient>*)event;
            SClient* client = swd->data;
            mImplData->mClientSet.erase(client->getNetAddress());
            //debug
            SResourceThreadManager::SImplData::SRemovedClientList::iterator it = mRemovedClientList.find(client);
            SASSERT(it == mRemovedClientList.end());
            //end
            mImplData->mRemovedClientList.push_back(client);   
            SEventWithData<SClient>* cData = new SEventWithData<SClient>(SEvent::DESTROY_CLIENT, client, false);
            SCommunicationThreadManager::getInstance()->postEvent(NULL, cData);
        }
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
    {
        SLog.msg("#### this client has been add to server ####\n");
        return;
    }
    char ipBuf[100];
    uint16_t port;
    event->getAddress().toString(ipBuf, 100, port);
    SLog::msg("### create new client ip = %s , port = %d ###\n", ipBuf, port);
    mImplData->mClientSet[event->getAddress()] = new SClient(event->getAddress(), event->getSocket());    
    SClient* sc = mImplData->mClientSet[event->getAddress()];
    sc->setState(SClient::CONNECTING);
    SEventWithData<SClient>* cData = new SEventWithData<SClient>(SEvent::CREATE_CLIENT, sc, false);
    SCommunicationThreadManager::getInstance()->postEvent(NULL, cData, SPostEvent::HIGH_PRIORITY);
    SEventWithData<SClient>* wData = new SEventWithData<SClient>(SEvent::CREATE_CLIENT, sc, false);
    SWorkingThreadManager::getInstance()->postEvent(NULL, wData, SPostEvent::HIGH_PRIORITY);
    delete event;
}
class RemoveClientFunctor
{
public:
    RemoveClientFunctor(list<SClient*>& l) : mRemoveList(l)
    {
    }
    void operator()(const SClient* client)
    {
        if(client->canRemove())
        {
            mRemoveList.push_back(client);
        }
    }
private:
    list<SClient*>& mRemoveList;
};
static bool whetherRemove(const SClient* client)
{
    return client->canRemove();
}
void SResourceThreadManager::processEvents()
{
    SActivityThread::processEvents();
    list<SClient*> canDeletedList;
    for_each(mImplData->mRemovedClientList.begin(), mImplData->mRemovedClientList.end(), RemovedClientFunctor(canDeletedList));
    mImplData->mRemovedClientList.remove_if(whetherRemove);
    list<SClient*>::iterator it;
    for(it = canDeletedList.begin() ; it != canDeletedList.end() ; it++)
    {
        SClient* client = *it;
        delete client;
    }

}
SClient* SResourceThreadManager::findClient(const SNetAddress& sa)
{
    SAutoMutex mutex(&mItemData->mClientSetMutex);
    SResourceThreadManager::SImplData::SClientSet::iterator it = mImplData->mClientSet.find(sa);
    if(it == mImplData->mClientSet.end())
        return NULL;
    else
        return it.second;
}
