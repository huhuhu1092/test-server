#include "SWorkingThreadManager.h"
#include "SCommandEvent.h"
#include "SClient.h"
#include "SEvent.h"
#include "SLog.h"
#include "SCommandEventFactory.h"
#include "STime.h"
#include "SCommunicationThreadManager.h"
#include <list>
#include <map>
#include <memory>
using namespace std;
typedef map<SNetAddress, SClient*> SClientMap;
class SClientMapFunctorBase
{
public:
    virtual void handle(SClient* client, const SNetAddress& address) {}
};
class SRemoveClientFunctor : public SClientMapFunctorBase
{
public:
    SRemoveClientFunctor(SClientMap& cm) : clients(cm)
    {}
    void handle(SClient* client, const SNetAddress& address)
    {
        clients.erase(address);
    }
    SClientMap& clients;
};
class SWorkingThreadManager::SImplData
{
public:
    ~SImplData();
    bool createNewClient(SCreateClientEvent* event);
    bool destroyClient(SDestroyClientEvent* event);
    bool mapClient(SClient* client, SNetAddress& clientAddress, SClientMapFunctorBase& handler);
public:
    SClientMap mClients;
    SMutex mClientsMutex;
    SClientList mRemovedClientList;
    auto_ptr<SCommandEventFactory> mCommandEventFactory;
};
SWorkingThreadManager::SImplData::~SImplData()
{}
bool SWorkingThreadManager::SImplData::mapClient(SClient* client, SNetAddress& clientAddress, SClientMapFunctorBase& handler)
{
    bool found = false;
    mClientsMutex.lock();
    try
    {
        SClientMap::iterator it;
        for(it = mClients.begin() ; it != mClients.end() ; it++)
        {
            if(it->second == client)
            {
                clientAddress = client->getNetAddress();
                found = true;
                break;
            }
        }
        if(found)
        {
            handler.handle(client, clientAddress);
        }
    }
    catch(...)
    {
        SLog::msg("### destroy client exception ####\n");
    }
    mClientsMutex.unlock();
    return found;
}
bool SWorkingThreadManager::SImplData::destroyClient(SDestroyClientEvent* event)
{
    SClient* client = event->mClient;
    SClientList::iterator listIt = find(mRemovedClientList.begin(), mRemovedClientList.end(), client);
    if(listIt != mRemovedClientList.end())
        return false;
    SNetAddress na;
    SRemoveClientFunctor removeClientFunctor(mClients);
    bool found = mapClient(client, na, removeClientFunctor);
    if(found)
    {
        mRemovedClientList.push_back(client);
        client->setState(SClient::EXITED);
    }
    SRemoveClientEvent* rcEvent = new SRemoveClientEvent();
    rcEvent->client = client;
    rcEvent->address = client->getNetAddress();
    rcEvent->createTime = client->getCreateTime();
    SCommunicationThreadManager::getInstance()->postEvent(NULL, rcEvent);
    delete event;
    return true;
}
bool SWorkingThreadManager::SImplData::createNewClient(SCreateClientEvent* event)
{
    SASSERT(event);
    SAutoMutex mutex(&mClientsMutex);
    SClientMap::iterator it = mClients.find(event->getAddress());
    if(it != mClients.end())
        return false;
    ///// debug
    char ipBuf[100];
    uint16_t port;
    event->getAddress().toString(ipBuf, 100, port);
    SLog::msg("### create new client ip = %s , port = %d ###\n", ipBuf, port);
    ////end
    mClients[event->getAddress()] = new SClient(event->getAddress(), event->getSocket(), STime::getCurrentTimeMS());
    SClient* client = mClients[event->getAddress()];
    client->setState(SClient::CONNECTED);
    delete event;
    return true;
}
////////////////////////////////////////////
SWorkingThreadManager* SWorkingThreadManager::instance = NULL;
SWorkingThreadManager* SWorkingThreadManager::getInstance()
{
    if(instance == NULL)
    {  
        instance = new SWorkingThreadManager;
    }
    return instance;
}
bool SWorkingThreadManager::event(SEvent* event) 
{
    switch(event->type())
    {
    case SEvent::CREATE_CLIENT:
        {
            SLog::msg("### create client in SWorkingThreadManager ####\n");
            return mImplData->createNewClient((SCreateClientEvent*)event);
        }
    case SEvent::DESTROY_CLIENT:
        {
            SLog::msg("### destroy client ####\n");
            return mImplData->destroyClient((SDestroyClientEvent*)event);
        }
    case SEvent::NEW_INCOMING_DATA:
	{
	    SEventWithData<SClient>* e = (SEventWithData<SClient>*)event;
	    SClient* client =  e->data;
	    client->processMessageFromClient();
	    delete e;
	    return true; 
	}
    case SEvent::Command:
	    return processCommandEvent((SCommandEvent*)event);
    default:
	    break;
    }
    return false;
}
bool SWorkingThreadManager::eventFilter(SObject* recerver, SEvent* event)
{
    return false;
}
bool SWorkingThreadManager::processCommandEvent(SCommandEvent* event)
{
    if(event == NULL)
	    return false;
    bool ret = event->handle();    
    if(event->canDelete())
    {
        delete event;
    }
    return ret;
}
SWorkingThreadManager::SWorkingThreadManager() : mImplData(new SImplData)
{}
SWorkingThreadManager::~SWorkingThreadManager()
{
    delete mImplData;
}
class GetRemoveClientFunctor
{
public:
    GetRemoveClientFunctor(list<const SClient*>& l) : mRemoveList(l)
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
    list<const SClient*>& mRemoveList;
};
static bool whetherRemove(const SClient* client)
{
    return client->canRemove();
}
void SWorkingThreadManager::processEvents()
{
    SActivityThread::processEvents();
    list<const SClient*> canDeletedList;
    for_each(mImplData->mRemovedClientList.begin(), mImplData->mRemovedClientList.end(), GetRemoveClientFunctor(canDeletedList));
    mImplData->mRemovedClientList.remove_if(whetherRemove);
    list<const SClient*>::iterator it;
    for(it = canDeletedList.begin() ; it != canDeletedList.end() ; it++)
    {
        const SClient* client = *it;
        delete client;
    }

}
bool SWorkingThreadManager::getClientList(SClientList& clientList)
{
    bool ret = true;
    mImplData->mClientsMutex.lock();
    SClientMap::iterator it;
    try
    {
        for(it = mImplData->mClients.begin() ; it != mImplData->mClients.end() ; it++)
        {
            clientList.push_back(it->second);
        }
    }
    catch(...)
    {
        ret = false;
    }
    mImplData->mClientsMutex.unlock();
    return ret;
}
void SWorkingThreadManager::setCommandEventFactory(SCommandEventFactory* factory)
{
    mImplData->mCommandEventFactory.reset(factory);
}
SClient* SWorkingThreadManager::findClient(const SNetAddress& sa)
{
    SAutoMutex mutex(&mImplData->mClientsMutex);
    SClientMap::iterator it = mImplData->mClients.find(sa);
    if(it == mImplData->mClients.end())
        return NULL;
    else
        return it->second;
}
SCommandEvent* SWorkingThreadManager::create(int commandId, unsigned char* data)
{
    return mImplData->mCommandEventFactory->create(commandId, data);    
}

