#include "SClientManager.h"
#include "SMutex.h"
#include "SClient.h"
#include "SNetAddress.h"
#include "SCommandEvent.h"
#include "SCommandEventFactory.h"
#include "SUtil.h"
#include <map>
#include <list>

///////////////////////////////////
class SClientManager::Impl
{
public:
    void removeClient(const SNetAddress&);
    void processRequest();
    void processResponse();
    typedef map<SNetAddress, SClient*> ClientSet;
    ClientSet mClientSet;
    SMutex mClientSetMutex;
    SMessageStream mCommandStream;
    SMessageStream mResponseStream;
    typedef list<SClient*> RemovedClientList;
    RemovedClientList mRemovedClientList;
    auto_ptr<SCommandEventFactory> mCommandFactory;
    SClientManager* mParent;
};
void SClientManager::Impl::removeClient(const SNetAddress& address)
{
    SAutoMutex mutex(&mClientSetMutex);
    SClientManager::Impl::ClientSet::iterator it = mClientSet.find(address);
    SClient* c = NULL;
    if(it != mClientSet.end())
    {
        c = it->second;
        mClientSet.erase(it);
    }
    if(c != NULL)
    {
        mRemovedClientList.push_back(c);
    }
}
void SClientManager::Impl::processRequest()
{
    int ret = SMessageStream::NO_ERROR;
    SMessage m;
    while((ret = mCommandStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        int commandId = m.data[0];
        switch(commandId)
        {
        case DELETE_CLIENT:
            {
                SASSERT(m.data[1] == 9);
                uint32_t ip;
                memcpy(&ip, &m.data[3], 4);
                uint16_t port;
                memcpy(&port, &m.data[7], 2);
                SNetAddress sa(ip, port);
                removeClient(sa);
                break;
            }
        case SEND_MSG:
            {
                break;
            }
        default:
            {
                SCommandEvent* command = mCommandFactory->create(commandId, m.data);
                if(command)
                    command->handle();
            }
            break;
        }    
        m.release();
    }
}
void SClientManager::Impl::processResponse()
{

}
///////////////////////////////////
bool SAddressEqual::isSatisfied(const void* con) const
{
    const SNetAddress* data = (const SNetAddress*)con;
    if(*data == mAddress)
       return true;
    else
       return false; 
}
//////////////////////////////////////
SClientManager* SClientManager::mInstance = NULL;
SClientManager* SClientManager::getInstance()
{
    if(mInstance == NULL)
        mInstance = new SClientManager;
    return mInstance;
}
SClientManager::SClientManager() : mImpl(new Impl)
{
    mImpl->mParent = this;
}
void SClientManager::init(SCommandEventFactory* factory)
{
    mImpl->mCommandFactory.reset(factory);
}
SClient* SClientManager::addClient(const SNetAddress& address, const SSocket& s)
{
    SAutoMutex mutex(&mImpl->mClientSetMutex);
    SClientManager::Impl::ClientSet::iterator it = mImpl->mClientSet.find(address);
    if(it != mImpl->mClientSet.end())
        return it->second;
    mImpl->mClientSet[address] = new SClient(address, s);    
    return mImpl->mClientSet[address];
}

void SClientManager::handle(SDoMapCondition* condition, SDoMapFunction* handler) const
{
    SAutoMutex mutex(&mImpl->mClientSetMutex);
    SClientManager::Impl::ClientSet::iterator it;
    for(it = mImpl->mClientSet.begin() ; it != mImpl->mClientSet.end() ; it++)
    {
        if(condition == NULL)
        {
            handler->handle(&it->second);
        }
        else if(condition->isSatisfied(&it->first))
        {
            handler->handle(&it->second);
        }
    }
}

void SClientManager::process()
{
    mImpl->processRequest();
    mImpl->processResponse();
    SClientManager::Impl::RemovedClientList::iterator it ;
    for(it = mImpl->mRemovedClientList.begin() ; it != mImpl->mRemovedClientList.end() ; it++)
    {
        SClient* sc = *it;
        if(sc->canRemove())
        {
            delete sc;
        }
    }
}
