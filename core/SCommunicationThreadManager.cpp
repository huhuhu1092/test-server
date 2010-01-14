#include "SCommunicationThreadManager.h"
#include "SType.h"
#include "SSocket.h"
#include "SClient.h"
#include "SLog.h"
#include "SEvent.h"
#include "SCommandEvent.h"
#include "SWorkingThreadManager.h"
#include <vector>
using namespace std;
#if defined(WIN32)
#else
#include <poll.h>
#endif
SCommunicationThreadManager* SCommunicationThreadManager::instance = NULL;
SCommunicationThreadManager* SCommunicationThreadManager::getInstance()
{
    if(instance == NULL)
        instance = new SCommunicationThreadManager();
    return instance;

}
SCommunicationThreadManager::SCommunicationThreadManager(int bufferSize)
{
    mBufferSize = bufferSize;
    mBuffer = new unsigned char[bufferSize]; 
}
SCommunicationThreadManager::~SCommunicationThreadManager()
{
    delete[] mBuffer;
}
void SCommunicationThreadManager::processEvents()
{
    SActivityThread::processEvents();
    SClientList::iterator it;
    mClientList.clear();
    //SLog::msg("### before get Clients ####\n");
    bool ret = SWorkingThreadManager::getInstance()->getClientList(mClientList);
    //struct pollfd* readClientPollList = (struct pollfd*)malloc(sizeof(struct pollfd) * mClientList.size());
    //vector<SClient*> clients(mClientList.size());
    //SLog::msg("### current connection num = %d #####\n", mClientList.size());
    //int maxi = 0;
    //int i = 0;
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        if(!isClientInRemovingList(sc))
        {
            //readClientPollList[i].fd = sc->mSocket.getSocket();
            //readClientPollList[i].events = POLLRDNORM;
            //clients[i] = sc;
            //i++;
            //maxi++;
            sc->readData();
        }
    }
    /* 
    poll(readClientPollList, maxi, INFTIM);
    for(i = 0 ; i < maxi ; i++)
    {
        if(readClientPollList[i].revents & (POLLRDNORM | POLLERR))
        {
            clients[i]->readData();
        }
    } 
    */
    //SLog::msg("### start write #####\n");
    /*
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        if(!isClientInRemovingList(sc))
        {
            //SLog::msg("### start write client %p #####\n", sc);
            sc->writeData();
        }
    } 
   */ 
    //free(readClientPollList);
}
void SCommunicationThreadManager::clearBuffer()
{
    memset(mBuffer, 0, mBufferSize);
}
class RemoveClientFunctor
{
public:
private:
};
bool SCommunicationThreadManager::event(SEvent* event)
{
    switch(event->type())
    {
    case SEvent::CREATE_CLIENT:
        {
            /*
            SEventWithData<SClient>* e = (SEventWithData<SClient>*)event;
            SLog::msg("### new client in SCommunicationThreadManager::event @@@\n");
            mClientList.push_back(e->data);
            e->data->setState(SClient::CONNECTED);
            delete event;
            */
            return true;
        }
    case SEvent::DESTROY_CLIENT:
        {
            /*
            SEventWithData<SClient>* e = (SEventWithData<SClient>*)event;
            SLog::msg("### remove client in SCommunicationThreadManager::event ####\n");
            mClientList.remove(e->data);
            e->data->setState(SClient::EXITED);
            delete e;
            */
            return true;
        }
    case SEvent::REMOVE_CLIENT:
	{
	    SRemoveClientEvent* rcEvent = (SRemoveClientEvent*)event;
	    ClientData cd;
	    cd.client = rcEvent->client;
	    cd.clientAddress = rcEvent->address;
	    cd.clientCreateTime = rcEvent->createTime;
        mRemovingClientDataList.remove(cd);
        delete rcEvent;
	    return true;
	}
    case SEvent::Command:
        {
            SCommandEvent* ce = (SCommandEvent*)event;
            bool ret = ce->handle();
            if(ce->canDelete())
            {
                delete ce;
            }
            return true;
        }
    }
    return false;
}
bool SCommunicationThreadManager::eventFilter(SObject* r, SEvent* event)
{}
void SCommunicationThreadManager::addRemovedClientData(SClient* client, STimeMS createTime, SNetAddress address)
{
    ClientData cd;
    cd.client = client;
    cd.clientAddress = address;
    cd.clientCreateTime = createTime;
    SClientDataList::iterator it = find(mRemovingClientDataList.begin(), mRemovingClientDataList.end(), cd);
    if(it != mRemovingClientDataList.end())
        return;
    mRemovingClientDataList.push_back(cd);
}
bool SCommunicationThreadManager::isClientInRemovingList(SClient* client)
{
    SClientDataList::iterator it;
    for(it = mRemovingClientDataList.begin() ; it != mRemovingClientDataList.end() ; it++)
    {
	ClientData cd;
	cd.client = client;
	cd.clientAddress = client->getNetAddress();
	cd.clientCreateTime = client->getCreateTime();
        if(*it == cd)
	    return true;
    }
    return false;
}
