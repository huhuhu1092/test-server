#include "SCommunicationThreadManager.h"
#include "SType.h"
#include "SSocket.h"
#include "SClient.h"
#include "SLog.h"
#include "SEvent.h"
#include "SCommandEvent.h"
#include "SWorkingThreadManager.h"
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
    bool ret = SWorkingThreadManager::getInstance()->getClientList(mClientList);
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        if(!isClientInRemovingList(sc))
            sc->readData();
    }  
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        if(!isClientInRemovingList(sc))
            sc->processMessageFromClient();
    }  
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        if(!isClientInRemovingList(sc))
            sc->writeData();
    }  


}
void SCommunicationThreadManager::clearBuffer()
{
    memset(mBuffer, 0, mBufferSize);
}
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
