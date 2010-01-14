#include "SOutputThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SEvent.h"
#include "SClient.h"
SOutputThreadManager* SOutputThreadManager::instance = NULL;
SOutputThreadManager* SOutputThreadManager::getInstance()
{
    if(instance == NULL)
        instance = new SOutputThreadManager;
    return instance;
}
SOutputThreadManager::SOutputThreadManager()
{}
static void _addClient(SClient* client, SClientList& clientList)
{
    SClientList::iterator it = find(clientList.begin(), clientList.end(), client);
    if(it != clientList.end())
        return;
    clientList.push_back(client);
}
bool SOutputThreadManager::event(SEvent* e)
{
    switch(e->type())
    {
    case SEvent::OUTPUT_DATA:
        {
            SOutputDataEvent* event = (SOutputDataEvent*)e;
            SClient* client = event->client;
            //if(SWorkingThreadManager::getInstance()->isClientValid(client, event->address, event->clientCreateTime))
            {
                SSocket* out = client->getSocket();
                int len = out->send((unsigned char *)event->data, event->len);
                SOutputDataResponse* e = new SOutputDataResponse();
                if(len != event->len)
                {
                    e->error = SOutputDataResponse::ERROR;
                }
                else
                {
                    e->error = SOutputDataResponse::OK;
                }
                SWorkingThreadManager::getInstance()->postEvent(NULL, e);
            }
            delete event;
            return true;
        }
    case SEvent::REMOVE_CLIENT:
        {
            SRemoveClientEvent* removeClientEvent = (SRemoveClientEvent*)e;
            SRemoveClientEvent* response = new SRemoveClientEvent;
            response->client = removeClientEvent->client;
            response->address = removeClientEvent->address;
            response->createTime = removeClientEvent->createTime;
            SWorkingThreadManager::getInstance()->postEvent(NULL, response, SPostEvent::LOW_PRIORITY); 
            delete removeClientEvent;
            return true;
        }
    default:
        return false;
    }
    return false;
}
bool SOutputThreadManager::eventFilter(SObject*, SEvent*)
{
    return false;
}
void SOutputThreadManager::processEvents()
{
    mSem.v();
    SActivityThread::processEvents(); 
}
void SOutputThreadManager::sendOutput(SOutputDataEvent* e)
{
    SActivityThread::postEvent(NULL, e);
    mSem.p(); 
}
