#include "SCommunicationThreadManager.h"
#include "SType.h"
#include "SSocket.h"
#include "SClient.h"
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
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        sc->readData();
    }  
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
        sc->process();
    }  
    for(it = mClientList.begin() ; it != mClientList.end() ; it++)
    {
        SClient* sc = *it;
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
            SEventWithData<SClient>* e = (SEventWithData<SClient>*)event;
            mClientList.push_back(e->data);
            delete event;
        }
    }
}
bool SCommunicationThreadManager::eventFilter(SObject* r, SEvent* event)
{}

