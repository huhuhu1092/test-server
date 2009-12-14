#include "SWorkingThreadManager.h"
#include "SCommandEvent.h"
#include "SClient.h"
#include "SEvent.h"
#include <list>
using namespace std;
class SWorkingThreadManager::SImplData
{
public:
    typedef list<SClient*> SClientList;
    SClientList mClientList;
};
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
            SEventWithData<SClient>* e = (SEventWithData<SClient>*)event;
            mImplData->mClientList.push_back(e->data);
            delete event;
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
    return event->handle();    
}
SWorkingThreadManager::SWorkingThreadManager() : mImplData(new SImplData)
{}
SWorkingThreadManager::~SWorkingThreadManager()
{
    delete mImplData;
}
