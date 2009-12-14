#include "SWorkdingThreadManager.h"
SWorkingThreadManager* SWorkdingThreadManager::instance = NULL;
SWorkdingThreadManager::SWorkdingThreadManager* getInstance()
{
    if(instance == NULL)
    {  
        instance = new SWorkingThreadManager;
    }
    return instance;
}
bool SWorkdingThreadManager::event(SEvent* event) 
{
    switch(event->type())
    {
    case SEvent::Command:
	    return processCommandEvent((SCommandEvent*)event);
    default:
	    break;
    }
    return false;
}
bool SWorkdingThreadManager::eventFilter(SObject* recerver, SEvent* event)
{
    return false;
}
bool SWorkdingThreadManager::processCommandEvent(SCommandEvent* event)
{
    if(event == NULL)
	return false;
    return event->handle();    
}
