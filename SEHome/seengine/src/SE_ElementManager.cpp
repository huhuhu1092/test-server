#include "SE_ElementManager.h"
#include "SE_Element.h"
#include "SE_Utils.h"
#include <algorithm>
SE_ElementID SE_ElementManager::addElement(const SE_ElementID& parent, SE_Element* e, bool linkToParent)
{
    if(e == NULL)
        return SE_ElementID::INVALID;
    return mElements.add(parent, e, linkToParent);
}
SE_Element* SE_ElementManager::findElement(const SE_ElementID& id)
{
    if(id == SE_ElementID::INVALID)
        return NULL;
    return mElements.find(id);
}
void SE_ElementManager::addElement(SE_Element* parent, SE_Element* e)
{
	mElements.add(parent, e);
	//parent->addChild(e);
}
SE_Element* SE_ElementManager::removeElement(const SE_ElementID& id)
{
    if(id == SE_ElementID::INVALID)
        return NULL;
    return mElements.remove(id);
}
SE_Element* SE_ElementManager::getParent(const SE_ElementID& id)
{
    if(id == SE_ElementID::INVALID)
        return NULL;
    return mElements.getParent(id);
}
std::vector<SE_Element*> SE_ElementManager::getChildren(const SE_ElementID& id)
{
    return mElements.getChildren(id);
}
void SE_ElementManager::releaseElement(SE_Element* element, int delay)
{
    mElements.release(element, delay);
}
void SE_ElementManager::releaseElement(const SE_ElementID& id, int delay)
{
    mElements.release(id, delay);
}
void SE_ElementManager::addEvent(SE_ElementEvent* event)
{
    if(!event->isNeedMerge())
    {
        mElementEventList.push_back(event);
    }
    else
    {
        std::list<SE_ElementEvent*>::iterator it;
        bool merged = false;
        for(it = mElementEventList.begin() ; it != mElementEventList.end() ; it++)
        {
            SE_ElementEvent* e = *it;
            if(e->getType() == event->getType())
            {
                bool b = event->merge(e);
                if(b)
                    merged = b;
            }
             
        }
        if(merged)
            delete event;
        else
            mElementEventList.push_back(event);
    }
}
void SE_ElementManager::update()
{
    std::list<SE_ElementEvent*> retList = mElementEventList;
    mElementEventList.clear();
    std::list<SE_ElementEvent*>::iterator it;
    for(it = retList.begin(); it != retList.end() ; it++)
    {
        SE_ElementEvent* e = *it;
        e->run();
    }
    for_each(retList.begin(), retList.end(), SE_DeleteObject());
}