#include "SE_ElementManager.h"
SE_ElementID SE_ElementManager::addElement(const SE_ElementID& parent, SE_Element* e)
{
    if(e == NULL)
        return SE_ElementID::INVALID;
    return mElements.add(parent, e);
}
SE_Element* SE_ElementManager::findElement(const SE_ElementID& id)
{
    if(id == SE_ElementID::INVALID)
        return NULL;
    return mElements.find(id);
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
std::vector<SE_Element*> SE_ElementManager::getChildren(const SE_ElementID& id) const
{
    return mElements.getParent(id);
}
void SE_ElementManager::releaseElement(SE_Element* element, int delay = SE_TreeStructManager<SE_Element>::RELEASE_DELAY)
{
    mElements.release(element, delay);
}
void SE_ElementManager::releaseElement(const SE_ElementID& id, int delay = SE_TreeStructManager<SE_Element>::RELEASE_DELAY)
{
    mElements.release(element, delay);
}

