#include "SE_SimObjectManager.h"
void SE_SimObjectManager::set(const SE_SimObjectID& simObjectID, SE_SimObject* simObject)
{
    mSimObjectManager.set(simObjectID, simObject);
}
SE_SimObject* SE_SimObjectManager::get(const SE_SimObjectID& simObjectID)
{
    return mSimObjectManager.get(simObjectID);
}
void SE_SimObjectManager::remove(const SE_SimObjectID& simObjectID)
{
    mSimObjectManager.remove(simObjectID);
}
