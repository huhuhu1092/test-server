#include "SResourceThreadManager.h"
SResourceThreadManager* SResourceThreadManager::instance = NULL;
SResourceThreadManager* SResourceThreadManager::getInstance()
{
    if(instance == NULL)
        instance = new SResourceThreadManager;
    return instance;
}
bool SResourceThreadManager::event(SEvent*)
{
    return false;
}
bool SResourceThreadManager::eventFilter(SObject*, SEvent*)
{
    return false;
}
