#include "SOutputThreadManager.h"
SOutputThreadManager* SOutputThreadManager::instance = NULL;
SOutputThreadManager* SOutputThreadManager::getInstance()
{
    if(instance == NULL)
        instance = new SOutputThreadManager;
    return instance;
}
SOutputThreadManager::SOutputThreadManager()
{}
bool SOutputThreadManager::event(SEvent*)
{
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
