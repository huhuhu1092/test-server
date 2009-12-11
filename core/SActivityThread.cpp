#include "SActivityThread.h"
#include <algorithm>
#include <functional>
SActivityThread::SActivityThread(const char* name) : SObject(name)
{
    mCanPostEvent = true;
    mExit = false;
    mRetCode = 0;
}
SActivityThread::~SActivityThread()
{
    mPostEventListMutex.lock();
    try
    {
        releasePostEventList(mPostEventList);
    } 
    catch(...)
    {
    }

    mPostEventListMutex.unlock();
}
class isPriorityLessFunObj
{
public:
    isPriorityLessFunObj(int pri) : priority(pri)
    {}
    bool operator()(const SPostEvent* it)
    {
        if(it->priority < priority)
            return true;
        else
            return false;
    } 
private:
    int priority;
};
void SActivityThread::postEvent(SObject* receiver, SEvent* event, int priority)
{
    bool canPost = true;
    mCanPostEventMutex.lock();
    canPost = mCanPostEvent;
    mCanPostEventMutex.unlock();
    if(!canPost)
        return;
    if(event == NULL)
        return;
    if(receiver == NULL)
        receiver = this;
    SPostEvent* pe = new SPostEvent(receiver, event, priority);
    SAutoMutex mutex(&mPostEventListMutex);
    if(mPostEventList.empty())
    {
        mPostEventList.push_back(pe);
    }
    else
    {
        SPostEventList::iterator it = find_if(mPostEventList.begin(), mPostEventList.end(), isPriorityLessFunObj(priority));
        if(it != mPostEventList.end())
        {
            mPostEventList.insert(it, pe);
        } 
        else
        {
            mPostEventList.push_back(pe);
        }
    }
}
bool SActivityThread::sendEvent(SObject* receiver, SEvent* event)
{
    if(event == NULL)
        return false;
    if(receiver == NULL)
        receiver = NULL;
    receiver->event(event);
}
int SActivityThread::exec()
{
    while(!mExit)
    {
        processEvents();
    }
    return mRetCode;
}
void SActivityThread::processEvents()
{
    SPostEventList postEventList;
    bool copyOk = true;;
    mPostEventListMutex.lock();
    try
    {
        postEventList = mPostEventList;
        mPostEventList.clear();
    }
    catch(...)
    {
        copyOk = false;
    }
    mPostEventListMutex.unlock();
    if(!copyOk)
    {
        mExit = true;
        mRetCode = PROCESS_EVENT_ERROR;
        return;
    }
    for(SPostEventList::iterator it = postEventList.begin(); 
        it != postEventList.end(); it++)
    {
        SPostEvent* pe = *it;
        SEvent* e = pe->event;
        SObject* r = pe->receiver;
        sendEvent(r, e);
    } 
    releasePostEventList(postEventList);
}
void SActivityThread::setCanPostEvent(bool v)
{
    SAutoMutex mutex(&mCanPostEventMutex);
    mCanPostEvent = v;
}
bool SActivityThread::getCanPostEvent()
{
    bool canPost = true;
    mCanPostEventMutex.lock();
    canPost = mCanPostEvent;
    mCanPostEventMutex.unlock();
    return canPost;
}

void SActivityThread::releasePostEventList(SPostEventList& pl)
{
    SPostEventList::iterator it;
    for(it = pl.begin(); it != pl.end() ; it++)
    {
        SPostEvent* pe = *it;
        delete pe;
    }

}
bool SActivityThread::eventFilter(SObject* receiver, SEvent* event)
{
    return false;
}
bool SActivityThread::event(SEvent* event)
{
    return false;
}

