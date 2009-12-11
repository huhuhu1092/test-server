#ifndef SACTIVITYTHREAD_H
#define SACTIVITYTHREAD_H
#include "SObject.h"
#include "SMutex.h"
#include "SEvent.h"
#include <list>
using namespace std;
class SActivityThread : public SObject
{
public:
    enum RET_CODE {NO_ERROR, PROCESS_EVENT_ERROR};
    SActivityThread(const char* name = 0);
    virtual ~SActivityThread();
    //if receiver is NULL, this event will be handled by    // SActivityThread
    void postEvent(SObject* receiver, SEvent* event, int priority = SPostEvent::NORMAL_PRIORITY);
    // the event handler has responsibility to delete event
    bool sendEvent(SObject* receiver, SEvent* event);
    int exec();
    void setCanPostEvent(bool v);
    bool getCanPostEvent();
    virtual bool event(SEvent*);
    virtual bool eventFilter(SObject* , SEvent* );
protected:
    virtual void processEvents();
protected:
    typedef list<SPostEvent*> SPostEventList;
    SPostEventList mPostEventList;
    SMutex mPostEventListMutex;
    bool mCanPostEvent;
    SMutex mCanPostEventMutex;
    bool mExit;
    int mRetCode;
protected:
    void releasePostEventList(SPostEventList& pl);
};
#endif
