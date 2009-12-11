#ifndef SOBJECT_H
#define SOBJECT_H
#include <string>
#include <list>
using namespace std;
class SEvent;
class SObject
{
public:
    SObject(const char* name = 0);
    virtual ~SObject();
    virtual bool eventFilter(SObject* , SEvent* );
    virtual bool event(SEvent* );
    void installEventFilter(SObject *);
    void removeEventFilter(SObject *);
protected:
    string mName;
    typedef list<SObject*> SEventFilterList;
    SEventFilterList mEventFilterList;
};
#endif
