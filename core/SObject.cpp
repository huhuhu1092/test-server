#include "SObject.h"
SObject::SObject(const char* name) : mName(name)
{

}
SObject::~SObject()
{
}
bool SObject::eventFilter(SObject* receiver, SEvent* event)
{
    return false;
}
bool SObject::event(SEvent* event)
{
    return false;
}
void SObject::installEventFilter(SObject *ef)
{
    mEventFilterList.push_back(ef);
}
void SObject::removeEventFilter(SObject *ef)
{
    mEventFilterList.remove(ef);
}

