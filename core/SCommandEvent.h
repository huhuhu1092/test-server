#ifndef SCOMMANDEVENT_H
#define SCOMMANDEVENT_H
#include <string>
#include "SEvent.h"
using namespace std;
class SCommandEvent : public SEvent
{
public:
    SCommandEvent() : SEvent(SEvent::Command)
    {}
    virtual ~SCommandEvent() {}
    virtual bool handle() = 0;
};
#endif
