#ifndef SEVENT_H
#define SEVENT_H
class SObject;
class SEvent;
class SPostEvent
{
public:
    enum {HIGH_PRIORITY, NORMAL_PRIORITY, LOW_PRIORITY};
    SPostEvent(SObject* r, SEvent* e, int priority)
    {
        receiver = r;
        event = e;
        this->priority = priority;
    }
    SPostEvent() : receiver(0), event(0), priority(0)
    {}
    SObject* receiver;
    SEvent* event;
    int priority;
};
class SEvent
{
public:
    enum Type {
        CREATE_CLIENT,
        LOG_IN,
        QUIT,
        EXIT_THREAD_LOOP,
        DESTROY_CLIENT,
        User = 1000,
        MaxUser = 65535
    };
    SEvent(Type t);
    virtual ~SEvent();
    Type type()
    {
        return mType;
    }    
private:
    Type mType;

};
#endif
