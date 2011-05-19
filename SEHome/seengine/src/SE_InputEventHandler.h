#ifndef SE_INPUTEVENTHANDLER_H
#define SE_INPUTEVENTHANDLER_H
class SE_MotionEvent;
class SE_Element;
class SE_MotionEventHandler
{
public:
    virtual ~SE_MotionEventHandler() {}
    virtual bool handle(const SE_MotionEvent& event, SE_Element* element) = 0;
};
class SE_ElementClickHandler
{
public:
    virtual ~SE_ElementClickHandler() {}
    virtual bool handle(SE_Element* element);
};
#endif
