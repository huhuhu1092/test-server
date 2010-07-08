#ifndef SE_COMMAND_H
#define SE_COMMAND_H
#include "SE_Common.h"
#include "SE_Time.h"
typedef SE_CommonID SE_CommandID;
class SE_Command
{
public:
    SE_Command(SE_Application* app, SE_TimeMS waittime = 0, bool realTime = true) : mApp(app), mWaitTime(waitTime), mbRealTime(realTime)
    {
    }
    virtual ~SE_Command();
    virtual void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta) = 0;
    virtual bool expire(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
    SE_Application* mApp;
    SE_TimeMS mWaitTime;
    bool mbRealTime;
};
#endif
