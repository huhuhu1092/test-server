#ifndef SE_LOADTHREAD_H
#define SE_LOADTHREAD_H
#include "SE_Thread.h"
#include "SE_Command.h"
#include <string>
class SE_ResourceManager;
class SE_Application;
class SE_LoadThread : public SE_Thread
{
public:
    SE_LoadThread(SE_ResourceManager* rs, const std::string& f);
protected:
    void run();
private:
    SE_ResourceManager* resourceManager;
    std::string fileName;
};
class SE_ResourceReadyCommand : public SE_Command
{
public:
    SE_ResourceReadyCommand(SE_Application* app);
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
};
#endif
