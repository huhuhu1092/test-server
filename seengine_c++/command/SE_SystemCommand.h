#ifndef SE_SYSTEMCOMMAND_H
#define SE_SYSTEMCOMMAND_H
#include "SE_Command.h"
#include <string>
class SE_Application;
class SE_InitAppCommand : public SE_Command
{
public:
    SE_InitAppCommand(SE_Application* app);
    ~SE_InitAppCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    std::string dataPath;
    std::string fileName; // the prefix of the file
};
class SE_UpdateCameraCommand : public SE_Command
{
public:
    SE_UpdateCameraCommand(SE_Application* app);
    ~SE_UpdateCameraCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
public:
    int width;
    int height;
};
#endif
