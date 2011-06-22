#ifndef SE_NETDATACOMMAND_H
#define SE_NETDATACOMMAND_H
#include "SE_Command.h"
class SE_NetMessage;
class SE_NetDataCommand : public SE_Command
{
public:
	SE_NetDataCommand(SE_Application* app, SE_NetMessage* msg);
	~SE_NetDataCommand();
    void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
    SE_NetMessage* mMsg;	
};
#endif
