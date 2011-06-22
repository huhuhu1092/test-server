#ifndef SE_COMMANDQUEUE_H
#define SE_COMMANDQUEUE_H
#include "SE_Mutex.h"
#include <list>
class SE_Command;
class SE_CommandQueue
{
public:
    enum TYPE {THREAD_SAFE, THREAD_UNSAFE};	
    SE_CommandQueue(TYPE t);
	bool enqueue(SE_Command* c);
	void process(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
	void setCanAddCommand(bool b);
	bool canAddCommand();
private:
	std::list<SE_Command*> mCommandQueue;
	SE_Mutex mCommandQueueMutex;
	bool mCanAddCommand;
	SE_Mutex mCanAddCommandMutex;
	const TYPE mType;
};
#endif
