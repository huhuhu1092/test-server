#include "SE_CommandQueue.h"
#include "SE_Command.h"
SE_CommandQueue::SE_CommandQueue(TYPE t) : mType(t)
{
	mCanAddCommand = true;
}
bool SE_CommandQueue::enqueue(SE_Command* c)
{
	if(!canAddCommand())
		return false;
	if(mType == THREAD_SAFE)
	{
		SE_AutoMutex m(&mCommandQueueMutex);
		mCommandQueue.push_back(c);
	}
	else
	{
		mCommandQueue.push_back(c);
	}
	return true;
}
void SE_CommandQueue::process(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
    std::list<SE_Command*>::iterator it;
	std::list<SE_Command*> tmpList;
	if(mType == THREAD_SAFE)
	{
	    mCommandQueueMutex.lock();
	    tmpList = mCommandQueue;
        mCommandQueue.clear();
  	    mCommandQueueMutex.unlock();
	}
	else
	{
		tmpList = mCommandQueue;
		mCommandQueue.clear();
	}
    for(it = tmpList.begin(); it != tmpList.end(); )
    {
        SE_Command* c = *it;
        if(c->expire(realDelta, simulateDelta))
        {
            c->handle(realDelta, simulateDelta);
            delete c;
            tmpList.erase(it++);
        }
		else
		{
			it++;
		}
    }
    if(tmpList.empty())
        return;
	if(mType == THREAD_SAFE)
	{
	    mCommandQueueMutex.lock();
        mCommandQueue.splice(mCommandQueue.begin(), tmpList, tmpList.begin(), tmpList.end()); 
	    mCommandQueueMutex.unlock();
	}
	else
	{
		mCommandQueue.splice(mCommandQueue.begin(), tmpList, tmpList.begin(), tmpList.end());
	}
}
void SE_CommandQueue::setCanAddCommand(bool b)
{
	if(mType == THREAD_SAFE)
	{
		SE_AutoMutex m(&mCanAddCommandMutex);
		mCanAddCommand = b;
	}
	else
	{
		mCanAddCommand = b;
	}
}
bool SE_CommandQueue::canAddCommand()
{
	if(mType == THREAD_SAFE)
	{
		SE_AutoMutex m(&mCanAddCommandMutex);
		return mCanAddCommand;
	}
	else
	{
		return mCanAddCommand;
	}
}
