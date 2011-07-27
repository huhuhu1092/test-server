#include "base/Reference.h"
#include "base/Log.h"
namespace oms
{
	RefBase::RefBase(bool threadSafe) : mRefCount(0) , mMutex(NULL)
	{
		if(threadSafe)
		{
			mMutex = new Mutex;
		}
	}
	void RefBase::incStrong(void* id)
	{
		if(mMutex)
			mMutex->lock();
		++mRefCount;
		if(mMutex)
			mMutex->unlock();
	}
	void RefBase::decStrong(void* id)
	{
		if(mMutex)
			mMutex->lock();
		--mRefCount;
		if(mMutex)
			mMutex->unlock();
		if(mRefCount <= 0)
            delete this;
	}
    RefBase::~RefBase()
    {
		if(mMutex)
			delete mMutex;
		if(mRefCount > 0)
		{
			LOGI("## error: delete object which ref count > 0 ##\n");
		}
	}
}
