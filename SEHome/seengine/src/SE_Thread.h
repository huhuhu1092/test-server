#ifndef SE_THREAD_H
#define SE_THREAD_H
#include <pthread.h>
#include "SE_Common.h"
#include "SE_Mutex.h"
#include "SE_TreeStruct.h"
#include <string>
typedef void* (*THREADFUNC)(void*);
class SE_Thread : public SE_ListStruct<SE_Thread>
{
    friend class _ThreadCommand;
public:
    enum PRIORITY {LOW, NORMAL, HIGHT};
    SE_Thread(bool deleteAfterEnd = true);
    virtual ~SE_Thread();
    pthread_t id() const
    {
        return mID;
    }
	void setName(const std::string& name)
	{
		mName = name;
	}
	std::string getName() const
	{
		return mName;
	}
    void start();
    static pthread_t currentThread();
    static bool isEqual(pthread_t left, pthread_t right);
    bool isEnd();
    void reset();
protected:
    virtual void run() = 0;
    virtual void threadLoop();
private:
    static void createThread(THREADFUNC f, void* args);
    static void* threadFun(void *);
private:
    SE_DECLARE_NONECOPY(SE_Thread);
private:
    pthread_t mID;
    bool mIsThreadRunEnd;
    SE_Mutex mEndStateMutex;
    const bool mIsDeleteAfterEnd;
	std::string mName;
};
#endif
