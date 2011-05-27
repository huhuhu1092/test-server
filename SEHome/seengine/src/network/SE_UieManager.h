#ifndef SE_UIEMANAGER_H
#define SE_UIEMANAGER_H
#include "SE_Thread.h"
#include "SE_MessageStream.h"
#include "SE_Socket.h"
#include "SE_Mutex.h"
#include "SE_Command.h"
class SE_NetAddress;
class SE_SocketServer;
class SE_UieAcceptThread : public SE_Thread
{
public:
    SE_UieAcceptThread(const SE_NetAddress& address);
	~SE_UieAcceptThread();
protected:
    void run();
private:
    SE_SocketServer* mServer;
};
class SE_UieClientWriteThread;
class SE_UieClientReadThread : public SE_Thread
{
public:
    SE_UieClientReadThread(const SE_ClientProp& clientProp);
	~SE_UieClientReadThread();
	void setWriteThread(SE_UieClientWriteThread* t);
protected:
    void run();
private:
    SE_ClientProp mClientProp;
	SE_NetMessageStream mInputStream;
	SE_UieClientWriteThread* mWriteThread;
};
class SE_UieClientWriteThread : public SE_Thread
{
public:
	SE_UieClientWriteThread(const SE_ClientProp& clientProp);
	~SE_UieClientWriteThread();
    void addOutputMessage(unsigned char opID, unsigned char* data, int dataLen);
protected:
    void run();
private:
	SE_ClientProp mClientProp;
	SE_NetMessageStream mOutputStream;
	SE_Mutex mOutputStreamMutex;
	SE_MutexCondition mOutputStreamCond;
	bool mExit;
};
class SE_AddToThreadManager : public SE_Command
{
public:
	SE_AddToThreadManager(SE_Application* app, SE_Thread* readThread, SE_Thread* writeThread) : SE_Command(app)
	{
		mReadThread = readThread;
		mWriteThread = writeThread;
	}
	void handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta);
private:
	SE_Thread* mReadThread;
	SE_Thread* mWriteThread;
};
#endif
