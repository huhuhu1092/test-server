#ifndef SE_UIEMANAGER_H
#define SE_UIEMANAGER_H
#include "SE_Thread.h"
#include "SE_MessageStream.h"
#include "SE_Socket.h"
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
    SE_UieClientThread(const SE_ClientProp& clientProp);
	~SE_UieClientThread();
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
    void addOutputMessage(int len, char* data, bool own);
protected:
    void run();
private:
	SE_ClientProp mClientProp;
	SE_NetMessageStream mOutputStream;
	SE_Mutex mOutputStreamMutex;
	SE_Condition mOutputStreamCond;
	bool mExit;
};
#endif
