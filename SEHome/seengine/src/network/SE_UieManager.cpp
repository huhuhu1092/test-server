#include "SE_UieManager.h"
#include "SE_Application.h"
#include "SE_ThreadManager.h"
#include "SE_NetDataCommand.h"
#include "SE_Socket.h"
#include "SE_Log.h"
SE_UieAcceptThread::SE_UieAcceptThread(const SE_NetAddress& address)
{
	mServer = new SE_SocketServer(SE_STREAM, address);
	setName("acceptthread");
}
SE_UieAcceptThread::~SE_UieAcceptThread()
{
	if(mServer)
	{
		delete mServer;
	}
}
void SE_UieAcceptThread::run()
{
	if(mServer->getError() != SE_NO_ERROR)
	{
		LOGI("#### server accept socket error ####\n");
		return;
	}
	while(true)
	{
		SE_ClientProp clientProp = mServer->accept();
		SE_UieClientThread* uiClient = new SE_UieClientThread(clientProp);
		SE_ThreadManager* threadManager = SE_GET_THREADMANAGER();
		threadManager->add(uiClient);
		uiClient->start();
	}
}
///////
SE_UieClientThread::SE_UieClientThread(const SE_ClientProp& clientProp)
{
	mClientProp = clientProp;
}
SE_UieClientThread::~SE_UieClientThread()
{
	mClientProp.socket.close();
}
void SE_UieClientThread::run()
{
	unsigned char buf[1024];
	while(true)
	{
		memset(buf, 0, 1024);
		int n = mClientProp.socket.read(buf, 1024);
		if(n > 0)
		{
			LOGI("## read num = %d ##\n", n);
			LOGI("## first byte = %d ##\n", buf[0]);
			mInputStream.addMessagePacket(buf, n, false);
			SE_NetMessage* recvMessage = new SE_NetMessage;
			while(mInputStream.getNextMessage(recvMessage) == SE_NetMessageStream::SE_NO_ERROR)
			{
				LOGI("## recive message $##\n");
			    SE_NetDataCommand* netDataCommand = new SE_NetDataCommand(SE_Application::getInstance(), recvMessage);
				SE_Application::getInstance()->postCommand(netDataCommand);
			}
		}
		SE_NetMessage sendMessage;
		while(mOutputStream.getNextMessage(&sendMessage) == SE_NetMessageStream::SE_NO_ERROR)
		{
			mClientProp.socket.send(sendMessage.data, sendMessage.len);
		}
	}
}
void SE_UieClientThread::addOutputMessage(int len, char* data, bool own)
{
	
}
