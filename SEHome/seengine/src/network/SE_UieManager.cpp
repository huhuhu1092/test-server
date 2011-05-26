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
		SE_UieClientReadThread* uiReadClient = new SE_UieClientReadThread(clientProp);
		SE_UieClientWriteThread* uiWriteClient = new SE_UieClientWriteThread(clientProp);
		uiReadClient->setWriteThread(uiWriteClient);
		SE_ThreadManager* threadManager = SE_GET_THREADMANAGER();
		threadManager->add(uiReadClient);
		threadManager->add(uiWriteClient);
		uiReadClient->start();
		uiWriteClient->start();
	}
}
///////
SE_UieClientReadThread::SE_UieClientReadThread(const SE_ClientProp& clientProp)
{
	mClientProp = clientProp;
}
SE_UieClientReadThread::~SE_UieClientReadThread()
{
	mClientProp.socket.close();
}
void SE_UieClientReadThread::setWriteThread(SE_UieClientWriteThread* t)
{
	mWriteThread = t;
}
void SE_UieClientReadThread::run()
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
				LOGI("## recive message ###\n");
			    SE_NetDataCommand* netDataCommand = new SE_NetDataCommand(SE_Application::getInstance(), recvMessage);
				SE_Application::getInstance()->postCommand(netDataCommand);
			}
		}
		else if(n == 0)
		{
			LOGI("#### connect close ###\n");
			break;
		}
	}
	LOGI("## end of read thread ##\n");
	mClientProp.socket.close();
	char buf[3];
	buf[0] = 0;
	
	mWriteThread->addOutputMessage()
}
///////////
SE_UieClientWriteThread::SE_UieClientWriteThread(const SE_ClientProp& clientProp)
{
	mExit = false;
	mOutputStreamCond.setMutex(&mOutputStreamMutex);
}
SE_UieClientWriteThread::~SE_UieClientWriteThread()
{}
void SE_UieClientWriteThread::addOutputMessage(int len, char* data, bool own)
{
	mOutputStreamMutex.lock();
	mOutputStream.addMessagePacket(data, len);
	mOutputStreamCond.signal();
	mOutputStreamMutex.unlock();
}
void SE_UieClientWriteThread::run()
{
	while(!mExit)
	{

	    SE_NetMessage sendMessage;
		mOutputStreamMutex.lock();
        if(mOutputStream.getNextMessage(&sendMessage) == SE_NetMessageStream::SE_NO_ERROR)
	    {
			char i = sendMessage.data[0];
			if(i == 0)
			{
				mExit = true;
			}
			else
		        mClientProp.socket.send(sendMessage.data, sendMessage.len);
	    }
		else
		{
            mOutputStreamCond.wait();
		}
		mOutputStreamMutex.unlock();
	}
	LOGI("### end of write thread ###\n");
}