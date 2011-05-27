#include "SE_UieManager.h"
#include "SE_Application.h"
#include "SE_ThreadManager.h"
#include "SE_NetDataCommand.h"
#include "SE_Socket.h"
#include "SE_Log.h"
#include "SE_Utils.h"
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
		SE_AddToThreadManager* c = new SE_AddToThreadManager(SE_Application::getInstance(), uiReadClient, uiWriteClient);
		SE_Application::getInstance()->postCommand(c);
	}
}
///////
SE_UieClientReadThread::SE_UieClientReadThread(const SE_ClientProp& clientProp)
{
	mClientProp = clientProp;
	setName("uiereadthread");
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
		LOGI("### before read ###\n");
		int n = mClientProp.socket.readRaw(buf, 1024);
		LOGI("### end read : %d ####\n", n);
		if(n > 0)
		{
			LOGI("## read num = %d ##\n", n);
			LOGI("## first byte = %d ##\n", buf[0]);
			mInputStream.addMessagePacket(buf, n, false);
			SE_NetMessage* recvMessage = new SE_NetMessage;
			while(mInputStream.getNextMessage(recvMessage) == SE_NetMessageStream::SE_NO_ERROR)
			{
				LOGI("## recieve message ###\n");
			    SE_NetDataCommand* netDataCommand = new SE_NetDataCommand(SE_Application::getInstance(), recvMessage);
				SE_Application::getInstance()->postCommand(netDataCommand);
			}
			//mClientProp.socket.send(buf, n);
		}
		else if(n == 0)
		{
			LOGI("#### connect close ###\n");
			break;
		}
	}
	LOGI("## end of read thread ##\n");
	mWriteThread->addOutputMessage(0, NULL, 0);//this is exit command
}
///////////
SE_UieClientWriteThread::SE_UieClientWriteThread(const SE_ClientProp& clientProp)
{
	mExit = false;
	mClientProp = clientProp;
	mOutputStreamCond.setMutex(&mOutputStreamMutex);
	setName("uiewritethread");
}
SE_UieClientWriteThread::~SE_UieClientWriteThread()
{}
void SE_UieClientWriteThread::addOutputMessage(unsigned char opID, unsigned char* data, int dataLen)
{
	if(data == NULL)
	{
		dataLen = 0;
	}
	int size = 3 + dataLen;
	unsigned char* buffer = new unsigned char[size];
	buffer[0] = opID;
	short s = SE_Util::host2NetInt16(size);
    memcpy(buffer + 1, &s, 2);
	if(dataLen > 0)
	{
		memcpy(buffer + 3, data, dataLen);
	}
	mOutputStreamMutex.lock();
	mOutputStream.addMessagePacket(buffer, size);
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
			int i = sendMessage.data[0];
			if(i == 0)
			{
				mExit = true;
			}
			else
			{
				LOGI("### write thread write len = %d ###\n", sendMessage.len);
		        mClientProp.socket.send(sendMessage.data, sendMessage.len);
				LOGI("### end write data ###\n");
			}
	    }
		else
		{
            mOutputStreamCond.wait();
		}
		mOutputStreamMutex.unlock();
	}
	LOGI("### end of write thread ###\n");
}
void SE_AddToThreadManager::handle(SE_TimeMS realDelta, SE_TimeMS simulateDelta)
{
	SE_ThreadManager* threadManager = SE_GET_THREADMANAGER();
	threadManager->add(mReadThread);
	threadManager->add(mWriteThread);
	mReadThread->start();
	mWriteThread->start();
}