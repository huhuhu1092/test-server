#include "SCommunicationThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SResourceThreadManager.h"
#include "SThread.h"
#include "SNetAddress.h"
#include "SCommandEventFactoryImpl.h"
#include "SLog.h"
class SCommunicationThread : public SThread
{
public:
    SCommunicationThread();
    void readyToRun();
    void threadLoop();
private:
    SCommunicationThreadManager* mCTM;
};
SCommunicationThread::SCommunicationThread()
{
    mCTM = SCommunicationThreadManager::getInstance();
}
void SCommunicationThread::readyToRun()
{

}
void SCommunicationThread::threadLoop()
{
    int ret = mCTM->exec();

}
/////////////////
class SResourceThread : public SThread
{
public:
    SResourceThread();
    void readyToRun();
    void threadLoop();
private:
    SResourceThreadManager* mRMT;
};
SResourceThread::SResourceThread()
{
    mRMT = SResourceThreadManager::getInstance();
    SCommandEventFactory* factory = new SCommandEventFactoryImpl();
    mRMT->startup(factory);
}
void SResourceThread::readyToRun()
{

}
void SResourceThread::threadLoop()
{
    int ret = mRMT->exec();

}

//////////////////////////////////
class SWorkingThread : public SThread
{
public:
    SWorkingThread();
    void readyToRun();
    void threadLoop();
private:
    SWorkingThreadManager* mSTM;
};
SWorkingThread::SWorkingThread()
{
    mSTM = SWorkingThreadManager::getInstance();
}
void SWorkingThread::readyToRun()
{

}
void SWorkingThread::threadLoop()
{
    int ret = mSTM->exec();

}
///////////////////////////////////
static SCommunicationThread* sCommunicationThread;
static SResourceThread* sResourceThread;
static SWorkingThread* sWorkingThread;
///////////////////////////////////
int main(int argc, char** argv)
{
    sCommunicationThread = new SCommunicationThread();
    sResourceThread = new SResourceThread();
    sWorkingThread = new SWorkingThread();
    sCommunicationThread->run();
    sResourceThread->run();
    sWorkingThread->run();
    SNetAddress serverAddress((const char*)NULL, SUtil::Host2NetInt16(10000));
    SSocketServer ss(STREAM, serverAddress);
    if(ss.getError() != SSocketServer::NO_ERROR)
        return -1;
    while(true)
    {
        SClientProp cp = ss.accept();
        if(ss.getError() != SSocketServer::NO_ERROR)
        {
            SLog::msg("## accept error ###\n");
            continue;
        }
        SEvent* e = new SCreateClientEvent(cp.address, cp.socket);
        SResourceThreadManager::getInstance()->postEvent(NULL, e, SPostEvent::HIGH_PRIORITY);
    }

}
