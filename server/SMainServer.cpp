#include "SCommunicationThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SResourceThreadManager.h"
#include "SThread.h"
#include "SNetAddress.h"
#include "SCommandEventFactoryImpl.h"
#include "SOutputThreadManager.h"
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
/*
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
*/
///////////////////////////////////
class SOutputThread : public SThread
{
public:
    SOutputThread();
    void readyToRun();
    void threadLoop();
private:
    SOutputThreadManager* mOTM;
};
SOutputThread::SOutputThread()
{
    mOTM = SOutputThreadManager::getInstance();
}
void SOutputThread::readyToRun()
{

}
void SOutputThread::threadLoop()
{
    int ret = mOTM->exec();
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
    SCommandEventFactory* factory = new SCommandEventFactoryImpl();
    mSTM->setCommandEventFactory(factory);

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
//static SResourceThread* sResourceThread;
static SWorkingThread* sWorkingThread;
static SOutputThread* sOutputThread;
///////////////////////////////////
int main(int argc, char** argv)
{
    sWorkingThread = new SWorkingThread();
    sCommunicationThread = new SCommunicationThread();
    sOutputThread = new SOutputThread();
    //sResourceThread = new SResourceThread();
    sOutputThread->run();
    sWorkingThread->run();
    sCommunicationThread->run();
    //sResourceThread->run();
    SNetAddress serverAddress((const char*)NULL, SUtil::Host2NetInt16(10000));
    SSocketServer ss(STREAM, serverAddress);
    if(ss.getError() != S_NO_ERROR)
    {
        SLog::msg("#### server init error : %d ####\n", ss.getError());
        return -1;
    }
    while(true)
    {
        SClientProp cp = ss.accept();
        SLog::msg("## new client ##\n");
        if(ss.getError() != S_NO_ERROR)
        {
            SLog::msg("## accept error ###\n");
            continue;
        }
        SEvent* e = new SCreateClientEvent(cp.address, cp.socket);
        SWorkingThreadManager::getInstance()->postEvent(NULL, e, SPostEvent::HIGH_PRIORITY);
    }

}
