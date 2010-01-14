#ifndef SMAINSERVER_H
#define SMAINSERVER_H
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

class SMainServer
{
public:
    static SMainServer* getInstance();
    void startup();
    SThreadId getOutputThreadId();
    SThreadId getWorkingThreadId();
    SThreadId getInputThreadId(); 
private:
    SMainServer();
private:
    static SMainServer* mInstance;
    SCommunicationThread* mCommunicationThread;
    SWorkingThread* mWorkingThread;
    SOutputThread* mOutputThread;
    
};
#endif
