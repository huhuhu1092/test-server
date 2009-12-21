#include "SClient.h"
#include "SMessageStream.h"
#include "SCommandEventFactory.h"
#include "SCommandEvent.h"
#include "SResourceThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SLog.h"
#include "SClientConnectionState.h"
static const int BUFSIZE = 256 * 1024;
SClient::SClient(const SNetAddress& address, const SSocket& s) : mAddress(address), mSocket(s)
{
    mCurrentConnectionState = new SClientExitedState(this);
    mState = EXITED;
    mCanRemove = true;
}
SClient::~SClient()
{
    delete mCurrentConnectionState;
}
void SClient::processMessageFromClient()
{
    SMessage m;
    int ret;
    while((ret = mInputStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        SCommandEvent* event = SResourceThreadManager::getInstance()->create(m.data[0], m.data); 
        //event->setData(this);
        event->setClientID(mAddress);
        SLog::msg("##### read event: %d ####\n", event->type());
        SWorkingThreadManager::getInstance()->postEvent(NULL, event);
        m.release();
    }
    
}
void SClient::setCanRemove(bool r)
{
    mCanRemoveMutex.lock();
    mCanRemove = r;
    mCanRemoveMutex.unlock();
}
bool SClient::canRemove()
{
    bool canRemove;
    mCanRemoveMutex.lock();
    canRemove = mCanRemove;
    mCanRemoveMutex.unlock();
    return mState == EXITED && canRemove;
}
void SClient::readData()
{
    unsigned char buffer[BUFSIZE];
    int readNum = mSocket.read(buffer, BUFSIZE);
    if(readNum > 0)
    {
        SLog::msg("#### read num = %d ###\n", readNum);
        mInputStream.addMessagePacket(buffer, readNum);
    }
    else if(readNum == 0)
    {
        STATE currentState = getState();
        if(currentState == EXITING)
            return;
        char ip[100];
        uint16_t port;
        mAddress.toString(ip, port);
        SLog::msg("#### client %s, %d disconnected ####", ip, port);
        SEventWithData<SClient>* event = new SEventWithData<Client>(SEvent::DESTROY_CLIENT, this, false);
        setState(SClient::EXITING);
        SResourceManagerThread::postEvent(NULL, event);
    }
}
void SClient::writeData()
{
    SMessage m;
    int ret;
    while((ret = mOutputStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        SLog::msg("### out put len = %d ###\n", m.len);
        mSocket.send(m.data, m.len);
        m.release();
    }
}
bool SClient::connectionStateTransition(SClientConnectionState* conState)
{
    if(mCurrentConnectionState)
    {
        delete mCurrentConnectionState;
    }
    mCurrentConnectionState = conState;
}
bool SClient::canHandleEvent(SEvent* se)
{
    return true; 
}
