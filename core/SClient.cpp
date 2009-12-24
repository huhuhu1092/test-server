#include "SClient.h"
#include "SMessageStream.h"
#include "SCommandEventFactory.h"
#include "SCommandEvent.h"
#include "SResourceThreadManager.h"
#include "SWorkingThreadManager.h"
#include "SCommunicationThreadManager.h"
#include "SLog.h"
#include "SClientConnectionState.h"
static const int BUFSIZE = 256 * 1024;
SClient::SClient(const SNetAddress& address, const SSocket& s, const STimeMS& createTime) : mAddress(address), mSocket(s), mCreateTime(createTime)
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
    if(getState() == EXITED)
        return;
    SMessage m;
    int ret;
    while((ret = mInputStream.getNextMessage(&m)) == SMessageStream::S_NO_ERROR)
    {
        SCommandEvent* event = SWorkingThreadManager::getInstance()->create(m.data[0], m.data); 
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
bool SClient::canRemove() const
{
    bool canRemove;
    mCanRemoveMutex.lock();
    canRemove = mCanRemove;
    mCanRemoveMutex.unlock();
    return mState == EXITED && canRemove;
}
void SClient::readData()
{
    if(getState() == EXITED)
        return;
    unsigned char buffer[BUFSIZE];
    int readNum = mSocket.read(buffer, BUFSIZE);
    if(readNum > 0)
    {
        SLog::msg("#### read num = %d ###\n", readNum);
        mInputStream.addMessagePacket(buffer, readNum);
	SEventWithData<SClient>* e = new SEventWithData<SClient>(SEvent::NEW_INCOMING_DATA, this, false);
	SWorkingThreadManager::getInstance()->postEvent(NULL, e);
    }
    else if(readNum == 0)
    {
        SCommunicationThreadManager::getInstance()->addRemovedClientData(this, mCreateTime, mAddress);
        STATE currentState = getState();
        char ip[100];
        uint16_t port;
        mAddress.toString(ip, 100, port);
        SLog::msg("#### client %s, %d disconnected ####\n", ip, port);
        SDestroyClientEvent* event = new SDestroyClientEvent(this);
        SWorkingThreadManager::getInstance()->postEvent(NULL, event, SPostEvent::LOW_PRIORITY);
    }
}
class SOutputMessagePacket : public SMessagePacketFunctor
{
public:
    SOutputMessagePacket(SSocket& s) : ss(s)
    {}
    void handleMessagePacket(SMessagePacket* packet)
    {
        SLog::msg("### output len = %d ####\n", packet->len);
        ss.send(packet->mData, packet->len);
    }
    SSocket& ss;
};

void SClient::writeData()
{
    SOutputMessagePacket outputMessageFunctor(mSocket);
    mOutputStream.mapMessagePacket(outputMessageFunctor, true);
    /*
    SMessage m;
    int ret;
    while((ret = mOutputStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        SLog::msg("### out put len = %d ###\n", m.len);
        mSocket.send(m.data, m.len);
        m.release();
    }
    */
}
bool SClient::connectionStateTransition(SClientConnectionState* conState)
{
    if(mCurrentConnectionState)
    {
        delete mCurrentConnectionState;
    }
    mCurrentConnectionState = conState;
}
bool SClient::canHandleEvent(SEvent* se) const
{
    return true; 
}
