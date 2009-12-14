#include "SClient.h"
#include "SMessageStream.h"
#include "SCommandEventFactory.h"
#include "SCommandEvent.h"
#include "SResourceThreadManager.h"
#include "SWorkingThreadManager.h"
static const int BUFSIZE = 256 * 1024;
SClient::SClient(const SNetAddress& address, const SSocket& s) : mAddress(address), mSocket(s)
{}
void SClient::process()
{
    SMessage m;
    int ret;
    while((ret = mInputStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        SCommandEvent* event = SResourceThreadManager::getInstance()->create(m.data[0], m.data); 
        event->setData(this);
        SWorkingThreadManager::getInstance()->postEvent(NULL, event);
        m.release();
    }
    
}
bool SClient::canRemove()
{
    SAutoMutex mutex(&mCanRemoveMutex);
    return mCanRemove;
}
void SClient::readData()
{
    unsigned char buffer[BUFSIZE];
    int readNum = mSocket.read(buffer, BUFSIZE);
    if(readNum > 0)
    {
        mInputStream.addMessagePacket(buffer, readNum);
    }
}
void SClient::writeData()
{
    SMessage m;
    int ret;
    while((ret = mOutputStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        mSocket.send(m.data, m.len);
        m.release();
    }
}
