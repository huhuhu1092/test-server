#include "SClient.h"
#include "SMessageStream.h"
#include "SCommandEventFactory.h"
#include "SCommandEvent.h"
static const int BUFSIZE = 256 * 1024;
SClient::SClient(const SNetAddress& address, const SSocket& s) : mAddress(address), mSocket(s)
{}
void SClient::process()
{
    SMessage m;
    int ret;
    while((ret = mInputStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        SCommandEvent* event = SResourceManagerThread::getInstance()->create(m.data[0], m.data); 
        SWorkingThreadManager::postEvent(NULL, event);
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
    int readNum = mSocket->read(buffer, BUFSIZE);
    if(readNum > 0)
    {
        mInputStream.addMessagePacket(buffer, readNum);
    }
}
void SClient::writeData()
{
    SMessage m;
    int ret;
    while((ret = mOutStream.getNextMessage(&m)) == SMessageStream::NO_ERROR)
    {
        mSocket->send(m.data, m.len);
        m.release();
    }
}
