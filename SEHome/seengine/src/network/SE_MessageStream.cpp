#include "SE_MessageStream.h"
//#include "SE_Mutex.h"
#include "SE_Utils.h"
#include "SE_Log.h"
#include "SE_Common.h"
#include <list>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//////////////////////////////////

//////////////////////////////////
class SE_NetMessageStream::SE_NetMessageStreamImpl
{
public:
    ~SE_NetMessageStreamImpl();
    typedef list<SE_NetMessagePacket*> SE_NetMessagePacketList;
    int getDataLen();
    void backupOffset(vector<int>& offsetBackup);
    void changeOffset(vector<int>& offsetBackup);
    SE_NetMessagePacketList mMessagePacketList;
    //SE_Mutex mMessagePacketListMutex;
};
SE_NetMessageStream::SE_NetMessageStreamImpl::~SE_NetMessageStreamImpl()
{
    //SE_AutoMutex mutex(&mMessagePacketListMutex);
    SE_NetMessagePacketList::iterator it;
    for(it = mMessagePacketList.begin() ; it != mMessagePacketList.end() ; it++)
    {
        delete *it;
    } 
}
int SE_NetMessageStream::SE_NetMessageStreamImpl::getDataLen()
{
    SE_NetMessagePacket* front = mMessagePacketList.front();
    unsigned char* dataStart = front->mData + front->offset;
    SE_ASSERT(front->offset < front->len);
    uint16_t dataLen = 0;
    int leftDataLen = front->len - front->offset;
    if(leftDataLen >= 3)
    {
        memcpy(&dataLen, dataStart + 1, 2);
        dataLen = SE_Util::net2HostInt16(dataLen);
        return dataLen;
    }
    else
    {
        unsigned char headerData[3];
        unsigned char* p = headerData;
        int currentLen = front->len - front->offset;
        memcpy(headerData, dataStart, currentLen);
        leftDataLen = 3 - (front->len - front->offset);
        p += currentLen;
        SE_NetMessagePacketList::iterator it = mMessagePacketList.begin();
        it++;
        for(; it != mMessagePacketList.end() ; it++)
        {
            SE_NetMessagePacket* mp = *it;
            SE_ASSERT(mp->offset < mp->len);
            currentLen = mp->len - mp->offset;
            if(leftDataLen <= currentLen)
            {
                memcpy(p, mp->mData + mp->offset, leftDataLen);
                leftDataLen = 0;
                break;
            }
            else
            {
                memcpy(p, mp->mData + mp->offset, currentLen);
                leftDataLen -= currentLen;
                p += currentLen;
            }
        }
        if(leftDataLen == 0)
        {
            memcpy(&dataLen, headerData + 1, 2);
            dataLen = SE_Util::net2HostInt16(dataLen);
        }
        return dataLen;
    }

}
void SE_NetMessageStream::SE_NetMessageStreamImpl::backupOffset(vector<int>& offsetBackup)
{
    SE_NetMessagePacketList::iterator it;
    int i;
    for(it = mMessagePacketList.begin(), i = 0; it != mMessagePacketList.end(); it++, i++)
    {
        offsetBackup[i] = (*it)->offset;
    }
}
void SE_NetMessageStream::SE_NetMessageStreamImpl::changeOffset(vector<int>& offsetBackup)
{
    SE_NetMessagePacketList::iterator it;
    int i;
    for(it = mMessagePacketList.begin(), i = 0; it != mMessagePacketList.end(); it++, i++)
    {
        (*it)->offset = offsetBackup[i];
    }

}
//////////////////////////////////////
static bool canDelete(const SE_NetMessagePacket* mp)
{
    return mp->isAllConsumed();
}
///////////////////////////////////////
SE_NetMessageStream::SE_NetMessageStream() : mImpl(new SE_NetMessageStreamImpl)
{
    
}
SE_NetMessageStream::~SE_NetMessageStream()
{}
int SE_NetMessageStream::getNextMessage(SE_NetMessage* out)
{
    int ret = SE_WAIT_MORE;
    //SE_AutoMutex mutex(&mImpl->mMessagePacketListMutex);
    if(mImpl->mMessagePacketList.empty())
        return ret;
    uint16_t dataLen = mImpl->getDataLen();
    if(dataLen == 0)
    {
        ret = SE_WAIT_MORE;
        return ret;
    }
    uint8_t* outData = new uint8_t[dataLen];
    uint8_t* outDataHead = outData;
    vector<int> offsetBackup(mImpl->mMessagePacketList.size(), 0);
    mImpl->backupOffset(offsetBackup);
    SE_NetMessageStreamImpl::SE_NetMessagePacketList::iterator it = mImpl->mMessagePacketList.begin();
    int leftDataLen = dataLen;
    unsigned char* dataStart = NULL;
    int i = 0;
    for(; it != mImpl->mMessagePacketList.end(); it++, i++)
    {
        SE_NetMessagePacket* mp = *it;
        int currentLen = mp->len - mp->offset;
        dataStart = mp->mData + mp->offset;
        if(currentLen >= leftDataLen)
        {
            memcpy(outData, dataStart, leftDataLen);
            //mp->offset += leftDataLen;
            offsetBackup[i] += leftDataLen;
            leftDataLen = 0;
            break;
        }
        else
        {
            memcpy(outData, dataStart, currentLen);
            leftDataLen -= currentLen;
            //mp->offset += currentLen;
            offsetBackup[i] += currentLen;
            outData += currentLen;
        }
    }
    if(leftDataLen == 0)
    {
        ret = SE_NO_ERROR;
        out->data = outDataHead;
        out->len = dataLen;
        mImpl->changeOffset(offsetBackup);
    }
    else
    {
        ret = SE_WAIT_MORE;
    }
    //delete message packet
    SE_NetMessageStreamImpl::SE_NetMessagePacketList deleteMessagePackets;
    for(it = mImpl->mMessagePacketList.begin(); it != mImpl->mMessagePacketList.end(); it++)
    {
        SE_NetMessagePacket* data = *it;
        if(data->isAllConsumed())
        {
            deleteMessagePackets.push_back(data);
        }
    }
    mImpl->mMessagePacketList.remove_if(canDelete);
    for(it = deleteMessagePackets.begin() ; it != deleteMessagePackets.end(); it++)
    {
        SE_NetMessagePacket* data = *it;
        delete data;
    }
    //end
    return ret;
}
int SE_NetMessageStream::addMessagePacket(unsigned char* data, int len, bool own)
{
    SE_ASSERT(data != NULL && len > 0);
    SE_NetMessagePacket* newPacket = new SE_NetMessagePacket;
    newPacket->mOwn = own;
    if(!own)
    {
        newPacket->mData = new unsigned char[len];
        memcpy(newPacket->mData, data, len);
    }
    else
    {
        newPacket->mData = data;
    }
    newPacket->len = len;
    //mImpl->mMessagePacketListMutex.lock();
    //SE_AutoMutex mutex(&mImpl->mMessagePacketListMutex);
    mImpl->mMessagePacketList.push_back(newPacket);
    //mImpl->mMessagePacketListMutex.unlock();
    return SE_NO_ERROR;
}
int SE_NetMessageStream::getMessagePacketCount()
{
    int count = 0;
    //mImpl->mMessagePacketListMutex.lock();
    //SE_AutoMutex mutex(&mImpl->mMessagePacketListMutex);
    count = mImpl->mMessagePacketList.size();
    //mImpl->mMessagePacketListMutex.unlock();
    return count;
}
void SE_NetMessageStream::mapMessagePacket(SE_NetMessagePacketFunctor& functor, bool clearPacketList)
{
    //SLog::msg("map lock start\n");
    //SE_AutoMutex mutex(&mImpl->mMessagePacketListMutex);
    //SLog::msg("map lock end\n");
    SE_NetMessageStreamImpl::SE_NetMessagePacketList::iterator it;
    for(it = mImpl->mMessagePacketList.begin() ; it != mImpl->mMessagePacketList.end(); it++)
    {
        SE_NetMessagePacket* smp = *it;
        //SLog::msg("### output packe %p ####\n", smp);
        functor.handleMessagePacket(smp);
    }
    if(clearPacketList)
    {
        for(it = mImpl->mMessagePacketList.begin() ; it != mImpl->mMessagePacketList.end(); it++)
        {
            SE_NetMessagePacket* smp = *it;
            delete smp;
        }
        mImpl->mMessagePacketList.clear();
    }
}
