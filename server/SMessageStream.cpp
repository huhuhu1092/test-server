#include "SMessageStream.h"
#include "SMutex.h"
#include "SUtil.h"
#include <list>
#include <vector>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
//////////////////////////////////
class SMessagePacket
{
public:
    SMessagePacket()
    {
        //mPrev = mNext = NULL;
        mData = NULL;
        len = 0;
        offset = 0;
    }
    //SMessagePacket* mPrev;
    //SMessagePacket* mNext;
    bool isAllConsumed() const
    {
        return offset == len;
    }
    unsigned char* mData;
    int len;
    int offset;
};

//////////////////////////////////
class SMessageStream::SMessageStreamImpl
{
public:
    typedef list<SMessagePacket*> SMessagePacketList;
    int getDataLen();
    void backupOffset(vector<int>& offsetBackup);
    void changeOffset(vector<int>& offsetBackup);
    SMessagePacketList mMessagePacketList;
    SMutex mMessagePacketListMutex;
};
int SMessageStream::SMessageStreamImpl::getDataLen()
{
    SMessagePacket* front = mMessagePacketList.front();
    unsigned char* dataStart = front->mData + front->offset;
    SASSERT(front->offset < front->len);
    uint16_t dataLen = 0;
    int leftDataLen = front->len - front->offset;
    if(leftDataLen >= 3)
    {
        memcpy(&dataLen, dataStart + 1, 2);
        dataLen = SUtil::Net2HostInt16(dataLen);
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
        SMessagePacketList::iterator it = mMessagePacketList.begin();
        it++;
        for(; it != mMessagePacketList.end() ; it++)
        {
            SMessagePacket* mp = *it;
            SASSERT(mp->offset < mp->len);
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
            dataLen = SUtil::Net2HostInt16(dataLen);
        }
        return dataLen;
    }

}
void SMessageStream::SMessageStreamImpl::backupOffset(vector<int>& offsetBackup)
{
    SMessagePacketList::iterator it;
    int i;
    for(it = mMessagePacketList.begin(), i = 0; it != mMessagePacketList.end(); it++, i++)
    {
        offsetBackup[i] = (*it)->offset;
    }
}
void SMessageStream::SMessageStreamImpl::changeOffset(vector<int>& offsetBackup)
{
    SMessagePacketList::iterator it;
    int i;
    for(it = mMessagePacketList.begin(), i = 0; it != mMessagePacketList.end(); it++, i++)
    {
        (*it)->offset = offsetBackup[i];
    }

}
//////////////////////////////////////
static bool canDelete(const SMessagePacket* mp)
{
    return mp->isAllConsumed();
}
///////////////////////////////////////
SMessageStream::SMessageStream() : mImpl(new SMessageStreamImpl)
{
    
}
SMessageStream::~SMessageStream()
{}
int SMessageStream::getNextMessage(SMessage* out)
{
    /*
    if(mHead == NULL)
        return WAIT_MORE;
    SMessagePacket* p = mHead;
    unsigned char* dataStart = p->mData + offset;
    if(dataStart == (p->mData + len))    
    {
        mHead = p->mNext;
    }
    */
    int ret = WAIT_MORE;
    if(mImpl->mMessagePacketList.empty())
        return ret;
//    mImpl->mMessagePacketListMutex.lock();
    SAutoMutex mutex(&mImpl->mMessagePacketListMutex);
    try
    {
        uint16_t dataLen = mImpl->getDataLen();
        if(dataLen == 0)
        {
            ret = WAIT_MORE;
            return ret;
        }
        uint8_t* outData = new uint8_t[dataLen];
        uint8_t* outDataHead = outData;
        vector<int> offsetBackup(mImpl->mMessagePacketList.size(), 0);
        mImpl->backupOffset(offsetBackup);
        SMessageStreamImpl::SMessagePacketList::iterator it = mImpl->mMessagePacketList.begin();
        int leftDataLen = dataLen;
        unsigned char* dataStart = NULL;
        int i = 0;
        for(; it != mImpl->mMessagePacketList.end(); it++, i++)
        {
            SMessagePacket* mp = *it;
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
            ret = NO_ERROR;
            out->data = outDataHead;
            out->len = dataLen;
            mImpl->changeOffset(offsetBackup);
        }
        else
        {
            ret = WAIT_MORE;
        }
        mImpl->mMessagePacketList.remove_if(canDelete);
    }
    catch(...)
    {

    }
    return ret;
}
int SMessageStream::addMessagePacket(unsigned char* data, int len)
{
    SMessagePacket* newPacket = new SMessagePacket;
    /*
    if(mTail == NULL)
    {
        mTail = newPacket;
        mHead = newPacket;
    }    
    else
    {
        newPacket->mPrev = mTail;
        mTail->mNext = newPacket;
        mTail = newPacket;
    }
    */
    newPacket->mData = new unsigned char[len];
    memcpy(newPacket->mData, data, len);
    newPacket->len = len;
    //mImpl->mMessagePacketListMutex.lock();
    SAutoMutex mutex(&mImpl->mMessagePacketListMutex);
    try
    {
        mImpl->mMessagePacketList.push_back(newPacket);
    }
    catch(...)
    {

    }
    //mImpl->mMessagePacketListMutex.unlock();
    return NO_ERROR;
}
int SMessageStream::getMessagePacketCount()
{
    int count = 0;
    //mImpl->mMessagePacketListMutex.lock();
    SAutoMutex mutex(&mImpl->mMessagePacketListMutex);
    count = mImpl->mMessagePacketList.size();
    //mImpl->mMessagePacketListMutex.unlock();
    return count;
}
