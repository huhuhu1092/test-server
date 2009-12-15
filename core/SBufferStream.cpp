#include "SBufferStream.h"
#include "SUtil.h"
#include "SLog.h"
#include <cstring>
using namespace std;
SBufferStream::SBufferStream(char* data, int len, bool readOnly, bool own, bool netOrder)
{
    SASSERT(data != 0 && len > 0);
    mBuffer = data;
    mLen = len;
    mOwn = own;
    //mWriteOffset = 0;
    //mReadOffset = 0;
    mNetOrder = netOrder;
    mOffset = 0;
    mReadOnly = readOnly;
}
SBufferStream::SBufferStream(int len, bool readOnly, bool netOrder)
{
    SASSERT(len > 0);
    mLen = len;
    mBuffer = new char[mLen];
    mOwn = true;
    mNetOrder = netOrder;
    mOffset = 0;
    mReadOnly = readOnly;
    //mWriteOffset = 0;
    //mReadOffset = 0;
}
SBufferStream::~SBufferStream()
{
    if(mOwn)
        delete[] mBuffer;
}
const char* SBufferStream::getBuffer()
{
    return mBuffer;
}
bool SBufferStream::writeChar(char c)
{
    if(mReadOnly)
        return false;
    if(mOffset + 1 > mLen)
        return false;
    memcpy(mBuffer + mOffset, &c, 1);
    mOffset++;
    return true;
}
bool SBufferStream::writeString(const char* str)
{
    if(mReadOnly)
        return false;
    int strLen = strlen(str);
    int strHeaderLen = sizeof(int);
    if(mOffset + strLen + strHeaderLen > mLen)
        return false;
    if(mNetOrder)
    {
        int netStrLen = SUtil::Host2NetInt32(strLen);
        memcpy(mBuffer + mOffset, &netStrLen, sizeof(int));
    }
    else
        memcpy(mBuffer + mOffset, &strLen, sizeof(int));
    memcpy(mBuffer + mOffset + sizeof(int), str, strLen);
    mOffset += (strLen + strHeaderLen);
    return true;
}
bool SBufferStream::writeInt(int i)
{
    if(mReadOnly)
        return false;
    
    if(mOffset + sizeof(int) > mLen)
        return false;
    if(mNetOrder)
    {
        int netI = SUtil::Host2NetInt32(i);
        memcpy(mBuffer + mOffset, &netI, sizeof(int));
    }
    else
        memcpy(mBuffer + mOffset, &i, sizeof(int));
    mOffset += sizeof(int);
    return true;
}
bool SBufferStream::writeShort(short int i)
{
    if(mReadOnly)
        return false;

    if(mOffset + sizeof(short int) > mLen)
        return false;
    if(mNetOrder)
    {
        short int netI = SUtil::Host2NetInt16(i);
        memcpy(mBuffer + mOffset, &netI, sizeof(short int));
    }
    else
        memcpy(mBuffer + mOffset, &i, sizeof(short int));
    mOffset += sizeof(short int);
    return true;
}
bool SBufferStream::writeFloat(float f)
{
    if(mReadOnly)
        return false;

    if(mOffset + sizeof(float) > mLen)
        return false;
    if(mNetOrder)
    {
        int floatI = SUtil::Host2NetInt32(*reinterpret_cast<long *>(&f));
        memcpy(mBuffer + mOffset, &floatI, sizeof(int));
    }
    memcpy(mBuffer + mOffset, &f, sizeof(float));
    mOffset += sizeof(float);
    return true;
}
bool SBufferStream::writeDouble(double d)
{}
bool SBufferStream::writeNetAddress(const SNetAddress& na)
{

}
bool SBufferStream::readChar(char& c)
{    
    if(!mReadOnly)
        return false;

    if(mOffset + 1 > mLen)
        return false;
    memcpy(&c, mBuffer + mOffset, 1);
    mOffset += 1;
    return true;
}
bool SBufferStream::readString(char* str, int& len)
{
    if(!mReadOnly)
        return false;
    if(mOffset + sizeof(int) > mLen)
        return false;
    if(mNetOrder)
    {
        int netStrLen;
        memcpy(&netStrLen, mBuffer + mOffset, sizeof(int));
        int strLen = SUtil::Net2HostInt32(netStrLen);
        if(mOffset + sizeof(int) + strLen > mLen)
            return false;
        str = new char[strLen];
        memcpy(str, mBuffer + mOffset + sizeof(int), strLen);
        len = strLen;
        mOffset += sizeof(int) + strLen;
        return true;
    }
    else
    {
        memcpy(&len, mBuffer + mOffset, sizeof(int));
        if(mOffset + sizeof(int) + len > mLen)
            return false;
        str = new char[len];
        memcpy(str, mBuffer + mOffset + sizeof(int), len);
        mOffset += sizeof(int) + len;
        return true;
    }
}
bool SBufferStream::readInt(int& i)
{
    if(!mReadOnly)
        return false;
    if(mOffset + sizeof(int) > mLen)
        return false;
    if(mNetOrder)
    {
        int netI;
        memcpy(&netI, mBuffer + mOffset, sizeof(int));
        i = SUtil::Net2HostInt32(netI);
    }
    else
        memcpy(&i, mBuffer + mOffset, sizeof(int));
    mOffset += sizeof(int);
    return true;
}
bool SBufferStream::readShort(short int& i)
{
    if(!mReadOnly)
        return false;
    if(mOffset + sizeof(short int) > mLen)
        return false;
    if(mNetOrder)
    {
        short int netI;
        memcpy(&netI, mBuffer + mOffset, sizeof(short int));
        i = SUtil::Net2HostInt16(netI);
    }
    else
        memcpy(&i, mBuffer + mOffset, sizeof(short int));
    mOffset += sizeof(short int);
    return true;

}
bool SBufferStream::readFloat(float& f)
{
    if(!mReadOnly)
        return false;
    if(mOffset + sizeof(float) > mLen)
        return false;
    if(mNetOrder)
    {
        int intF;
        memcpy(&intF, mBuffer + mOffset, sizeof(float)); 
        intF = SUtil::Net2HostInt32(intF);
        f = *((float*)&intF);
    }
    else
    {
        int intF;
        memcpy(&intF, mBuffer + mOffset, sizeof(float));
        f = *((float*)&intF);
    }
    mOffset += sizeof(float);
    return true;
}
bool SBufferStream::readDouble(double& d)
{}
bool SBufferStream::readNetAddress(SNetAddress& na)
{}

