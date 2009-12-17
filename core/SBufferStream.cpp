#include "SBufferStream.h"
#include "SUtil.h"
#include "SLog.h"
#include <cstring>
using namespace std;
SBufferStreamOutput::SBufferStreamOutput(char* data, int len, bool own, bool netOrder)
{
    SASSERT(data != 0 && len > 0);
    mBuffer = data;
    mLen = len;
    mOwn = own;
    mNetOrder = netOrder;
    mOffset = 0;
}
SBufferStreamOutput::SBufferStreamOutput(int len, bool netOrder)
{
    SASSERT(len > 0);
    mLen = len;
    mBuffer = new char[mLen];
    mOwn = true;
    mNetOrder = netOrder;
    mOffset = 0;
}
SBufferStreamOutput::~SBufferStreamOutput()
{
    if(mOwn)
        delete[] mBuffer;
}
const char* SBufferStreamOutput::getBuffer()
{
    return mBuffer;
}
bool SBufferStreamOutput::writeChar(char c)
{
    if(mOffset + 1 > mLen)
        return false;
    memcpy(mBuffer + mOffset, &c, 1);
    mOffset++;
    return true;
}
bool SBufferStreamOutput::writeString(const char* str)
{
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
bool SBufferStreamOutput::writeInt(int i)
{
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
bool SBufferStreamOutput::writeShort(short int i)
{
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
bool SBufferStreamOutput::writeFloat(float f)
{
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
bool SBufferStreamOutput::writeDouble(double d)
{
    return false;
}
bool SBufferStreamOutput::writeNetAddress(const SNetAddress& na)
{
    return false;
}
///////////////////////////////////////////
SBufferStreamInput::SBufferStreamInput(const char* data, int len, bool own, bool netOrder)
{
    SASSERT(data != 0 && len > 0);
    mBuffer = data;
    mLen = len;
    mOwn = own;
    mNetOrder = netOrder;
    mOffset = 0;

}
SBufferStreamInput::~SBufferStreamInput()
{
    if(mOwn)
        delete[] mBuffer;
}

bool SBufferStreamInput::readChar(char& c)
{    
    if(mOffset + 1 > mLen)
        return false;
    memcpy(&c, mBuffer + mOffset, 1);
    mOffset += 1;
    return true;
}
bool SBufferStreamInput::readString(char*& str, int& len)
{
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
bool SBufferStreamInput::readInt(int& i)
{
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
bool SBufferStreamInput::readShort(short int& i)
{
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
bool SBufferStreamInput::readFloat(float& f)
{
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
bool SBufferStreamInput::readDouble(double& d)
{
    return false;
}
bool SBufferStreamInput::readNetAddress(SNetAddress& na)
{
    return false;
}

