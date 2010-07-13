#include "SE_Buffer.h"
SE_BufferOutput::SE_BufferOutput(bool netOrder)
{
    mLen = DEFAULT_LEN;
    mOffset = 0;
    mNetOrder = netOrder;
    mData = new char[mLen];
}
SE_BufferOutput::SE_BufferOutput(int size, bool netOrder)
{
    mLen = size;
    mNetOrder = netOrder;
    mOffset = 0;
    mData = new char[mLen];
}
SE_BufferOutput::~SE_BufferOutput()
{
    delete[] mData;
}
void SE_BufferOutput::writeByte(char c)
{
    writeBytes(&c, 1);
}
void SE_BufferOutput::writeInt(int i)
{
    int outI = i
    if(mNetOrder)
    {
        outI = SE_Util::host2NetInt32(i);
    }
    writeBytes(&outI, 4);
}
void SE_BufferOutput::writeShort(short s)
{
    short outS = s;
    if(mNetOrder)
    {
        outs = SE_Util::host2NetInt16(s);
    }
    writeBytes(&outS, 2);
}
void SE_BufferOutput::writeFloat(float f)
{
    int* outF = (int*)&f;
    float tmp;
    if(mNetOrder)
    {
        tmp = SE_Util::host2NetInt32(*outF);
        outF = (int*)&tmp;
    }
    writeBytes(outF, 4);
}
void SE_BufferOutput::writeVector2f(const SE_Vector2f& v)
{
    for(int i = 0 ; i < 2 ; i++)
    {
        writeFloat(v.d[i]);
    }
}
void SE_BufferOutput::writeVector3f(const SE_Vector3f& v)
{
    for(int i = 0 ; i < 3 ; i++)
    {
        writeFloat(v.d[i]);
    }
}
void SE_BufferOutput::writeVector3i(const SE_Vector3i& v)
{
    for(int i = 0 ; i < 3 ; i++)
    {
        writeInt(v.d[i]);
    }
}
void SE_BufferOutput::writeVector4f(const SE_Vector4f& v)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        writeFloat(v.d[i]);
    }
}
void SE_BufferOutput::writeMatrix2f(const SE_Matrix2f& m)
{
    for(int i = 0 ; i < 2 ; i++)
    {
        for(int j = 0 ; j < 2 ; j++)
            writeFloat(m.get(i, j));
    }
}
void SE_BufferOutput::writeMatrix3f(const SE_Matrix3f& m)
{
    for(int i = 0 ; i < 3 ; i++)
    {
        for(int j = 0 ; j < 3 ; j++)
            writeFloat(m.get(i, j));
    }
}
void SE_BufferOutput::writeMatrix4f(const SE_Matrix4f& m)
{
    for(int i = 0 ; i < 4 ; i++)
    {
        for(int j = 0 ; j < 4 ; j++)
            writeFloat(m.get(i, j));
    }
}
void SE_BufferOutput::writeQuat(const SE_Quat& q)
{
    writeVector4f(q.toVector4f());
}
void SE_BufferOutput::writeString(const char* str, int count)
{
    writeInt(count);
    writeBytes(str, count);
}
void SE_BufferOutput::writeString(const char* str)
{
    int len = strlen(str);
    writeString(str, len);
}
void SE_BufferOutput::writeBytes(const char* cb, int count)
{
    if((mOffset + count) <= mLen)
    {
        memcpy(mData + mOffset, cb, count);
        mOffset += count;
    }
    else
    {
        int newLen = mLen + count + DEFAULT_LEN * 3 / 4;
        char* newData = new char[newLen];
        if(!newData)
            return;
        memcpy(newData, mData, mLen);
        delete[] mData;
        mLen = newLen;
        mData = newData;
        memcpy(mData + mOffset, cb, count);
        mOffset += count;        
    }
}
void SE_BufferOutput::writeIntArray(int* ia, int count)
{
    for(int i = 0 ; i < count ; i++)
    {
        writeInt(ia[i]);
    }
}
void SE_BufferOutput::writeShortArray(short* sa, int count)
{
    for(int i = 0 ; i < count ; i++)
    {
        writeShort(sa[i]);
    }
}
void SE_BufferOutput::writeFloatArray(float* fa, int count)
{
    for(int i = 0 ; i < count ; i++)
    {
        writeFloat(fa[i]);
    }
}
const char* SE_BufferOutput::getData()
{
    return mData;
}
int SE_BufferOutput::getDataLen()
{
    return mOffset;
}
