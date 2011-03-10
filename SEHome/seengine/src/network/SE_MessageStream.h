#ifndef SE_MESSAGESTREAM_H
#define SE_MESSAGESTREAM_H
#include <memory>
using namespace std ;
// the first byte of data is the messageID
struct SE_NetMessage
{
    SE_NetMessage()
    {
        data = NULL;
        len = 0;
    }
    ~SE_NetMessage()
    {
        release();
    }
    void release()
    {
        if(data != NULL)
        {
            delete[] data;
            len = 0;
            data = NULL;
        }
    }
    unsigned char* data;
    int len;
};
class SE_NetMessagePacket
{
public:
    SE_NetMessagePacket()
    {
        //mPrev = mNext = NULL;
        mData = NULL;
        len = 0;
        offset = 0;
        mOwn = false;
    }
    ~SE_NetMessagePacket()
    {
        if(mData != NULL)
            delete[] mData;
    }
    bool isAllConsumed() const
    {
        return offset == len;
    }
    unsigned char* mData;
    int len;
    int offset;
    bool mOwn;
};
/*
 * this functor will be invoked when travel through message paceket list
 * it will lock message packet list, so dont do long time computation in 
 * this functor.
 * */
class SE_NetMessagePacketFunctor
{
public:
    virtual void handleMessagePacket(SE_NetMessagePacket* packet) {}
};
class SE_NetMessageStream
{
public:
    enum {SE_NO_ERROR, SE_WAIT_MORE};
    SE_NetMessageStream();
    ~SE_NetMessageStream();
    int getNextMessage(SE_NetMessage* out);
    //if own is false, addMessagePacket will copy data to its own buffer
    //
    int addMessagePacket(unsigned char* data, int len, bool own = false);
    int getMessagePacketCount();
    void mapMessagePacket(SE_NetMessagePacketFunctor& functor, bool clearPacketList = false);
private:
    class SE_NetMessageStreamImpl;
    auto_ptr<SE_NetMessageStreamImpl> mImpl;
};
#endif
