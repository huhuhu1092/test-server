#ifndef SMESSAGESTREAM_H
#define SMESSAGESTREAM_H
#include <memory>
using namespace std ;
// the first byte of data is the messageID
struct SMessage
{
    SMessage()
    {
        data = NULL;
        len = 0;
    }
    ~SMessage()
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
class SMessagePacket
{
public:
    SMessagePacket()
    {
        //mPrev = mNext = NULL;
        mData = NULL;
        len = 0;
        offset = 0;
        mOwn = false;
    }
    ~SMessagePacket()
    {
        if(mData != NULL && mOwn)
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
class SMessagePacketFunctor
{
public:
    virtual void handleMessagePacket(SMessagePacket* packet) {}
};
class SMessageStream
{
public:
    enum {NO_ERROR, WAIT_MORE};
    SMessageStream();
    ~SMessageStream();
    int getNextMessage(SMessage* out);
    int addMessagePacket(unsigned char* data, int len, bool own = false);
    int getMessagePacketCount();
    void mapMessagePacket(SMessagePacketFunctor& functor, bool clearPacketList = false);
private:
    class SMessageStreamImpl;
    auto_ptr<SMessageStreamImpl> mImpl;
};
#endif
