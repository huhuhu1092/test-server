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
    unsigned char* data;
    int len;
};

class SMessageStream
{
public:
    enum {NO_ERROR, WAIT_MORE};
    SMessageStream();
    ~SMessageStream();
    int getNextMessage(SMessage* out);
    int addMessagePacket(unsigned char* data, int len);
    int getMessagePacketCount();
private:
    class SMessageStreamImpl;
    auto_ptr<SMessageStreamImpl> mImpl;
};
#endif
