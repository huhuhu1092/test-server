#include "SMessageStream.h"
#include "SType.h"
#include "SUtil.h"
static void setDataLen(uint8_t* data, uint16_t len)
{
    uint16_t data_len = SUtil::Host2NetInt16(len);
    memcpy(data, &data_len, 2);
}
int main(int argc, char** argv)
{
    fprintf(stderr, "#### start test SMessageStream #######\n");
    SMessageStream ss;
    uint8_t* data = new uint8_t[100];
    memset(data, 'a', 100);
    data[0] = 151;
    uint16_t data_len = SUtil::Host2NetInt16(50);
    memcpy(data + 1, &data_len, 2);
    ss.addMessagePacket(data, 2);
    ss.addMessagePacket(data + 2, 48);
    SASSERT(ss.getMessagePacketCount() == 2);
    SMessage m;
    int ret = ss.getNextMessage(&m);
    SASSERT(ss.getMessagePacketCount() == 0);
    SASSERT(ret == SMessageStream::NO_ERROR);
    ret = ss.getNextMessage(&m);
    SASSERT(ret == SMessageStream::WAIT_MORE);
    SASSERT(m.len == 50);
    memcpy(&data_len, m.data + 1, 2);
    data_len = SUtil::Net2HostInt16(data_len);
    SASSERT(data_len == 50);  
    SASSERT(m.data[0] == 151);
    uint8_t* dataStart = m.data + 3;
    for(int i = 0 ; i < 47 ; i++)
    {
        SASSERT(dataStart[i] == 'a');
    }
    delete []data;
    //delete []m.data;
    fprintf(stderr, "case 1 pass\n");

    data = new uint8_t[1000];
    memset(data, 'b', 1000);
    data[0] = 152;
    setDataLen(data + 1, 1000);
    ss.addMessagePacket(data, 100);
    ss.addMessagePacket(data + 100, 500);
    ret = ss.getNextMessage(&m);
    SASSERT(ret == SMessageStream::WAIT_MORE);
    SASSERT(ss.getMessagePacketCount() == 2);
    ss.addMessagePacket(data + 600, 400);
    ret = ss.getNextMessage(&m);
    SASSERT(ret == SMessageStream::NO_ERROR);
    SASSERT(ss.getMessagePacketCount() == 0);
    delete[] data;
    //delete[] m.data;
    fprintf(stderr, "case 2 pass\n");


    fprintf(stderr, "### end test SMessageStream ####\n");
    return 0;
}
