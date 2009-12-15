#include "SClientManager.h"
#include "SClient.h"
#include "SCommandEventFactory.h"
#include "SNetAddress.h"
#include "SSocket.h"
#include "SUtil.h"
#include "SBufferStream.h"
#include "SLog.h"
#include "../../server/SCommandEventDefine.h"
int main(int argc, char** argv)
{
    SNetAddress na("192.168.2.184", SUtil::Host2NetInt16(10000));
    SSocketClient client(STREAM, na);
    if(client.getError() != SSocketClient::NO_ERROR)
        return -1;
    SLoginCommandEvent se("aa", "bb");
    char* out;
    int len;
    se.pack(out, len);
    int ret = client.send((unsigned char*)out, len);
    SLog::msg("#### write num = %d ####\n", ret);
    unsigned char buffer[258 * 1024];
    client.read(buffer, 258 * 1024);
    return 0;
    /*
    SCommandEventFactory* factory = new SCommandEventFactory;
    SClientManager* instance = SClientManager::getInstance();
    instance->init(factory);
    SNetAddress sa(1, 1);
    SSocket s(1);
    instance->addClient(sa, s);
    sa.setIp(2);
    sa.setPort(2);
    s.setSocket(2);
    instance->addClient(sa, s);
    */
}
