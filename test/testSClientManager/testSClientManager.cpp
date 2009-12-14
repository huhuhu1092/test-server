#include "SClientManager.h"
#include "SClient.h"
#include "SCommandEventFactory.h"
#include "SNetAddress.h"
#include "SSocket.h"
#include "SUtil.h"
int main(int argc, char** argv)
{
    SNetAddress na("192.168.2.184", SUtil::Host2NetInt16(10000));
    SSocketClient client(STREAM, na);
    if(client.getError() != SSocketClient::NO_ERROR)
        return -1;
    char buf[100];
    buf[0] = 1;
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
