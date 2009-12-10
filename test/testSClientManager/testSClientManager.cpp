#include "SClientManager.h"
#include "SClient.h"
#include "SCommandFactory.h"
#include "SNetAddress.h"
#include "SSocket.h"
int main(int argc, char** argv)
{
    SCommandFactory* factory = new SCommandFactory;
    SClientManager* instance = SClientManager::getInstance();
    instance->init(factory);
    SNetAddress sa(1, 1);
    SSocket s(1);
    instance->addClient(sa, s);
    sa.setIp(2);
    sa.setPort(2);
    s.setSocket(2);
    instance->addClient(sa, s);
}
