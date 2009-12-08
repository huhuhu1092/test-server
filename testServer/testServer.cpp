#include "SSocket.h"
#include "SMessageStream.h"
#include "SUtil.h"
#include <stdio.h>
#include <stdlib.h>
#include <list>
using namespace std;
class SClient
{
public:
    SClient(const SClientProp& sp) : mClientAccessPoint(sp)
    {}
    void process();
private:
    SClientProp mClientAccessPoint;
    SMessageStream mClientMessageStream;
};
void SClient::process()
{

}
//////////////////////
class SClientManager
{
public:
    void addClient(const SClientProp& sp);
    void removeClient(const SClientProp& sp);
    void process();
private:
    list<SClient*> mClients;
};
////////////
int main(int argc, char** argv)
{
    SNetAddress serverAddress((const char*)NULL, SUtil::Host2NetInt16(10000));
    SSocketServer ss(STREAM, serverAddress);
    while(true)
    {
        SClientProp cp = ss.accept();

    }
    return 1; 
}
