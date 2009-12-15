#ifndef SSOCKET_H
#define SSOCKET_H
#include "SType.h"
#include "SNetAddress.h"
enum {STREAM, DATAGRAM};
class SSocket
{
public:
    SSocket(int fd);
    SSocket()
    {
        mSocket = -1;
    }
    ~SSocket();
    void setSocket(int fd)
    {
        mSocket = fd;
    }
    int getSocket() const
    {
        return mSocket;
    }
    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
    int close();
private:
    int mSocket;
};
struct SClientProp
{
    SClientProp(SSocket s, SNetAddress a) : socket(s), address(a)
    {
    }
    SClientProp()
    {}
    SSocket socket;
    SNetAddress address;
};
class SSocketServer
{
public:
    enum {NO_ERROR, CREATE_ERROR, BIND_ERROR, LISTEN_ERROR, ACCEPT_ERROR};
    SSocketServer(int transferType, const SNetAddress& address);
    ~SSocketServer();
    int getError()
    {
        return mError;
    }
    SClientProp accept();
private:
    SSocket mServer;
    int mListenNum;
    int mError;
};
class SSocketClient
{
public:
    enum {NO_ERROR, CREATE_ERROR, CONNECT_ERROR};
    SSocketClient(int tranferType,const SNetAddress& address);
    ~SSocketClient();
    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
    int getError();
private:
    SSocket mRemote;
    int mError;
};
#endif
