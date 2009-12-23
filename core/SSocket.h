#ifndef SSOCKET_H
#define SSOCKET_H
#include "SType.h"
#include "SNetAddress.h"
#if defined(WIN32)
#include <winsock2.h>
typedef SOCKET SSOCKET_TYPE;
typedef int socklen_t;
#else
typedef int SSOCKET_TYPE;
typedef -1 SOCKET_ERROR;
typedef -1 INVALID_SOCKET;
#endif
enum {STREAM, DATAGRAM};
enum {S_NO_ERROR, CREATE_ERROR, BIND_ERROR, LISTEN_ERROR, CONNECT_ERROR, ACCEPT_ERROR};
class SSocket
{
public:
    SSocket(SSOCKET_TYPE fd);
    SSocket()
    {
        mSocket = INVALID_SOCKET;
    }
    ~SSocket();
    void setSocket(SSOCKET_TYPE fd)
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
    SSOCKET_TYPE mSocket;
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
    //enum {NO_ERROR, CREATE_ERROR, CONNECT_ERROR};
    SSocketClient(int tranferType,const SNetAddress& address);
    ~SSocketClient();
    int send(const unsigned char* data, int size);
    int read(unsigned char* outBuffer, int size);
    int getError();
    int getSocket()
    {
        return mRemote.getSocket();
    }
private:
    SSocket mRemote;
    int mError;
};
#endif
